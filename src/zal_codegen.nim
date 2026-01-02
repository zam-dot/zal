# zal_codegen.nim - Generate C code from AST
import zal_parser
import std/[strutils, tables]

# =========================== CONTEXT TYPES ============================
type CodegenContext* = enum
  cgGlobal
  cgFunction
  cgExpression
  cgArena 

# =========================== REFERENCE COUNTING IMPLEMENTATION ============================
const RC_HEADER {.used.} = r"""
#ifndef ZAL_ARENA_H
#define ZAL_ARENA_H
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
typedef struct {
    size_t refcount;
    size_t weak_count;
    size_t array_count;
} RCHeader;
#define RC_HEADER_SIZE sizeof(RCHeader)
#define RC_GET_HEADER(ptr) ((RCHeader*)((char*)(ptr) - RC_HEADER_SIZE))
#define ZAL_RELEASE(ptr) do { rc_release(ptr); ptr = NULL; } while(0)
static inline void rc_weak_retain(void *ptr) {
    if (ptr) {
        RCHeader *header = RC_GET_HEADER(ptr);
        header->weak_count++;
    }
}
static inline void* rc_alloc(size_t size) {
    RCHeader* header = (RCHeader*)calloc(1, RC_HEADER_SIZE + size);
    if (header) {
        header->refcount = 1;
        header->weak_count = 0;
        header->array_count = 0; // Default
    }
    return header ? (char*)header + RC_HEADER_SIZE : NULL;
}
static inline void* rc_alloc_array(size_t elem_size, size_t count) {
    RCHeader* header = (RCHeader*)calloc(1, sizeof(RCHeader) + (elem_size * count));
    if (header) {
        header->refcount = 1;
        header->weak_count = 0;
        header->array_count = count; // <--- Store it here!
        memset((char*)header + RC_HEADER_SIZE, 0, elem_size * count);
    }
    return header ? (char*)header + RC_HEADER_SIZE : NULL;
}
static inline void rc_release(void *ptr) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);

    if (--header->refcount == 0) {
        if (header->weak_count == 0) {
            free(header);
        }
    }
}
#define rc_new_array(type, count) (type*)rc_alloc_array(sizeof(type), count)
#define rc_string_new(str) ({ \
    const char* _s = (str); \
    size_t _len = _s ? strlen(_s) : 0; \
    char* _d = (char*)rc_alloc(_len + 1); \
    if (_d) { strcpy(_d, _s); } \
    _d; \
})
static inline void rc_retain(void* ptr) {
    if (ptr) {
        RCHeader* header = RC_GET_HEADER(ptr);
        header->refcount++; // [cite: 6]
    }
}
static inline void rc_release_array(void *ptr, void (*destructor)(void*)) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);
    if (--header->refcount == 0) {
        if (destructor) {
            void **array = (void **)ptr;
            for (size_t i = 0; i < header->array_count; i++) {
                destructor(array[i]);
            }
        }
        if (header->weak_count == 0) {
            free(header);
        }
    }
}
static inline void rc_weak_release(void *ptr) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);
    if (--header->weak_count == 0) {
        if (header->refcount == 0) {
            free(header);
        }
    }
}
#endif
"""

# In zal_codegen.nim, add this constant (or rename your existing one):
const ARENA_HEADER = r"""
#ifndef ARENA_H
#define ARENA_H
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

typedef struct {
    uint8_t *buffer;
    size_t   offset;
    size_t   capacity;
} Arena;

typedef struct {
    size_t size;  // Store array size!
} ArenaArrayHeader;

#define ARENA_ARRAY_HEADER_SIZE sizeof(ArenaArrayHeader)
#define ARENA_GET_HEADER(ptr) ((ArenaArrayHeader*)((char*)(ptr) - ARENA_ARRAY_HEADER_SIZE))

static inline void *arena_alloc_with_size(Arena *a, size_t size, size_t array_size) {
    if (size == 0) return NULL;
    
    size_t total_size = ARENA_ARRAY_HEADER_SIZE + size;
    size_t aligned_size = (total_size + 7) & ~7;
    
    if (a->offset + aligned_size <= a->capacity) {
        void *ptr = &a->buffer[a->offset + ARENA_ARRAY_HEADER_SIZE];
        ArenaArrayHeader *header = (ArenaArrayHeader*)&a->buffer[a->offset];
        header->size = array_size;
        a->offset += aligned_size;
        return ptr;
    }
    return NULL;
}

static inline void* arena_alloc_array_with_size(Arena *a, size_t elem_size, size_t count) {
    size_t total_size = elem_size * count;
    void *ptr = arena_alloc_with_size(a, total_size, count);
    if (ptr) {
        memset(ptr, 0, total_size);
    }
    return ptr;
}

static inline size_t arena_array_len(void *ptr) {
    if (!ptr) return 0;
    return ARENA_GET_HEADER(ptr)->size;
}

// Keep old functions for backward compatibility
static inline void *arena_alloc(Arena *a, size_t size) {
    return arena_alloc_with_size(a, size, 0);
}

static inline void* arena_alloc_array(Arena *a, size_t elem_size, size_t count) {
    return arena_alloc_array_with_size(a, elem_size, count);
}

static inline void arena_reset(Arena *a) { a->offset = 0; }
static inline char* arena_string_new(Arena *a, const char* str) {
    if (!str) return NULL;
    size_t len = strlen(str);
    char *result = (char*)arena_alloc_with_size(a, len + 1, len + 1);
    if (result) {
        strcpy(result, str);
    }
    return result;
}
static inline Arena arena_init_dynamic(size_t capacity) {
    uint8_t *buffer = (uint8_t *)calloc(1, capacity);
    if (!buffer) {
        fprintf(stderr, "ERROR: Failed to allocate %zu bytes for arena\n", capacity);
        exit(1);
    }
    return (Arena){.buffer = buffer, .offset = 0, .capacity = capacity};
}
static inline void arena_free(Arena *a) {
    if (a->buffer) {
        free(a->buffer);
        a->buffer = NULL;
    }
    a->capacity = 0;
    a->offset = 0;
}
static inline Arena arena_init(void *backing_buffer, size_t capacity) {
    return (Arena){.buffer = (uint8_t *)backing_buffer, .offset = 0, .capacity = capacity};
}
#endif
"""

var 
  rcVariables = initTable[string, bool]()
  arenaVariables = initTable[string, bool]()
  maxArenaSize = 262144  # Default 256KB
  actualArenaSize = 0

# =========================== HELPER FUNCTIONS ============================
proc indentLine(code: string, context: CodegenContext): string =
  case context
  of cgFunction:
    "  " & code
  else:
    code

proc escapeString(str: string): string =
  for ch in str:
    case ch
    of '\n': result &= "\\n"
    of '\t': result &= "\\t"
    of '\r': result &= "\\r"
    of '\\': result &= "\\\\"
    of '"':  result &= "\\\""
    else:    result &= ch

# =========================== FORWARD DECLARATIONS ============================
proc generateExpression(node: Node): string
proc generateBlock(node: Node, context: CodegenContext): string
proc generateSwitch(node: Node, context: CodegenContext): string
proc generateForRange(node: Node, context: CodegenContext): string
proc generateCall(node: Node, context: CodegenContext, errorVar: string = ""): string 
proc generateFieldAccess(node: Node): string
proc generateAddressOf(node: Node): string
proc generateDeref(node: Node): string
proc generateStructLiteral(node: Node): string
proc generateIndexExpr(node: Node): string
proc generateArrayLiteral(node: Node): string
proc generateArrayType(node: Node): string
proc getCleanupString(vars: seq[tuple[name: string, typeName: string, isArray: bool]]): string
# reference counting forward declarations:
proc generateRcNew(node: Node, context: CodegenContext): string
proc generateRcRetain(node: Node, context: CodegenContext): string
proc generateRcRelease(node: Node, context: CodegenContext): string
proc generateWeakRef(node: Node, context: CodegenContext): string
proc generateStrongRef(node: Node, context: CodegenContext): string
proc generateRcInit(node: Node, context: CodegenContext): string

# =========================== REFERENCE COUNTING HELPER FUNCTIONS ============================
proc generateRcRetainStmt(varName: string): string = 
  "rc_retain(" & varName & ");\n"

proc generateRcReleaseStmt(varName: string): string = 
  "rc_release(" & varName & ");\n"

proc isRcVariable(varName: string): bool =
  rcVariables.hasKey(varName)

# ======================== REFERENCE COUNTING GENERATORS (for AST nodes) =========================
proc generateRcRetain(node: Node, context: CodegenContext): string =
  let target = generateExpression(node.rcTarget)
  var code = "rc_retain(" & target & ")"
  
  if context != cgExpression:
    code &= ";\n"
    return indentLine(code, context)
  else: return code

# ======================== REFERENCE COUNTING GENERATORS (for AST nodes) =========================
proc resetCodegenState*() =
  rcVariables.clear()
  arenaVariables.clear()
  actualArenaSize = 0

# ======================== REFERENCE COUNTING GENERATORS (for AST nodes) ========================
proc generateRcRelease(node: Node, context: CodegenContext): string =
  let target = generateExpression(node.rcTarget)
  var code = "rc_release(" & target & ")"
  
  if context != cgExpression:
    code &= ";\n"
    return indentLine(code, context)
  else: return code

# =========================== ARRAY TYPE DETECTION ============================
proc isArrayType(typeName: string): (bool, string, string) =
  if typeName.contains("{") and typeName.contains("}"):
    let 
      bracketPos = typeName.find('[')
      closeBracket = typeName.find(']')
    if closeBracket > bracketPos:
      let 
        elemType = typeName[0..<bracketPos]
        size = typeName[bracketPos+1..<closeBracket]
      return (true, elemType, size)
  
  if typeName.endsWith("*"):
    if typeName == "char*": return (false, "", "")
    let baseType = typeName[0..^2].strip()
    if " " notin baseType: return (true, baseType, "")
  return (false, "", "")

proc generateWeakRef(node: Node, context: CodegenContext): string =
  let target = generateExpression(node.refTarget)
  var code = "rc_weak(" & target & ")"
  
  if context != cgExpression:
    code &= ";\n"
    return indentLine(code, context)
  else: return code

proc generateStrongRef(node: Node, context: CodegenContext): string =
  let target = generateExpression(node.refTarget)
  var code = "rc_weak_to_strong(" & target & ")"
  
  if context != cgExpression:
    code &= ";\n"
    return indentLine(code, context)
  else: return code

# =========================== REFERENCE COUNTING HELPERS ============================
proc isReferenceCountedType(typeName: string): bool =
  if typeName.len == 0: return false
  
  let (isArray, _, _) = isArrayType(typeName)
  if isArray: return true
  
  if typeName.endsWith("*"):
    if typeName == "char*": return true
    let baseType = typeName[0..^2].strip()
    return isReferenceCountedType(baseType)
  
  case typeName
  of "string", "String": return true
  else:
    return false

# =========================== REFERENCE COUNTING GENERATORS ============================
proc generateRcNew(node: Node, context: CodegenContext): string =
  var 
    code = ""
    typeName = node.rcType

  if typeName == "string":
    typeName = "char*"

  let (isArray, elemType, size) = isArrayType(typeName)
  if isArray:
    code = "rc_new_array(" & elemType & ", " & size & ")"
    
    if node.rcArgs.len > 0:
      code &= ";\n"
      code &= "for (int i = 0; i < " & size & "; i++) {\n"
      code &= "  " & elemType & " initData = " & generateExpression(node.rcArgs[0]) & ";\n"
      code &= "  memcpy(&array[i], &initData, sizeof(" & elemType & "));\n"
      code &= "}\n"
  else:
    if typeName == "char*":
      if node.rcArgs.len == 1: code = "rc_string_new(" & generateExpression(node.rcArgs[0]) & ")"
      else: code = "rc_string_new(\"\")"
    else:
      code = "rc_new(" & typeName
      if node.rcArgs.len > 0:
        code &= ", "
        for i, arg in node.rcArgs:
          if i > 0: code &= ", "
          code &= generateExpression(arg)
      code &= ")"
  
  if context != cgExpression:
    code &= ";\n"
    return indentLine(code, context)
  else: return code

# =========================== INITIALIZATION GENERATORS ============================
proc generateRcInit(node: Node, context: CodegenContext): string =
  var 
    code = ""
    hasStructLiteralStyle = false
  if node.rcArgs.len > 0:
    let firstArg = node.rcArgs[0]
    hasStructLiteralStyle = 
      firstArg.kind == nkBinaryExpr and 
      firstArg.op == "=" and
      firstArg.left.kind == nkIdentifier
  
  if hasStructLiteralStyle:
    let tempVar = "__rc_init_" & $node.line & "_" & $node.col
    var initCode = node.rcType & " " & tempVar & " = "
    
    var fields: seq[string] = @[]
    for arg in node.rcArgs:
      if arg.kind == nkBinaryExpr and arg.op == "=":
        let 
          fieldName = generateExpression(arg.left)
          fieldValue = generateExpression(arg.right)
        fields.add("." & fieldName & " = " & fieldValue)
    
    if fields.len > 0: initCode &= "{" & fields.join(", ") & "};\n"
    else: initCode &= "{};\n"
    
    initCode &= node.rcType & "* __rc_result = rc_new(" & node.rcType & ", " & tempVar & ");\n"
    code = initCode
    
    if context == cgExpression: code = "__rc_result"
    else: code = initCode
      
  else:
    code = "rc_new(" & node.rcType
    
    if node.rcArgs.len > 0:
      code &= ", "
      for i, arg in node.rcArgs:
        if i > 0: code &= ", "
        code &= generateExpression(arg)
    code &= ")"
  
  if context != cgExpression:
    code &= ";\n"
    return indentLine(code, context)
  else: return code

# ========================= BASIC EXPRESSION GENERATORS =========================
proc generateLiteral(node: Node): string =
  case node.kind
  of nkLiteral:
    if node.literalValue == "NULL": "NULL" else: node.literalValue
  of nkStringLit: "\"" & escapeString(node.literalValue) & "\""
  else: ""

# =========================== IDENTIFIER GENERATORS =============================
proc generateIdentifier(node: Node): string {.used.} =
  node.identName

# =========================== DEREFERENCE GENERATORS ============================
proc generateDeref(node: Node): string =
  return "*" & generateExpression(node.operand)

# =========================== ADDRESS OF GENERATORS ============================
proc generateAddressOf(node: Node): string =
  return "&" & generateExpression(node.operand)

# =========================== EXPRESSION GENERATORS ============================
proc generateExpression(node: Node): string =
  if node == nil:
    return ""

  case node.kind
  of nkFieldAccess: result = generateFieldAccess(node)
  of nkCall: result = generateCall(node, cgExpression)
  of nkStructLiteral: result = generateStructLiteral(node)
  of nkIndexExpr: result = generateIndexExpr(node)
  of nkArrayLit, nkArenaArrayLit: result = generateArrayLiteral(node)  # Handle both here!
  of nkBinaryExpr: result =
      generateExpression(node.left) & " " & node.op & " " &
      generateExpression(node.right)
  of nkIdentifier: result = node.identName
  of nkArrayType: result = generateArrayType(node)
  of nkLiteral, nkStringLit: result = generateLiteral(node)
  of nkAddressOf: result = generateAddressOf(node)
  of nkDeref: result = generateDeref(node)
  of nkGroup: result = "(" & generateExpression(node.groupExpr) & ")"
  of nkRcNew: result = generateRcNew(node, cgExpression)
  of nkRcInit: result = generateRcInit(node, cgExpression)
  of nkRcRetain: result = generateRcRetain(node, cgExpression)
  of nkRcRelease: result = generateRcRelease(node, cgExpression)
  of nkWeakRef: result = generateWeakRef(node, cgExpression)
  of nkStrongRef: result = generateStrongRef(node, cgExpression)
  else: result = "/* ERROR: unhandled expression */"

# =========================== GROUP GENERATORS ============================
proc generateGroup(node: Node, context: CodegenContext): string {.used.} =
  "(" & generateExpression(node.groupExpr) & ")"

# =========================== LOOP GENERATORS ============================
proc generateFor(node: Node, context: CodegenContext): string {.used.} =
  var code = ""

  if node.forInit == nil and node.forCondition != nil and node.forUpdate == nil:
    code = "while (" & generateExpression(node.forCondition) & ") {\n"
  elif node.forInit == nil and node.forCondition == nil and node.forUpdate == nil:
    code = "while (1) {\n"
  else:
    code = "for ("
    if node.forInit != nil:
      case node.forInit.kind
      of nkVarDecl: code &= "int " & node.forInit.varName & " = " & generateExpression(node.forInit.varValue)
      of nkAssignment: code &= generateExpression(node.forInit.left) & " = " & generateExpression(node.forInit.right)
      else: code &= generateExpression(node.forInit)
    code &= "; "

    if node.forCondition != nil: code &= generateExpression(node.forCondition)
    code &= "; "
    if node.forUpdate != nil: code &= generateExpression(node.forUpdate)
    code &= ") {\n"

  let bodyCode = generateBlock(node.forBody, cgFunction)
  for line in bodyCode.splitLines:
    if line.len > 0: code &= "  " & line & "\n"
  code &= "}\n"

  return indentLine(code, context)

# =========================== RANGE GENERATORS ============================
proc generateForRange(node: Node, context: CodegenContext): string =
  if node.rangeTarget == nil:
    return indentLine("/* ERROR: No range target */\n", context)

  var
    isRange = false
    startVal, endVal: string
    code = ""

  if node.rangeTarget.kind == nkBinaryExpr and node.rangeTarget.op == "..":
    isRange = true
    if node.rangeTarget.left != nil:  startVal = generateExpression(node.rangeTarget.left)
    if node.rangeTarget.right != nil: endVal = generateExpression(node.rangeTarget.right)
  
  if isRange:
    if node.rangeValue != nil: 
      code = "for (int " & node.rangeValue.identName & " = " & startVal & "; " &
          node.rangeValue.identName & " <= " & endVal & "; " & node.rangeValue.identName & "++) {\n"
    elif node.rangeIndex != nil: 
      code = "for (int " & node.rangeIndex.identName & " = " & startVal & "; " &
          node.rangeIndex.identName & " <= " & endVal & "; " & node.rangeIndex.identName & "++) {\n"
    else: code = "for (int _i = " & startVal & "; _i <= " & endVal & "; _i++) {\n"
  else:
    let target = generateExpression(node.rangeTarget)
    var isString = false

    case node.rangeTarget.kind
    of nkStringLit: isString = true
    of nkIdentifier:
      let name = node.rangeTarget.identName
      if name == "s" or name == "str" or name == "text" or name.endsWith("Str") or name.endsWith("String"): 
        isString = true
    else:
      if target.startsWith("\"") or target.contains("char*"): isString = true

    if isString:
      code = "for (int _i = 0; " & target & "[_i] != '\\0'; _i++) {\n"
      if node.rangeIndex != nil: code &= "  int " & node.rangeIndex.identName & " = _i;\n"
      if node.rangeValue != nil: code &= "  char " & node.rangeValue.identName & " = " & target & "[_i];\n"
    else:
      var isArenaArray = false
      
      if node.rangeTarget.kind == nkIdentifier:
        let varName = node.rangeTarget.identName
        if arenaVariables.hasKey(varName):
          isArenaArray = true
          stderr.writeLine("WARNING: Can't get size of arena array '", varName, "' in for loop")
      
      if isArenaArray:
        code = "for (int _i = 0; _i < /* UNKNOWN: arena array size - using RC_GET_HEADER as fallback */ RC_GET_HEADER(" & target & ")->array_count; _i++) {\n"
      else:
        code = "for (int _i = 0; _i < RC_GET_HEADER(" & target & ")->array_count; _i++) {\n"
      
      if node.rangeIndex != nil: code &= "  int " & node.rangeIndex.identName & " = _i;\n"
      if node.rangeValue != nil:
        var elemType = "int" 
        if node.rangeTarget.kind == nkArrayLit and node.rangeTarget.elements.len > 0:
          let firstElem = node.rangeTarget.elements[0]
          if firstElem.kind == nkStringLit: elemType = "char*"
          elif firstElem.kind == nkLiteral:
            if firstElem.literalValue.contains('.') or firstElem.literalValue.contains('e') or
                firstElem.literalValue.contains('E'): 
              elemType = "double"
        code &= "  " & elemType & " " & node.rangeValue.identName & " = " & target & "[_i];\n"

  if node.rangeBody != nil:
    let bodyCode = generateBlock(node.rangeBody, cgFunction)
    for line in bodyCode.splitLines:
      if line.len > 0: code &= "  " & line & "\n"

  code &= "}\n"
  return indentLine(code, context)

# =========================== STATEMENT GENERATORS ============================
proc generateCBlock(node: Node, context: CodegenContext): string {.used.} =
  var cCode = node.cCode.strip(leading = false, trailing = true)
  let looksLikeFunction = (" int " in cCode or " void " in cCode or " char " in cCode or " float " in cCode or
                           " double " in cCode) and "(" in cCode and "){" in cCode
  if looksLikeFunction:
    if context == cgFunction: result = cCode.replace("\n", "\n  ")
    else: result = cCode
  else: result = cCode
  if result.len > 0 and result[^1] != '\n': result &= "\n"

# =========================== TYPE INFERENCE ============================
proc inferTypeFromIdentifier(name: string): string =
  if name == "argc": return "int"
  if name == "argv": return "char**"

  if name.endsWith("Ptr"):
    let 
      baseName = name[0..^4]
      baseType = inferTypeFromIdentifier(baseName)
    return baseType & "*"
  
  elif name.endsWith("_ptr"):
    let 
      baseName = name[0..^5]
      baseType = inferTypeFromIdentifier(baseName)
    return baseType & "*"
  
  elif name.startsWith("p_"):
    let 
      baseName = name[2..^1]
      baseType = inferTypeFromIdentifier(baseName)
    return baseType & "*"
  
  elif name.endsWith("Str") or name.endsWith("String") or name.endsWith("_str"):    return "char*"
  elif name.endsWith("Count") or name.endsWith("Size") or name.endsWith("Len"):     return "size_t"
  elif name.endsWith("Flag") or name.endsWith("Enabled") or name.startsWith("is"):  return "bool"
  elif name.endsWith("F") or name.endsWith("_f") or name.contains("Float"):         return "double"

  elif name == "str" or name == "text" or name == "message" or 
       name == "path" or name == "filename" or name == "url": return "char*"
  elif name == "count" or name == "length" or name == "size" or 
       name == "index" or name == "i" or name == "j" or name == "k": return "int"
  elif name == "flag" or name == "enabled" or name == "visible" or 
       name == "active" or name == "ready": return "bool"
  else: return "int"

# =========================== TYPE INFERENCE ============================
proc inferTypeFromExpression(node: Node): string =
  if node == nil: return "int"

  case node.kind
  of nkCall:
    let funcName = node.callFunc
    
    case funcName
    of "alloc":
      if node.callArgs.len >= 1:
        let typeArg = node.callArgs[0]
        var elemType = "int"
        
        case typeArg.kind
        of nkIdentifier: elemType = typeArg.identName
        of nkLiteral, nkStringLit: elemType = typeArg.literalValue
        else: elemType = "int"
    
        if elemType == "string": elemType = "char*"
        return elemType & "*" 

  of nkRcNew:
    var typeName = node.rcType
    if typeName == "string" or typeName == "char*":       return "char*" 
    if typeName.contains("[") and typeName.contains("]"): return typeName & "*"
    else: return typeName & "*"

  of nkRcInit:
    var typeName = node.rcType
    return typeName & "*"

  of nkAddressOf:
    let targetType = inferTypeFromExpression(node.operand)
    return targetType & "*"
  
  of nkDeref:
    let ptrType = inferTypeFromExpression(node.operand)
    if ptrType.endsWith("*"): return ptrType[0..^2].strip()
    else: return ptrType
  
  of nkLiteral:
    let val = node.literalValue
    if val == "NULL": return "void*" 
    if val.contains('.') or val.contains('e') or val.contains('E'): return "double"
    else:
      if val == "true" or val == "false": return "bool"
      return "int"
  
  of nkStructLiteral: return node.structType
  of nkStringLit: return "char*"
  of nkIdentifier: return inferTypeFromIdentifier(node.identName)
  
  of nkArrayLit:
    if node.elements.len > 0: return inferTypeFromExpression(node.elements[0])
    else: return "int"

  of nkBinaryExpr:
    let 
      leftType  = inferTypeFromExpression(node.left)
      rightType = inferTypeFromExpression(node.right)

    if leftType == "double" or rightType == "double": return "double"
    elif leftType == "float" or rightType == "float": return "float"
    elif leftType == "char*" or rightType == "char*": 
      if node.op in ["+", "==", "!=", "<", ">", "<=", ">="]: return "char*"
      else: return "int"
    elif leftType == "bool" or rightType == "bool":
      if node.op in ["&&", "||", "==", "!="]: return "bool"
      else: return "int"
    else: return "int"
  
  of nkFieldAccess: return "int"
  
  of nkIndexExpr:
    let arrayType = inferTypeFromExpression(node.left)
    if arrayType.endsWith("*"): return arrayType[0..^2].strip()
    else: return "int"
  else: return "int"

# =========================== VARIABLE DECLARATION ============================
proc generateVarDecl(node: Node, context: CodegenContext): string =
  # ==================== SECTION 1: STRUCT LITERALS ====================
  # Handle struct literals first
  if node.varValue != nil and node.varValue.kind == nkStructLiteral:
    var typeName = node.varType
    if typeName.len == 0:
      # Type inference for := syntax
      typeName = node.varValue.structType
    
    # Generate struct initialization
    var code = typeName & " " & node.varName & " = " & 
               generateStructLiteral(node.varValue) & ";\n"
    return indentLine(code, context)
  
  # ==================== SECTION 2: ARENA ARRAYS ====================
  # @arena(size) arr = {1, 2, 3} or @arena arr = {1, 2, 3}
  if node.varValue != nil and node.varValue.kind == nkArenaArrayLit:
    if context == cgGlobal:
      echo "ERROR: @arena arrays must be inside a function, not at global scope"
      echo "  at line ", node.line, ":", node.col
      return indentLine("// ERROR: @arena arrays cannot be global\n", context)
    
    let arrayNode = node.varValue
    
    var isMatrix = false
    if arrayNode.elements.len > 0:
      for elem in arrayNode.elements:
        if elem.kind == nkArrayLit or elem.kind == nkArenaArrayLit:
          isMatrix = true
          break
    
    var arenaSizeBytes = maxArenaSize
    if node.varType.startsWith("arena:"):
      let sizeStr = node.varType[6..^1]
      try:
        let sizeNum = parseInt(sizeStr)
        arenaSizeBytes = sizeNum
        if sizeNum > actualArenaSize: actualArenaSize = sizeNum
      except: discard
    
    if isMatrix:
      var elemType = "int"
      if arrayNode.elements.len > 0:
        let firstRow = arrayNode.elements[0]
        if (firstRow.kind == nkArrayLit or firstRow.kind == nkArenaArrayLit) and 
           firstRow.elements.len > 0:
          let firstElem = firstRow.elements[0]
          case firstElem.kind
          of nkStringLit: elemType = "char*"
          of nkLiteral:
            if firstElem.literalValue.contains('.') or
               firstElem.literalValue.contains('e') or 
               firstElem.literalValue.contains('E'):
              elemType = "double"
            else: elemType = "int"
          else: elemType = "int"
      
      var code = ""
      if elemType == "int":
        code = "int** " & node.varName & " = (int**)arena_alloc_array(&global_arena, sizeof(int*), " & 
               $arrayNode.elements.len & ");\n"
      elif elemType == "double":
        code = "double** " & node.varName & " = (double**)arena_alloc_array(&global_arena, sizeof(double*), " & 
               $arrayNode.elements.len & ");\n"
      elif elemType == "char*":
        code = "char*** " & node.varName & " = (char***)arena_alloc_array(&global_arena, sizeof(char**), " & 
               $arrayNode.elements.len & ");\n"
      
      for i, row in arrayNode.elements:
        if row.kind == nkArrayLit or row.kind == nkArenaArrayLit:
          let rowSize = row.elements.len
          if elemType == "int":
            code &= indentLine(node.varName & "[" & $i & "] = (int*)arena_alloc_array(&global_arena, sizeof(int), " & 
                    $rowSize & ");\n", context)
          elif elemType == "double":
            code &= indentLine(node.varName & "[" & $i & "] = (double*)arena_alloc_array(&global_arena, sizeof(double), " & 
                    $rowSize & ");\n", context)
          elif elemType == "char*":
            code &= indentLine(node.varName & "[" & $i & "] = (char**)arena_alloc_array(&global_arena, sizeof(char*), " & 
                    $rowSize & ");\n", context)
          
          for j, elem in row.elements:
            if elem.kind == nkStringLit:
              code &= indentLine(node.varName & "[" & $i & "][" & $j & "] = arena_string_new(&global_arena, \"" & 
                      escapeString(elem.literalValue) & "\");\n", context)
            else:
              let val = generateExpression(elem)
              code &= indentLine(node.varName & "[" & $i & "][" & $j & "] = " & val & ";\n", context)
      
      arenaVariables[node.varName] = true
      return indentLine(code, context)
    
    else:
      # ========== FLAT ARENA ARRAY ==========
      let elemCount = arrayNode.elements.len
      
      var elemType = "int"
      if arrayNode.elements.len > 0:
        let firstElem = arrayNode.elements[0]
        case firstElem.kind
        of nkStringLit: elemType = "char*"
        of nkLiteral:
          if firstElem.literalValue.contains('.') or
             firstElem.literalValue.contains('e') or 
             firstElem.literalValue.contains('E'):
            elemType = "double"
          else: elemType = "int"
        else: elemType = "int"
      
      var code = elemType & "* " & node.varName & " = (" & elemType & "*)arena_alloc_array_with_size(&global_arena, sizeof(" & 
           elemType & "), " & $elemCount & ");\n"
      
      for i, elem in arrayNode.elements:
        case elem.kind
        of nkStringLit:
          code &= indentLine(node.varName & "[" & $i & "] = arena_string_new(&global_arena, \"" & 
                  escapeString(elem.literalValue) & "\");\n", context)
        else:
          code &= indentLine(node.varName & "[" & $i & "] = " & 
                  generateExpression(elem) & ";\n", context)
      
      arenaVariables[node.varName] = true
      return indentLine(code, context)  
  # ==================== SECTION 3: ARRAY LITERALS (RC arrays) ====================
  # Syntax: arr := {1, 2, 3} or arr = {1, 2, 3}
  if node.varValue != nil and node.varValue.kind == nkArrayLit:
    let arrayNode = node.varValue
    
    var isMatrix = false
    if arrayNode.elements.len > 0:
      for elem in arrayNode.elements:
        if elem.kind == nkArrayLit:
          isMatrix = true
          break
    
    if isMatrix:
      let generatedCode = generateArrayLiteral(arrayNode)
      
      var inferredType = "int**"
      if arrayNode.elements.len > 0:
        let firstRow = arrayNode.elements[0]
        if firstRow.kind == nkArrayLit and firstRow.elements.len > 0:
          let firstElem = firstRow.elements[0]
          case firstElem.kind
          of nkStringLit: inferredType = "char***"
          of nkLiteral:
            if firstElem.literalValue.contains('.') or 
               firstElem.literalValue.contains('e') or 
               firstElem.literalValue.contains('E'):
              inferredType = "double**"
            else: inferredType = "int**"
          else: inferredType = "int**"
      
      rcVariables[node.varName] = true
      let code = inferredType & " " & node.varName & " = " & generatedCode & ";\n"
      return indentLine(code, context)
    else:
      var 
        elemType = "int"
        isStringArray = false
      
      if arrayNode.elements.len > 0:
        let firstElem = arrayNode.elements[0]
        case firstElem.kind
        of nkStringLit:
          elemType = "char*"
          isStringArray = true
        of nkLiteral:
          if firstElem.literalValue.contains('.') or 
             firstElem.literalValue.contains('e') or 
             firstElem.literalValue.contains('E'):
            elemType = "double"
          else: elemType = "int"
        else: elemType = "int"
      
      rcVariables[node.varName] = true
      var code = elemType & "* " & node.varName & " = rc_new_array(" & 
                 elemType & ", " & $arrayNode.elements.len & ");\n"
      
      # Initialize array elements
      for i, elem in arrayNode.elements:
        case elem.kind
        of nkStringLit:
          code &= indentLine(node.varName & "[" & $i & "] = rc_string_new(\"" & 
                  escapeString(elem.literalValue) & "\");\n", context)
        else:
          code &= indentLine(node.varName & "[" & $i & "] = " & 
                          generateExpression(elem) & ";\n", context)
      
      return indentLine(code, context)
  
  # ==================== SECTION 4: ALLOC/GETMEM CALLS ====================
  # Syntax: ptr := alloc(T, size) or ptr = getmem(size)
  if node.varValue != nil and node.varValue.kind == nkCall:
    let callNode = node.varValue
    
    if callNode.callFunc == "alloc" or callNode.callFunc == "getmem":
      var typeName = node.varType
      
      if typeName.len == 0:
        if callNode.callFunc == "alloc" and callNode.callArgs.len >= 2:
          let typeArg = callNode.callArgs[0]
          if typeArg.kind == nkIdentifier: typeName = typeArg.identName & "*"
          else: typeName = "void*"
        else: typeName = "void*"
      
      var code = typeName & " " & node.varName & " = " & 
                 generateCall(callNode, cgExpression) & ";\n"
      
      rcVariables[node.varName] = true
      return indentLine(code, context)
  
  # ==================== SECTION 5: MULTI-VARIABLE DECLARATIONS ====================
  # Syntax: a, b := func() or a, b = func()
  if ',' in node.varName:
    let 
      names       = node.varName.split(',')
      firstName   = names[0].strip()
      secondName  = names[1].strip()
    
    var multiCode = ""
    
    if node.varValue != nil and node.varValue.kind == nkCall:
      let funcCall = node.varValue
      
      if ',' in node.varType:
        let types       = node.varType.split(',')
        let firstType   = types[0].strip()
        let secondType  = types[1].strip()
        
        multiCode = secondType & " " & secondName & " = NULL;\n"
        multiCode &= firstType & " " & firstName & " = " & 
                     generateCall(funcCall, cgExpression, secondName) & ";\n"
      else:
        multiCode = "char* " & secondName & " = NULL;\n"
        multiCode &= "int " & firstName & " = " & 
                     generateCall(funcCall, cgExpression, secondName) & ";\n"
      
      return indentLine(multiCode, context)
  
  # ==================== SECTION 6: STRING LITERALS ====================
  # Syntax: str := "hello" or str = "hello"
  if node.varValue != nil and node.varValue.kind == nkStringLit:
    rcVariables[node.varName] = true
    
    var code = "char* " & node.varName & " = rc_string_new("
    code &= generateExpression(node.varValue) & ");\n"
    return indentLine(code, context)
  
  # ==================== SECTION 7: REGULAR VARIABLES ====================
  # All other cases: int x = 5, double y = 3.14, etc.
  
  # Determine variable type
  var 
    typeName    = node.varType
    isEnumVar   = false
    isStringVar = false
    isArrayVar  = false
  
  if typeName.len > 0 and typeName[0].isUpperAscii():
    isEnumVar = true
  
  # Type inference for := syntax
  if typeName.len == 0 and node.varValue != nil:
    if node.varValue.kind == nkStringLit:
      typeName = "char*"
      isStringVar = true
    elif node.varValue.kind == nkArrayLit: isArrayVar = true
    else: typeName = inferTypeFromExpression(node.varValue)
  
  # Handle array types
  elif typeName.endsWith("[]"):
    isArrayVar = true
    typeName = typeName[0 ..^ 3]
  elif typeName.contains("[") and typeName.contains("]"): isArrayVar = true
  elif typeName == "char*": isStringVar = true
  
  var code = ""
  
  if isArrayVar:
    if node.varValue != nil and node.varValue.kind == nkArrayLit:
      code = typeName & " " & node.varName & "[] = "
    else: code = typeName & "* " & node.varName & " = "
  elif isStringVar: code = "char* " & node.varName & " = "
  elif isEnumVar: code = typeName & " " & node.varName & " = "
  else: code = typeName & " " & node.varName & " = "
  
  # Add initialization value or default
  if node.varValue != nil:
    code &= generateExpression(node.varValue)
  else:
    # Default values based on type
    if isStringVar or typeName == "char*": code &= "NULL"
    elif typeName in ["int", "long", "short", "size_t"]: code &= "0"
    elif typeName in ["float", "double"]: code &= "0.0"
    elif typeName == "bool": code &= "false"
    else: code &= "NULL"
  
  code &= ";\n"
  
  # Mark as RC variable if needed
  if isReferenceCountedType(typeName): rcVariables[node.varName] = true
  return indentLine(code, context)

# =========================== ENUM GENERATORS ============================
proc generateEnum(node: Node): string {.used.} =
  var code = "typedef enum {\n"
  for i, valueStr in node.enumValues:
    code &= "    " & valueStr
    if i < node.enumValues.len - 1:
      code &= ","
    code &= "\n"
  code &= "} " & node.enumName & ";\n\n"
  return code

# ============================ DECLARATION GENERATORS ============================
proc generateConstDecl(node: Node, context: CodegenContext): string {.used.} =
  var
    constExpr = "0"
    constType = "int"

  if node.constType.len > 0: constType = node.constType
  else:
    case node.constValue.kind
    of nkLiteral:
      if node.constValue.literalValue.contains('.'):
        constExpr = node.constValue.literalValue
        constType = "double"
      else:
        constExpr = node.constValue.literalValue
        constType = "int"

    of nkStringLit:
      constExpr = "\"" & escapeString(node.constValue.literalValue) & "\""
      constType = "char*"

    of nkIdentifier: constExpr = node.constValue.identName
    else: discard

  if constExpr == "0": constExpr = generateExpression(node.constValue)
  if context == cgFunction:
    var code = "const " & constType & " " & node.constName & " = " & constExpr & ";\n"
    return indentLine(code, context)
  else: return "#define " & node.constName & " " & constExpr & "\n"

# ============================ CALL GENERATORS ============================
proc generateCall(node: Node, context: CodegenContext, errorVar: string = ""): string =
  let funcName = node.callFunc
  var callCode = ""
  
  case funcName
  of "alloc":
    if node.callArgs.len == 2:
      let
        typeArg    = node.callArgs[0]
        countArg   = node.callArgs[1]
      var typeName = "int" 
      
      case typeArg.kind
      of nkIdentifier:           typeName = typeArg.identName
      of nkLiteral, nkStringLit: typeName = typeArg.literalValue
      else: typeName = "int"
      
      if typeName   == "string": typeName = "char*"
      callCode      = "rc_new_array(" & typeName & ", " & generateExpression(countArg) & ")"
    else: callCode  = "rc_new_array(int, 0)"
  
  of "getmem":
    if node.callArgs.len > 0: callCode = "rc_alloc(" & generateExpression(node.callArgs[0]) & ")"
    else: callCode = "rc_alloc(0)"
  
  of "free", "freemem":
    if node.callArgs.len > 0:
      let arg = generateExpression(node.callArgs[0])
      callCode = "rc_release(" & arg & ")" 
    else: callCode = "rc_release(NULL)"

  of "len":
    if node.callArgs.len == 1:
      let arg = generateExpression(node.callArgs[0])
      
      var isArena = false
      if node.callArgs[0].kind == nkIdentifier:
        let varName = node.callArgs[0].identName
        if arenaVariables.hasKey(varName): isArena = true
      
      if isArena: callCode = "arena_array_len(" & arg & ")"
      else: callCode = "RC_GET_HEADER(" & arg & ")->array_count"
    else: callCode = "0"
    
    if context != cgExpression:
      callCode &= ";\n"
      return indentLine(callCode, context)
    else: return callCode
  
  of "print":
    callCode = "printf("
    if node.callArgs.len > 0:
      for i, arg in node.callArgs:
        if i > 0: callCode &= ", "
        callCode &= generateExpression(arg)
    callCode &= ")"  
  else:
    callCode = funcName & "("
    if node.callArgs.len > 0:
      for i, arg in node.callArgs:
        if i > 0: callCode &= ", "
        callCode &= generateExpression(arg)
    
    if errorVar.len > 0:
      if node.callArgs.len > 0: callCode &= ", "
      callCode &= "&" & errorVar
    callCode &= ")"
  
  if context != cgExpression:
    callCode &= ";\n"
    return indentLine(callCode, context)
  else: return callCode

# =========================== ASSIGNMENT GENERATORS ===========================
proc generateAssignment(node: Node, context: CodegenContext): string {.used.} =
  var 
    leftCode = ""
    varName = ""
  
  case node.left.kind
  of nkIdentifier:
    leftCode = node.left.identName
    varName = node.left.identName
  of nkFieldAccess:
    leftCode = generateFieldAccess(node.left)
    if node.left.base.kind == nkIdentifier: varName = node.left.base.identName
  else: leftCode = generateExpression(node.left)
  
  var code = ""
  
  if varName.len > 0 and isRcVariable(varName):
    code &= "if (" & leftCode & ") " & generateRcReleaseStmt(varName)
    code &= leftCode & " = " & generateExpression(node.right) & ";\n"
    code &= "if (" & leftCode & ") " & generateRcRetainStmt(varName)
  else: code = leftCode & " = " & generateExpression(node.right) & ";\n"
  
  return indentLine(code, context)

# ============================ RETURN GENERATORS =============================
proc generateReturn(node: Node, context: CodegenContext): string {.used.} =
  var code = ""
  
  case node.callArgs.len
  of 0: code = "return;"
  of 1:
    let retVal = generateExpression(node.callArgs[0])
    code = "return " & retVal & ";"
  of 2:
    let 
      retVal = generateExpression(node.callArgs[0])
      errExpr = generateExpression(node.callArgs[1])
    
    if errExpr == "NULL" or errExpr == "nil" or errExpr == "0":
      code = "*error_out = NULL;\n"
      code &= "  return " & retVal & ";"
    else:
      code = "*error_out = " & errExpr & ";\n"
      code &= "  return " & retVal & ";"
  else: code = "return /* too many values */;"
  return indentLine(code, context)

# ============================ ARRAY TYPE GENERATORS ==========================
proc generateArrayType(node: Node): string =
  let sizeStr =
    if node.size != nil: generateExpression(node.size)
    else: ""

  if sizeStr.len > 0: return node.elemType & "[" & sizeStr & "]"
  else: return node.elemType & "[]"

# ============================= INDEX GENERATORS ==============================
proc generateIndexExpr(node: Node): string =
  return generateExpression(node.left) & "[" & generateExpression(node.right) & "]"

# ============================ ARRAY GENERATORS =============================
proc generateArrayLiteral(node: Node): string =
  # Helper functions
  proc isNestedArray(n: Node): bool =
    n.elements.len > 0 and (n.elements[0].kind == nkArrayLit or n.elements[0].kind == nkArenaArrayLit)
  
  proc getElementType(elem: Node): string =
    case elem.kind
    of nkStringLit: "char*"
    of nkLiteral:
      let val = elem.literalValue
      if val.contains('.') or val.contains('e') or val.contains('E'): "double"
      else: "int"
    else: "int"
  
  proc getInnermostElementType(arr: Node): string =
    if arr.elements.len == 0: "int"
    else: getElementType(arr.elements[0])
  
  # Main logic
  let 
    isArenaArray  = (node.kind == nkArenaArrayLit)
    isEmpty       = (node.elements.len == 0)
    isMatrix      = not isEmpty and isNestedArray(node)
  
  if isEmpty:
    if isMatrix:
      if isArenaArray: return "({\n    int** _tmp = (int**)arena_alloc_array(&global_arena, sizeof(int*), 0);\n    _tmp;\n})"
      else: return "rc_new_array(int*, 0)"
    else:
      if isArenaArray: return "({\n    int* _tmp = (int*)arena_alloc_array(&global_arena, sizeof(int), 0);\n    _tmp;\n})"
      else: return "rc_new_array(int, 0)"
  
  # Determine types
  let baseType = 
    if isMatrix:
      let 
        firstRow = node.elements[0]
        innerType = getInnermostElementType(firstRow)
      if innerType == "char*": "char***"
      elif innerType == "double": "double**"
      else: "int**"
    else:
      let elemType = getElementType(node.elements[0])
      if elemType == "char*": "char**"
      elif elemType == "double": "double*"
      else: "int*"
  
  # For code generation
  proc generateFlatArray(): string =
    if isArenaArray:
      let base = if baseType.endsWith("*"): baseType[0..^2] else: baseType
      var code = "({\n    " & baseType & " _tmp = (" & baseType & ")arena_alloc_array(&global_arena, sizeof(" & 
                 base & "), " & $node.elements.len & ");\n"
      if node.elements.len > 0:
        code &= "    if (_tmp) {\n"
        for i, elem in node.elements:
          if elem.kind == nkStringLit:
            code &= "        _tmp[" & $i & "] = arena_string_new(&global_arena, \"" & 
                    escapeString(elem.literalValue) & "\");\n"
          else:
            let val = generateExpression(elem)
            code &= "        _tmp[" & $i & "] = " & val & ";\n"
        code &= "    }\n"
      code &= "    _tmp;\n})"
      return code
    else:
      let base = if baseType.endsWith("*"): baseType[0..^2] else: baseType
      var code = "({\n    " & baseType & " _tmp = rc_new_array(" & base & ", " & $node.elements.len & ");\n"
      for i, elem in node.elements:
        if elem.kind == nkStringLit:
          code &= "    _tmp[" & $i & "] = rc_string_new(\"" & escapeString(elem.literalValue) & "\");\n"
        else:
          let val = generateExpression(elem)
          code &= "    _tmp[" & $i & "] = " & val & ";\n"
      code &= "    _tmp;\n})"
      return code
  
  proc generateMatrix(): string =
    if isArenaArray:
      let base = 
        if baseType.endsWith("***"): baseType[0..^4]
        elif baseType.endsWith("**"): baseType[0..^3]
        else: baseType
      
      var code = "({\n    " & baseType & " _tmp = (" & baseType & ")arena_alloc_array(&global_arena, sizeof(" & 
                 base & "*), " & $node.elements.len & ");\n"
      code &= "    if (_tmp) {\n"
      
      for i, row in node.elements:
        if row.kind == nkArrayLit or row.kind == nkArenaArrayLit:
          let 
            rowSize = row.elements.len
            rowType = 
              if baseType == "char***": "char**"
              elif baseType == "double**": "double*"
              else: "int*"
          
          code &= "        _tmp[" & $i & "] = (" & rowType & ")arena_alloc_array(&global_arena, sizeof(" & 
                  base & "), " & $rowSize & ");\n"
          code &= "        if (_tmp[" & $i & "]) {\n"
          
          for j, elem in row.elements:
            if elem.kind == nkStringLit:
              code &= "            _tmp[" & $i & "][" & $j & "] = arena_string_new(&global_arena, \"" & 
                      escapeString(elem.literalValue) & "\");\n"
            else:
              let val = generateExpression(elem)
              code &= "            _tmp[" & $i & "][" & $j & "] = " & val & ";\n"
          code &= "        }\n"
      code &= "    }\n    _tmp;\n})"
      return code
    else:
      # RC array matrix
      let outerSize = node.elements.len
      let outerType = 
        if baseType == "char***": "char**"
        elif baseType == "double**": "double*"
        else: "int*"
      
      var code = "({\n    " & baseType & " _tmp = rc_new_array(" & outerType & ", " & $outerSize & ");\n"
      
      for i, row in node.elements:
        if row.kind == nkArrayLit or row.kind == nkArenaArrayLit:
          let 
            rowSize = row.elements.len
            innerType = getInnermostElementType(row)
          
          code &= "    _tmp[" & $i & "] = rc_new_array(" & innerType & ", " & $rowSize & ");\n"
          
          for j, elem in row.elements:
            if elem.kind == nkStringLit:
              code &= "    _tmp[" & $i & "][" & $j & "] = rc_string_new(\"" & 
                      escapeString(elem.literalValue) & "\");\n"
            else:
              let val = generateExpression(elem)
              code &= "    _tmp[" & $i & "][" & $j & "] = " & val & ";\n"
      code &= "    _tmp;\n})"
      return code
  
  # Generate the appropriate array
  if isMatrix: generateMatrix()
  else: generateFlatArray()

# ============================== IF GENERATORS ================================
proc generateIf(node: Node, context: CodegenContext): string =
  var code = "if (" & generateExpression(node.ifCondition) & ") {\n"

  let thenCode = generateBlock(node.ifThen, context)
  for line in thenCode.splitLines:
    if line.len > 0: code &= "  " & line & "\n"
  code &= "}"

  if node.ifElse != nil: 
    code &= " else "
    if node.ifElse.kind == nkIf: code &= generateIf(node.ifElse, context)
    else:
      code &= "{\n"
      let elseCode = generateBlock(node.ifElse, context)
      for line in elseCode.splitLines:
        if line.len > 0: code &= "  " & line & "\n"
      code &= "}"
  code &= "\n"
  return indentLine(code, context)

# =========================== STRUCT GENERATORS ============================
proc generateStruct(node: Node): string {.used.} =
  var code = "typedef struct {\n"

  for field in node.structFields: 
    code &= "  " & field.varType & " " & field.varName & ";\n"
  code &= "} " & node.structName & ";\n\n"
  return code

# ============================ STRUCT LITERAL GENERATORS ======================
proc generateStructLiteral(node: Node): string =
  if node.fieldValues.len == 1 and node.fieldValues[0].left.identName == "value":
    # Handle simple value case
    return generateExpression(node.fieldValues[0].right)
  
  var 
    resultStruct = "{"  # Start C struct literal
    initializers: seq[string]
  
  for assignment in node.fieldValues:
    let 
      fieldName  = assignment.left.identName
      fieldValue = generateExpression(assignment.right)
    initializers.add("." & fieldName & " = " & fieldValue)
  
  resultStruct.add(initializers.join(", "))
  resultStruct.add("}")
  return resultStruct

# ============================ FIELD ACCESS GENERATORS ============================
proc generateFieldAccess(node: Node): string =
  var resultField = ""
  if node.base.kind == nkFieldAccess: resultField = generateFieldAccess(node.base)
  else: resultField = generateExpression(node.base)
  resultField &= "." & node.field.identName
  return resultField

# ============================= RC CLEANUP ================================
proc getCleanupString(vars: seq[tuple[name: string, typeName: string, isArray: bool]]): string =
  var cleanup = ""
  for rcVar in vars:
    # Check if it's a pointer-to-pointer (2D array)
    if rcVar.typeName.endsWith("**"):
      cleanup &= "    if (" & rcVar.name & ") {\n"
      cleanup &= "        for (size_t _i = 0; _i < RC_GET_HEADER(" & rcVar.name & ")->array_count; _i++) {\n"
      cleanup &= "            if (" & rcVar.name & "[_i]) rc_release(" & rcVar.name & "[_i]);\n"
      cleanup &= "        }\n"
      cleanup &= "        rc_release(" & rcVar.name & ");\n"
      cleanup &= "    }\n"
    else:
      cleanup &= "    if (" & rcVar.name & ") rc_release(" & rcVar.name & ");\n"
  return cleanup

# ============================== GENERATE BLOCK ================================
proc generateBlock(node: Node, context: CodegenContext): string =
  var 
    blockResult = ""
    localRCVars: seq[tuple[name: string, typeName: string, isArray: bool]] = @[]
  
  if node.statements.len > 0:
    for stmt in node.statements:
      case stmt.kind
      of nkVarDecl:
        blockResult &= generateVarDecl(stmt, context)
        
        if stmt.varType.len > 0:
          let (isArr, _, _) = isArrayType(stmt.varType)
          if isReferenceCountedType(stmt.varType) or isArr:
            localRCVars.add((name: stmt.varName, typeName: stmt.varType, isArray: isArr))
        elif stmt.varValue != nil:
          if stmt.varValue.kind == nkArrayLit:
            # Check if it's a matrix (nested array)
            var 
              isMatrix = false
              elemType = "int"
            if stmt.varValue.elements.len > 0:
              let first = stmt.varValue.elements[0]
              if first.kind == nkArrayLit:
                isMatrix = true
                if first.elements.len > 0:
                  let firstElem = first.elements[0]
                  if firstElem.kind == nkStringLit: elemType = "char**"
                  elif firstElem.kind == nkLiteral and (firstElem.literalValue.contains('.') or firstElem.literalValue.contains('e')):
                    elemType = "double*"
                  else: elemType = "int*"
              
            if isMatrix:
              localRCVars.add((name: stmt.varName, typeName: elemType & "*", isArray: true))
            else:
              var elemType = "int"
              if stmt.varValue.elements.len > 0:
                let first = stmt.varValue.elements[0]
                if first.kind == nkStringLit: elemType = "char*"
                elif first.kind == nkLiteral and (first.literalValue.contains('.') or first.literalValue.contains('e')):
                  elemType = "double"
              localRCVars.add((name: stmt.varName, typeName: elemType & "*", isArray: true))
          else:
            let 
              inferred = inferTypeFromExpression(stmt.varValue)
              (isArr, _, _) = isArrayType(inferred)
            if isReferenceCountedType(inferred) or isArr:
              localRCVars.add((name: stmt.varName, typeName: inferred, isArray: isArr))

      of nkConstDecl: 
        blockResult &= generateConstDecl(stmt, context)
      of nkReturn:
        if localRCVars.len > 0:
          blockResult &= "\n    // Cleanup before return\n"
          blockResult &= getCleanupString(localRCVars)
        blockResult &= "    " & generateReturn(stmt, context)
        return blockResult

      of nkAssignment: blockResult &= generateAssignment(stmt, context)
      of nkCall:       blockResult &= generateCall(stmt, context, "")
      of nkIf:         blockResult &= generateIf(stmt, context)
      of nkFor:        blockResult &= generateFor(stmt, context)
      of nkForRange:   blockResult &= generateForRange(stmt, context)
      of nkSwitch:     blockResult &= generateSwitch(stmt, context)
      of nkCBlock:     blockResult &= generateCBlock(stmt, context)
      else: discard

  if localRCVars.len > 0:
    blockResult &= "\n    // Block scope cleanup\n"
    blockResult &= getCleanupString(localRCVars)
      
  return blockResult

# =========================== SWITCH GENERATOR ============================
proc generateSwitch(node: Node, context: CodegenContext): string =
  if node.switchTarget == nil: 
    return indentLine("/* ERROR: No switch target */\n", context)
  
  var code = "switch (" & generateExpression(node.switchTarget) & ") {\n"
  
  for caseNode in node.cases:
    for i, value in caseNode.caseValues:
      code &= "  case " & generateExpression(value) & ":\n"
      if i == caseNode.caseValues.len - 1:
        let bodyCode = generateBlock(caseNode.caseBody, cgFunction)
        for line in bodyCode.splitLines:
          if line.len > 0: 
            code &= "    " & line & "\n"
        code &= "    break;\n"
  
  if node.defaultCase != nil:
    code &= "  default:\n"
    let defaultCode = generateBlock(node.defaultCase.defaultBody, cgFunction)
    for line in defaultCode.splitLines:
      if line.len > 0: 
        code &= "    " & line & "\n"
    code &= "    break;\n"
  
  code &= "}\n"
  return indentLine(code, context)

# =========================== DEFER GENERATORS ============================
proc generateDefer(node: Node, context: CodegenContext): string =
  if node.deferExpr == nil: return ""
  return generateExpression(node.deferExpr) & ";"

# =========================== STRUCTURE GENERATORS ============================
proc generateFunction(node: Node, hasArenaArrays: bool = false): string =
  var code = ""
  
  if node.funcName == "main": 
    if node.params.len > 0:
      var paramList: seq[string] = @[]
      for param in node.params:
        var cType = param.varType
        if param.varName == "argv": cType = "char**"
        elif cType == "string": cType = "char*"
        paramList.add(cType & " " & param.varName)
      
      code = "int main(" & paramList.join(", ") & ") {\n"
    else: code = "int main() {\n"
    
    if hasArenaArrays:
      code &= "  // Initialize arena\n"
      code &= "  global_arena = arena_init_dynamic(" & $actualArenaSize & ");\n"
    
    if node.body != nil:
      let bodyCode = generateBlock(node.body, cgFunction)
      code &= bodyCode
    
    if hasArenaArrays:
      code &= "  // Clean up arena\n"
      code &= "  arena_free(&global_arena);\n"
    
    if not code.contains("return 0;"): code &= "  return 0;\n"
    code &= "}\n"
  else:
    var paramList: seq[string] = @[]
    for param in node.params:
      var cType = param.varType
      if cType == "string": cType = "char*"
      paramList.add(cType & " " & param.varName)
    
    var returnType = node.returnType
    if returnType == "string": returnType = "char*"
    if returnType == "void" and node.returnsError: returnType = "int"
    
    code = returnType & " " & node.funcName & "(" & paramList.join(", ") & ") {\n"
    
    if node.returnsError: code &= "  char** error_out = NULL;\n"
    
    if node.body != nil:
      let bodyCode = generateBlock(node.body, cgFunction)
      code &= bodyCode
    
    if returnType == "void" and not code.contains("return"): code &= "}\n"
    else: code &= "}\n"
  
  return code

# =========================== CASE GENERATOR ============================
proc generateCase(node: Node, context: CodegenContext): string {.used.} =
  var code = ""
  for i, value in node.caseValues:
    if i > 0: code &= "case "
    else:     code &= "case "
    code &= generateExpression(value)
    if i < node.caseValues.len - 1: code &= ":\n"
  
  code &= ":\n"
  code &= generateBlock(node.caseBody, cgFunction)
  return code

# =========================== DEFAULT GENERATOR ============================
proc generateDefault(node: Node, context: CodegenContext): string {.used.} =
  var code = "default:\n"
  code &= generateBlock(node.defaultBody, cgFunction)
  return code

# ============================= PROGRAM GENERATOR ==============================
proc checkNodeForArena(n: Node): bool =
  case n.kind
  of nkArenaArrayLit: return true
  of nkVarDecl:
    if n.varValue != nil and n.varValue.kind == nkArenaArrayLit: return true
    else: return false
  of nkBlock:
    for stmt in n.statements:
      if checkNodeForArena(stmt): 
        return true
    return false
  of nkFunction:
    if n.body != nil: return checkNodeForArena(n.body)
    else: return false
  of nkIf:
    if n.ifThen != nil and checkNodeForArena(n.ifThen): return true
    if n.ifElse != nil and checkNodeForArena(n.ifElse): return true
    return false
  of nkFor:
    if n.forBody != nil: return checkNodeForArena(n.forBody)
    else: return false
  of nkForRange:
    if n.rangeBody != nil: return checkNodeForArena(n.rangeBody)  # FIX: rangeBody, not forBody
    else: return false
  else: return false

# ============================= SCAN FOR ARENA SIZES ==============================
proc scanForArenaSizes(node: Node): int =
  if node == nil: return 0
  
  case node.kind
  of nkProgram:
    for funcNode in node.functions:
      let size = scanForArenaSizes(funcNode)
      if size > result: result = size
      
  of nkFunction:
    result = scanForArenaSizes(node.body)
      
  of nkBlock:
    for stmt in node.statements:
      let size = scanForArenaSizes(stmt)
      if size > result: result = size
      
  of nkVarDecl:
    if node.varType.startsWith("arena:"):
      let sizeStr = node.varType[6..^1]
      try:
        let sizeNum = parseInt(sizeStr)
        if sizeNum > result: result = sizeNum
      except: discard
        
  of nkIf:
    let thenSize = scanForArenaSizes(node.ifThen)
    if thenSize > result: result = thenSize
    
    let elseSize = scanForArenaSizes(node.ifElse)
    if elseSize > result: result = elseSize
      
  of nkFor:
    let bodySize = scanForArenaSizes(node.forBody)
    if bodySize > result: result = bodySize
      
  of nkForRange:
    let bodySize = scanForArenaSizes(node.rangeBody)
    if bodySize > result: result = bodySize
      
  else: result = 0

# ============================= PROGRAM GENERATOR ==============================
proc generateProgram(node: Node): string =
  rcVariables.clear()
  arenaVariables.clear()
  actualArenaSize = scanForArenaSizes(node)
  
  if actualArenaSize == 0:
    actualArenaSize = maxArenaSize
  
  var
    functionCode =    ""
    hasCMain =        false
    haZalMain =       false
    userIncludes =    ""
    userCode =        ""
    defines =         ""
    otherTopLevel =   ""
    structsCode =     ""
    hasArenaArrays =  false 

  for funcNode in node.functions:
    if checkNodeForArena(funcNode):
      hasArenaArrays = true
      break

  for funcNode in node.functions:
    case funcNode.kind
    of nkStruct:    structsCode &= generateStruct(funcNode)
    of nkEnum:      structsCode &= generateEnum(funcNode)
    of nkConstDecl: defines &= generateConstDecl(funcNode, cgGlobal)
    of nkCBlock:
      let cCode = generateCBlock(funcNode, cgGlobal)
      
      let cleanCode = cCode.strip()
      if cleanCode.startsWith("#include") or 
         "#include <" in cCode or 
         "#include \"" in cCode:
        userIncludes &= cCode
      else: userCode &= cCode
      
      if "int main()" in cCode: hasCMain = true
    of nkFunction:
      functionCode &= generateFunction(funcNode, hasArenaArrays)
      if funcNode.funcName == "main": haZalMain = true
    of nkVarDecl: otherTopLevel &= generateVarDecl(funcNode, cgGlobal)
    of nkRcNew, nkRcRetain, nkRcRelease, nkWeakRef, nkStrongRef:
      userCode &= generateCBlock(funcNode, cgGlobal) & "\n"
    else: discard
  
  # ========== BUILD THE FINAL C CODE ==========
  result = userIncludes 
  result &= RC_HEADER & "\n\n"
  if hasArenaArrays:
    result &= ARENA_HEADER & "\n\n"
    result &= "static Arena global_arena;\n\n"
  result &= userCode 
  result &= defines & structsCode & otherTopLevel
  result &= functionCode
  
  if not hasCMain and not haZalMain:
    result &= "\nint main() {\n"
    result &= "  // Auto-generated entry point\n"
    if hasArenaArrays:
      result &= "  global_arena = arena_init_dynamic(" & $actualArenaSize & ");\n"
    result &= "  return 0;\n"
    result &= "}\n"
  
  return result

# =========================== MAIN DISPATCH ============================
proc generateC*(node: Node, context: string = "global"): string =
  let cgContext =
    if context == "function": cgFunction
    elif context == "global": cgGlobal
    else: cgGlobal

  case node.kind
  of nkProgram:         generateProgram(node)
  of nkPackage:         "// Package: " & node.packageName & "\n"
  of nkFunction:        generateFunction(node)
  of nkStruct:          generateStruct(node)
  of nkFieldAccess:     generateFieldAccess(node)
  of nkStructLiteral:   return generateStructLiteral(node)
  of nkEnum:            return generateEnum(node)
  of nkAssignment:      generateAssignment(node, cgContext)
  of nkReturn:          generateReturn(node, cgContext)
  of nkBlock:           generateBlock(node, cgContext)
  of nkCBlock:          generateCBlock(node, cgContext)
  of nkConstDecl:       generateConstDecl(node, cgContext)
  of nkArrayType:       generateArrayType(node)
  of nkIdentifier:      generateIdentifier(node)
  of nkCall:            return generateCall(node, cgExpression)
  of nkIf:              generateIf(node, cgContext)
  of nkFor:             generateFor(node, cgContext)
  of nkAddressOf:       return generateAddressOf(node)
  of nkDeref:           return generateDeref(node)
  of nkForRange:        return generateForRange(node, cgContext)
  of nkSwitch:          generateSwitch(node, cgContext)
  of nkDefer:           return generateDefer(node, cgContext)
  of nkCase:            "/* case */"
  of nkDefault:         "/* default */"
  of nkSwitchExpr:      "/* switch_expr */"
  of nkGroup:           generateGroup(node, cgContext)
  of nkElse:            ""
  of nkPtrType:         "/* ptr_type */"
  of nkRefType:         "/* ref_type */"
  # Reference counting nodes
  of nkRcNew:           return generateRcNew(node, cgContext)
  of nkRcRetain:        return generateRcRetain(node, cgContext)
  of nkRcRelease:       return generateRcRelease(node, cgContext)
  of nkWeakRef:         return generateWeakRef(node, cgContext)
  of nkStrongRef:       return generateStrongRef(node, cgContext)
  of nkRcInit:          return generateRcInit(node, cgContext)
  # Arena nodes - ADD THESE
  of nkArena:           return "/* arena declaration - not implemented */"
  of nkArenaAlloc:      return "/* arena allocation - not implemented */"
  of nkArenaArrayLit:   return generateArrayLiteral(node)
  # These already call generateExpression which handles nkArenaArrayLit
  of nkBinaryExpr, nkIndexExpr, nkArrayLit: return generateExpression(node)
  of nkVarDecl, nkInferredVarDecl: return generateVarDecl(node, cgContext)
  of nkLiteral, nkStringLit: return generateLiteral(node)
