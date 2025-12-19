# zal_codegen.nim - Generate C code from AST
import zal_parser
import std/[strutils]

# =========================== CONTEXT TYPES ============================
type CodegenContext* = enum
  cgGlobal # Top-level (functions, global variables)
  cgFunction # Inside a function body
  cgExpression # Inside an expression

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
proc generateForRange(node: Node, context: CodegenContext): string
proc generateCall(node: Node, context: CodegenContext, errorVar: string = ""): string 
proc generateExpression(node: Node): string
proc generateBlock(node: Node, context: CodegenContext): string
proc generateFieldAccess(node: Node): string
proc generateAddressOf(node: Node): string
proc generateDeref(node: Node): string
proc generateStructLiteral(node: Node): string
proc generateIndexExpr(node: Node): string
proc generateArrayLiteral(node: Node): string
proc generateArrayType(node: Node): string
proc generateSwitch(node: Node, context: CodegenContext): string

# =========================== BASIC EXPRESSION GENERATORS ============================
proc generateLiteral(node: Node): string =
  case node.kind
  of nkLiteral:
    if node.literalValue == "NULL": "NULL" else: node.literalValue
  of nkStringLit: "\"" & escapeString(node.literalValue) & "\""
  else: ""

proc generateIdentifier(node: Node): string =
  node.identName

# =========================== DEREFERENCE GENERATORS ============================
proc generateDeref(node: Node): string =
  return "*" & generateExpression(node.operand)

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
  of nkArrayLit: result = generateArrayLiteral(node)
  of nkBinaryExpr: result =
      generateExpression(node.left) & " " & node.op & " " &
      generateExpression(node.right)
  of nkIdentifier: result = node.identName
  of nkArrayType: result = generateArrayType(node)
  of nkLiteral, nkStringLit: result = generateLiteral(node)
  of nkAddressOf: result = generateAddressOf(node)
  of nkDeref: result = generateDeref(node)  # Now it's defined
  of nkGroup: result = "(" & generateExpression(node.groupExpr) & ")"
  else: result = "/* ERROR: unhandled expression */"

# =========================== GROUP GENERATORS ============================
proc generateGroup(node: Node, context: CodegenContext): string =
  "(" & generateExpression(node.groupExpr) & ")"

# =========================== LOOP GENERATORS ============================
proc generateFor(node: Node, context: CodegenContext): string =
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

  # Check if this is a range expression (start..end)
  if node.rangeTarget.kind == nkBinaryExpr and node.rangeTarget.op == "..":
    isRange = true
    if node.rangeTarget.left != nil:
      startVal = generateExpression(node.rangeTarget.left)
    if node.rangeTarget.right != nil:
      endVal = generateExpression(node.rangeTarget.right)

  var code = ""

  if isRange:
    # Handle range loops: for x in start..end
    if node.rangeValue != nil: 
      code = "for (int " & node.rangeValue.identName & " = " & startVal & "; " &
          node.rangeValue.identName & " <= " & endVal & "; " & node.rangeValue.identName & "++) {\n"
    elif node.rangeIndex != nil: 
      code = "for (int " & node.rangeIndex.identName & " = " & startVal & "; " &
          node.rangeIndex.identName & " <= " & endVal & "; " & node.rangeIndex.identName & "++) {\n"
    else: 
      code = "for (int _i = " & startVal & "; _i <= " & endVal & "; _i++) {\n"
  else:
    # Handle collection loops: for x in collection
    let target = generateExpression(node.rangeTarget)
    var isString = false

    # Check if target is a string
    case node.rangeTarget.kind
    of nkStringLit:
      isString = true
    of nkIdentifier:
      let name = node.rangeTarget.identName
      if name == "s" or name == "str" or name == "text" or name.endsWith("Str") or name.endsWith("String"): 
        isString = true
    else:
      # Check the generated expression
      if target.startsWith("\"") or target.contains("char*"): 
        isString = true

    if isString:
      # String iteration
      code = "for (int _i = 0; " & target & "[_i] != '\\0'; _i++) {\n"
      if node.rangeIndex != nil: 
        code &= "  int " & node.rangeIndex.identName & " = _i;\n"
      if node.rangeValue != nil: 
        code &= "  char " & node.rangeValue.identName & " = " & target & "[_i];\n"
    else:
      # Array iteration
      code = "for (int _i = 0; _i < (int)(sizeof(" & target & ") / sizeof(" & target & "[0])); _i++) {\n"
      if node.rangeIndex != nil: 
        code &= "  int " & node.rangeIndex.identName & " = _i;\n"
      if node.rangeValue != nil:
        var elemType = "int" 
        if node.rangeTarget.kind == nkArrayLit and node.rangeTarget.elements.len > 0:
          let firstElem = node.rangeTarget.elements[0]
          if firstElem.kind == nkStringLit: 
            elemType = "char*"
          elif firstElem.kind == nkLiteral:
            if firstElem.literalValue.contains('.') or firstElem.literalValue.contains('e') or
                firstElem.literalValue.contains('E'): 
              elemType = "double"
        code &= "  " & elemType & " " & node.rangeValue.identName & " = " & target & "[_i];\n"

  # Add body code
  if node.rangeBody != nil:
    let bodyCode = generateBlock(node.rangeBody, cgFunction)
    for line in bodyCode.splitLines:
      if line.len > 0: 
        code &= "  " & line & "\n"

  code &= "}\n"
  return indentLine(code, context)

# =========================== STATEMENT GENERATORS ============================
proc generateCBlock(node: Node, context: CodegenContext): string =
  var cCode = node.cCode.strip(leading = false, trailing = true)

  # Check if this looks like a function definition
  let looksLikeFunction = (" int " in cCode or " void " in cCode or " char " in cCode or " float " in cCode or
                           " double " in cCode) and "(" in cCode and "){" in cCode

  if looksLikeFunction:
    # Function-like C block
    if context == cgFunction:
      # Inside a function body - indent it
      result = cCode.replace("\n", "\n  ")
    else:
      # At global scope or in expression - no indentation
      result = cCode
  else:
    # Not a function - just use as-is
    result = cCode
  
  # Ensure it ends with newline
  if result.len > 0 and result[^1] != '\n': 
    result &= "\n"

# =========================== TYPE INFERENCE ============================
proc inferTypeFromIdentifier(name: string): string =
  # Try to infer type from identifier name patterns
  
  # Check for pointer patterns first
  if name.endsWith("Ptr"):
    let baseName = name[0..^4]  # Remove "Ptr"
    let baseType = inferTypeFromIdentifier(baseName)
    return baseType & "*"
  
  elif name.endsWith("_ptr"):
    let baseName = name[0..^5]  # Remove "_ptr"
    let baseType = inferTypeFromIdentifier(baseName)
    return baseType & "*"
  
  elif name.startsWith("p_"):
    let baseName = name[2..^1]  # Remove "p_"
    let baseType = inferTypeFromIdentifier(baseName)
    return baseType & "*"
  
  # Check for string patterns
  elif name.endsWith("Str") or name.endsWith("String") or name.endsWith("_str"):
    return "char*"
  
  # Check for size/count patterns
  elif name.endsWith("Count") or name.endsWith("Size") or name.endsWith("Len"):
    return "size_t"
  
  # Check for boolean patterns
  elif name.endsWith("Flag") or name.endsWith("Enabled") or name.startsWith("is"):
    return "bool"
  
  # Check for float patterns
  elif name.endsWith("F") or name.endsWith("_f") or name.contains("Float"):
    return "double"
  
  # Common variable names that suggest types
  elif name == "str" or name == "text" or name == "message" or 
       name == "path" or name == "filename" or name == "url":
    return "char*"
  
  elif name == "count" or name == "length" or name == "size" or 
       name == "index" or name == "i" or name == "j" or name == "k":
    return "int"
  
  elif name == "flag" or name == "enabled" or name == "visible" or 
       name == "active" or name == "ready":
    return "bool"
  
  else:
    # Default to int for unknown identifiers
    return "int"

# =========================== TYPE INFERENCE ============================
proc inferTypeFromExpression(node: Node): string =
  if node == nil: 
    return "int"

  case node.kind
  of nkAddressOf:
    let targetType = inferTypeFromExpression(node.operand)
    return targetType & "*"
  
  of nkDeref:
    let ptrType = inferTypeFromExpression(node.operand)
    # Remove one level of pointer
    if ptrType.endsWith("*"):
      return ptrType[0..^2].strip()
    else:
      return ptrType
  
  of nkLiteral:
    let val = node.literalValue
    if val == "NULL": 
      return "void*" 
    if val.contains('.') or val.contains('e') or val.contains('E'): 
      return "double"
    else:
      if val == "true" or val == "false": 
        return "bool"
      return "int"
  
  of nkStructLiteral: 
    return node.structType
  
  of nkStringLit:     
    return "char*"
  
  of nkIdentifier:
    return inferTypeFromIdentifier(node.identName)
  
  of nkCall:
    let funcName = node.callFunc
    case funcName
    of "alloc":
      if node.callArgs.len >= 1:
        let typeArg = node.callArgs[0]
        if typeArg.kind == nkIdentifier: 
          return typeArg.identName & "*"
      return "void*"
    
    of "getmem":
      return "void*"
    
    of "len":
      return "size_t"
    
    of "sizeof":
      return "size_t"
    
    else:
      # For other function calls, we don't know the return type
      return "int"

  of nkArrayLit:
    if node.elements.len > 0:
      return inferTypeFromExpression(node.elements[0])
    else: 
      return "int"

  of nkBinaryExpr:
    let 
      leftType = inferTypeFromExpression(node.left)
      rightType = inferTypeFromExpression(node.right)

    # Type promotion rules
    if leftType == "double" or rightType == "double": 
      return "double"
    elif leftType == "float" or rightType == "float": 
      return "float"
    elif leftType == "char*" or rightType == "char*": 
      # String concatenation or comparison
      if node.op in ["+", "==", "!=", "<", ">", "<=", ">="]:
        return "char*"
      else:
        return "int"
    elif leftType == "bool" or rightType == "bool":
      # Boolean operations
      if node.op in ["&&", "||", "==", "!="]:
        return "bool"
      else:
        return "int"
    else: 
      return "int"
  
  of nkFieldAccess:
    # For field access, we need to know the struct type
    return "int"
  
  of nkIndexExpr:
    # Array indexing returns the element type
    let arrayType = inferTypeFromExpression(node.left)
    # If it's a pointer type, remove one * to get element type
    if arrayType.endsWith("*"):
      return arrayType[0..^2].strip()
    else:
      # Assume it's an array of ints
      return "int"
  
  else: 
    return "int"

# =========================== VARIABLE DECLARATION ============================
proc generateVarDecl(node: Node, context: CodegenContext): string =
  if node.varValue != nil:
    echo "  value kind: ", node.varValue.kind
  
  var cType = node.varType
  
  # Check for array type: [size]type
  if cType.startsWith("[") and cType.contains("]"):
    let closeBracket = cType.find(']')
    if closeBracket > 0:
      let 
        sizePart = cType[1..<closeBracket]  # Should be "3"
        elemType = cType[closeBracket+1..^1]  # Should be "int"
      
      var code = ""
      if node.varValue != nil and node.varValue.kind == nkArrayLit:
        # With initializer: [3]int = [1,2,3] -> int arr[3] = {1,2,3}
        code = elemType & " " & node.varName & "[" & sizePart & "] = " & generateExpression(node.varValue)
      else:
        # Without initializer
        code = elemType & " " & node.varName & "[" & sizePart & "]"
      
      code &= ";\n"
      return indentLine(code, context)
  
  # Check for enum types (uppercase first letter)
  if cType.len > 0 and cType[0].isUpperAscii() and cType != "NULL":
    let valStr = if node.varValue != nil: 
                   " = " & generateExpression(node.varValue) 
                 else: " = 0"
    return indentLine(cType & " " & node.varName & valStr & ";", context)
  
  # Check if this looks like an enum type (starts with uppercase)
  if cType.len > 0 and cType[0].isUpperAscii() and cType != "NULL":
    # For enums, keep the type as-is
    let valStr = if node.varValue != nil: 
                   " = " & generateExpression(node.varValue) 
                 else: " = 0"
    return indentLine(cType & " " & node.varName & valStr & ";", context)

  if node.varValue != nil and node.varValue.kind == nkCall:
    if node.varValue.callFunc == "getmem":
      cType &= "*"  # Turn 'double' into 'double*'
  
    let valStr = if node.varValue != nil: 
                   " = " & generateExpression(node.varValue) 
                 else: ""
                 
    return indentLine(cType & " " & node.varName & valStr & ";", context)

  if ',' in node.varName:
    let 
      names      = node.varName.split(',')
      firstName  = names[0].strip()
      secondName = names[1].strip()
 
    var code = ""
    
    if node.varValue != nil and node.varValue.kind == nkCall:
      let funcCall = node.varValue
      
      if ',' in node.varType:
        let types      = node.varType.split(',')
        let firstType  = types[0].strip() 
        let secondType = types[1].strip()
        
        code = secondType & " " & secondName & " = NULL;\n"
        code &= firstType & " " & firstName & " = " & 
             generateCall(funcCall, cgExpression, secondName) & ";\n"
        return indentLine(code, context)
      else:
        code = "char* " & secondName & " = NULL;\n"
        code &= "int " & firstName & " = " & 
             generateCall(funcCall, cgExpression, secondName) & ";\n"
        return indentLine(code, context)
  
  var
    typeName = node.varType
    isArray = false
    isString = false
    isEnum = false  # Add this flag

  # Check if this is an enum type
  if typeName.len > 0 and typeName[0].isUpperAscii():
    # Check if we have a corresponding enum definition
    # For now, assume any uppercase type is an enum
    isEnum = true

  if typeName.len == 0 and node.varValue != nil:
    if node.varValue.kind == nkStringLit:
      typeName = "char*"
      isString = true
    elif node.varValue.kind == nkArrayLit:
      isArray = true
      if node.varValue.elements.len > 0:
        let firstElem = node.varValue.elements[0]
        if firstElem.kind == nkStringLit: typeName = "char*"
        elif firstElem.kind == nkLiteral:
          if firstElem.literalValue.contains('.') or 
            firstElem.literalValue.contains('e') or 
            firstElem.literalValue.contains('E'):
            typeName = "double"
          else: typeName = "int"
    else: typeName = inferTypeFromExpression(node.varValue)

  elif typeName.endsWith("[]"):
    isArray = true
    typeName = typeName[0 ..^ 3]
  elif typeName.contains("[") and typeName.contains("]"): isArray = true
  elif typeName == "char*": isString = true

  if node.varValue != nil and node.varValue.kind == nkCall:
    if node.varValue.callFunc == "getmem" or node.varValue.callFunc == "alloc":
      if not typeName.endsWith("*"):
        typeName &= "*"

  var code = ""
  if isArray:
    if node.varValue != nil and 
      node.varValue.kind == nkArrayLit: code = typeName & " " & node.varName & "[] = "
    else: code = typeName & "* " & node.varName & " = "
  elif isString: code = "char* " & node.varName & " = "
  elif isEnum: code = typeName & " " & node.varName & " = "  # Use enum type
  else: code = typeName & " " & node.varName & " = "

  if node.varValue != nil: code &= generateExpression(node.varValue)
  else:
    if isString or typeName == "char*": code &= "NULL"
    elif typeName in ["int", "long", "short", "size_t"]: code &= "0"
    elif typeName in ["float", "double"]: code &= "0.0"
    elif typeName == "bool": code &= "false"
    else: code &= "NULL" 

  code &= ";\n"
  return indentLine(code, context)

# =========================== ENUM GENERATORS ============================
proc generateEnum(node: Node): string =
  var code = "typedef enum {\n"
  for i, valueStr in node.enumValues:
    code &= "    " & valueStr
    if i < node.enumValues.len - 1:
      code &= ","
    code &= "\n"
  code &= "} " & node.enumName & ";\n\n"
  return code

# ============================ DECLARATION GENERATORS ============================
proc generateConstDecl(node: Node, context: CodegenContext): string =
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
      if typeArg.kind == nkIdentifier: typeName = typeArg.identName
      callCode     = "malloc(" & generateExpression(countArg) & " * sizeof(" & typeName & "))"
    else: callCode = "malloc(0)"
  
  of "len":
    if node.callArgs.len == 1:
      let arg      = generateExpression(node.callArgs[0])
      callCode     = "sizeof(" & arg & ") / sizeof(" & arg & "[0])"
    else: callCode = "0 /* len() error */"

  of "sizeof":
    if node.callArgs.len > 0:
      let typeName = node.callArgs[0].identName
      let mappedType = case typeName:
        of "float64": "double"
        of "int32": "int"
        else: typeName
      callCode = "sizeof(" & mappedType & ")"
    else: callCode = "0"
  
  of "getmem":
    if node.callArgs.len > 0: callCode = "malloc(" & generateExpression(node.callArgs[0]) & ")"
    else: callCode = "malloc(0)"
  
  of "free", "freemem":
    if node.callArgs.len > 0:
      let arg      = generateExpression(node.callArgs[0])
      callCode     = "free(" & arg & ")" 
    else: callCode = "free(NULL)"
  
  of "print":
    # Just rename 'print' to 'printf' - same arguments, same behavior
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
  else:
    return callCode

# =========================== ASSIGNMENT GENERATORS ===========================
proc generateAssignment(node: Node, context: CodegenContext): string =
  var   leftCode = ""
  case  node.left.kind
  of    nkIdentifier:   leftCode = node.left.identName
  of    nkFieldAccess:  leftCode = generateFieldAccess(node.left)
  else:                 leftCode = generateExpression(node.left)

  var code = leftCode & " = " & generateExpression(node.right) & ";\n"
  return indentLine(code, context)

# ============================ RETURN GENERATORS =============================
proc generateReturn(node: Node, context: CodegenContext): string =
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
  else:
    code = "return /* too many values */;"
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
  var elements: seq[string]
  for elem in node.elements:
    elements.add(generateExpression(elem))
  return "{" & elements.join(", ") & "}"

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
proc generateStruct(node: Node): string =
  var code = "typedef struct {\n"

  # Change node.fields to node.structFields
  for field in node.structFields: 
    code &= "  " & field.varType & " " & field.varName & ";\n"
  
  code &= "} " & node.structName & ";\n\n"
  return code

proc generateStructLiteral(node: Node): string =
  # Check if this is actually an enum initialization
  if node.fieldValues.len == 1:
    let assignment = node.fieldValues[0]
    if assignment.left.identName == "value":
      # This is an enum initialization
      return generateExpression(assignment.right)
  
  # Regular struct literal
  var 
    resultStruct = "{"
    initializers: seq[string]
  for assignment in node.fieldValues:
    initializers.add("." & assignment.left.identName & " = " & 
    generateExpression(assignment.right))
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


# ============================== GENERATE BLOCK ================================
proc generateBlock(node: Node, context: CodegenContext): string =
  if node == nil: return ""
  var
    blockResult = ""
    deferStack: seq[string] = @[]

  for stmt in node.statements:
    case stmt.kind
    of nkDefer:
      let deferCode = generateExpression(stmt.deferExpr)
      deferStack.add(deferCode & ";\n")
    else:
      var stmtCode = ""
      case stmt.kind
      of nkAssignment: stmtCode = generateAssignment(stmt, context)
      of nkReturn:     stmtCode = generateReturn(stmt, context)
      of nkCBlock:     stmtCode = generateCBlock(stmt, context)
      of nkVarDecl:    stmtCode = generateVarDecl(stmt, context)
      of nkConstDecl:  stmtCode = generateConstDecl(stmt, context)
      of nkCall:       stmtCode = generateCall(stmt, context)
      of nkIf:         stmtCode = generateIf(stmt, context)
      of nkFor:        stmtCode = generateFor(stmt, context)
      of nkForRange:   stmtCode = generateForRange(stmt, cgFunction)
      of nkSwitch:     stmtCode = generateSwitch(stmt, context)
      else:
        continue 

      if stmtCode.len > 0: blockResult &= stmtCode

  if deferStack.len > 0:
    for i in countdown(deferStack.len - 1, 0):
      blockResult &= indentLine(deferStack[i], context)

  return blockResult

# =========================== STRUCTURE GENERATORS ============================
proc generateFunction(node: Node): string =
  var code = ""
  
  if node.funcName == "main":
    code = "int main() {\n"
  else:
    code = node.returnType & " " & node.funcName & "("
    
    if node.params.len > 0:
      for i, param in node.params:
        if i > 0: code &= ", "
        code &= param.varType & " " & param.varName

    if node.returnsError:
      if node.params.len > 0: code &= ", "
      code &= "char** error_out"
    elif node.params.len == 0:
      code &= "void"
    code &= ") {\n"
  
  if node.returnsError:
    code &= "  *error_out = NULL;\n"
  
  var
    deferStack: seq[string] = @[]
    hasExplicitReturn = false

  if node.body != nil:
    for stmt in node.body.statements:
      if stmt.kind == nkDefer:
        let deferCode = generateCall(stmt.deferExpr, cgExpression, "") & ";\n"
        deferStack.add("  " & deferCode)
      elif stmt.kind == nkReturn:
        hasExplicitReturn = true
        
        var returnCode = ""
        if deferStack.len > 0:
          returnCode &= "\n  // Execute deferred statements\n"
          for i in countdown(deferStack.len - 1, 0):
            returnCode &= deferStack[i]

        returnCode &= generateReturn(stmt, cgFunction)
        code &= returnCode
      else:
        case stmt.kind
        of nkAssignment:    code &= generateAssignment(stmt, cgFunction)
        of nkCall:          code &= generateCall(stmt, cgFunction, "")
        of nkVarDecl:       code &= generateVarDecl(stmt, cgFunction)
        of nkCBlock:        code &= generateCBlock(stmt, cgFunction)
        of nkConstDecl:     code &= generateConstDecl(stmt, cgFunction)
        of nkIf:            code &= generateIf(stmt, cgFunction)
        of nkFor:           code &= generateFor(stmt, cgFunction)
        of nkForRange:      code &= generateForRange(stmt, cgFunction)
        of nkSwitch:        code &= generateSwitch(stmt, cgFunction)
        else: discard
  
  if not hasExplicitReturn:
    if deferStack.len > 0:
      code &= "\n  // Deferred statements\n"
      for i in countdown(deferStack.len - 1, 0):
        code &= deferStack[i]

    if node.funcName == "main": code &= "  return 0;\n"
    elif node.returnType != "void":
      code &= "  // WARNING: Missing return value\n"
      code &= "  return 0;\n"
  
  code &= "}\n"
  return code

# =========================== SWITCH GENERATOR ============================
proc generateSwitch(node: Node, context: CodegenContext): string =
  var code = "switch (" & generateExpression(node.switchTarget) & ") {\n"
  if node.switchTarget == nil: return ""

  for caseNode in node.cases:
    for i, value in caseNode.caseValues:
      code &= "  case " & generateExpression(value) & ":\n"

    let bodyCode = generateBlock(caseNode.caseBody, cgFunction)
    for line in bodyCode.splitLines:
      if line.len > 0: code &= "    " & line & "\n"
    code &= "    break;\n"

  if node.defaultCase != nil:
    code &= "  default:\n"
    let defaultCode = generateBlock(node.defaultCase.defaultBody, cgFunction)
    for line in defaultCode.splitLines:
      if line.len > 0: code &= "    " & line & "\n"
    code &= "    break;\n"
  code &= "}\n"
  return indentLine(code, context)

# =========================== DEFER GENERATOR ============================
proc generateDefer(node: Node, context: CodegenContext): string =
  return ""

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
proc generateProgram(node: Node): string =
  var
    functionCode =    ""
    hasCMain =        false
    haZalMain =  false
    includes =        ""
    defines =         ""
    otherTopLevel =   ""
    structsCode =     ""

  # First pass: collect everything in correct order
  for funcNode in node.functions:
    case funcNode.kind
    of nkStruct:    structsCode &= generateStruct(funcNode)
    of nkEnum:      structsCode &= generateEnum(funcNode)
    of nkConstDecl: defines &= generateConstDecl(funcNode, cgGlobal)

    of nkCBlock:
      let   cCode = generateCBlock(funcNode, cgGlobal)
      if    cCode.strip().startsWith("#include"): includes &= cCode
      else: otherTopLevel &= cCode
      if    "int main()" in cCode: hasCMain = true

    of nkFunction:
      functionCode &= generateFunction(funcNode)
      if funcNode.funcName == "main": haZalMain = true
    
    of nkVarDecl: otherTopLevel &= generateVarDecl(funcNode, cgGlobal)
    else: discard
  
  result = includes & defines & structsCode & otherTopLevel & functionCode
  
  if not hasCMain and not haZalMain:
    result &= "\nint main() {\n"
    result &= "  // Auto-generated entry point\n"
    result &= "  return 0;\n"
    result &= "}\n"

# =========================== MAIN DISPATCH ============================
proc generateC*(node: Node, context: string = "global"): string =
  let     cgContext =
    if    context == "function": cgFunction
    elif  context == "global": cgGlobal
    else: cgGlobal

  case node.kind
  of nkStruct:          generateStruct(node)
  of nkFieldAccess:     generateFieldAccess(node)
  of nkStructLiteral:   return generateStructLiteral(node)
  of nkProgram:         generateProgram(node)
  of nkPackage:         "// Package: " & node.packageName & "\n"
  of nkFunction:        generateFunction(node)
  of nkAssignment:      generateAssignment(node, cgContext)
  of nkReturn:          generateReturn(node, cgContext)
  of nkBlock:           generateBlock(node, cgContext)
  of nkCBlock:          generateCBlock(node, cgContext)
  of nkConstDecl:       generateConstDecl(node, cgContext)
  of nkArrayType:       generateArrayType(node)
  of nkIdentifier:      generateIdentifier(node)
  of nkEnum:            return generateEnum(node)
  of nkCall:            return generateCall(node, cgExpression)
  of nkIf:              generateIf(node, cgContext)
  of nkFor:             generateFor(node, cgContext)
  of nkAddressOf:       return generateAddressOf(node)  # Add this
  of nkDeref:           return generateDeref(node)      # Add this
  of nkForRange:        return generateForRange(node, cgContext)
  of nkSwitch:          generateSwitch(node, cgContext)
  of nkDefer:           return generateDefer(node, cgContext)
  of nkCase:            "/* case */"
  of nkDefault:         "/* default */"
  of nkSwitchExpr:      "/* switch_expr */"
  of nkGroup:           generateGroup(node, cgContext)
  of nkElse:            ""
  of nkPtrType:         "/* ptr_type */"    # Add handler
  of nkRefType:         "/* ref_type */"    # Add handler
  of nkBinaryExpr, nkIndexExpr, nkArrayLit: generateExpression(node)
  of nkVarDecl, nkInferredVarDecl:          generateVarDecl(node, cgContext)
  of nkLiteral, nkStringLit:                generateLiteral(node)

