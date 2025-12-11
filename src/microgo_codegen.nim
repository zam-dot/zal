# microgo_codegen.nim - Generate C code from AST
import microgo_parser
import std/[strutils, strformat]

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
  ## Escape special characters for C string literals
  for ch in str:
    case ch
    of '\n':
      result &= "\\n"
    of '\t':
      result &= "\\t"
    of '\r':
      result &= "\\r"
    of '\\':
      result &= "\\\\"
    of '"':
      result &= "\\\""
    else:
      result &= ch

# microgo_codegen.nim - Update forward declarations section:

# =========================== FORWARD DECLARATIONS ============================
proc generateFor(node: Node, context: CodegenContext): string
proc generateExpression(node: Node): string
proc generateBlock(node: Node, context: CodegenContext): string
proc generateIf(node: Node, context: CodegenContext): string

# ⭐⭐ ADD THESE FOR STRUCTS AND ARRAYS ⭐⭐
proc generateStruct(node: Node): string
proc generateFieldAccess(node: Node): string
proc generateVarDecl(node: Node, context: CodegenContext): string
proc generateStructLiteral(node: Node): string
proc generateIndexExpr(node: Node): string # <-- ADD
proc generateArrayLiteral(node: Node): string # <-- ADD
proc generateArrayType(node: Node): string

# =========================== BASIC EXPRESSION GENERATORS ============================
proc generateLiteral(node: Node): string =
  case node.kind
  of nkLiteral:
    node.literalValue
  of nkStringLit:
    "\"" & escapeString(node.literalValue) & "\""
  else:
    ""

proc generateIdentifier(node: Node): string =
  node.identName

proc generateGroup(node: Node, context: CodegenContext): string =
  "(" & generateExpression(node.groupExpr) & ")"

# =========================== STATEMENT GENERATORS ============================
proc generateCBlock(node: Node, context: CodegenContext): string =
  var cCode = node.cCode.strip(leading = false, trailing = true)

  # Check for function definitions in non-global context
  let looksLikeFunction =
    (
      " int " in cCode or " void " in cCode or " char " in cCode or " float " in cCode or
      " double " in cCode
    ) and "(" in cCode and "){" in cCode

  if looksLikeFunction and context != cgGlobal:
    echo fmt"""
Warning: Function definition inside function at line {node.line}
this may not compile to standard C.
Consider moving to top level:
@c {{ ... }}"""

  if context == cgFunction:
    result = cCode.replace("\n", "\n  ")
  else:
    result = cCode

  if result.len > 0 and result[^1] != '\n':
    result &= "\n"

# microgo_codegen.nim - FIX generateExpression
proc generateExpression(node: Node): string =
  if node == nil:
    return ""

  echo "DEBUG generateExpression: node.kind=",
    node.kind, " callFunc=", (if node.kind == nkCall: node.callFunc else: "N/A")

  case node.kind
  of nkFieldAccess:
    return generateFieldAccess(node)
  of nkCall:
    # Get the raw call (no semicolon)
    var callCode = ""
    let funcName = node.callFunc

    if funcName == "getmem":
      callCode = "malloc(" & generateExpression(node.callArgs[0]) & ")"
    elif funcName == "freemem":
      callCode = "free(" & generateExpression(node.callArgs[0]) & ")"
    elif funcName == "print":
      # ... handle print ...
      callCode = "printf(...)" # Your print logic
    else:
      callCode = funcName & "("
      # ... build args ...
      callCode &= ")"

    return callCode # No semicolon!
  of nkStructLiteral:
    return generateStructLiteral(node)
  of nkIndexExpr: # <-- ADD THIS
    return generateIndexExpr(node)
  of nkArrayLit: # <-- ADD THIS
    return generateArrayLiteral(node)
  of nkBinaryExpr:
    generateExpression(node.left) & " " & node.op & " " & generateExpression(node.right)
  of nkIdentifier:
    node.identName
  of nkArrayType:
    return generateArrayType(node)
  of nkLiteral, nkStringLit:
    generateLiteral(node)
  of nkGroup:
    "(" & generateExpression(node.groupExpr) & ")"
  else:
    echo "ERROR in generateExpression: Unhandled node kind: ", node.kind
    "/* ERROR: unhandled expression */"

# =========================== DECLARATION GENERATORS ============================
proc generateVarDecl(node: Node, context: CodegenContext): string =
  var typeName = "int" # Default fallback
  var isArray = false

  # ⭐⭐ SPECIAL CASE: If assigning getmem result, default to void* ⭐⭐
  if node.varValue != nil and node.varValue.kind == nkCall and
      node.varValue.callFunc == "getmem":
    typeName = "void*"
  elif node.varType.len > 0:
    # Use explicit type if provided
    typeName = node.varType

    # Check if it's an array type (ends with [])
    if typeName.endsWith("[]"):
      isArray = true
      typeName = typeName[0 ..^ 3] # Remove "[]" from type
  elif node.varValue != nil:
    # Infer from value
    case node.varValue.kind
    of nkArrayLit:
      # Infer array type from first element
      isArray = true
      if node.varValue.elements.len > 0:
        let firstElem = node.varValue.elements[0]
        case firstElem.kind
        of nkLiteral:
          if firstElem.literalValue.contains('.'):
            typeName = "double"
          else:
            typeName = "int"
        of nkStringLit:
          typeName = "char*"
        else:
          typeName = "int"
      else:
        typeName = "int" # Empty array
    of nkLiteral:
      if node.varValue.literalValue.contains('.'):
        typeName = "double"
      else:
        typeName = "int"
    of nkStringLit:
      typeName = "char*"
    of nkCall:
      # If it's a function call (like getmem), default to void*
      typeName = "void*"
    else:
      typeName = "int"

  # Build the declaration
  var code = ""
  if isArray:
    # C syntax: type name[] = { ... };
    code = typeName & " " & node.varName & "[] = "
  else:
    code = typeName & " " & node.varName & " = "

  if node.varValue != nil:
    code &= generateExpression(node.varValue)
  else:
    code &= "0"

  code &= ";\n"
  return indentLine(code, context)

# ============================ DECLARATION GENERATORS ============================
proc generateConstDecl(node: Node, context: CodegenContext): string =
  var
    constExpr = "0"
    constType = "int"

  # ✅ USE THE EXPLICIT TYPE IF PROVIDED
  if node.constType.len > 0:
    constType = node.constType
  else:
    # Infer from value if no explicit type
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
    of nkIdentifier:
      constExpr = node.constValue.identName
    else:
      discard

  # Generate constExpr from value
  if constExpr == "0": # Only if we didn't set it above
    constExpr = generateExpression(node.constValue)

  if context == cgFunction:
    var code = "const " & constType & " " & node.constName & " = " & constExpr & ";\n"
    return indentLine(code, context)
  else:
    return "#define " & node.constName & " " & constExpr & "\n"

# ============================ CALL GENERATORS ============================
# microgo_codegen.nim - FIX generateCall to handle context properly
proc generateCall(node: Node, context: CodegenContext): string =
  let funcName = node.callFunc

  echo "DEBUG generateCall: funcName='", funcName, "' context=", context

  var callCode = ""

  # Build the function call
  case funcName
  of "getmem":
    callCode = "malloc("
    if node.callArgs.len > 0:
      callCode &= generateExpression(node.callArgs[0])
    else:
      callCode &= "0"
    callCode &= ")"
  of "freemem":
    callCode = "free("
    if node.callArgs.len > 0:
      callCode &= generateExpression(node.callArgs[0])
    else:
      callCode &= "NULL"
    callCode &= ")"
  of "print":
    callCode = "printf("
    if node.callArgs.len == 0:
      callCode &= "\"\\n\""
    else:
      let firstArg = node.callArgs[0]
      case firstArg.kind
      of nkStringLit:
        callCode &= generateExpression(firstArg)
        for i in 1 ..< node.callArgs.len:
          callCode &= ", " & generateExpression(node.callArgs[i])
      of nkLiteral:
        if firstArg.literalValue.contains('.') or firstArg.literalValue.contains('e') or
            firstArg.literalValue.contains('E'):
          callCode &= "\"%g\", " & generateExpression(firstArg)
        else:
          callCode &= "\"%d\", " & generateExpression(firstArg)
      of nkIdentifier:
        callCode &= "\"%d\", " & generateExpression(firstArg)
      else:
        callCode &= "\"%d\", " & generateExpression(firstArg)
    callCode &= ")"
  else:
    # Regular function call
    callCode = funcName & "("
    if node.callArgs.len > 0:
      for i, arg in node.callArgs:
        if i > 0:
          callCode &= ", "
        callCode &= generateExpression(arg)
    callCode &= ")"

  # ⭐⭐ CRITICAL FIX: Add semicolon for statements ⭐⭐
  if context != cgExpression:
    callCode &= ";\n"
    return indentLine(callCode, context)
  else:
    return callCode # No semicolon for expressions

# =========================== ASSIGNMENT GENERATORS ===========================
proc generateAssignment(node: Node, context: CodegenContext): string =
  # Generate left side based on type
  var leftCode = ""
  case node.left.kind
  of nkIdentifier:
    leftCode = node.left.identName
  of nkFieldAccess:
    leftCode = generateFieldAccess(node.left)
  else:
    # Try expression as fallback
    leftCode = generateExpression(node.left)

  var code = leftCode & " = " & generateExpression(node.right) & ";\n"
  return indentLine(code, context)

# ============================ RETURN GENERATORS =============================
proc generateReturn(node: Node, context: CodegenContext): string =
  var code = "return"
  if node.callArgs.len > 0:
    code &= " " & generateExpression(node.callArgs[0])
  code &= ";\n"
  return indentLine(code, context)

# ============================ ARRAY TYPE GENERATORS ==========================
proc generateArrayType(node: Node): string =
  # Convert from your AST format to C type
  # Example: nkArrayType with elemType="int", size=Node(literalValue="5")
  # Should generate: "int[5]"

  let sizeStr =
    if node.size != nil:
      generateExpression(node.size) # Get the size expression
    else:
      "" # For unsized arrays

  if sizeStr.len > 0:
    return node.elemType & "[" & sizeStr & "]"
  else:
    return node.elemType & "[]" # Or handle dynamic arrays

# ============================= INDEX GENERATORS ==============================
proc generateIndexExpr(node: Node): string =
  let base = generateExpression(node.left)
  let index = generateExpression(node.right)
  return base & "[" & index & "]"

# ============================ ARRAY GENERATORS =============================
proc generateArrayLiteral(node: Node): string =
  var elements: seq[string]
  for elem in node.elements:
    elements.add(generateExpression(elem))
  return "{" & elements.join(", ") & "}"

# ============================== IF GENERATORS ================================
proc generateIf(node: Node, context: CodegenContext): string =
  var code = "if (" & generateExpression(node.ifCondition) & ") {\n"

  # Generate the then block
  let thenCode = generateBlock(node.ifThen, context)
  # Add indentation to each line of the then block
  for line in thenCode.splitLines:
    if line.len > 0:
      code &= "  " & line & "\n"

  code &= "}"

  # Handle else/else if
  if node.ifElse != nil:
    code &= " else "
    if node.ifElse.kind == nkIf:
      # else if - recursively generate
      code &= generateIf(node.ifElse, context)
    else:
      # else block
      code &= "{\n"
      let elseCode = generateBlock(node.ifElse, context)
      for line in elseCode.splitLines:
        if line.len > 0:
          code &= "  " & line & "\n"
      code &= "}"

  code &= "\n"
  return indentLine(code, context)

# =========================== STRUCT GENERATORS ============================
proc generateStruct(node: Node): string =
  var code = "typedef struct {\n"

  # 1. FIX: Use 'node.fields' (where the fields are stored)
  for field in node.fields:
    # This inner logic is correct, as fields are nkVarDecl nodes:
    code &= "  " & field.varType & " " & field.varName & ";\n"

  # 2. FIX: Use 'node.structName' (where the name is stored)
  code &= "} " & node.structName & ";\n\n"
  return code

proc generateStructLiteral(node: Node): string =
  echo "DEBUG: Generating struct literal for type: ", node.structType
  var resultStruct = "{"

  var initializers: seq[string]
  for assignment in node.fieldValues:
    let fieldName = assignment.left.identName
    let fieldValue = generateExpression(assignment.right)

    initializers.add("." & fieldName & " = " & fieldValue)

  resultStruct.add(initializers.join(", "))
  resultStruct.add("}")
  return resultStruct

# ============================ FIELD ACCESS GENERATORS ============================
proc generateFieldAccess(node: Node): string =
  # obj.field
  let base = generateExpression(node.base) # <-- FIX: node.base
  let field = node.field.identName # <-- FIX: node.field.identName

  # For now, simple: base.field
  # Later: handle pointers (base->field)
  return base & "." & field

# ============================== GENERATE BLOCK ================================
proc generateBlock(node: Node, context: CodegenContext): string =
  if node == nil:
    return ""

  var blockResult = ""
  for stmt in node.statements:
    var stmtCode = ""

    case stmt.kind
    of nkAssignment:
      stmtCode = generateAssignment(stmt, context)
    of nkReturn:
      stmtCode = generateReturn(stmt, context)
    of nkCBlock:
      stmtCode = generateCBlock(stmt, context)
    of nkVarDecl:
      stmtCode = generateVarDecl(stmt, context)
    of nkConstDecl:
      stmtCode = generateConstDecl(stmt, context)
    of nkCall:
      stmtCode = generateCall(stmt, context)
    of nkIf:
      stmtCode = generateIf(stmt, context)
    of nkFor:
      stmtCode = generateFor(stmt, context)
    else:
      continue # Skip unsupported statement types

    if stmtCode.len > 0:
      blockResult &= stmtCode
      if stmtCode[^1] != '\n':
        blockResult &= "\n"

  return blockResult

# =========================== STRUCTURE GENERATORS ============================
proc generateFunction(node: Node): string =
  var code = ""

  if node.funcName == "main":
    code = "int main() {\n"
  else:
    code = node.returnType & " " & node.funcName & "("

    # Generate parameters with correct types
    if node.params.len > 0:
      for i, param in node.params:
        if i > 0:
          code &= ", "
        # param is nkVarDecl, so use varType and varName
        code &= param.varType & " " & param.varName

    code &= ") {\n"

  # Generate the function body (which is a block)
  if node.body != nil:
    code &= generateBlock(node.body, cgFunction)

  if node.funcName == "main":
    code &= "  return 0;\n"

  code &= "}\n"
  return code

# ============================= PROGRAM GENERATOR ==============================
proc generateProgram(node: Node): string =
  var
    functionCode = ""
    hasCMain = false
    hasMicroGoMain = false
    includes = "" # Separate includes section
    defines = "" # Separate defines section
    otherTopLevel = "" # Other top-level code
    structsCode = ""

  # First pass: collect everything in correct order
  for funcNode in node.functions:
    case funcNode.kind
    of nkStruct:
      structsCode &= generateStruct(funcNode)
    of nkConstDecl:
      defines &= generateConstDecl(funcNode, cgGlobal)
    of nkCBlock:
      let cCode = generateCBlock(funcNode, cgGlobal)
      # Check if it's an include statement
      if cCode.strip().startsWith("#include"):
        includes &= cCode
      else:
        otherTopLevel &= cCode
      if "int main()" in cCode:
        hasCMain = true
    of nkFunction:
      functionCode &= generateFunction(funcNode)
      if funcNode.funcName == "main":
        hasMicroGoMain = true
    of nkVarDecl:
      # Handle global variables if you want
      otherTopLevel &= generateVarDecl(funcNode, cgGlobal)
    else:
      discard

  # Build result in correct order: includes first, then defines, then other code
  result = includes & defines & structsCode & otherTopLevel & functionCode

  # Add auto-generated main if none exists
  if not hasCMain and not hasMicroGoMain:
    result &= "\nint main() {\n"
    result &= "  // Auto-generated entry point\n"
    result &= "  return 0;\n"
    result &= "}\n"

# =========================== LOOP GENERATORS ============================
proc generateFor(node: Node, context: CodegenContext): string =
  var code = "for ("

  # Initialization
  if node.forInit != nil:
    case node.forInit.kind
    of nkVarDecl:
      code &=
        "int " & node.forInit.varName & " = " & generateExpression(
          node.forInit.varValue
        )
    of nkAssignment:
      code &=
        generateExpression(node.forInit.left) & " = " &
        generateExpression(node.forInit.right)
    else:
      # Regular expression
      code &= generateExpression(node.forInit)
  code &= "; "

  # Condition
  if node.forCondition != nil:
    code &= generateExpression(node.forCondition)
  code &= "; "

  # Update
  if node.forUpdate != nil:
    code &= generateExpression(node.forUpdate)
  code &= ") {\n"

  # Body
  let bodyCode = generateBlock(node.forBody, cgFunction)
  for line in bodyCode.splitLines:
    if line.len > 0:
      code &= "  " & line & "\n"

  code &= "}\n"
  return indentLine(code, context)

# =========================== MAIN DISPATCH ============================
proc generateC*(node: Node, context: string = "global"): string =
  let cgContext =
    if context == "function":
      cgFunction
    elif context == "global":
      cgGlobal
    else:
      cgGlobal

  case node.kind
  of nkStruct:
    generateStruct(node)
  of nkFieldAccess:
    generateFieldAccess(node)
  of nkStructLiteral:
    return generateStructLiteral(node)
  of nkProgram:
    generateProgram(node)
  of nkPackage:
    "// Package: " & node.packageName & "\n"
  of nkFunction:
    generateFunction(node)
  of nkAssignment:
    generateAssignment(node, cgContext)
  of nkReturn:
    generateReturn(node, cgContext)
  of nkBlock:
    generateBlock(node, cgContext)
  of nkCBlock:
    generateCBlock(node, cgContext)
  of nkVarDecl:
    generateVarDecl(node, cgContext)
  of nkConstDecl:
    generateConstDecl(node, cgContext)
  of nkBinaryExpr, nkIndexExpr, nkArrayLit: # Group them together
    generateExpression(node)
  of nkArrayType: # <-- ADD THIS CASE
    generateArrayType(node)
  of nkIdentifier:
    generateIdentifier(node)
  of nkLiteral, nkStringLit:
    generateLiteral(node)
  of nkCall:
    generateCall(node, cgContext)
  of nkIf:
    generateIf(node, cgContext)
  of nkFor:
    generateFor(node, cgContext)
  of nkGroup:
    generateGroup(node, cgContext)
  of nkElse:
    ""
