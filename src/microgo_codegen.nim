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

# =========================== FORWARD DECLARATIONS ============================
proc generateExpression(node: Node): string
proc generateBlock(node: Node, context: CodegenContext): string
proc generateFieldAccess(node: Node): string
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
  of nkStringLit:
    "\"" & escapeString(node.literalValue) & "\""
  else:
    ""

proc generateIdentifier(node: Node): string =
  node.identName

# =========================== EXPRESSION GENERATORS ============================
proc generateExpression(node: Node): string =
  if node == nil:
    return ""

  case node.kind
  of nkFieldAccess:
    return generateFieldAccess(node)
  of nkCall:
    var callCode = ""
    let funcName = node.callFunc

    if funcName == "getmem":
      callCode = "(size_t)malloc(" & generateExpression(node.callArgs[0]) & ")"
    elif funcName == "freemem":
      callCode = "free(" & generateExpression(node.callArgs[0]) & ")"
    elif funcName == "print":
      callCode = "printf(...)"
    elif funcName == "len":
      if node.callArgs.len == 1:
        let arg = generateExpression(node.callArgs[0])
        callCode = "sizeof(" & arg & ") / sizeof(" & arg & "[0])"
      else:
        callCode = "0"
    else:
      callCode = funcName & "("
      if node.callArgs.len > 0:
        for i, arg in node.callArgs:
          if i > 0:
            callCode &= ", "
          callCode &= generateExpression(arg)
      callCode &= ")"

    return callCode # No semicolon!
  of nkStructLiteral:
    return generateStructLiteral(node)
  of nkIndexExpr:
    return generateIndexExpr(node)
  of nkArrayLit:
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

# =========================== GROUP GENERATORS ============================
proc generateGroup(node: Node, context: CodegenContext): string =
  "(" & generateExpression(node.groupExpr) & ")"

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

# =========================== DECLARATION GENERATORS ============================
proc inferTypeFromExpression(expr: Node): string =
  ## Infer C type from an expression node
  if expr == nil:
    return "int"

  case expr.kind
  of nkLiteral:
    if expr.literalValue.contains('.'):
      return "double"
    else:
      return "int"
  of nkStringLit:
    return "char*"
  of nkCall:
    let funcName = expr.callFunc
    case funcName
    of "getmem":
      return "size_t"
    of "openFile", "fopen", "openfile":
      return "FILE *"
    of "malloc", "calloc":
      return "void*"
    of "len":
      return "size_t"
    of "print":
      return "int" # printf returns int
    else:
      # Pattern matching on function names
      if funcName.contains("open") or funcName.contains("fopen") or
          funcName.contains("file") or funcName.contains("File"):
        return "FILE*"
      elif funcName.contains("alloc") or funcName.contains("malloc"):
        return "void*"
      elif funcName.contains("str") or funcName.contains("string"):
        return "char*"
      else:
        return "size_t*" # Generic pointer fallback
  of nkArrayLit:
    if expr.elements.len > 0:
      return inferTypeFromExpression(expr.elements[0]) & "*" # Array becomes pointer
    else:
      return "int*"
  else:
    return "size_t*"

# Then simplify generateVarDecl:
proc generateVarDecl(node: Node, context: CodegenContext): string =
  var typeName = "int"
  var isArray = false

  if node.varType.len > 0:
    typeName = node.varType
    if typeName.endsWith("[]"):
      isArray = true
      typeName = typeName[0 ..^ 3]
  elif node.varValue != nil:
    typeName = inferTypeFromExpression(node.varValue)
    # Check if value is array literal
    if node.varValue.kind == nkArrayLit:
      isArray = true
      # Remove the * that inferTypeFromExpression might have added
      if typeName.endsWith("*"):
        typeName = typeName[0 ..^ 2]

  # Build the declaration
  var code = ""
  if isArray:
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
  if constExpr == "0":
    constExpr = generateExpression(node.constValue)

  if context == cgFunction:
    var code = "const " & constType & " " & node.constName & " = " & constExpr & ";\n"
    return indentLine(code, context)
  else:
    return "#define " & node.constName & " " & constExpr & "\n"

# ============================ CALL GENERATORS ============================
proc generateCall(node: Node, context: CodegenContext): string =
  let funcName = node.callFunc
  var callCode = ""

  case funcName
  of "len":
    # Generate: sizeof(arr) / sizeof(arr[0])
    if node.callArgs.len == 1:
      let arg = generateExpression(node.callArgs[0])
      callCode = "sizeof(" & arg & ") / sizeof(" & arg & "[0])"
    else:
      callCode = "0 /* len() error */"
  of "getmem":
    callCode = "(size_t)malloc("
    if node.callArgs.len > 0:
      callCode &= generateExpression(node.callArgs[0])
    else:
      callCode &= "0"
    callCode &= ")"
  of "freemem":
    callCode = "free((void*)("
    if node.callArgs.len > 0:
      callCode &= generateExpression(node.callArgs[0])
    else:
      callCode &= "0"
    callCode &= "))"
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

    # Add regular arguments
    if node.callArgs.len > 0:
      for i, arg in node.callArgs:
        if i > 0:
          callCode &= ", "
        callCode &= generateExpression(arg)

    callCode &= ")"

  if context != cgExpression:
    callCode &= ";\n"
    return indentLine(callCode, context)
  else:
    return callCode

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
    leftCode = generateExpression(node.left)

  var code = leftCode & " = " & generateExpression(node.right) & ";\n"
  return indentLine(code, context)

# ============================ RETURN GENERATORS =============================
proc generateReturn(node: Node, context: CodegenContext): string =
  var code = ""

  if node.callArgs.len == 0:
    code = "return"
  elif node.callArgs.len == 1:
    let retVal = generateExpression(node.callArgs[0])

    if node.callArgs[0].kind == nkStringLit:
      code = "*error_out = " & retVal & ";\n"
      code &= "  return"
    else:
      code = "return " & retVal
  else:
    discard

  code &= ";\n"
  return indentLine(code, context)

# ============================ ARRAY TYPE GENERATORS ==========================
proc generateArrayType(node: Node): string =
  let sizeStr =
    if node.size != nil:
      generateExpression(node.size)
    else:
      ""

  if sizeStr.len > 0:
    return node.elemType & "[" & sizeStr & "]"
  else:
    return node.elemType & "[]"

# ============================= INDEX GENERATORS ==============================
proc generateIndexExpr(node: Node): string =
  let
    base = generateExpression(node.left)
    index = generateExpression(node.right)
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
      code &= generateIf(node.ifElse, context)
    else:
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

  for field in node.fields:
    code &= "  " & field.varType & " " & field.varName & ";\n"

  code &= "} " & node.structName & ";\n\n"
  return code

proc generateStructLiteral(node: Node): string =
  var resultStruct = "{"

  var initializers: seq[string]
  for assignment in node.fieldValues:
    let
      fieldName = assignment.left.identName
      fieldValue = generateExpression(assignment.right)

    initializers.add("." & fieldName & " = " & fieldValue)

  resultStruct.add(initializers.join(", "))
  resultStruct.add("}")
  return resultStruct

# ============================ FIELD ACCESS GENERATORS ============================
proc generateFieldAccess(node: Node): string =
  let
    base = generateExpression(node.base)
    field = node.field.identName

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
    of nkSwitch:
      stmtCode = generateSwitch(stmt, context)
    else:
      continue

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

    if node.params.len > 0:
      for i, param in node.params:
        if i > 0:
          code &= ", "
        code &= param.varType & " " & param.varName

    if node.returnsError:
      if node.params.len > 0:
        code &= ", "
      code &= "char** error_out"
    code &= ") {\n"

  if node.returnsError:
    code &= "  *error_out = NULL;\n"

  # Generate body
  if node.body != nil:
    code &= generateBlock(node.body, cgFunction)

  # Add default return for main
  if node.funcName == "main":
    code &= "  return 0;\n"

  code &= "}\n"
  return code

# =========================== SWITCH GENERATOR ============================
proc generateSwitch(node: Node, context: CodegenContext): string =
  var code = "switch (" & generateExpression(node.switchTarget) & ") {\n"

  # Check if node has proper data
  if node.switchTarget == nil:
    return ""

  for caseNode in node.cases:
    for i, value in caseNode.caseValues:
      code &= "  case " & generateExpression(value) & ":\n"

    # Generate case body (with proper indentation)
    let bodyCode = generateBlock(caseNode.caseBody, cgFunction)
    for line in bodyCode.splitLines:
      if line.len > 0:
        code &= "    " & line & "\n"

    # Add break after each case (unless it falls through)
    code &= "    break;\n"

  # Generate default case
  if node.defaultCase != nil:
    code &= "  default:\n"
    let defaultCode = generateBlock(node.defaultCase.defaultBody, cgFunction)
    for line in defaultCode.splitLines:
      if line.len > 0:
        code &= "    " & line & "\n"
    code &= "    break;\n"

  code &= "}\n"
  return indentLine(code, context)

# =========================== CASE GENERATOR ============================
proc generateCase(node: Node, context: CodegenContext): string {.used.} =
  var code = ""
  for i, value in node.caseValues:
    if i > 0:
      code &= "case "
    else:
      code &= "case "
    code &= generateExpression(value)

    if i < node.caseValues.len - 1:
      code &= ":\n"
  code &= ":\n"

  # Add the body
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
    functionCode = ""
    hasCMain = false
    hasMicroGoMain = false
    includes = ""
    defines = ""
    otherTopLevel = ""
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
  of nkBinaryExpr, nkIndexExpr, nkArrayLit:
    generateExpression(node)
  of nkArrayType:
    generateArrayType(node)
  of nkIdentifier:
    generateIdentifier(node)
  of nkLiteral, nkStringLit:
    generateLiteral(node)
  of nkCall:
    return generateCall(node, cgExpression)
  of nkIf:
    generateIf(node, cgContext)
  of nkFor:
    generateFor(node, cgContext)
  of nkSwitch:
    generateSwitch(node, cgContext)
  of nkCase:
    "/* case */"
  of nkDefault:
    "/* default */"
  of nkSwitchExpr:
    "/* switch_expr */"
  of nkGroup:
    generateGroup(node, cgContext)
  of nkElse:
    ""
