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
proc generateFor(node: Node, context: CodegenContext): string
proc generateExpression(node: Node): string
proc generateBlock(node: Node, context: CodegenContext): string
proc generateIf(node: Node, context: CodegenContext): string

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

proc generateExpression(node: Node): string =
  if node == nil:
    return ""

  case node.kind
  of nkBinaryExpr:
    generateExpression(node.left) & " " & node.op & " " & generateExpression(node.right)
  of nkIdentifier:
    generateIdentifier(node)
  of nkLiteral, nkStringLit:
    generateLiteral(node)
  of nkCall:
    # Generate function call
    var callCode = node.callFunc & "("
    if node.callArgs.len > 0:
      for i, arg in node.callArgs:
        if i > 0:
          callCode &= ", "
        callCode &= generateExpression(arg)
    callCode &= ")"
    return callCode
  of nkGroup:
    "(" & generateExpression(node.groupExpr) & ")"
  else:
    # Add handlers for other expression-like nodes
    case node.kind
    of nkAssignment:
      generateExpression(node.target) & " = " & generateExpression(node.value)
    else:
      echo "Error: Cannot generate expression for node kind: ", node.kind
      "ERROR"

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
proc generateVarDecl(node: Node, context: CodegenContext): string =
  var typeName = "int" # Default fallback

  # Use the explicit type if provided
  if node.varType.len > 0:
    typeName = node.varType
  elif node.varValue != nil:
    # Infer from value if no explicit type
    case node.varValue.kind
    of nkLiteral:
      if node.varValue.literalValue.contains('.'):
        typeName = "double"
      else:
        typeName = "int"
    of nkStringLit:
      typeName = "char*"
    else:
      typeName = "int" # Default

  var code = typeName & " " & node.varName & " = "

  if node.varValue != nil:
    code &= generateExpression(node.varValue)
  else:
    code &= "0" # Default value

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
proc generateCall(node: Node, context: CodegenContext): string =
  let funcName = node.callFunc

  if funcName != "print":
    # Regular function - simple!
    var callCode = funcName & "("
    if node.callArgs.len > 0:
      for i, arg in node.callArgs:
        if i > 0:
          callCode &= ", "
        callCode &= generateExpression(arg)
    callCode &= ");\n"
    return indentLine(callCode, context)

  # Otherwise, handle print (your existing code)
  var callCode = "printf("

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
      # Check if it looks like a float (contains '.' or scientific notation)
      if firstArg.literalValue.contains('.') or firstArg.literalValue.contains('e') or
          firstArg.literalValue.contains('E'):
        callCode &= "\"%g\", " & generateExpression(firstArg) # Float
      else:
        callCode &= "\"%d\", " & generateExpression(firstArg) # Integer
    of nkIdentifier:
      # For identifiers, default to %d (integer)
      callCode &= "\"%d\", " & generateExpression(firstArg)
    else:
      callCode &= "\"%d\", " & generateExpression(firstArg)

  callCode &= ");\n"
  return indentLine(callCode, context)

# =========================== ASSIGNMENT GENERATORS ===========================
proc generateAssignment(node: Node, context: CodegenContext): string =
  var code =
    generateExpression(node.target) & " = " & generateExpression(node.value) & ";\n"
  return indentLine(code, context)

# ============================ RETURN GENERATORS =============================
proc generateReturn(node: Node, context: CodegenContext): string =
  var code = "return"
  if node.callArgs.len > 0:
    code &= " " & generateExpression(node.callArgs[0])
  code &= ";\n"
  return indentLine(code, context)

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

  # First pass: collect everything in correct order
  for funcNode in node.functions:
    case funcNode.kind
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
  result = includes & defines & otherTopLevel & functionCode

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
        generateExpression(node.forInit.target) & " = " &
        generateExpression(node.forInit.value)
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
  of nkBinaryExpr:
    generateExpression(node)
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
