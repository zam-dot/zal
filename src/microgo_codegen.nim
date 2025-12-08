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

proc generateExpression(node: Node): string =
  case node.kind
  of nkBinaryExpr:
    generateExpression(node.left) & " " & node.op & " " & generateExpression(node.right)
  of nkIdentifier:
    generateIdentifier(node)
  of nkLiteral, nkStringLit:
    generateLiteral(node)
  else:
    ""

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

proc generateVarDecl(node: Node, context: CodegenContext): string =
  var code = "int " & node.varName & " = " & generateExpression(node.varValue) & ";\n"
  return indentLine(code, context)

proc generateCall(node: Node, context: CodegenContext): string =
  var callCode = ""

  if node.callFunc == "print":
    callCode = "printf("

    if node.callArgs.len == 0:
      callCode &= "\"\\n\""
    else:
      let firstArg = node.callArgs[0]

      if firstArg.kind == nkStringLit:
        # Check for format specifiers vs argument count
        let str = firstArg.literalValue
        var percentCount = 0
        for ch in str:
          if ch == '%':
            inc(percentCount)

        if percentCount > 0 and node.callArgs.len == 1:
          echo fmt"""
Warning at line {node.line}:
String has {percentCount} % characters
But print() has only 1 argument
Did you forget arguments for the format specifiers?
"""

        callCode &= generateExpression(firstArg)
        for i in 1 ..< node.callArgs.len:
          callCode &= ", " & generateExpression(node.callArgs[i])
      else:
        callCode &= "\"%d\", " & generateExpression(firstArg)

    callCode &= ");\n"
  else:
    callCode = node.callFunc & "("
    for i, arg in node.callArgs:
      if i > 0:
        callCode &= ", "
      callCode &= generateExpression(arg)
    callCode &= ");\n"

  return indentLine(callCode, context)

proc generateAssignment(node: Node, context: CodegenContext): string =
  var code =
    generateExpression(node.target) & " = " & generateExpression(node.value) & ";\n"
  return indentLine(code, context)

proc generateReturn(node: Node, context: CodegenContext): string =
  var code = "return"
  if node.callArgs.len > 0:
    code &= " " & generateExpression(node.callArgs[0])
  code &= ";\n"
  return indentLine(code, context)

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
    of nkCall:
      stmtCode = generateCall(stmt, context)
    of nkIf:
      stmtCode = generateIf(stmt, context)
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
    code = "void " & node.funcName & "() {\n"

  # Generate the function body (which is a block)
  if node.body != nil:
    code &= generateBlock(node.body, cgFunction)

  if node.funcName == "main":
    code &= "  return 0;\n"

  code &= "}\n"
  return code

proc generateProgram(node: Node): string =
  var
    topLevelCode = ""
    functionCode = ""
    hasCMain = false
    hasMicroGoMain = false

  for funcNode in node.functions:
    case funcNode.kind
    of nkCBlock:
      let cCode = generateCBlock(funcNode, cgGlobal)
      topLevelCode &= cCode
      if "int main()" in cCode:
        hasCMain = true
    of nkFunction:
      functionCode &= generateFunction(funcNode)
      if funcNode.funcName == "main":
        hasMicroGoMain = true
    else:
      discard

  result = topLevelCode & functionCode

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
  of nkElse:
    ""
