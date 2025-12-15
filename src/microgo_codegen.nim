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
proc generateForRange(node: Node, context: CodegenContext): string
proc generateCall(node: Node, context: CodegenContext): string
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
    result = generateFieldAccess(node)
  of nkCall:
    result = generateCall(node, cgExpression)
  of nkStructLiteral:
    result = generateStructLiteral(node)
  of nkIndexExpr:
    result = generateIndexExpr(node)
  of nkArrayLit:
    result = generateArrayLiteral(node)
  of nkBinaryExpr:
    result =
      generateExpression(node.left) & " " & node.op & " " &
      generateExpression(node.right)
  of nkIdentifier:
    result = node.identName
  of nkArrayType:
    result = generateArrayType(node)
  of nkLiteral, nkStringLit:
    result = generateLiteral(node)
  of nkGroup:
    result = "(" & generateExpression(node.groupExpr) & ")"
  else:
    echo "ERROR in generateExpression: Unhandled node kind: ", node.kind
    result = "/* ERROR: unhandled expression */"

# =========================== GROUP GENERATORS ============================
proc generateGroup(node: Node, context: CodegenContext): string =
  "(" & generateExpression(node.groupExpr) & ")"

# =========================== LOOP GENERATORS ============================
proc generateFor(node: Node, context: CodegenContext): string =
  var code = ""

  # Check what type of for loop this is
  if node.forInit == nil and node.forCondition != nil and node.forUpdate == nil:
    # Go-style: for condition { ... } -> while (condition) { ... }
    code = "while (" & generateExpression(node.forCondition) & ") {\n"
  elif node.forInit == nil and node.forCondition == nil and node.forUpdate == nil:
    # Go-style infinite loop: for { ... } -> while (1) { ... }
    code = "while (1) {\n"
  else:
    # C-style three-part for loop
    code = "for ("

    # Initialization
    if node.forInit != nil:
      case node.forInit.kind
      of nkVarDecl:
        code &=
          "int " & node.forInit.varName & " = " &
          generateExpression(node.forInit.varValue)
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

  # Generate body
  let bodyCode = generateBlock(node.forBody, cgFunction)
  for line in bodyCode.splitLines:
    if line.len > 0:
      code &= "  " & line & "\n"

  code &= "}\n"
  return indentLine(code, context)

# =========================== RANGE GENERATORS ============================
proc generateForRange(node: Node, context: CodegenContext): string =
  if node.rangeTarget == nil:
    return indentLine("/* ERROR: No range target */\n", context)

  # Check if target is a range (0..5)
  var isRange = false
  var startVal, endVal: string

  if node.rangeTarget.kind == nkBinaryExpr and node.rangeTarget.op == "..":
    isRange = true
    startVal = generateExpression(node.rangeTarget.left)
    endVal = generateExpression(node.rangeTarget.right)

  var code = ""

  if isRange:
    # Generate: for (int i = start; i <= end; i++)
    if node.rangeValue != nil:
      # for i in 0..5 (value is the index in this case)
      code =
        "for (int " & node.rangeValue.identName & " = " & startVal & "; " &
        node.rangeValue.identName & " <= " & endVal & "; " & node.rangeValue.identName &
        "++) {\n"
    elif node.rangeIndex != nil:
      # Shouldn't happen for ranges, but handle it
      code =
        "for (int " & node.rangeIndex.identName & " = " & startVal & "; " &
        node.rangeIndex.identName & " <= " & endVal & "; " & node.rangeIndex.identName &
        "++) {\n"
    else:
      code = "for (int _i = " & startVal & "; _i <= " & endVal & "; _i++) {\n"
  else:
    # Generate array iteration
    let target = generateExpression(node.rangeTarget)
    code =
      "for (int _i = 0; _i < sizeof(" & target & ") / sizeof(" & target &
      "[0]); _i++) {\n"

    if node.rangeIndex != nil:
      code &= "  int " & node.rangeIndex.identName & " = _i;\n"

    if node.rangeValue != nil:
      code &= "  int " & node.rangeValue.identName & " = " & target & "[_i];\n"

  # Generate body
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
    let val = expr.literalValue

    # Check for character literal
    if val.len >= 2 and val[0] == '\'' and val[^1] == '\'':
      return "char"

    # Check if it's float
    if val.contains('.') or val.contains('e') or val.contains('E'):
      return "double"
    else:
      # Check for special values
      if val == "NULL":
        return "void*"
      if val == "true" or val == "false":
        return "bool"

      # Assume integer
      return "int"
  of nkStringLit:
    return "char*"
  of nkIdentifier:
    # TODO: Look up in symbol table
    return "int"
  of nkCall:
    let funcName = expr.callFunc
    case funcName
    of "alloc":
      if expr.callArgs.len >= 1:
        let typeArg = expr.callArgs[0]
        if typeArg.kind == nkIdentifier:
          return typeArg.identName & "*" # int -> int*
      return "void*"
  of nkArrayLit:
    if expr.elements.len > 0:
      let elemType = inferTypeFromExpression(expr.elements[0])
      return elemType # RIGHT: returns "int" (base type)
    else:
      return "int" # Default for empty array
  of nkBinaryExpr:
    # Simple type inference
    let leftType = inferTypeFromExpression(expr.left)
    let rightType = inferTypeFromExpression(expr.right)

    if leftType == "double" or rightType == "double":
      return "double"
    elif leftType == "char*" or rightType == "char*":
      return "char*"
    else:
      return "int"
  else:
    return "int"

# Then simplify generateVarDecl:
proc generateVarDecl(node: Node, context: CodegenContext): string =
  if node.varValue != nil:
    if node.varValue.kind == nkCall:
      echo "  Call function: ", node.varValue.callFunc
  var typeName = node.varType
  var isArray = false

  # If no explicit type, infer from value
  if typeName.len == 0 and node.varValue != nil:
    typeName = inferTypeFromExpression(node.varValue)

    # Check if value is array literal
    if node.varValue.kind == nkArrayLit:
      isArray = true
      if typeName.endsWith("*"):
        typeName = typeName[0 ..^ 2]

  # Handle array types from explicit annotation
  elif typeName.endsWith("[]"):
    isArray = true
    typeName = typeName[0 ..^ 3]
  elif typeName.contains("[") and typeName.contains("]"):
    # Already has array dimensions
    isArray = true

  # Build the declaration
  var code = ""
  if isArray:
    # Check if we have an array literal to initialize
    if node.varValue != nil and node.varValue.kind == nkArrayLit:
      code = typeName & " " & node.varName & "[] = "
    else:
      # Dynamic array
      code = typeName & "* " & node.varName & " = "
  else:
    code = typeName & " " & node.varName & " = "

  if node.varValue != nil:
    code &= generateExpression(node.varValue)
  else:
    # Default initialization
    if typeName == "char*":
      code &= "NULL"
    elif typeName in ["int", "long", "short", "size_t"]:
      code &= "0"
    elif typeName in ["float", "double"]:
      code &= "0.0"
    elif typeName == "bool":
      code &= "false"
    else:
      code &= "NULL" # Pointer types

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
  of "alloc":
    echo "  Handling alloc function"
    # alloc(type, count) -> malloc(count * sizeof(type))
    if node.callArgs.len == 2:
      let
        typeArg = node.callArgs[0]
        countArg = node.callArgs[1]

      # Get type name from argument
      var typeName = "int" # default
      if typeArg.kind == nkIdentifier:
        typeName = typeArg.identName

      callCode =
        "malloc(" & generateExpression(countArg) & " * sizeof(" & typeName & "))"
    else:
      callCode = "malloc(0)"
  of "len":
    # Generate: sizeof(arr) / sizeof(arr[0])
    if node.callArgs.len == 1:
      let arg = generateExpression(node.callArgs[0])
      callCode = "sizeof(" & arg & ") / sizeof(" & arg & "[0])"
    else:
      callCode = "0 /* len() error */"
  of "getmem":
    if node.callArgs.len > 0:
      callCode = "malloc(" & generateExpression(node.callArgs[0]) & ")"
    else:
      callCode = "malloc(0)"
  of "free", "freemem":
    if node.callArgs.len > 0:
      let arg = generateExpression(node.callArgs[0])
      callCode = "free(" & arg & ")" # Should be just this
    else:
      callCode = "free(NULL)"
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

  # TODO: We need access to the deferStack here!
  # For now, just generate the return

  if node.callArgs.len == 0:
    code = "return"
  elif node.callArgs.len == 1:
    let retVal = generateExpression(node.callArgs[0])
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
  var deferStack: seq[string] = @[]

  for stmt in node.statements:
    case stmt.kind
    of nkDefer:
      # Collect defer expression
      let deferCode = generateExpression(stmt.deferExpr)
      deferStack.add(deferCode & ";\n")
    else:
      # Generate normal statement
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
        stmtCode = generateFor(stmt, context) # <-- FIXED!
      of nkForRange:
        stmtCode = generateForRange(stmt, cgFunction)
      of nkSwitch:
        stmtCode = generateSwitch(stmt, context)
      else:
        continue # Skip unknown statement types

      if stmtCode.len > 0:
        blockResult &= stmtCode

  # Add deferred statements at end of block (in reverse order)
  if deferStack.len > 0:
    blockResult &= "\n  // Deferred statements\n"
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

    # ADD PARAMETERS HERE (BEFORE closing parenthesis)
    if node.params.len > 0:
      for i, param in node.params:
        if i > 0:
          code &= ", "
        code &= param.varType & " " & param.varName
        echo "    Added param: ", param.varType, " ", param.varName
    else:
      code &= "void"
      echo "    No params, adding 'void'"

    # NOW close the parentheses
    code &= ") {\n"
    echo "  Final signature: ", code

  if node.returnsError:
    code &= "  *error_out = NULL;\n"

  # Track defer statements
  var deferStack: seq[string] = @[]
  var hasExplicitReturn = false

  # Generate function body, collecting defers
  if node.body != nil:
    for stmt in node.body.statements:
      if stmt.kind == nkDefer:
        # Collect defer expression for later
        let deferCode = generateCall(stmt.deferExpr, cgExpression) & ";\n"
        deferStack.add("  " & deferCode)
      elif stmt.kind == nkReturn:
        # Handle return statement specially
        hasExplicitReturn = true

        # Generate return with defer execution first
        var returnCode = ""
        if deferStack.len > 0:
          returnCode &= "\n  // Execute deferred statements\n"
          for i in countdown(deferStack.len - 1, 0):
            returnCode &= deferStack[i]

        # Add the actual return
        if stmt.callArgs.len == 0:
          returnCode &= "  return;\n"
        elif stmt.callArgs.len == 1:
          let retVal = generateExpression(stmt.callArgs[0])
          returnCode &= "  return " & retVal & ";\n"
        else:
          returnCode &= "  return;\n"

        code &= returnCode
      else:
        # Generate normal statement
        case stmt.kind
        of nkAssignment:
          code &= generateAssignment(stmt, cgFunction)
        of nkCall:
          code &= generateCall(stmt, cgFunction)
        of nkVarDecl:
          code &= generateVarDecl(stmt, cgFunction)
        of nkCBlock:
          code &= generateCBlock(stmt, cgFunction)
        of nkConstDecl:
          code &= generateConstDecl(stmt, cgFunction)
        of nkIf:
          code &= generateIf(stmt, cgFunction)
        of nkFor:
          code &= generateFor(stmt, cgFunction)
        of nkForRange: # <-- ADD THIS!
          code &= generateForRange(stmt, cgFunction)
        of nkSwitch:
          code &= generateSwitch(stmt, cgFunction)
        else:
          discard

  # If no explicit return, add defer cleanup and default return
  if not hasExplicitReturn:
    if deferStack.len > 0:
      code &= "\n  // Deferred statements\n"
      for i in countdown(deferStack.len - 1, 0):
        code &= deferStack[i]

    # Add default return for main
    if node.funcName == "main":
      code &= "  return 0;\n"
    elif node.returnType != "void":
      # Non-void functions need a return value
      code &= "  // WARNING: Missing return value\n"
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

# =========================== DEFER GENERATOR ============================
proc generateDefer(node: Node, context: CodegenContext): string =
  # Defer statements are handled in generateBlock
  return ""

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
  of nkVarDecl, nkInferredVarDecl:
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
  of nkForRange:
    return generateForRange(node, cgContext)
  of nkSwitch:
    generateSwitch(node, cgContext)
  of nkDefer:
    return generateDefer(node, cgContext)
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
