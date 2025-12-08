# microgo_codegen.nim - Generate C code from AST
import microgo_parser
import strutils

proc generateC*(node: Node, context: string = "global"): string =
  case node.kind
  of nkProgram:
    result = ""

    var
      hasCMain = false
      hasMicroGoMain = false
      topLevelCode = ""

    for funcNode in node.functions:
      if funcNode.kind == nkCBlock:
        let cCode = generateC(funcNode, "global")
        topLevelCode &= cCode & "\n"
        if "int main()" in cCode:
          hasCMain = true
      elif funcNode.kind == nkFunction:
        if funcNode.funcName == "main":
          hasMicroGoMain = true
        result &= generateC(funcNode, "global") & "\n"

    result = topLevelCode & result

    if not hasCMain and not hasMicroGoMain:
      result &= "\nint main() {\n"
      result &= "  // Auto-generated entry point\n"
      result &= "  return 0;\n"
      result &= "}\n"
  of nkPackage:
    result = "// Package: " & node.packageName & "\n"
  of nkFunction:
    if node.funcName == "main":
      result = "int main() {\n"
    else:
      result = "void " & node.funcName & "() {\n"

    result &= generateC(node.body, "function")

    if node.funcName == "main":
      result &= "  return 0;\n"

    result &= "}\n"
  of nkAssignment:
    result =
      generateC(node.target, context) & " = " & generateC(node.value, context) & ";\n"
    if context == "function":
      result = "  " & result
  of nkReturn:
    if node.callArgs.len > 0:
      result = "return " & generateC(node.callArgs[0], context) & ";\n"
    else:
      result = "return;\n"
    if context == "function":
      result = "  " & result
  of nkBlock:
    result = ""
    for stmt in node.statements:
      let stmtCode = generateC(stmt, context)
      if context == "function":
        result &= "  " & stmtCode
      else:
        result &= stmtCode

      if stmtCode.len > 0 and stmtCode[^1] != '\n':
        result &= "\n"
  of nkCBlock:
    var cCode = node.cCode

    cCode = cCode.strip(leading = false, trailing = true)

    let looksLikeFunction =
      (
        " int " in cCode or " void " in cCode or " char " in cCode or " float " in cCode or
        " double " in cCode
      ) and "(" in cCode and "){" in cCode

    if looksLikeFunction and context != "global":
      echo "Warning: Function definition inside function at line ", node.line
      echo "This may not compile in standard C."
      echo "Consider moving to top level:"
      echo "  @c { ... }  # Instead of inside func ... { @c { ... } }"

    result = cCode
    if result.len > 0 and result[^1] != '\n':
      result &= "\n"

    if context == "function":
      result =
        "  " & result.replace("\n", "\n  ").strip(leading = false, trailing = true) &
        "\n"
  of nkVarDecl:
    result = "int " & node.varName & " = " & generateC(node.varValue, context) & ";\n"
    if context == "function":
      result = "  " & result
  of nkBinaryExpr:
    result =
      generateC(node.left, context) & " " & node.op & " " &
      generateC(node.right, context)
  of nkIdentifier:
    result = node.identName
  of nkLiteral:
    result = node.literalValue
  of nkStringLit:
    var escaped = ""
    for ch in node.literalValue:
      case ch
      of '\n':
        escaped &= "\\n"
      of '\t':
        escaped &= "\\t"
      of '\r':
        escaped &= "\\r"
      of '\\':
        escaped &= "\\\\"
      of '"':
        escaped &= "\\\""
      else:
        escaped &= ch

    result = "\"" & escaped & "\""
  of nkCall:
    var callCode = ""

    if node.callFunc == "print":
      callCode = "printf("

      if node.callArgs.len == 0:
        callCode &= "\"\\n\""
      else:
        let firstArg = node.callArgs[0]
        case firstArg.kind
        of nkStringLit:
          let str = firstArg.literalValue

          var percentCount = 0
          for ch in str:
            if ch == '%':
              inc(percentCount)

          if percentCount > 0 and node.callArgs.len == 1:
            echo "Warning at line ", node.line, ":"
            echo "  String has ", percentCount, " % characters"
            echo "  But print() has only 1 argument"
            echo "  Did you forget arguments for the format specifiers?"

          callCode &= generateC(firstArg, context)
          for i in 1 ..< node.callArgs.len:
            callCode &= ", " & generateC(node.callArgs[i], context)
        else:
          callCode &= "\"%d\", " & generateC(firstArg, context)

      callCode &= ");\n"
    else:
      callCode = node.callFunc & "("
      for i, arg in node.callArgs:
        if i > 0:
          callCode &= ", "
        callCode &= generateC(arg, context)
      callCode &= ");\n"

    if context == "function":
      result = "  " & callCode
    else:
      result = callCode
