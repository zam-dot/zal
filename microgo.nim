# microgo.nim - Main compiler executable
import src/microgo_lexer
import src/microgo_parser
import src/microgo_codegen
import std/[os, osproc, strutils]

proc compileFile(filename: string) =
  echo "Compiling ", filename, "..."

  let
    source = readFile(filename)
    tokens = lex(source)
    parser = newParser(tokens)
    ast = parseProgram(parser)

  if ast == nil:
    echo "Compilation failed!"
    quit(1)

  let cCode = generateC(ast)

  # Create output filename: change .mg to .c
  var outputFile = filename
  if filename.endsWith(".mg"):
    outputFile = filename[0 ..^ 4] & ".c"
  else:
    outputFile = filename & ".c"

  writeFile(outputFile, cCode)
  discard execShellCmd("clang-format -i " & outputFile & " 2>/dev/null")

  echo "✅ Generated ", outputFile

  # Optional: compile with gcc
  var exeFile = filename
  if filename.endsWith(".mg"):
    exeFile = filename[0 ..^ 4]
  else:
    exeFile = filename

  let compileCmd = "gcc " & outputFile & " -o " & exeFile
  echo "Running: ", compileCmd

  let result = execCmdEx(compileCmd)
  if result.exitCode == 0:
    echo "✅ Compiled to ", exeFile
    echo "Run with: ./" & exeFile
  else:
    echo "Compilation failed:"
    echo result.output

when isMainModule:
  if paramCount() == 0:
    echo "MicroGo Compiler"
    echo "Usage: microgo <file.mg>"
    echo ""
    echo "Example:"
    echo "  microgo hello.mg"
    echo "  ./hello"
    quit(1)

  let filename = paramStr(1)
  if not fileExists(filename):
    echo "File not found: ", filename
    quit(1)

  compileFile(filename)
