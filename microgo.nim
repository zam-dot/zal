# microgo.nim - Main compiler executable
import src/microgo_lexer
import src/microgo_parser
import src/microgo_codegen
import std/[os, osproc, strutils]

# ======================== COMPILE TO C CODE =============================
proc compileToC(source: string): string =
  let tokens = lex(source)
  let parser = newParser(tokens)
  let ast = parseProgram(parser)

  if ast == nil:
    raise newException(ValueError, "Parsing failed!")
  return generateC(ast)

# =========================== FORMAT CODE ================================
proc formatCode(cCode: string, filename: string): string =
  let tmpFile = filename & ".tmp.c"
  writeFile(tmpFile, cCode)

  let formatResult = execShellCmd("clang-format -i " & tmpFile & " 2>/dev/null")
  if formatResult == 0:
    result = readFile(tmpFile)
  else:
    result = cCode

  removeFile(tmpFile)
  return result

# ========================== COMPILE WITH GCC =============================
proc compileWithGCC(cFilename: string, outputExe: string): bool =
  let compileCmd = "gcc " & cFilename & " -o " & outputExe
  let theResult = execCmdEx(compileCmd)

  if theResult.exitCode == 0:
    echo "Compiled to: ", outputExe
    return true
  else:
    echo "Compilation failed:"
    echo theResult.output
    return false

# ========================== COMPILE FILE =================================
proc compileFile(filename: string, compileToExecutabe: bool = true): bool =
  echo "Compiling ", filename, "..."

  if not fileExists(filename):
    echo "File not found: ", filename
    return false

  let
    source = readFile(filename)
    cCode = compileToC(source)

  var baseName = filename
  if filename.endsWith(".mg"):
    baseName = filename[0 ..^ 4]

  let
    cFilename = baseName & ".c"
    formattedCCode = formatCode(cCode, cFilename)

  writeFile(cFilename, formattedCCode)
  echo "Generated ", cFilename

  if compileToExecutabe:
    let outputExe = baseName
    if compileWithGCC(cFilename, outputExe):
      echo "Run with: ./", outputExe
      return true
    else:
      return false
  else:
    return true

proc showUsage() =
  echo """
MicroGo Compiler"
Usage: microgo [options] <file.mg>"

Options:"
  -c     Compile to C only, don't create executable"
  --help Show this help message"

Examples:"
  microgo hello.mg          # Compile to executable"
  microgo -c hello.mg       # Compile to C only"
  ./hello                   # Run the compiled program"
"""

# ========================== MAIN======================================
proc main() =
  if paramCount() == 0:
    showUsage()
    quit(1)

  var
    filename = ""
    compileToExecutabe = true

  for i in 1 .. paramCount():
    let arg = paramStr(i)

    case arg
    of "-c":
      compileToExecutabe = false
    of "--help", "-h":
      showUsage()
      quit(0)
    else:
      if arg.startsWith("-"):
        echo "Unknown option: ", arg
        showUsage()
        quit(1)
      else:
        filename = arg

  if filename == "":
    echo "No file specified"
    showUsage()
    quit(1)

  if not compileFile(filename, compileToExecutabe):
    quit(1)

when isMainModule:
  main()
