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
  echo "DEBUG: Input code length: ", cCode.len
  echo "DEBUG: First 100 chars: ", cCode[0 .. min(99, cCode.high)]
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
proc compileAndRun(cFilename: string): bool =
  let cmd = "tcc -run " & cFilename & " 2>&1"
  let theResult = execCmdEx(cmd)

  if theResult.exitCode == 0:
    echo "✅ Program output:"
    echo theResult.output
    return true
  else:
    echo "❌ Compilation/Runtime failed:"
    echo theResult.output
    return false
  #  let theResult = execCmdEx(compileCmd)
  # if theResult.exitCode == 0:
  #   #    echo "Compiled to: ", outputExe
  #   return true
  # else:
  #   echo "Compilation failed:"
  #   echo theResult.output
  #   return false

# ========================== COMPILE FILE =================================
proc compileFile(filename: string, runImmediately: bool = true): bool =
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

  if runImmediately:
    if compileAndRun(cFilename):
      return true
    else:
      return false
  else:
    return true

proc showUsage() =
  echo """
MicroGo Compiler
Usage: 
  microgo <file.mg>           # Compile and run immediately
  microgo run <file.mg>       # Same as above
  microgo build <file.mg>     # Compile to C only
  
Options:
  -h, --help                  Show this help message

Examples:
  microgo hello.mg            # Compile and run
  microgo run hello.mg        # Same as above
  microgo build hello.mg      # Generate hello.c only
"""

proc runFile(filename: string): bool =
  compileFile(filename, runImmediately = true)

proc buildFile(filename: string): bool =
  compileFile(filename, runImmediately = false)

# ============================= MAIN ======================================
proc main() =
  if paramCount() == 0:
    showUsage()
    quit(1)

  let command = paramStr(1)

  case command
  of "run":
    if paramCount() < 2:
      echo "Usage: microgo run <file.mg>"
      quit(1)
    let filename = paramStr(2)
    if not runFile(filename):
      quit(1)
  of "build":
    if paramCount() < 2:
      echo "Usage: microgo build <file.mg>"
      quit(1)
    let filename = paramStr(2)
    if not buildFile(filename):
      quit(1) # Missing this check!
  of "--help", "-h":
    showUsage()
    quit(0)
  else:
    # Check if it's a flag or filename
    if command.startsWith("-"):
      echo "Unknown option: ", command
      showUsage()
      quit(1)
    else:
      # Backward compatibility: treat as filename and run immediately
      let filename = command
      if not runFile(filename): # Changed to runFile
        quit(1) # Missing this check!

when isMainModule:
  main()
