# microgo.nim - Main compiler executable
import src/microgo_lexer
import src/microgo_parser
import src/microgo_codegen
import std/[os, osproc, strutils]

# ======================== RESOLVE IMPORTS ==============================
proc resolveImports(source: string, baseDir: string): string =
  var
    output = ""
    inCBlock = false
    i = 0
  let lines = source.splitLines()

  while i < lines.len:
    let
      line = lines[i]
      trimmed = line.strip()

    # Skip completely empty lines in source
    if trimmed.len == 0:
      inc(i)
      continue

    # Track if we're inside @c { ... }
    if trimmed.startsWith("@c") and "{" in trimmed:
      inCBlock = true
      output &= line & "\n"
    elif inCBlock and trimmed == "}":
      inCBlock = false
      output &= line & "\n"
    elif not inCBlock and trimmed.startsWith("@include"):
      # Process include OUTSIDE @c block
      let parts = trimmed.splitWhitespace(maxSplit = 1)
      if parts.len >= 2:
        var filename = parts[1].strip()
        filename = filename.strip(chars = {'"', '\''})

        let fullPath = baseDir / filename
        if fileExists(fullPath):
          let
            included = readFile(fullPath)
            includedDir = fullPath.parentDir()
            resolved = resolveImports(included, includedDir)

          # Add the included content WITHOUT extra blank lines
          if resolved.strip().len > 0:
            output &= resolved.strip() & "\n\n"
        else:
          echo "ERROR: @include file not found: ", fullPath
          output &= line & "\n"
    else:
      # Regular line
      output &= line & "\n"

    inc(i)

  return output.strip() & "\n"

# ======================== COMPILE TO C CODE =============================
proc compileToC(source: string, filename: string = ""): string =
  # Resolve imports before compiling
  let baseDir =
    if filename.len > 0:
      filename.parentDir()
    else:
      getCurrentDir()

  # echo "=== BEFORE resolveImports ==="
  # echo source
  # echo "============================="

  let resolvedSource = resolveImports(source, baseDir)

  # echo "=== AFTER resolveImports ==="
  # echo resolvedSource
  # echo "============================"

  # Then compile as normal
  let
    tokens = lex(resolvedSource)
    parser = newParser(tokens)
    ast = parseProgram(parser)

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

# ========================== RUN WITH TCC ===============================
proc compileAndRun(cFilename: string): bool =
  let
    cmd = "tcc -run " & cFilename & " 2>&1"
    theResult = execCmdEx(cmd)

  if theResult.exitCode == 0:
    echo "✅ Program output:"
    echo theResult.output
    return true
  else:
    echo "❌ Compilation/Runtime failed:"
    echo theResult.output
    return false

# ========================== COMPILE FILE =================================
proc compileFile(filename: string, runImmediately: bool = true): bool =
  echo "Compiling ", filename, "..."

  if not fileExists(filename):
    echo "File not found: ", filename
    return false

  let
    source = readFile(filename)
    cCode = compileToC(source, filename)

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

# =========================== SHOW USAGE ================================
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

# =========================== RUN FILE ===================================
proc runFile(filename: string): bool =
  compileFile(filename, runImmediately = true)

# =========================== BUILD FILE =================================
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
      quit(1)
  of "--help", "-h":
    showUsage()
    quit(0)
  else:
    if command.startsWith("-"):
      echo "Unknown option: ", command
      showUsage()
      quit(1)
    else:
      let filename = command
      if not runFile(filename):
        quit(1)

when isMainModule:
  main()
