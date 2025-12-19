# zal.nim - Main compiler executable
import src/zal_lexer
import src/zal_parser
import src/zal_codegen
import std/[os, osproc, strutils]

# Forward declaration
proc filterSelectiveImports(source: string, items: seq[string]): string

# ======================== RESOLVE IMPORTS ==============================
proc resolveImports(source: string, baseDir: string): string =
  var
    output = ""
    inCBlock = false
    i = 0

  let lines = source.splitLines()
  
  while i < lines.len:
    var line = lines[i]
    
    if line.strip().len == 0:
      inc(i)
      continue
    
    if line.strip().startsWith("@c") and "{" in line.strip():
      inCBlock = true
      output &= line & "\n"
    
    elif inCBlock and line.strip() == "}":
      inCBlock = false
      output &= line & "\n"
    
    elif not inCBlock and line.strip().startsWith("@include"):
      let trimmed = line.strip()
      var 
        filename = ""
        itemsToInclude: seq[string] = @[]
        quoteChar = '\0'
        startIdx = trimmed.find('"')

      if startIdx == -1:
        startIdx = trimmed.find('\'')
      
      if startIdx != -1:
        quoteChar = trimmed[startIdx]
        var endIdx = trimmed.find(quoteChar, startIdx + 1)
        if endIdx != -1:
          filename = trimmed[startIdx+1 ..< endIdx]
          
          # Check for { item1, item2 } after filename
          let afterFile = trimmed[endIdx+1 .. ^1].strip()
          if afterFile.startsWith("{"):
            var braceEnd = afterFile.find("}")
            if braceEnd != -1:
              let itemsStr = afterFile[1 ..< braceEnd].strip()
              if itemsStr.len > 0:
                for item in itemsStr.split(','):
                  itemsToInclude.add(item.strip())
      
      if filename.len == 0:
        output &= line & "\n"
        inc(i)
        continue
      
      let fullPath = baseDir / filename
      if fileExists(fullPath):
        let 
          included = readFile(fullPath)
          includedDir = baseDir / filename.parentDir()
          resolved = resolveImports(included, includedDir)
        
        if itemsToInclude.len > 0:
          let filtered = filterSelectiveImports(resolved, itemsToInclude)
          if filtered.strip().len > 0: output &= filtered & "\n\n"
        else:
          if resolved.strip().len > 0: output &= resolved & "\n\n"
      else:
        echo "ERROR: @include file not found: ", baseDir / filename
        output &= line & "\n"
    
    else:
      output &= line & "\n"
    inc(i)
  
  return output.strip() & "\n"

# ======================= FILTER SELECTIVE IMPORTS ======================
proc filterSelectiveImports(source: string, items: seq[string]): string =
  var 
    output = ""
    lines = source.splitLines()
    i = 0
  
  while i < lines.len:
    let 
      line = lines[i]
      trimmed = line.strip()
    
    if trimmed.len == 0:
      inc(i)
      continue
    
    # SKIP @c blocks during selective imports (we'll handle them separately)
    if trimmed.startsWith("@c"):
      # Skip the entire @c block for now
      var j = i
      var braceDepth = 0
      var foundOpening = false
      
      while j < lines.len:
        let currentLine = lines[j]
        
        for ch in currentLine:
          if ch == '{': 
            braceDepth += 1
            foundOpening = true
          elif ch == '}': 
            braceDepth -= 1
        
        if foundOpening and braceDepth == 0:
          i = j + 1
          break
        
        inc(j)
        if j == lines.len:
          i = j
          break
      continue
    
    var 
      shouldInclude = false
      itemName = ""
    
    if trimmed.startsWith("const "):
      let afterConst = trimmed[6..^1].strip()
      var nameEnd = afterConst.find({' ', '=', ':'})
      if nameEnd == -1: nameEnd = afterConst.len
      itemName = afterConst[0..<nameEnd].strip()
      
    elif trimmed.startsWith("func "):
      let afterFunc = trimmed[5..^1].strip()
      var nameEnd = afterFunc.find({' ', '('})
      if nameEnd == -1: nameEnd = afterFunc.len
      itemName = afterFunc[0..<nameEnd].strip()
      
    elif trimmed.startsWith("var "):
      let afterVar = trimmed[4..^1].strip()
      var nameEnd = afterVar.find({' ', '=', ':'})
      if nameEnd == -1: nameEnd = afterVar.len
      itemName = afterVar[0..<nameEnd].strip()
      
    elif trimmed.startsWith("struct "):
      let afterStruct = trimmed[7..^1].strip()
      var nameEnd = afterStruct.find({' ', '{'})
      if nameEnd == -1: nameEnd = afterStruct.len
      itemName = afterStruct[0..<nameEnd].strip()

    if itemName.len > 0 and items.contains(itemName): shouldInclude = true
    
    if shouldInclude:
      # Look back for a @c block immediately before this item
      var k = i - 1
      
      while k >= 0:
        let prevLine = lines[k]
        let prevTrimmed = prevLine.strip()
        
        if prevTrimmed.len > 0:
          if prevTrimmed.startsWith("@c"):
            # Found a @c block, include it
            var cBlockStart = k
            var cBlockDepth = 0
            var cFoundOpening = false
            
            # Include the entire @c block
            while cBlockStart < lines.len:
              output &= lines[cBlockStart] & "\n"
              
              for ch in lines[cBlockStart]:
                if ch == '{': 
                  cBlockDepth += 1
                  cFoundOpening = true
                elif ch == '}': 
                  cBlockDepth -= 1
              
              if cFoundOpening and cBlockDepth == 0:
                break
              
              inc(cBlockStart)
            break
          elif not prevTrimmed.startsWith("//") and prevTrimmed.len > 0:
            # Not a comment or empty line, and not @c block
            # So no adjacent @c block
            break
        dec(k)
      
      # Now include the actual item (function, struct, etc.)
      var j = i
      var braceDepth = 0
      var inFunctionOrStruct = false
      
      if trimmed.startsWith("func ") or trimmed.startsWith("struct "): 
        inFunctionOrStruct = true
      
      while j < lines.len:
        let currentLine = lines[j]
        output &= currentLine & "\n"
        
        if inFunctionOrStruct:
          for ch in currentLine:
            if ch == '{': braceDepth += 1
            elif ch == '}': braceDepth -= 1
        
        if inFunctionOrStruct and braceDepth == 0 and j > i: 
          break
        
        if not inFunctionOrStruct and j > i:
          let nextTrimmed = currentLine.strip()
          if (nextTrimmed.startsWith("const ") or 
              nextTrimmed.startsWith("func ") or 
              nextTrimmed.startsWith("var ") or 
              nextTrimmed.startsWith("struct ") or
              nextTrimmed.startsWith("@c")):
            break
        
        inc(j)
      i = j 
    else: 
      inc(i)
  
  return output

# ======================== COMPILE TO C CODE =============================
proc compileToC(source: string, filename: string = ""): string =
  let baseDir =
    if filename.len > 0: filename.parentDir()
    else: getCurrentDir()

  let 
    resolvedSource = resolveImports(source, baseDir)
    tokens = lex(resolvedSource)
    parser = newParser(tokens)
    ast = parseProgram(parser)

  if ast == nil: raise newException(ValueError, "Parsing failed!")
  return generateC(ast)

# =========================== FORMAT CODE ================================
proc formatCode(cCode: string, filename: string): string =
  let tmpFile = filename & ".tmp.c"
  writeFile(tmpFile, cCode)

  let formatResult = execShellCmd("clang-format -i " & tmpFile & " 2>/dev/null")
  if formatResult == 0: result = readFile(tmpFile)
  else: result = cCode

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
  if filename.endsWith(".zal"): baseName = filename[0 ..^ 4]

  var justFilename = extractFilename(baseName)
  if justFilename.endsWith(".zal"): justFilename = justFilename[0 ..^ 4]

  let
    cFilename = justFilename & "zal.c"
    formattedCCode = formatCode(cCode, cFilename)

  writeFile(cFilename, formattedCCode)
#  echo "Generated ", cFilename

  if runImmediately:
    if compileAndRun(cFilename): return true
    else: return false
  else: return true

# =========================== SHOW USAGE ================================
proc showUsage() =
  echo """
Zal Compiler
Usage: 
  zal <file.zal>           # Compile and run immediately (with tcc)
  zal init [name]          # Create new project
  
Options:
  -h, --help               # Show this help message

Examples:
  zal main.zal             # Fast compile & run with tcc
  zal init myproject       # Create new project
  zal init                 # Create 'myproject' folder
"""

# =========================== RUN FILE ===================================
proc runFile(filename: string): bool =
  compileFile(filename, runImmediately = true)

# =========================== BUILD FILE =================================
proc buildFile(filename: string): bool =
  compileFile(filename, runImmediately = false)

# =========================== INIT PROJECT ================================
proc initProject(projectName: string = "") =
  var actualName = projectName

  if actualName.len == 0:
    actualName = "myproject"
    echo "No project name provided, using '", actualName, "'"

  let projectDir = actualName

  # Create directory structure
  createDir(projectDir)
  createDir(projectDir / "src")
  createDir(projectDir / "output")
  createDir(projectDir / "bin")
  createDir(projectDir / "lib")

  # Create main.mg
  let mainContent =
    """// @include ../lib/lib.zal
@c {
    #include <stdio.h>
}

func main() {
    print("Hello from Zal!\n")
}
"""
  writeFile(projectDir / "src" / "main.zal", mainContent)

  let makefileContent =
    """# Zal Project Makefile
PROJECT = """ & actualName & "\n" &
    """
SRC_DIR = src
OUTPUT_DIR = output
BIN_DIR = bin

# Compilers
ZAL = zal
TCC = tcc
GCC = gcc

# Build rules
all: fast

# ULTRA FAST development cycle: zal -> tcc run
fast: $(SRC_DIR)/main.zal
	@$(ZAL) build $(SRC_DIR)/main.zal
	@$(TCC) -run main.zal.c
	@rm -f main.zal.c  # Clean up

# Traditional gcc build (optimized)
build: $(OUTPUT_DIR)/main.c
	@echo "🔨 Building with gcc..."
	$(GCC) -O2 $(OUTPUT_DIR)/main.zal.c -o $(BIN_DIR)/$(PROJECT)

$(OUTPUT_DIR)/main.c: $(SRC_DIR)/main.zal
	$(ZAL) build $(SRC_DIR)/main.zal
	@mkdir -p $(OUTPUT_DIR)
	@mv src/main.zal.c $(OUTPUT_DIR)/ 2>/dev/null || mv main.zal.c $(OUTPUT_DIR)/

run: build
	@./$(BIN_DIR)/$(PROJECT)

clean:
	rm -f $(OUTPUT_DIR)/*.c $(BIN_DIR)/* *.c  # Clean everything

# Aliases
dev: fast
prod: build

.PHONY: all build run clean fast dev prod
"""
  writeFile(projectDir / "Makefile", makefileContent)
# ============================= MAIN ======================================
proc main() =
  if paramCount() == 0:
    showUsage()
    quit(1)

  let command = paramStr(1)

  case command
  of "init":
    if paramCount() >= 2: initProject(paramStr(2))
    else: initProject()

  of "run":
    if paramCount() < 2:
      echo "Usage: zal run <file.zal>"
      quit(1)
    if not runFile(paramStr(2)): quit(1)

  of "build":
    if paramCount() < 2:
      echo "Usage: zal build <file.zal>"
      quit(1)
    if not buildFile(paramStr(2)): quit(1)

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
      if not runFile(filename): quit(1)

when isMainModule:
  main()
