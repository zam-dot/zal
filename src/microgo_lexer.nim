# microgo_lexer.nim 
import std/[strutils, tables]

# =========================== TOKEN DEFINITIONS ============================
type
  TokenKind* = enum
    # Keywords
    tkFor = "for"
    tkFunc = "func"
    tkIf = "if"
    tkElse = "else"
    tkImport = "import"
    tkInterface = "interface"
    tkPackage = "package"
    tkReturn = "return"
    tkStruct = "struct"
    tkType = "type"
    tkVar = "var"
    tkPrint = "print"

    # Literals and identifiers
    tkIdent = "identifier"
    tkIntLit = "integer"
    tkFloatLit = "float"
    tkStringLit = "string"

    # Operators and punctuation
    tkAssign = "="
    tkColon = ":"
    tkComma = ","
    tkDot = "."
    tkSemicolon = ";"
    tkPlus = "+"
    tkMinus = "-"
    tkStar = "*"
    tkSlash = "/"
    tkEq = "==" # Add these
    tkNe = "!="
    tkLt = "<"
    tkGt = ">"
    tkLe = "<="
    tkGe = ">="

    # Brackets
    tkLParen = "("
    tkRParen = ")"
    tkLBrace = "{"
    tkRBrace = "}"

    # Special
    tkEOF = "EOF"
    tkError = "error"
    tkCBlock = "cblock" # For @c { ... }

  Token* = ref object
    kind*: TokenKind
    lexeme*: string
    line*, col*: int
    case isLiteral*: bool
    of true:
      strVal*: string # For string literals
      numVal*: float # For numeric literals
    else:
      discard

# Keyword lookup table
const Keywords = {
  "for": tkFor,
  "func": tkFunc,
  "if": tkIf,
  "else": tkElse,
  "import": tkImport,
  "interface": tkInterface,
  "package": tkPackage,
  "return": tkReturn,
  "struct": tkStruct,
  "type": tkType,
  "var": tkVar,
  "print": tkPrint,
}.toTable

# =========================== HELPER FUNCTIONS ============================
proc createToken(
    kind: TokenKind,
    lexeme: string,
    line, col: int,
    isLiteral: bool = false,
    strVal: string = "",
    numVal: float = 0.0,
): Token =
  if isLiteral:
    Token(
      kind: kind,
      lexeme: lexeme,
      line: line,
      col: col,
      isLiteral: true,
      strVal: strVal,
      numVal: numVal,
    )
  else:
    Token(kind: kind, lexeme: lexeme, line: line, col: col, isLiteral: false)

# =========================== LEXER FUNCTIONS ============================
proc scanIdentifier(source: string, i: var int, line, col: var int): Token =
  let start = i
  while i < source.len and source[i] in {'a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'}:
    inc(i)
    inc(col)

  let
    lexeme = source[start ..< i]
    kind = Keywords.getOrDefault(lexeme, tkIdent)
    tokenCol = col - lexeme.len

  createToken(kind, lexeme, line, tokenCol)

proc scanNumber(source: string, i: var int, line, col: var int): Token =
  let start = i
  var isFloat = false

  # Integer part
  while i < source.len and source[i] in {'0' .. '9'}:
    inc(i)
    inc(col)

  # Decimal point?
  if i < source.len and source[i] == '.' and i + 1 < source.len and
      source[i + 1] in {'0' .. '9'}:
    isFloat = true
    inc(i) # Skip '.'
    inc(col)
    while i < source.len and source[i] in {'0' .. '9'}:
      inc(i)
      inc(col)

  # Optional exponent
  if i < source.len and source[i] in {'e', 'E'}:
    isFloat = true
    inc(i) # Skip 'e'
    inc(col)
    if i < source.len and source[i] in {'+', '-'}:
      inc(i)
      inc(col)
    while i < source.len and source[i] in {'0' .. '9'}:
      inc(i)
      inc(col)

  let
    lexeme = source[start ..< i]
    numVal = parseFloat(lexeme)
    tokenCol = col - lexeme.len
    kind = if isFloat: tkFloatLit else: tkIntLit

  createToken(kind, lexeme, line, tokenCol, true, "", numVal)

proc scanString(source: string, i: var int, line, col: var int): Token =
  let start = i
  inc(i) # Skip opening quote
  inc(col)

  var strVal = ""
  while i < source.len and source[i] != '"':
    if source[i] == '\\':
      inc(i)
      inc(col)
      if i < source.len:
        case source[i]
        of 'n':
          strVal.add('\n')
        of 't':
          strVal.add('\t')
        of 'r':
          strVal.add('\r')
        of '"':
          strVal.add('"')
        of '\\':
          strVal.add('\\')
        else:
          strVal.add('\\') # Unknown escape, keep backslash
      inc(i)
      inc(col)
    else:
      strVal.add(source[i])
      inc(i)
      inc(col)

  if i < source.len and source[i] == '"':
    inc(i) # Skip closing quote
    inc(col)
    let
      lexeme = source[start ..< i]
      tokenCol = col - lexeme.len
    createToken(tkStringLit, lexeme, line, tokenCol, true, strVal)
  else:
    createToken(tkError, "Unterminated string", line, col)

proc scanCBlock(source: string, i: var int, line, col: var int): Token =
  # Skip @c
  inc(i) # Skip @
  inc(col)
  inc(i) # Skip c
  inc(col)

  # Skip optional whitespace
  while i < source.len and source[i] in {' ', '\t'}:
    inc(i)
    inc(col)

  # Expect {
  if i >= source.len or source[i] != '{':
    return createToken(tkError, "Expected '{' after @c", line, col)

  # Skip the {
  inc(i)
  inc(col)

  # Capture everything until matching }
  var
    cCode = ""
    braceCount = 1 # We're inside the first {
  let startCol = col

  while i < source.len and braceCount > 0:
    case source[i]
    of '{':
      inc(braceCount)
      cCode.add(source[i])
    of '}':
      dec(braceCount)
      if braceCount > 0:
        cCode.add(source[i])
    else:
      cCode.add(source[i])

    inc(i)
    inc(col)

  createToken(tkCBlock, cCode, line, startCol)

# =========================== MAIN LEXER ============================
proc lex*(source: string): seq[Token] =
  var
    tokens: seq[Token]
    i = 0
    line = 1
    col = 1

  while i < source.len:
    let ch = source[i]

    case ch
    of 'a' .. 'z', 'A' .. 'Z', '_':
      tokens.add(scanIdentifier(source, i, line, col))
    of '0' .. '9':
      tokens.add(scanNumber(source, i, line, col))
    of '"':
      tokens.add(scanString(source, i, line, col))
    of '@':
      if i + 1 < source.len and source[i + 1] == 'c':
        tokens.add(scanCBlock(source, i, line, col))
      else:
        tokens.add(
          createToken(
            tkError,
            "Unexpected character after @: " &
              (if i + 1 < source.len: $source[i + 1] else: "EOF"),
            line,
            col,
          )
        )
        inc(i)
        inc(col)
    of '=':
      if i + 1 < source.len and source[i + 1] == '=':
        tokens.add(createToken(tkEq, "==", line, col))
        inc(i, 2)
        inc(col, 2)
      else:
        tokens.add(createToken(tkAssign, "=", line, col))
        inc(i)
        inc(col)
    of '!':
      if i + 1 < source.len and source[i + 1] == '=':
        tokens.add(createToken(tkNe, "!=", line, col))
        inc(i, 2)
        inc(col, 2)
      else:
        tokens.add(createToken(tkError, "Unexpected character: !", line, col))
        inc(i)
        inc(col)
    of '<':
      if i + 1 < source.len and source[i + 1] == '=':
        tokens.add(createToken(tkLe, "<=", line, col))
        inc(i, 2)
        inc(col, 2)
      else:
        tokens.add(createToken(tkLt, "<", line, col))
        inc(i)
        inc(col)
    of '>':
      if i + 1 < source.len and source[i + 1] == '=':
        tokens.add(createToken(tkGe, ">=", line, col))
        inc(i, 2)
        inc(col, 2)
      else:
        tokens.add(createToken(tkGt, ">", line, col))
        inc(i)
        inc(col)
    of '+':
      tokens.add(createToken(tkPlus, "+", line, col))
      inc(i)
      inc(col)
    of '-':
      tokens.add(createToken(tkMinus, "-", line, col))
      inc(i)
      inc(col)
    of '*':
      tokens.add(createToken(tkStar, "*", line, col))
      inc(i)
      inc(col)
    of ':':
      tokens.add(createToken(tkColon, ":", line, col))
      inc(i)
      inc(col)
    of ',':
      tokens.add(createToken(tkComma, ",", line, col))
      inc(i)
      inc(col)
    of '.':
      tokens.add(createToken(tkDot, ".", line, col))
      inc(i)
      inc(col)
    of ';':
      tokens.add(createToken(tkSemicolon, ";", line, col))
      inc(i)
      inc(col)
    of '(':
      tokens.add(createToken(tkLParen, "(", line, col))
      inc(i)
      inc(col)
    of ')':
      tokens.add(createToken(tkRParen, ")", line, col))
      inc(i)
      inc(col)
    of '{':
      tokens.add(createToken(tkLBrace, "{", line, col))
      inc(i)
      inc(col)
    of '}':
      tokens.add(createToken(tkRBrace, "}", line, col))
      inc(i)
      inc(col)
    of '/':
      if i + 1 < source.len and source[i + 1] == '/':
        # Line comment "//"
        while i < source.len and source[i] != '\n':
          inc(i)
          inc(col)
      else:
        tokens.add(createToken(tkSlash, "/", line, col))
        inc(i)
        inc(col)
    of ' ', '\t':
      inc(i)
      inc(col)
    of '\n':
      inc(line)
      col = 1
      inc(i)
    else:
      tokens.add(createToken(tkError, "Unexpected character: " & ch, line, col))
      inc(i)
      inc(col)

  # Add EOF marker
  tokens.add(createToken(tkEOF, "", line, col))
  return tokens

# =========================== UTILITIES ============================
proc `$`*(token: Token): string =
  ## Pretty print token for debugging
  case token.kind
  of tkStringLit:
    "Token(" & $token.kind & ", line " & $token.line & ":" & $token.col & ", \"" &
      token.strVal & "\")"
  of tkIntLit, tkFloatLit:
    "Token(" & $token.kind & ", line " & $token.line & ":" & $token.col & ", " &
      $token.numVal & ")"
  else:
    if token.lexeme.len > 0:
      "Token(" & $token.kind & ", line " & $token.line & ":" & $token.col & ", '" &
        token.lexeme & "')"
    else:
      "Token(" & $token.kind & ", line " & $token.line & ":" & $token.col & ")"
