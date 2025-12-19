# zal_lexer.nim 
import std/[tables, strutils]

# =========================== TOKEN DEFINITIONS ============================
type
  TokenKind* = enum
    # Keywords
    tkFor =          "for"
    tkFunc =         "func"
    tkIf =           "if"
    tkElse =         "else"
    tkReturn =       "return"
    tkStruct =       "struct"
    tkVar =          "var"
    tkConst =        "const"
    tkPrint =        "print"
    tkIntType =      "int"
    tkFloatType =    "float"
    tkStringType =   "string"
    tkBoolType =     "bool"
    tkGetMem =       "getmem"
    tkFreeMem =      "freemem"
    tkSizeOf =       "sizeof"
    tkSwitch =       "switch"
    tkCase =         "case"
    tkDefault =      "default"
    tkNil =          "nil"
    tkLen =          "len"
    tkSizeTType =    "size_t"
    tkDefer =        "defer"
    tkAlloc =        "alloc"
    tkIn =           "in"
    tkDotDot =       ".."
    tkEnum =         "enum"

    # Literals and identifiers
    tkIdent =       "identifier"
    tkIntLit =      "integer"
    tkFloatLit =    "floatlit"
    tkStringLit =   "strlit"
    tkNumber =      "number"
    tkCharLit =     "character"

    # Operators and punctuation
    tkAssign =      "="
    tkColon =       ":"
    tkComma =       ","
    tkDot =         "."
    tkSemicolon =   ";"
    tkPlus =        "+"
    tkMinus =       "-"
    tkStar =        "*"
    tkAmpersand =   "&"
    tkModulus =     "%"
    tkSlash =       "/"
    tkEq =          "=="
    tkNe =          "!="
    tkLt =          "<"
    tkGt =          ">"
    tkLe =          "<="
    tkGe =          ">="
    tkColonAssign = ":="
    tkAnd =         "&&"
    tkOr =          "||"

    # Brackets
    tkLParen =      "("
    tkRParen =      ")"
    tkLBrace =      "{"
    tkRBrace =      "}"
    tkLBracket =    "["
    tkRBracket =    "]"

    # Special
    tkEOF =         "EOF"
    tkError =       "error"
    tkCBlock =      "cblock" 

  Token* = ref object
    kind*: TokenKind
    lexeme*: string
    line*, col*: int
    case isLiteral*: bool
    of true:
      strVal*: string
      numVal*: float
    else: discard

# Keyword lookup table
const Keywords = {
  "for":      tkFor,
  "func":     tkFunc,
  "if":       tkIf,
  "else":     tkElse,
  "struct":   tkStruct,
  "var":      tkVar,
  "const":    tkConst,
  "print":    tkPrint,
  "return":   tkReturn,
  "getmem":   tkGetMem,
  "freemem":  tkFreeMem,
  "free":     tkFreeMem,
 # "sizeof":   tkSizeOf,
  "int":      tkIntType,
  "float":    tkFloatType,
  "string":   tkStringType,
  "bool":     tkBoolType,
  "size_t":   tkSizeTType,
  "switch":   tkSwitch,
  "case":     tkCase,
  "default":  tkDefault,
  "nil":      tkNil,
  "len":      tkLen,
  "defer":    tkDefer,
  "enum":     tkEnum,
  "alloc":    tkAlloc,
  "in":       tkIn,
  "NULL":     tkNil,
}.toTable

# =========================== HELPER FUNCTIONS ============================
proc createToken(
    kind:         TokenKind,
    lexeme:       string,
    line, col:    int,
    isLiteral:    bool = false,
    strVal:       string = "",
    numVal:       float = 0.0,
): Token =
  if isLiteral:
    Token(
      kind:       kind,
      lexeme:     lexeme,
      line:       line,
      col:        col,
      isLiteral:  true,
      strVal:     strVal,
      numVal:     numVal,
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

# =========================== SCAN NUMBER ============================
proc scanNumber(source: string, i: var int, line, col: var int): Token =
  let start = i
  var isFloat = false

  while i < source.len and source[i] in {'0' .. '9'}:
    inc(i)
    inc(col)

  if i < source.len and source[i] == '.' and i + 1 < source.len and source[i + 1] in {'0' .. '9'}:
    isFloat = true
    inc(i)
    inc(col)
    while i < source.len and source[i] in {'0' .. '9'}:
      inc(i)
      inc(col)

  if i < source.len and source[i] in {'e', 'E'}:
    isFloat = true
    inc(i)
    inc(col)
    if i < source.len and source[i] in {'+', '-'}:
      inc(i)
      inc(col)
    while i < source.len and source[i] in {'0' .. '9'}:
      inc(i)
      inc(col)

  let
    lexeme = source[start ..< i]
    tokenCol = col - lexeme.len
    kind = if isFloat: tkFloatLit else: tkIntLit

  createToken(kind, lexeme, line, tokenCol, true, "", parseFloat(lexeme))

proc scanString(source: string, i: var int, line, col: var int): Token =
  let
    startLine = line
    startCol = col

  inc(i)
  inc(col)

  var strVal = ""
  while i < source.len and source[i] != '"':
    if source[i] == '\\':
      inc(i)
      inc(col)
      if i < source.len:
        case source[i]
        of 'n':   strVal.add('\n')
        of 't':   strVal.add('\t')
        of 'r':   strVal.add('\r')
        of '"':   strVal.add('"')
        of '\\':  strVal.add('\\')
        else:     strVal.add('\\')
      inc(i)
      inc(col)
    else:
      strVal.add(source[i])
      inc(i)
      inc(col)

  if i >= source.len:
    return createToken(tkError, "Unterminated string", startLine, startCol)

  inc(i)
  inc(col)

  createToken(tkStringLit, "\"" & strVal & "\"", startLine, startCol, true, strVal)

# =========================== SCAN STRING ============================
proc scanCharLiteral(source: string, i: var int, line, col: var int): Token =
  let
    startLine = line
    startCol = col

  inc(i)
  inc(col)

  if i >= source.len:
    return createToken(tkError, "Unterminated character literal", startLine, startCol)

  var charVal =   '\0'
  if source[i] == '\\':
    inc(i)
    inc(col)
    if i >= source.len:
      return createToken(tkError, "Unterminated escape sequence", startLine, startCol)

    case source[i]
    of 'n':   charVal = '\n'
    of 't':   charVal = '\t'
    of 'r':   charVal = '\r'
    of '\'':  charVal = '\''
    of '\\':  charVal = '\\'
    of '0':   charVal = '\0'
    else:     charVal = source[i]

    inc(i)
    inc(col)
  else:
    charVal = source[i]
    inc(i)
    inc(col)

  if i >= source.len or source[i] != '\'':
    return createToken(tkError, "Expected closing quote", startLine, startCol)

  inc(i)
  inc(col)

  createToken(tkCharLit, "'" & charVal & "'", startLine, startCol, true, $charVal)

# =========================== SCAN C BLOCK ============================
proc scanCBlock(source: string, i: var int, line, col: var int): Token =
  let
    startLine = line
    startCol = col

  inc(i)  # Skip @
  inc(col)
  inc(i)  # Skip c
  inc(col)
  
  while i < source.len and source[i] in {' ', '\t', '\n', '\r'}:
    if source[i] == '\n':
      inc(line)
      col = 1
    else:
      inc(col)
    inc(i)

  if i >= source.len or source[i] != '{':
    return createToken(tkError, "Expected '{' after @c", line, col)

  inc(i)
  inc(col)
  
  var
    cCode      = ""
    braceCount = 1  

  while i < source.len and braceCount > 0:
    cCode.add(source[i])  # Add character first
    
    if source[i] == '{':
      inc(braceCount)
    elif source[i] == '}':
      dec(braceCount)
    
    inc(i)
    inc(col)

  # Remove the last '}' from cCode if we've already counted it
  if cCode.len > 0 and cCode[^1] == '}' and braceCount == 0:
    cCode = cCode[0..^2]

  createToken(tkCBlock, cCode, startLine, startCol)

# =========================== MAIN LEXER ============================
proc lex*(source: string): seq[Token] =
  var
    tokens: seq[Token]
    i    = 0
    line = 1
    col  = 1

  while i < source.len:
    let ch = source[i]

    case ch
    of 'a' .. 'z', 'A' .. 'Z', '_': tokens.add(scanIdentifier(source, i, line, col))
    of '0' .. '9': tokens.add(scanNumber(source, i, line, col))
    of '"': tokens.add(scanString(source, i, line, col))
    of '@':
      if i + 1 < source.len and source[i + 1] == 'c': tokens.add(scanCBlock(source, i, line, col))
      else:
        tokens.add(createToken(tkError, "Unexpected character after @: " & 
        (if i + 1 < source.len: $source[i + 1] else: "EOF"), line, col, ))
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
    of '%':
      tokens.add(createToken(tkModulus, "%", line, col))
      inc(i)
      inc(col)
    of ':':
      if i + 1 < source.len and source[i + 1] == '=':
        tokens.add(createToken(tkColonAssign, ":=", line, col))
        inc(i, 2)
        inc(col, 2)
      else:
        tokens.add(createToken(tkColon, ":", line, col))
        inc(i)
        inc(col)
    of ',':
      tokens.add(createToken(tkComma, ",", line, col))
      inc(i)
      inc(col)
    of '.':
      if i + 1 < source.len and source[i + 1] == '.':
        tokens.add(createToken(tkDotDot, "..", line, col))
        inc(i, 2)
        inc(col, 2)
      else:
        tokens.add(createToken(tkDot, ".", line, col))
        inc(i)
        inc(col)
    of ';':
      tokens.add(createToken(tkSemicolon, ";", line, col))
      inc(i)
      inc(col)
    of '&':
      if i + 1 < source.len and source[i + 1] == '&':
        tokens.add(createToken(tkAnd, "&&", line, col))
        inc(i, 2)
        inc(col, 2)
      else:
        # CHANGE THIS LINE:
        tokens.add(createToken(tkAmpersand, "&", line, col))  # Was tkError!
        inc(i)
        inc(col)
    of '|':
      if i + 1 < source.len and source[i + 1] == '|':
        tokens.add(createToken(tkOr, "||", line, col))
        inc(i, 2)
        inc(col, 2)
      else:
        tokens.add(createToken(tkError, "Unexpected character: |", line, col))
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
    of '[':
      tokens.add(createToken(tkLBracket, "[", line, col))
      inc(i)
      inc(col)
    of ']':
      tokens.add(createToken(tkRBracket, "]", line, col))
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
    of '\'': tokens.add(scanCharLiteral(source, i, line, col))
    else:
      tokens.add(createToken(tkError, "Unexpected character: " & ch, line, col))
      inc(i)
      inc(col)

  tokens.add(createToken(tkEOF, "", line, col))
  return tokens

# =========================== UTILITIES ============================
proc `$`*(token: Token): string =
  case token.kind
  of tkStringLit: "Token(" & $token.kind & ", line " & $token.line & ":" & 
    $token.col & ", \"" & token.strVal & "\")"

  of tkIntLit, tkFloatLit: "Token(" & $token.kind & ", line " & $token.line & 
    ":" & $token.col & ", " & $token.numVal & ")"
  else:
    if token.lexeme.len > 0: "Token(" & $token.kind & ", line " & $token.line & 
      ":" & $token.col & ", '" & token.lexeme & "')"
    else: "Token(" & $token.kind & ", line " & $token.line & ":" & $token.col & ")"
