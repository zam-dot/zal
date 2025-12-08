# microgo_lexer.nim 
import std/[strutils, tables]

type
  TokenKind* = enum
    tkFor = "for"
    tkFunc = "func"
    tkIf = "if"
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
      strVal*: string
      numVal*: float
    else:
      discard

# Keyword lookup table
const Keywords = {
  "for": tkFor,
  "func": tkFunc,
  "if": tkIf,
  "import": tkImport,
  "interface": tkInterface,
  "package": tkPackage,
  "return": tkReturn,
  "struct": tkStruct,
  "type": tkType,
  "var": tkVar,
  "print": tkPrint,
}.toTable

proc lex*(source: string): seq[Token] =
  var
    tokens: seq[Token]
    i = 0
    line = 1
    col = 1

  let n = source.len

  proc peek(offset: int = 0): char =
    if i + offset < n:
      source[i + offset]
    else:
      '\0'

  proc advance() =
    inc(i)
    inc(col)

  while i < n:
    let ch = peek()

    case ch
    # ============ WHITESPACE ============
    of ' ', '\t':
      advance()
    of '\n':
      inc(line)
      col = 1
      advance()

    # ============ IDENTIFIERS & KEYWORDS ============
    of 'a' .. 'z', 'A' .. 'Z', '_':
      var start = i
      while i < n and peek() in {'a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'}:
        advance()
      let lexeme = source[start ..< i]

      let kind =
        if lexeme in Keywords:
          Keywords[lexeme]
        else:
          tkIdent
      tokens.add(
        Token(
          kind: kind,
          lexeme: lexeme,
          line: line,
          col: col - lexeme.len,
          isLiteral: false,
        )
      )

    # ============ NUMBERS ============
    of '0' .. '9':
      var
        start = i
        isFloat = false

      # Integer part
      while i < n and peek() in {'0' .. '9'}:
        advance()

      # Decimal point?
      if peek() == '.' and peek(1) in {'0' .. '9'}:
        isFloat = true
        advance() # Skip '.'
        while i < n and peek() in {'0' .. '9'}:
          advance()

      # Optional exponent
      if peek() in {'e', 'E'}:
        isFloat = true
        advance() # Skip 'e'
        if peek() in {'+', '-'}:
          advance()
        while i < n and peek() in {'0' .. '9'}:
          advance()

      let
        lexeme = source[start ..< i]
        numVal = parseFloat(lexeme)

      tokens.add(
        Token(
          kind: if isFloat: tkFloatLit else: tkIntLit,
          lexeme: lexeme,
          line: line,
          col: col - lexeme.len,
          isLiteral: true,
          numVal: numVal,
        )
      )

    # ============ STRINGS ============
    of '"':
      var start = i
      advance() # Skip opening quote

      var strVal = ""
      while i < n and peek() != '"':
        if peek() == '\\':
          advance()
          case peek()
          of 'n':
            strVal.add('\n')
            advance()
          of 't':
            strVal.add('\t')
            advance()
          of 'r':
            strVal.add('\r')
            advance()
          of '"':
            strVal.add('"')
            advance()
          of '\\':
            strVal.add('\\')
            advance()
          else:
            strVal.add('\\') # Unknown escape, keep backslash
        else:
          strVal.add(peek())
          advance()

      if peek() == '"':
        advance() # Skip closing quote
        let lexeme = source[start ..< i]
        tokens.add(
          Token(
            kind: tkStringLit,
            lexeme: lexeme,
            line: line,
            col: col - lexeme.len,
            isLiteral: true,
            strVal: strVal,
          )
        )
      else:
        # Unterminated string
        tokens.add(
          Token(
            kind: tkError,
            lexeme: "Unterminated string",
            line: line,
            col: col,
            isLiteral: false,
          )
        )

    # ============ C BLOCKS (@c { ... }) ============
    of '@':
      if peek(1) == 'c':
        # Skip @c
        advance() # Skip @
        advance() # Skip c

        # Skip optional whitespace
        while peek() in {' ', '\t'}:
          advance()

        # Expect {
        if peek() != '{':
          tokens.add(
            Token(kind: tkError, lexeme: "Expected '{' after @c", line: line, col: col)
          )
          advance()
          continue

        # Skip the {
        advance()

        # Now capture everything until matching }
        var
          cCode = ""
          braceCount = 1 # We're inside the first {

        while i < n and braceCount > 0:
          let ch = peek()
          case ch
          of '{':
            inc(braceCount)
            cCode.add(ch)
            advance()
          of '}':
            dec(braceCount)
            if braceCount > 0:
              cCode.add(ch)
            advance()
          else:
            cCode.add(ch)
            advance()

        # Add C block token
        tokens.add(
          Token(
            kind: tkCBlock,
            lexeme: cCode,
            line: line,
            col: col - cCode.len - 3, # Adjust for @c{
            isLiteral: false,
          )
        )
      else:
        # @ followed by something else
        tokens.add(
          Token(
            kind: tkError,
            lexeme: "Unexpected character after @: " & peek(1),
            line: line,
            col: col,
            isLiteral: false,
          )
        )
        advance()

    # ============ OPERATORS & PUNCTUATION ============
    of '=':
      tokens.add(
        Token(kind: tkAssign, lexeme: "=", line: line, col: col, isLiteral: false)
      )
      advance()
    of '+':
      tokens.add(
        Token(kind: tkPlus, lexeme: "+", line: line, col: col, isLiteral: false)
      )
      advance()
    of ':':
      tokens.add(
        Token(kind: tkColon, lexeme: ":", line: line, col: col, isLiteral: false)
      )
      advance()
    of ',':
      tokens.add(
        Token(kind: tkComma, lexeme: ",", line: line, col: col, isLiteral: false)
      )
      advance()
    of '.':
      tokens.add(
        Token(kind: tkDot, lexeme: ".", line: line, col: col, isLiteral: false)
      )
      advance()
    of ';':
      tokens.add(
        Token(kind: tkSemicolon, lexeme: ";", line: line, col: col, isLiteral: false)
      )
      advance()
    of '(':
      tokens.add(
        Token(kind: tkLParen, lexeme: "(", line: line, col: col, isLiteral: false)
      )
      advance()
    of ')':
      tokens.add(
        Token(kind: tkRParen, lexeme: ")", line: line, col: col, isLiteral: false)
      )
      advance()
    of '{':
      tokens.add(
        Token(kind: tkLBrace, lexeme: "{", line: line, col: col, isLiteral: false)
      )
      advance()
    of '}':
      tokens.add(
        Token(kind: tkRBrace, lexeme: "}", line: line, col: col, isLiteral: false)
      )
      advance()

    # ============ COMMENTS (Go-style) ============
    of '/':
      if peek(1) == '/':
        # Line comment
        while i < n and peek() != '\n':
          advance()
      else:
        # Could be division operator, but for now error
        tokens.add(
          Token(
            kind: tkError,
            lexeme: "Unexpected character: " & ch,
            line: line,
            col: col,
            isLiteral: false,
          )
        )
        advance()

    # ============ UNKNOWN ============
    else:
      tokens.add(
        Token(
          kind: tkError,
          lexeme: "Unexpected character: " & ch,
          line: line,
          col: col,
          isLiteral: false,
        )
      )
      advance()

  # Add EOF marker
  tokens.add(Token(kind: tkEOF, lexeme: "", line: line, col: col, isLiteral: false))

  return tokens

# ============ SIMPLE PRETTY PRINTER ============
proc `$`*(token: Token): string =
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
