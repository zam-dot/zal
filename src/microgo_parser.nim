# microgo_parser.nim
import microgo_lexer
import strutils

# =========================== AST NODE DEFINITIONS ============================
type NodeKind* = enum
  nkProgram
  nkPackage
  nkFunction
  nkBlock
  nkCBlock
  nkVarDecl
  nkAssignment
  nkIdentifier
  nkLiteral
  nkStringLit
  nkReturn
  nkCall
  nkBinaryExpr = "binary"

type Node* = ref object
  kind*: NodeKind
  line*, col*: int
  case nodeKind*: NodeKind
  of nkProgram:
    functions*: seq[Node]
  of nkPackage:
    packageName*: string
  of nkFunction:
    funcName*: string
    params*: seq[Node]
    body*: Node
  of nkBlock:
    statements*: seq[Node]
  of nkCBlock:
    cCode*: string
  of nkBinaryExpr:
    left*: Node
    right*: Node
    op*: string
  of nkVarDecl:
    varName*: string
    varValue*: Node
  of nkAssignment:
    target*: Node
    value*: Node
  of nkIdentifier:
    identName*: string
  of nkLiteral, nkStringLit:
    literalValue*: string
  of nkCall:
    callFunc*: string
    callArgs*: seq[Node]
  else:
    discard

# =========================== PARSER STATE ============================
type Parser* = ref object
  tokens*: seq[Token]
  pos*: int
  current*: Token

# =========================== PARSER UTILITIES ============================
proc newParser*(tokens: seq[Token]): Parser =
  result = Parser(tokens: tokens, pos: 0)
  if tokens.len > 0:
    result.current = tokens[0]
  else:
    result.current = Token(kind: tkEOF, lexeme: "", line: 0, col: 0, isLiteral: false)

proc peek*(p: Parser, offset: int = 0): Token =
  let idx = p.pos + offset
  if idx < p.tokens.len:
    return p.tokens[idx]
  else:
    return Token(kind: tkEOF, lexeme: "", line: 0, col: 0, isLiteral: false)

proc advance*(p: Parser) =
  inc(p.pos)
  if p.pos < p.tokens.len:
    p.current = p.tokens[p.pos]
  else:
    p.current = Token(kind: tkEOF, lexeme: "", line: 0, col: 0, isLiteral: false)

proc expect*(p: Parser, kind: TokenKind): bool =
  if p.current.kind == kind:
    p.advance()
    return true
  return false

proc expectOrError*(p: Parser, kind: TokenKind, message: string): bool =
  if p.expect(kind):
    return true
  else:
    echo "Error: ", message, " at line ", p.current.line, ":", p.current.col
    return false

# =========================== BASIC NODE PARSERS ============================
proc parseIdentifier(p: Parser): Node =
  if p.current.kind == tkIdent:
    result = Node(
      kind: nkIdentifier,
      line: p.current.line,
      col: p.current.col,
      nodeKind: nkIdentifier,
      identName: p.current.lexeme,
    )
    p.advance()
  else:
    result = nil

proc parseLiteral(p: Parser): Node =
  case p.current.kind
  of tkIntLit, tkFloatLit:
    result = Node(
      kind: nkLiteral,
      line: p.current.line,
      col: p.current.col,
      nodeKind: nkLiteral,
      literalValue: p.current.lexeme,
    )
    p.advance()
  of tkStringLit:
    result = Node(
      kind: nkStringLit,
      line: p.current.line,
      col: p.current.col,
      nodeKind: nkStringLit,
      literalValue: if p.current.isLiteral: p.current.strVal else: p.current.lexeme,
    )
    p.advance()
  else:
    result = nil

proc parseCBlock(p: Parser): Node =
  if p.current.kind != tkCBlock:
    return nil

  let
    line = p.current.line
    col = p.current.col
    cCode = p.current.lexeme

  p.advance()
  return Node(kind: nkCBlock, line: line, col: col, nodeKind: nkCBlock, cCode: cCode)

# =========================== EXPRESSION PARSERS ============================
proc parsePrimary(p: Parser): Node =
  if p.current.kind == tkIdent:
    return parseIdentifier(p)
  elif p.current.kind in {tkIntLit, tkFloatLit, tkStringLit}:
    return parseLiteral(p)
  else:
    return nil

proc parseExpression(p: Parser): Node =
  var left = parsePrimary(p)
  if left == nil:
    return nil

  if p.current.kind == tkPlus:
    let
      op = p.current.lexeme
      line = left.line
      col = left.col

    p.advance()

    let right = parsePrimary(p)
    if right == nil:
      return left

    return Node(
      kind: nkBinaryExpr,
      line: line,
      col: col,
      nodeKind: nkBinaryExpr,
      left: left,
      right: right,
      op: op,
    )

  return left

# =========================== CALL PARSER ============================
proc parseCallArguments(p: Parser): seq[Node] =
  var args: seq[Node] = @[]

  if p.current.kind != tkRParen:
    # Parse first argument
    if p.current.kind == tkIdent:
      args.add(parseIdentifier(p))
    elif p.current.kind in {tkStringLit, tkIntLit, tkFloatLit}:
      args.add(parseLiteral(p))

    # Parse remaining arguments separated by commas
    while p.current.kind == tkComma:
      p.advance()
      if p.current.kind == tkIdent:
        args.add(parseIdentifier(p))
      elif p.current.kind in {tkStringLit, tkIntLit, tkFloatLit}:
        args.add(parseLiteral(p))

  return args

proc parseCall(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if p.current.kind notin {tkPrint, tkIdent}:
    return nil

  let funcName = p.current.lexeme
  p.advance()

  if not p.expectOrError(tkLParen, "Expected '(' after " & funcName):
    return nil

  let args = parseCallArguments(p)

  if not p.expectOrError(tkRParen, "Expected ')'"):
    return nil

  discard p.expect(tkSemicolon)

  return Node(
    kind: nkCall,
    line: line,
    col: col,
    nodeKind: nkCall,
    callFunc: funcName,
    callArgs: args,
  )

# =========================== STATEMENT PARSERS ============================
proc parseVarDecl(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkVar):
    return nil

  let ident = parseIdentifier(p)
  if ident == nil:
    echo "Error: Expected identifier after 'var' at line ", line, ":", col
    return nil

  if not p.expectOrError(tkAssign, "Expected '=' after variable name"):
    return nil

  let value = parseExpression(p)
  if value == nil:
    echo "Error: Expected expression at line ", line, ":", col
    return nil

  discard p.expect(tkSemicolon)

  return Node(
    kind: nkVarDecl,
    line: line,
    col: col,
    nodeKind: nkVarDecl,
    varName: ident.identName,
    varValue: value,
  )

proc parseStatement(p: Parser): Node =
  case p.current.kind
  of tkVar:
    return parseVarDecl(p)
  of tkCBlock:
    return parseCBlock(p)
  of tkPrint, tkIdent:
    return parseCall(p)
  else:
    return nil

# =========================== BLOCK AND FUNCTION PARSERS ============================
proc parseBlock(p: Parser): Node =
  if not p.expect(tkLBrace):
    return nil

  let
    line = p.current.line
    col = p.current.col
  var statements: seq[Node] = @[]

  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    let stmt = parseStatement(p)
    if stmt != nil:
      statements.add(stmt)
    else:
      p.advance() # Skip unexpected token

  if not p.expectOrError(tkRBrace, "Expected '}'"):
    return nil

  return
    Node(kind: nkBlock, line: line, col: col, nodeKind: nkBlock, statements: statements)

proc parseFunction(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkFunc):
    return nil

  let ident = parseIdentifier(p)
  if ident == nil:
    echo "Error: Expected function name after 'func' at line ", line, ":", col
    return nil

  if not p.expectOrError(tkLParen, "Expected '(' after function name"):
    return nil

  if not p.expectOrError(tkRParen, "Expected ')' after '('"):
    return nil

  let body = parseBlock(p)
  if body == nil:
    echo "Error: Expected function body at line ", line, ":", col
    return nil

  return Node(
    kind: nkFunction,
    line: line,
    col: col,
    nodeKind: nkFunction,
    funcName: ident.identName,
    body: body,
  )

proc parsePackage(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkPackage):
    return nil

  let ident = parseIdentifier(p)
  if ident == nil:
    echo "Error: Expected package name after 'package' at line ", line, ":", col
    return nil

  return Node(
    kind: nkPackage,
    line: line,
    col: col,
    nodeKind: nkPackage,
    packageName: ident.identName,
  )

# =========================== TOP-LEVEL PARSERS ============================
proc parseTopLevel(p: Parser): Node =
  case p.current.kind
  of tkFunc:
    return parseFunction(p)
  of tkCBlock:
    return parseCBlock(p)
  else:
    return nil

proc parseProgram*(p: Parser): Node =
  var allNodes: seq[Node] = @[]

  # Optional package declaration
  if p.current.kind == tkPackage:
    let packageNode = parsePackage(p)
    if packageNode != nil:
      allNodes.add(packageNode)

  # Parse top-level declarations
  while p.current.kind != tkEOF:
    let node = parseTopLevel(p)
    if node != nil:
      allNodes.add(node)
    else:
      p.advance() # Skip unexpected token

  return
    Node(kind: nkProgram, line: 1, col: 1, nodeKind: nkProgram, functions: allNodes)

# =========================== AST PRINTING ============================
proc printAst*(node: Node, indent: int = 0) =
  let spaces = "  ".repeat(indent)

  case node.kind
  of nkProgram:
    echo spaces, "Program:"
    for fn in node.functions:
      printAst(fn, indent + 1)
  of nkPackage:
    echo spaces, "Package: ", node.packageName
  of nkFunction:
    echo spaces, "Function: ", node.funcName, "()"
    printAst(node.body, indent + 1)
  of nkBlock:
    echo spaces, "Block:"
    for stmt in node.statements:
      printAst(stmt, indent + 1)
  of nkCBlock:
    echo spaces, "CBlock: ", node.cCode
  of nkVarDecl:
    echo spaces, "VarDecl: ", node.varName
    printAst(node.varValue, indent + 1)
  of nkIdentifier:
    echo spaces, "Identifier: ", node.identName
  of nkLiteral:
    echo spaces, "Literal: ", node.literalValue
  of nkStringLit:
    echo spaces, "String: \"", node.literalValue, "\""
  of nkCall:
    echo spaces, "Call: ", node.callFunc, "()"
    if node.callArgs.len > 0:
      echo spaces, "  Args:"
      for arg in node.callArgs:
        printAst(arg, indent + 2)
  of nkBinaryExpr:
    echo spaces, "BinaryExpr: ", node.op
    printAst(node.left, indent + 1)
    printAst(node.right, indent + 1)
  else:
    echo spaces, "Unknown node: ", node.kind
