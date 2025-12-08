# microgo_parser.nim
import microgo_lexer
import strutils

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

type Parser* = ref object
  tokens*: seq[Token]
  pos*: int
  current*: Token

# Forward declarations
proc parseIdentifier(p: Parser): Node
proc parseLiteral(p: Parser): Node
proc parseVarDecl(p: Parser): Node
proc parseCall(p: Parser): Node
proc parseBlock(p: Parser): Node
proc parseFunction(p: Parser): Node
proc parsePackage(p: Parser): Node
proc parseCBlock(p: Parser): Node
proc parsePrimary(p: Parser): Node
proc parseExpression(p: Parser): Node

# Parser implementation starts here
proc newParser*(tokens: seq[Token]): Parser =
  result = Parser(tokens: tokens, pos: 0)
  if tokens.len > 0:
    result.current = tokens[0]

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

proc parseCall(p: Parser): Node =
  echo "DEBUG PARSER: parseCall called, token=",
    p.current.kind, " '", p.current.lexeme, "'"
  let
    line = p.current.line
    col = p.current.col

  if p.current.kind notin {tkPrint, tkIdent}:
    echo "  -> Not a call (not print or ident)"
    return nil

  let funcName = p.current.lexeme
  p.advance()

  if not p.expect(tkLParen):
    echo "Error: Expected '(' after ", funcName, " at line ", line, ":", col
    return nil

  var args: seq[Node] = @[]

  if p.current.kind != tkRParen:
    # Parse first argument
    if p.current.kind == tkIdent:
      args.add(parseIdentifier(p))
    elif p.current.kind in {tkStringLit, tkIntLit, tkFloatLit}:
      args.add(parseLiteral(p))

  # In parseCall, fix the argument parsing:
  while p.current.kind == tkComma:
    p.advance()
    if p.current.kind == tkIdent:
      args.add(parseIdentifier(p))
    elif p.current.kind in {tkStringLit, tkIntLit, tkFloatLit}:
      args.add(parseLiteral(p))

  if not p.expect(tkRParen):
    echo "Error: Expected ')' at line ", line, ":", col
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
    let op = p.current.lexeme
    p.advance()

    let right = parsePrimary(p)
    if right == nil:
      return left

    return Node(
      kind: nkBinaryExpr,
      line: left.line,
      col: left.col,
      nodeKind: nkBinaryExpr,
      left: left,
      right: right,
      op: op,
    )

  return left

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

  if not p.expect(tkAssign):
    echo "Error: Expected '=' after variable name at line ", line, ":", col
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

proc parseBlock(p: Parser): Node =
  if not p.expect(tkLBrace):
    return nil

  let
    line = p.current.line
    col = p.current.col
  var statements: seq[Node] = @[]

  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    var stmt: Node = nil

    if p.current.kind == tkVar:
      stmt = parseVarDecl(p)
    elif p.current.kind == tkCBlock:
      stmt = parseCBlock(p)
    elif p.current.kind in {tkPrint, tkIdent}: # ⬅️ Should include tkPrint!
      stmt = parseCall(p)

    if stmt != nil:
      statements.add(stmt)
    else:
      p.advance()

  if not p.expect(tkRBrace):
    echo "Error: Expected '}' at line ", line, ":", col
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

  if not p.expect(tkLParen):
    echo "Error: Expected '(' after function name at line ", line, ":", col
    return nil

  if not p.expect(tkRParen):
    echo "Error: Expected ')' after '(' at line ", line, ":", col
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

proc parseProgram*(p: Parser): Node =
  var allNodes: seq[Node] = @[] # Everything at program level

  # Optional package
  if p.current.kind == tkPackage:
    discard parsePackage(p)

  while p.current.kind != tkEOF:
    if p.current.kind == tkFunc:
      let funcNode = parseFunction(p)
      if funcNode != nil:
        allNodes.add(funcNode)
    elif p.current.kind == tkCBlock:
      let cblockNode = parseCBlock(p)
      if cblockNode != nil:
        allNodes.add(cblockNode)
    else:
      p.advance()

  return
    Node(kind: nkProgram, line: 1, col: 1, nodeKind: nkProgram, functions: allNodes)

# AST Printing
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
