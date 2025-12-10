# microgo_parser.nim
import std/[strutils]
import microgo_lexer

# =========================== AST NODE DEFINITIONS ============================
type NodeKind* = enum
  nkProgram
  nkPackage
  nkFunction
  nkBlock
  nkCBlock
  nkVarDecl
  nkConstDecl
  nkAssignment
  nkIdentifier
  nkLiteral
  nkStringLit
  nkReturn
  nkCall
  nkBinaryExpr = "binary"
  nkIf = "if"
  nkElse = "else"
  nkFor = "for"
  nkGroup = "group"

# ================================= AST NODE ==================================
type Node* = ref object
  kind*: NodeKind
  line*, col*: int
  case nodeKind*: NodeKind
  of nkProgram:
    functions*: seq[Node]
  of nkGroup:
    groupExpr*: Node
  of nkPackage:
    packageName*: string
  of nkIf:
    ifCondition*: Node
    ifThen*: Node
    ifElse*: Node # Can be nil
  of nkFunction:
    funcName*: string
    params*: seq[Node]
    body*: Node
    returnType*: string
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
    varType*: string
    varValue*: Node
  of nkConstDecl:
    constName*: string
    constType*: string
    constValue*: Node
  of nkAssignment:
    target*: Node
    value*: Node
  of nkIdentifier:
    identName*: string
  of nkLiteral, nkStringLit:
    literalValue*: string
  of nkCall, nkReturn: # <-- Both share the same fields
    callFunc*: string
    callArgs*: seq[Node]
  of nkFor:
    forInit*: Node # Initialization (usually var declaration)
    forCondition*: Node # Condition expression
    forUpdate*: Node # Update statement
    forBody*: Node # Loop body
  else:
    discard

# =========================== PARSER STATE ============================
type Parser* = ref object
  tokens*: seq[Token]
  pos*: int
  current*: Token

# =========================== PRECEDENCE TABLE ============================
proc getPrecedence(kind: TokenKind): int =
  case kind
  of tkAssign: 1
  of tkEq, tkNe: 2
  of tkLt, tkGt, tkLe, tkGe: 3
  of tkPlus, tkMinus: 4
  of tkStar, tkSlash, tkModulus: 5
  else: 0

# =========================== FORWARD DECLARATIONS ============================
proc parseIdentifier(p: Parser): Node
proc parseLiteral(p: Parser): Node
proc parseConstDecl(p: Parser): Node
proc parseCall(p: Parser): Node
proc parseBlock(p: Parser): Node
proc parseFunction(p: Parser): Node
proc parseReturn(p: Parser): Node
proc parseCBlock(p: Parser): Node {.used.}
proc parseCallStatement(p: Parser): Node {.used.}
proc parseVarDecl(p: Parser): Node {.used.}
proc parsePrimary(p: Parser): Node
proc parseExpression(p: Parser, minPrecedence: int = 0): Node # UPDATED
proc parseFor(p: Parser): Node
proc parseVarDeclNoSemi(p: Parser): Node
proc parseIf(p: Parser): Node
proc parseAssignmentStatement(p: Parser): Node

# =========================== PARSER UTILITIES ============================
proc newParser*(tokens: seq[Token]): Parser =
  result = Parser(tokens: tokens, pos: 0)
  if tokens.len > 0:
    result.current = tokens[0]
  else:
    result.current = Token(kind: tkEOF, lexeme: "", line: 0, col: 0, isLiteral: false)

# =========================== TOKEN UTILITIES ============================
proc peek*(p: Parser, offset: int = 0): Token =
  let idx = p.pos + offset
  if idx < p.tokens.len:
    return p.tokens[idx]
  else:
    return Token(kind: tkEOF, lexeme: "", line: 0, col: 0, isLiteral: false)

# =========================== ADVANCE ============================
proc advance*(p: Parser) =
  inc(p.pos)
  if p.pos < p.tokens.len:
    p.current = p.tokens[p.pos]
  else:
    p.current = Token(kind: tkEOF, lexeme: "", line: 0, col: 0, isLiteral: false)

# =========================== EXPECT ============================
proc expect*(p: Parser, kind: TokenKind): bool =
  if p.current.kind == kind:
    p.advance()
    return true
  return false

# =========================== EXPECT OR ERROR ============================
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

# =========================== LITERAL PARSER ============================
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

# =========================== C BLOCK PARSER ============================
proc parseCBlock(p: Parser): Node =
  if p.current.kind != tkCBlock:
    return nil

  let
    line = p.current.line
    col = p.current.col
    cCode = p.current.lexeme

  p.advance()
  return Node(kind: nkCBlock, line: line, col: col, nodeKind: nkCBlock, cCode: cCode)

# =========================== EXPRESSION PARSER ============================
proc parseExpression(p: Parser, minPrecedence: int = 0): Node =
  # Parse primary expression (number, identifier, parenthesized, etc.)
  var left = parsePrimary(p)
  if left == nil:
    return nil

  # Keep parsing while we have operators with higher precedence
  while true:
    let curr = p.current
    if curr.kind == tkEOF:
      break

    # Check if current token is a binary operator
    let currPrec = getPrecedence(curr.kind)
    if currPrec < minPrecedence:
      break

    # Special handling for assignment (right-associative)
    if curr.kind == tkAssign:
      p.advance() # Skip =
      let right = parseExpression(p, currPrec - 1) # Note: -1 for right-associative
      left = Node(
        kind: nkBinaryExpr,
        line: left.line,
        col: left.col,
        nodeKind: nkBinaryExpr,
        left: left,
        right: right,
        op: "=",
      )
      continue

    # Regular binary operators (left-associative)
    if currPrec > 0:
      let op = curr.lexeme
      p.advance() # Skip operator

      # Parse right side with higher precedence
      let right = parseExpression(p, currPrec + 1) # Note: +1 for left-associative
      if right == nil:
        break

      left = Node(
        kind: nkBinaryExpr,
        line: left.line,
        col: left.col,
        nodeKind: nkBinaryExpr,
        left: left,
        right: right,
        op: op,
      )
      continue

    # Not a binary operator we recognize
    break

  return left

# =========================== CALL ARGUMENTS ============================
proc parseCallArguments(p: Parser): seq[Node] =
  var args: seq[Node] = @[]

  if p.current.kind != tkRParen:
    # Use parseExpression for ALL arguments
    let arg = parseExpression(p)
    if arg != nil:
      args.add(arg)

    # More arguments?
    while p.current.kind == tkComma:
      p.advance()
      let nextArg = parseExpression(p)
      if nextArg != nil:
        args.add(nextArg)

  return args

# =========================== CALL PARSER ============================
proc parseCall(p: Parser): Node =
  ## Parse a function call (used in BOTH statements and expressions)
  ## This version DOESN'T expect a semicolon
  let
    line = p.current.line
    col = p.current.col

  # Accept both print and regular identifiers
  if p.current.kind notin {tkPrint, tkIdent}:
    return nil

  let funcName = p.current.lexeme
  p.advance()

  if not p.expectOrError(tkLParen, "Expected '(' after " & funcName):
    return nil

  let args = parseCallArguments(p)

  if not p.expectOrError(tkRParen, "Expected ')'"):
    return nil

  return Node(
    kind: nkCall,
    line: line,
    col: col,
    nodeKind: nkCall,
    callFunc: funcName,
    callArgs: args,
  )

# =========================== CALL STATEMENT PARSER ============================
proc parseCallStatement(p: Parser): Node =
  ## Parse a function call that's a statement (ends with semicolon)
  ## Examples: print("hello");  add(2, 3);
  let callNode = parseCall(p) # Parse the actual call
  if callNode != nil:
    # Expect semicolon for statements
    discard p.expect(tkSemicolon)
  return callNode

proc parseCallExpr(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if p.current.kind != tkIdent:
    return nil

  let funcName = p.current.lexeme
  p.advance()

  if not p.expectOrError(tkLParen, "Expected '(' after " & funcName):
    return nil

  let args = parseCallArguments(p)

  if not p.expectOrError(tkRParen, "Expected ')'"):
    return nil

  return Node(
    kind: nkCall,
    line: line,
    col: col,
    nodeKind: nkCall,
    callFunc: funcName,
    callArgs: args,
  )

# =========================== PRIMARY PARSER ============================
proc parsePrimary(p: Parser): Node =
  case p.current.kind
  of tkIdent:
    # Check if it's a function call
    if p.peek(1).kind == tkLParen:
      return parseCallExpr(p)
    else:
      return parseIdentifier(p)
  of tkIntLit, tkFloatLit, tkStringLit:
    return parseLiteral(p)
  of tkLParen:
    let line = p.current.line
    let col = p.current.col
    p.advance() # Skip '('

    let expr = parseExpression(p)
    if expr == nil:
      echo "Error: Expected expression inside parentheses"
      return nil

    if not p.expect(tkRParen):
      echo "Error: Expected ')'"
      return nil

    # Create a group node
    return Node(kind: nkGroup, line: line, col: col, nodeKind: nkGroup, groupExpr: expr)
  of tkMinus, tkPlus: # Unary operators
    let op = p.current.lexeme
    let line = p.current.line
    let col = p.current.col
    p.advance() # Skip unary operator

    let operand = parsePrimary(p)
    if operand == nil:
      echo "Error: Expected expression after unary operator"
      return nil

    # For unary minus, create a literal 0 as left operand
    return Node(
      kind: nkBinaryExpr,
      line: line,
      col: col,
      nodeKind: nkBinaryExpr,
      left: Node(
        kind: nkLiteral, line: line, col: col, nodeKind: nkLiteral, literalValue: "0"
      ),
      right: operand,
      op: op,
    )
  else:
    return nil

# =========================== STATEMENT VAR DECL ============================
proc parseVarDecl(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkVar):
    return nil

  # Parse variable name
  let ident = parseIdentifier(p)
  if ident == nil:
    echo "Error: Expected identifier after 'var' at line ", line, ":", col
    return nil

  var varType = ""

  # Check for optional type annotation (Zig-like syntax)
  if p.current.kind == tkColon:
    p.advance() # Skip ':'

    # Parse type
    case p.current.kind
    of tkIntType:
      varType = "int"
      p.advance()
    of tkFloatType:
      varType = "double" # C uses double for float
      p.advance()
    of tkStringType:
      varType = "char*"
      p.advance()
    of tkBoolType:
      varType = "bool"
      p.advance()
    of tkIdent:
      # Custom type (like user-defined)
      let typeIdent = parseIdentifier(p)
      if typeIdent != nil:
        varType = typeIdent.identName
    else:
      echo "Error: Expected type after ':' at line ", p.current.line, ":", p.current.col
      return nil

  # Expect assignment operator
  if not p.expectOrError(tkAssign, "Expected '=' after variable declaration"):
    return nil

  # Parse the value
  let value = parseExpression(p)
  if value == nil:
    echo "Error: Expected expression at line ", p.current.line, ":", p.current.col
    return nil

  return Node(
    kind: nkVarDecl,
    line: line,
    col: col,
    nodeKind: nkVarDecl,
    varName: ident.identName,
    varType: varType, # Store the type
    varValue: value,
  )

# =========================== STATEMENT PARSERS ============================
proc parseConstDecl(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkConst):
    return nil

  let ident = parseIdentifier(p)
  if ident == nil:
    echo "Error: Expected identifier after 'const' at line ", line, ":", col
    return nil

  var constType = "" # Will remain empty if no type annotation

  # ✅ FIX: Check for optional type annotation
  if p.current.kind == tkColon:
    p.advance() # Skip ':'

    # Parse type
    case p.current.kind
    of tkIntType:
      constType = "int"
      p.advance()
    of tkFloatType:
      constType = "double" # C uses double for float
      p.advance()
    of tkStringType:
      constType = "char*"
      p.advance()
    of tkBoolType:
      constType = "bool"
      p.advance()
    of tkIdent:
      # Custom type
      let typeIdent = parseIdentifier(p)
      if typeIdent != nil:
        constType = typeIdent.identName
    else:
      echo "Error: Expected type after ':' at line ", p.current.line, ":", p.current.col
      return nil

  if not p.expectOrError(tkAssign, "Expected '=' after constant name"):
    return nil

  let value = parseExpression(p)
  if value == nil:
    echo "Error: Expected expression at line ", line, ":", col
    return nil

  return Node(
    kind: nkConstDecl,
    line: line,
    col: col,
    nodeKind: nkConstDecl,
    constName: ident.identName,
    constType: constType,
    constValue: value,
  )

# =========================== STATEMENT VAR DECL ============================
proc parseVarDeclNoSemi(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkVar):
    return nil

  # Parse variable name
  let ident = parseIdentifier(p)
  if ident == nil:
    echo "Error: Expected identifier after 'var' at line ", line, ":", col
    return nil

  var varType = ""

  # Check for optional type annotation
  if p.current.kind == tkColon:
    p.advance() # Skip ':'

    # Parse type
    case p.current.kind
    of tkIntType:
      varType = "int"
      p.advance()
    of tkFloatType:
      varType = "double"
      p.advance()
    of tkStringType:
      varType = "char*"
      p.advance()
    of tkBoolType:
      varType = "bool"
      p.advance()
    of tkIdent:
      # Custom type
      let typeIdent = parseIdentifier(p)
      if typeIdent != nil:
        varType = typeIdent.identName
    else:
      echo "Error: Expected type after ':' at line ", p.current.line, ":", p.current.col
      return nil

  # Expect assignment operator
  if not p.expectOrError(tkAssign, "Expected '=' after variable declaration"):
    return nil

  # Parse the value
  let value = parseExpression(p)
  if value == nil:
    echo "Error: Expected expression at line ", p.current.line, ":", p.current.col
    return nil

  # DON'T consume semicolon here - it will be consumed by the for loop parser
  return Node(
    kind: nkVarDecl,
    line: line,
    col: col,
    nodeKind: nkVarDecl,
    varName: ident.identName,
    varType: varType,
    varValue: value,
  )

# =========================== STATEMENT PARSERS ============================
proc parseStatement(p: Parser): Node =
  case p.current.kind
  of tkPrint:
    let callNode = parseCall(p) # Use parseCall instead of parseCallStatement
    # Don't expect semicolon here - parseCall doesn't consume it
    if callNode != nil and p.current.kind == tkSemicolon:
      p.advance() # Just consume the semicolon if present
    return callNode
  of tkIdent:
    # Try assignment first: x = 10;
    var assignment = parseAssignmentStatement(p)
    if assignment != nil:
      return assignment

    # If not assignment, try function call: add(2, 3);
    return parseCallStatement(p) # This one expects semicolon
  of tkVar:
    return parseVarDecl(p)
  of tkConst:
    return parseConstDecl(p)
  of tkCBlock:
    return parseCBlock(p)
  of tkIf:
    return parseIf(p)
  of tkFor:
    return parseFor(p)
  of tkReturn:
    return parseReturn(p)
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

# =========================== FUNCTION PARSER ============================
proc parseFunction(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkFunc):
    return nil

  let ident = parseIdentifier(p)
  if ident == nil:
    echo "Error: Expected function name after 'func'"
    return nil

  if not p.expectOrError(tkLParen, "Expected '(' after function name"):
    return nil

  var params: seq[Node] = @[]

  # Check if there are ANY parameters (not empty)
  if p.current.kind != tkRParen:
    # PARSE FIRST PARAMETER
    let paramName = parseIdentifier(p)
    if paramName == nil:
      echo "Error: Expected parameter name"
      return nil

    # Parse parameter type (REQUIRED in function parameters)
    if not p.expectOrError(tkColon, "Expected ':' after parameter name"):
      return nil

    # Parse parameter type
    var paramType = "int"
    case p.current.kind
    of tkIntType:
      paramType = "int"
      p.advance()
    of tkFloatType:
      paramType = "double"
      p.advance()
    of tkStringType:
      paramType = "char*"
      p.advance()
    of tkBoolType:
      paramType = "bool"
      p.advance()
    of tkIdent:
      let typeIdent = parseIdentifier(p)
      if typeIdent != nil:
        paramType = typeIdent.identName
    else:
      echo "Error: Expected parameter type at line ", p.current.line, ":", p.current.col
      return nil

    # Create parameter node
    let paramNode = Node(
      kind: nkVarDecl,
      line: paramName.line,
      col: paramName.col,
      nodeKind: nkVarDecl,
      varName: paramName.identName,
      varType: paramType,
      varValue: nil,
    )
    params.add(paramNode)

    # Parse additional parameters (comma-separated)
    while p.current.kind == tkComma:
      p.advance() # Skip comma

      # Parse next parameter name
      let nextParamName = parseIdentifier(p)
      if nextParamName == nil:
        echo "Error: Expected parameter name after comma"
        return nil

      # ✅ FIXED: Expect colon for type annotation
      if not p.expectOrError(tkColon, "Expected ':' after parameter name"):
        return nil

      # Parse parameter type
      var nextParamType = "int"
      case p.current.kind
      of tkIntType:
        nextParamType = "int"
        p.advance()
      of tkFloatType:
        nextParamType = "double"
        p.advance()
      of tkStringType:
        nextParamType = "char*"
        p.advance()
      of tkBoolType:
        nextParamType = "bool"
        p.advance()
      of tkIdent:
        let typeIdent = parseIdentifier(p)
        if typeIdent != nil:
          nextParamType = typeIdent.identName
      else:
        echo "Error: Expected parameter type at line ",
          p.current.line, ":", p.current.col
        return nil

      # Create parameter node
      let nextParamNode = Node(
        kind: nkVarDecl,
        line: nextParamName.line,
        col: nextParamName.col,
        nodeKind: nkVarDecl,
        varName: nextParamName.identName,
        varType: nextParamType,
        varValue: nil,
      )
      params.add(nextParamNode)

  # Expect closing parenthesis
  if not p.expect(tkRParen):
    return nil

  # Parse return type (optional, defaults to void)
  var returnType = "void" # Default
  if p.current.kind in {tkIdent, tkIntType, tkFloatType, tkStringType, tkBoolType}:
    case p.current.kind
    of tkIntType:
      returnType = "int"
      p.advance()
    of tkFloatType:
      returnType = "double"
      p.advance()
    of tkStringType:
      returnType = "char*"
      p.advance()
    of tkBoolType:
      returnType = "bool"
      p.advance()
    of tkIdent:
      let returnTypeIdent = parseIdentifier(p)
      if returnTypeIdent != nil:
        returnType = returnTypeIdent.identName
    else:
      discard

  let body = parseBlock(p)
  if body == nil:
    echo "Error: Expected function body"
    return nil

  return Node(
    kind: nkFunction,
    line: line,
    col: col,
    nodeKind: nkFunction,
    funcName: ident.identName,
    params: params,
    body: body,
    returnType: returnType,
  )

# =========================== PACKAGE PARSER ============================
# Add to microgo_parser.nim after parseStatement proc
proc parseReturn(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkReturn):
    return nil

  var returnValue: Node = nil

  # Check if there's a return value (not just 'return;')
  if p.current.kind != tkSemicolon:
    returnValue = parseExpression(p)

  return Node(
    kind: nkReturn,
    line: line,
    col: col,
    nodeKind: nkReturn,
    callArgs:
      if returnValue != nil:
        @[returnValue]
      else:
        @[],
  )

# =========================== TOP-LEVEL PARSERS ============================
proc parseTopLevel(p: Parser): Node =
  case p.current.kind
  of tkFunc:
    return parseFunction(p)
  of tkCBlock:
    return parseCBlock(p)
  of tkConst: # Handle global constants
    return parseConstDecl(p)
  of tkVar: # Handle global variables (optional)
    return parseVarDecl(p)
  else:
    return nil

# =========================== PROGRAM PARSER ============================
proc parseProgram*(p: Parser): Node =
  var allNodes: seq[Node] = @[]

  # Parse top-level declarations
  while p.current.kind != tkEOF:
    let node = parseTopLevel(p)
    if node != nil:
      allNodes.add(node)
    else:
      p.advance() # Skip unexpected token

  return
    Node(kind: nkProgram, line: 1, col: 1, nodeKind: nkProgram, functions: allNodes)

# =========================== FOR LOOP PARSER ============================
proc parseFor(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkFor):
    return nil

  # EXPECT OPENING PAREN
  if not p.expectOrError(tkLParen, "Expected '(' after 'for'"):
    return nil

  # Parse initialization (optional)
  var init: Node = nil
  if p.current.kind == tkSemicolon:
    # Empty init
    p.advance()
  elif p.current.kind == tkVar:
    # Variable declaration - use parseVarDeclNoSemi
    init = parseVarDeclNoSemi(p)
    # Now consume the semicolon
    if not p.expect(tkSemicolon):
      echo "Error: Expected ';' after for init at line ", line, ":", col
      return nil
  else:
    # Could be an expression (like i = 0)
    init = parseExpression(p)
    if init != nil:
      if not p.expect(tkSemicolon):
        echo "Error: Expected ';' after for init at line ", line, ":", col
        return nil
    else:
      # Empty init
      discard p.expect(tkSemicolon)

  # Parse condition (optional)
  var condition: Node = nil
  if p.current.kind == tkSemicolon:
    # Empty condition
    p.advance()
  else:
    condition = parseExpression(p)
    if condition != nil:
      if not p.expect(tkSemicolon):
        echo "Error: Expected ';' after for condition at line ", line, ":", col
        return nil
    else:
      # Empty condition
      discard p.expect(tkSemicolon)

  # Parse update (optional)
  var update: Node = nil
  if p.current.kind == tkRParen:
    # Empty update - do nothing, closing paren will be consumed below
    discard
  else:
    update = parseExpression(p)
    if update == nil:
      # Empty update is allowed
      discard

  # EXPECT CLOSING PAREN
  if not p.expectOrError(tkRParen, "Expected ')' after for clauses"):
    return nil

  # Parse body
  let body = parseBlock(p)
  if body == nil:
    echo "Error: Expected for loop body at line ", line, ":", col
    return nil

  return Node(
    kind: nkFor,
    line: line,
    col: col,
    nodeKind: nkFor,
    forInit: init,
    forCondition: condition,
    forUpdate: update,
    forBody: body,
  )

# =========================== ASSIGNMENT PARSER ============================
proc parseAssignmentStatement(p: Parser): Node =
  # Try to parse: identifier = expression ;
  let startPos = p.pos
  let ident = parseIdentifier(p)
  if ident == nil:
    return nil

  if p.current.kind != tkAssign:
    # Not an assignment - reset parser position
    p.pos = startPos
    p.current = p.tokens[startPos]
    return nil

  p.advance() # Skip =

  let value = parseExpression(p) # This already works with default parameter
  if value == nil:
    echo "Error: Expected expression after '=' at line ",
      p.current.line, ":", p.current.col
    return nil

  return Node(
    kind: nkAssignment,
    line: ident.line,
    col: ident.col,
    nodeKind: nkAssignment,
    target: ident,
    value: value,
  )

# =========================== CONTROL FLOW PARSERS ============================
proc parseIf(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkIf):
    return nil

  # Parse condition
  let condition = parseExpression(p)
  if condition == nil:
    echo "Error: Expected condition after 'if' at line ", line, ":", col
    return nil

  # Parse then block
  let thenBlock = parseBlock(p)
  if thenBlock == nil:
    echo "Error: Expected block after if condition at line ", line, ":", col
    return nil

  # Check for else
  var elseBlock: Node = nil
  if p.current.kind == tkElse: # This will now work
    p.advance()
    if p.current.kind == tkIf:
      elseBlock = parseIf(p) # else if
    else:
      elseBlock = parseBlock(p)
      if elseBlock == nil:
        echo "Error: Expected block after 'else' at line ", line, ":", col

  return Node(
    kind: nkIf,
    line: line,
    col: col,
    nodeKind: nkIf,
    ifCondition: condition,
    ifThen: thenBlock,
    ifElse: elseBlock,
  )

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
  of nkFor:
    echo spaces, "For loop:"
    if node.forInit != nil:
      echo spaces, "  Init:"
      printAst(node.forInit, indent + 2)
    if node.forCondition != nil:
      echo spaces, "  Condition:"
      printAst(node.forCondition, indent + 2)
    if node.forUpdate != nil:
      echo spaces, "  Update:"
      printAst(node.forUpdate, indent + 2)
    echo spaces, "  Body:" # <-- Move this outside the if statement
    printAst(node.forBody, indent + 2) # <-- And this too
  of nkCall, nkReturn: # <-- Add nkReturn here too
    if node.kind == nkCall:
      echo spaces, "Call: ", node.callFunc, "()"
    else: # nkReturn
      echo spaces, "Return:"

    if node.callArgs.len > 0:
      echo spaces, "  Args/Value:"
      for arg in node.callArgs:
        printAst(arg, indent + 2)
  of nkBinaryExpr:
    echo spaces, "BinaryExpr: ", node.op
    printAst(node.left, indent + 1)
    printAst(node.right, indent + 1)
  else:
    echo spaces, "Unknown node: ", node.kind
