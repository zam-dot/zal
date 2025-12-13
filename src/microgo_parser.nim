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
  nkStruct = "struct"
  nkFieldAccess = "field_access"
  nkStructLiteral = "struct_literal"
  nkIndexExpr = "index_expr"
  nkArrayLit = "array_literal"
  nkArrayType = "array_type"
  nkSwitch = "switch"
  nkCase = "case"
  nkDefault = "default"
  nkSwitchExpr = "switch_expr"

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
  of nkStruct:
    structName*: string
    fields*: seq[Node]
  of nkIf:
    ifCondition*: Node
    ifThen*: Node
    ifElse*: Node
  of nkFunction:
    funcName*: string
    params*: seq[Node]
    body*: Node
    returnType*: string
    returnsError*: bool
  of nkBlock:
    statements*: seq[Node]
  of nkCBlock:
    cCode*: string
  of nkBinaryExpr, nkAssignment, nkIndexExpr:
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
  of nkIdentifier:
    identName*: string
  of nkLiteral, nkStringLit:
    literalValue*: string
  of nkCall, nkReturn:
    callFunc*: string
    callArgs*: seq[Node]
    target*: Node
  of nkFieldAccess:
    base*: Node
    field*: Node
  of nkStructLiteral:
    structType*: string
    fieldValues*: seq[Node]
  of nkArrayLit:
    elements*: seq[Node]
  of nkArrayType:
    elemType*: string
    size*: Node
  of nkSwitch:
    switchTarget*: Node
    cases*: seq[Node]
    defaultCase*: Node
  of nkCase:
    caseValues*: seq[Node]
    caseBody*: Node
  of nkDefault:
    defaultBody*: Node
  of nkSwitchExpr:
    switchExprTarget*: Node
    switchExprCases*: seq[Node]
    switchExprElse*: Node
  of nkFor:
    forInit*: Node
    forCondition*: Node
    forUpdate*: Node
    forBody*: Node
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
proc parseReturn(p: Parser): Node
proc parsePrimary(p: Parser): Node
proc parseExpression(p: Parser, minPrecedence: int = 0): Node
proc parseFor(p: Parser): Node
proc parseIf(p: Parser): Node
proc parseAssignmentStatement(p: Parser): Node
proc parseSwitch(p: Parser): Node

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
  of tkCharLit:
    result = Node(
      kind: nkLiteral, # Or nkCharLit if you want separate node type
      line: p.current.line,
      col: p.current.col,
      nodeKind: nkLiteral,
      literalValue: p.current.lexeme, # This will be like 'A'
    )
    p.advance()
  # ADD THIS CASE:
  of tkNil:
    result = Node(
      kind: nkLiteral,
      line: p.current.line,
      col: p.current.col,
      nodeKind: nkLiteral,
      literalValue: "NULL",
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
      p.advance()
      let right = parseExpression(p, currPrec - 1)
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
      p.advance()

      # Parse right side with higher precedence
      let right = parseExpression(p, currPrec + 1)
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

# =========================== TYPE PARSER ============================
proc parseType(p: Parser): string =
  # Handle pointer types: *int, **int, *void, etc.
  if p.current.kind == tkStar:
    p.advance()
    let baseType = parseType(p)
    return baseType & "*"

  # Handle array types (you have this)
  elif p.current.kind == tkLBracket:
    p.advance()
    let sizeNode = parseExpression(p)
    var sizeStr = ""
    if sizeNode != nil:
      case sizeNode.kind
      of nkLiteral:
        sizeStr = sizeNode.literalValue
      else:
        sizeStr = "0"

    if not p.expect(tkRBracket):
      echo "Error: Expected ']'"
      return ""

    let elemType = parseType(p)
    return "[" & sizeStr & "]" & elemType

  # Handle basic types
  else:
    case p.current.kind
    of tkIntType:
      p.advance()
      return "int"
    of tkFloatType:
      p.advance()
      return "double"
    of tkStringType:
      p.advance()
      return "char*"
    of tkBoolType:
      p.advance()
      return "bool"
    of tkSizeTType:
      p.advance()
      return "size_t"
    of tkIdent:
      let ident = parseIdentifier(p)
      return ident.identName
    else:
      return ""

# =========================== ARRAY LITERAL PARSER ============================
proc parseArrayLiteral(p: Parser): Node =
  let line = p.current.line
  let col = p.current.col

  if not p.expect(tkLBracket):
    return nil

  var elements: seq[Node] = @[]

  # Check for empty array []
  if p.current.kind == tkRBracket:
    p.advance()
    return Node(
      kind: nkArrayLit, line: line, col: col, nodeKind: nkArrayLit, elements: elements
    )

  # Parse first element
  var elem = parseExpression(p)
  if elem != nil:
    elements.add(elem)

  # Parse remaining elements
  while p.current.kind == tkComma:
    p.advance()
    elem = parseExpression(p)
    if elem != nil:
      elements.add(elem)
    else:
      break

  if not p.expect(tkRBracket):
    echo "Error: Expected ']'"
    return nil

  return Node(
    kind: nkArrayLit, line: line, col: col, nodeKind: nkArrayLit, elements: elements
  )

# =========================== STRUCT PARSER ============================
proc parseStruct(p: Parser): Node =
  let line = p.current.line
  let col = p.current.col

  if not p.expect(tkStruct):
    return nil

  # Parse struct name
  let nameIdent = parseIdentifier(p)
  if nameIdent == nil:
    echo "Error: Expected struct name at line ", line, ":", col
    return nil

  # Expect '{'
  if not p.expectOrError(tkLBrace, "Expected '{' after struct name"):
    return nil

  var fields: seq[Node] = @[]

  # Parse fields until '}'
  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    # Parse field names (could be multiple: x, y: double)
    var fieldNames: seq[string] = @[]

    # First field name
    let firstField = parseIdentifier(p)
    if firstField == nil:
      echo "Error: Expected field name at line ", p.current.line, ":", p.current.col
      return nil
    fieldNames.add(firstField.identName)

    # More field names (comma-separated)
    while p.current.kind == tkComma:
      p.advance()
      let nextField = parseIdentifier(p)
      if nextField == nil:
        echo "Error: Expected field name after comma"
        return nil
      fieldNames.add(nextField.identName)

    # Expect colon
    if not p.expectOrError(tkColon, "Expected ':' after field names"):
      return nil

    # Parse field type
    var fieldType = ""
    case p.current.kind
    of tkIntType:
      fieldType = "int"
      p.advance()
    of tkFloatType:
      fieldType = "double"
      p.advance()
    of tkStringType:
      fieldType = "char*"
      p.advance()
    of tkBoolType:
      fieldType = "bool"
      p.advance()
    of tkSizeTType:
      fieldType = "size_t"
      p.advance()
    of tkIdent:
      let typeIdent = parseIdentifier(p)
      if typeIdent != nil:
        fieldType = typeIdent.identName
    else:
      echo "Error: Expected field type at line ", p.current.line, ":", p.current.col
      return nil

    # Create field nodes (one per name)
    for fieldName in fieldNames:
      let fieldNode = Node(
        kind: nkVarDecl,
        line: line,
        col: col,
        nodeKind: nkVarDecl,
        varName: fieldName,
        varType: fieldType,
        varValue: nil,
      )
      fields.add(fieldNode)

    # Optional comma/semicolon between fields
    if p.current.kind == tkComma:
      p.advance()

  # Expect '}'
  if not p.expectOrError(tkRBrace, "Expected '}' at end of struct"):
    return nil

  # Create struct node - need new node type nkStruct
  return Node(
    kind: nkStruct,
    line: line,
    col: col,
    nodeKind: nkStruct,
    structName: nameIdent.identName,
    fields: fields,
  )

proc parseStructLiteral(p: Parser, structName: string): Node =
  let
    line = p.current.line
    col = p.current.col
  p.advance()

  var fieldValues: seq[Node]
  while p.current.kind != tkRBrace:
    let fieldName = parseIdentifier(p)
    if fieldName == nil:
      echo "Error: Expected field name in struct literal."
      return nil

    # Expect '=' separator
    if not p.expect(tkAssign):
      echo "Error: Expected '=' after field name in struct literal."
      return nil

    # Parse the value expression
    let fieldValue = parseExpression(p)
    if fieldValue == nil:
      echo "Error: Expected value after '=' in struct literal."
      return nil

    # Create assignment node
    let assignmentNode = Node(
      kind: nkAssignment,
      line: fieldName.line,
      col: fieldName.col,
      nodeKind: nkAssignment,
      left: fieldName,
      right: fieldValue,
    )
    fieldValues.add(assignmentNode)

    # Expect comma or closing brace
    if p.current.kind == tkComma:
      p.advance()
    elif p.current.kind != tkRBrace:
      echo "Error: Expected ',' or '}' after field value."
      return nil

  p.advance()

  return Node(
    kind: nkStructLiteral,
    line: line,
    col: col,
    nodeKind: nkStructLiteral,
    structType: structName,
    fieldValues: fieldValues,
  )

# =========================== CALL PARSER ============================
proc parseCall(p: Parser): Node =
  let line = p.current.line
  let col = p.current.col

  # Accept len, print, getmem, freemem, and regular identifiers
  if p.current.kind notin {tkPrint, tkGetMem, tkFreeMem, tkIdent, tkLen}:
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
  let callNode = parseCall(p)
  if callNode != nil:
    discard p.expect(tkSemicolon)
  return callNode

# =========================== CALL EXPRESSION PARSER ===========================
proc parseCallExpr(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  # Accept identifiers AND the new getmem/freemem tokens
  if p.current.kind notin {tkIdent, tkPrint, tkGetMem, tkFreeMem, tkLen}:
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
  of tkIdent, tkPrint, tkGetMem, tkFreeMem:
    if p.peek(1).kind == tkLBrace:
      let structName = p.current.lexeme
      p.advance()
      return parseStructLiteral(p, structName)

    if p.peek(1).kind == tkLParen:
      return parseCallExpr(p)
    else:
      # Parse identifier (including len as variable name? maybe not)
      if p.current.kind == tkLen:
        # len without parentheses - treat as identifier or error?
        echo "Error: len must be called with parentheses: len(arr)"
        return nil
      else:
        # Regular identifier
        let base = parseIdentifier(p)
        if base != nil:
          # Check for field access OR array index
          if p.current.kind == tkDot:
            p.advance()
            let field = parseIdentifier(p)
            if field == nil:
              echo "Error: Expected field name after '.'"
              return nil
            # Create field access node
            return Node(
              kind: nkFieldAccess,
              line: base.line,
              col: base.col,
              nodeKind: nkFieldAccess,
              base: base,
              field: field,
            )
          elif p.current.kind == tkLBracket:
            # ARRAY INDEXING: arr[2]
            p.advance()
            let index = parseExpression(p)
            if index == nil:
              echo "Error: Expected index expression"
              return nil
            if not p.expect(tkRBracket):
              echo "Error: Expected ']' after index"
              return nil
            return Node(
              kind: nkIndexExpr,
              line: base.line,
              col: base.col,
              nodeKind: nkIndexExpr,
              left: base,
              right: index,
            )
        return base

  # ==================== ARRAY LITERALS ====================
  of tkLBracket: # Array literal: [1, 2, 3]
    return parseArrayLiteral(p)

  # ==================== LITERALS ====================
  of tkIntLit, tkFloatLit, tkStringLit:
    return parseLiteral(p)
  of tkNil:
    return parseLiteral(p)
  of tkLen:
    # len MUST be followed by '('
    if p.peek(1).kind == tkLParen:
      return parseCallExpr(p)
    else:
      # Return an error node instead of nil
      echo "Error: len must be called with parentheses at line ",
        p.current.line, ":", p.current.col
      p.advance()
      return nil

  # ==================== PARENTHESES ====================
  of tkLParen:
    let
      line = p.current.line
      col = p.current.col
    p.advance()

    let expr = parseExpression(p)
    if expr == nil:
      echo "Error: Expected expression inside parentheses"
      return nil

    if not p.expect(tkRParen):
      echo "Error: Expected ')'"
      return nil

    # Wrap the expression in nkGroup if it was just parenthesized
    var baseNode =
      Node(kind: nkGroup, line: line, col: col, nodeKind: nkGroup, groupExpr: expr)

    # Check for dot access AFTER the closing parenthesis (e.g., (a+b).field)
    if p.current.kind == tkDot:
      p.advance()
      let field = parseIdentifier(p)
      if field == nil:
        echo "Error: Expected field name after '.'"
        return nil

      # Create field access node where the base is the Group node
      return Node(
        kind: nkFieldAccess,
        line: baseNode.line,
        col: baseNode.col,
        nodeKind: nkFieldAccess,
        base: baseNode,
        field: field,
      )

    # Check for array indexing AFTER the closing parenthesis: (arr)[2]
    if p.current.kind == tkLBracket:
      p.advance()
      let index = parseExpression(p)
      if index == nil:
        echo "Error: Expected index expression"
        return nil
      if not p.expect(tkRBracket):
        echo "Error: Expected ']' after index"
        return nil
      return Node(
        kind: nkIndexExpr,
        line: baseNode.line,
        col: baseNode.col,
        nodeKind: nkIndexExpr,
        left: baseNode,
        right: index,
      )

    return baseNode

  # ==================== UNARY OPERATORS ====================
  of tkMinus, tkPlus:
    let
      op = p.current.lexeme
      line = p.current.line
      col = p.current.col
    p.advance()

    let operand = parsePrimary(p)
    if operand == nil:
      echo "Error: Expected expression after unary operator"
      return nil

    # Example: -x becomes 0 - x
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

  # ==================== C BLOCKS ====================
  of tkCBlock:
    return parseCBlock(p)
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
    p.advance()

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
    of tkSizeTType:
      varType = "size_t"
      p.advance()
    of tkIdent:
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
    varType: varType,
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

  var constType = ""

  if p.current.kind == tkColon:
    p.advance()

    # Parse type
    case p.current.kind
    of tkIntType:
      constType = "int"
      p.advance()
    of tkFloatType:
      constType = "double"
      p.advance()
    of tkStringType:
      constType = "char*"
      p.advance()
    of tkBoolType:
      constType = "bool"
      p.advance()
    of tkIdent:
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
    p.advance()

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
    of tkSizeTType:
      varType = "size_t"
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
    let callNode = parseCall(p)
    if callNode != nil and p.current.kind == tkSemicolon:
      p.advance()
    return callNode
  of tkGetMem, tkFreeMem:
    let callNode = parseCall(p)
    if callNode != nil and p.current.kind == tkSemicolon:
      p.advance()
    return callNode
  of tkIdent:
    var assignment = parseAssignmentStatement(p)
    if assignment != nil:
      return assignment
    return parseCallStatement(p)
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
  of tkSwitch:
    return parseSwitch(p)
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
      p.advance()
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
    # In all type parsing places, add:
    of tkSizeTType:
      paramType = "size_t"
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
      p.advance()

      # Parse next parameter name
      let nextParamName = parseIdentifier(p)
      if nextParamName == nil:
        echo "Error: Expected parameter name after comma"
        return nil

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
      # In all type parsing places, add:
      of tkSizeTType:
        nextParamType = "size_t"
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
  var returnType = "void"
  if p.current.kind == tkColon:
    p.advance()
    returnType = parseType(p)

  var returnsError = false
  if p.current.kind == tkError:
    p.advance()
    returnsError = true

  # Check for optional colon before return type
  if p.current.kind == tkColon:
    p.advance()

    # Now parse the type
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
    of tkSizeTType:
      returnType = "size_t"
      p.advance()
    of tkIdent:
      let returnTypeIdent = parseIdentifier(p)
      if returnTypeIdent != nil:
        returnType = returnTypeIdent.identName
    else:
      echo "Warning: Expected return type after ':'"
  elif p.current.kind in {tkIdent, tkIntType, tkFloatType, tkStringType, tkBoolType}:
    # Old style without colon (backward compatibility)
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
    returnsError: returnsError,
  )

# =========================== PACKAGE PARSER ============================
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
  of tkConst:
    return parseConstDecl(p)
  of tkVar:
    return parseVarDecl(p)
  of tkStruct:
    return parseStruct(p)
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
      p.advance()

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
    p.advance()
  elif p.current.kind == tkVar:
    init = parseVarDeclNoSemi(p)
    if not p.expect(tkSemicolon):
      echo "Error: Expected ';' after for init at line ", line, ":", col
      return nil
  else:
    init = parseExpression(p)
    if init != nil:
      if not p.expect(tkSemicolon):
        echo "Error: Expected ';' after for init at line ", line, ":", col
        return nil
    else:
      discard p.expect(tkSemicolon)

  # Parse condition (optional)
  var condition: Node = nil
  if p.current.kind == tkSemicolon:
    p.advance()
  else:
    condition = parseExpression(p)
    if condition != nil:
      if not p.expect(tkSemicolon):
        echo "Error: Expected ';' after for condition at line ", line, ":", col
        return nil
    else:
      discard p.expect(tkSemicolon)

  # Parse update (optional)
  var update: Node = nil
  if p.current.kind == tkRParen:
    discard
  else:
    update = parseExpression(p)
    if update == nil:
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
  let
    startPos = p.pos
    left = parsePrimary(p)
  if left == nil:
    return nil

  if p.current.kind != tkAssign:
    # Not an assignment - reset parser position
    p.pos = startPos
    p.current = p.tokens[startPos]
    return nil

  p.advance()

  let value = parseExpression(p)
  if value == nil:
    echo "Error: Expected expression after '=' at line ",
      p.current.line, ":", p.current.col
    return nil

  return Node(
    kind: nkAssignment,
    line: left.line,
    col: left.col,
    nodeKind: nkAssignment,
    left: left,
    right: value,
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
  if p.current.kind == tkElse:
    p.advance()
    if p.current.kind == tkIf:
      elseBlock = parseIf(p)
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

# =========================== SWITCH PARSER ============================
proc parseSwitch(p: Parser): Node =
  let line = p.current.line
  let col = p.current.col

  if not p.expect(tkSwitch):
    return nil

  # Parse target expression
  if not p.expectOrError(tkLParen, "Expected '(' after 'switch'"):
    return nil

  let target = parseExpression(p)
  if target == nil:
    echo "Error: Expected switch expression"
    return nil

  if not p.expectOrError(tkRParen, "Expected ')' after switch expression"):
    return nil

  if not p.expectOrError(tkLBrace, "Expected '{' after switch"):
    return nil

  var cases: seq[Node] = @[]
  var defaultCase: Node = nil

  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    if p.current.kind == tkCase:
      p.advance()

      var caseValues: seq[Node] = @[]

      # Parse first value
      var value = parseExpression(p)
      if value == nil:
        echo "Error: Expected case value"
        return nil
      caseValues.add(value)

      # Parse additional values (Zig supports comma-separated)
      while p.current.kind == tkComma:
        p.advance()
        value = parseExpression(p)
        if value == nil:
          echo "Error: Expected case value after comma"
          return nil
        caseValues.add(value)

      if not p.expectOrError(tkColon, "Expected ':' after case values"):
        return nil

      # Parse case body (could be block or single statement)
      var body = parseBlock(p)
      if body == nil:
        # Try single statement
        let stmt = parseStatement(p)
        if stmt != nil:
          # Wrap single statement in block
          let stmtNode = Node(
            kind: nkBlock,
            line: stmt.line,
            col: stmt.col,
            nodeKind: nkBlock,
            statements: @[stmt],
          )
          body = stmtNode
        else:
          echo "Error: Expected case body"
          return nil

      let caseNode = Node(
        kind: nkCase,
        line: line,
        col: col,
        nodeKind: nkCase,
        caseValues: caseValues,
        caseBody: body,
      )
      cases.add(caseNode)
    elif p.current.kind == tkDefault:
      p.advance()

      if not p.expectOrError(tkColon, "Expected ':' after default"):
        return nil

      var body = parseBlock(p)
      if body == nil:
        # Try single statement
        let stmt = parseStatement(p)
        if stmt != nil:
          let stmtNode = Node(
            kind: nkBlock,
            line: stmt.line,
            col: stmt.col,
            nodeKind: nkBlock,
            statements: @[stmt],
          )
          body = stmtNode
        else:
          echo "Error: Expected default body"
          return nil

      defaultCase = Node(
        kind: nkDefault, line: line, col: col, nodeKind: nkDefault, defaultBody: body
      )
    else:
      echo "Error: Expected 'case' or 'default' in switch"
      return nil

  if not p.expect(tkRBrace):
    echo "Error: Expected '}' at end of switch"
    return nil
  return Node(
    kind: nkSwitch,
    line: line,
    col: col,
    nodeKind: nkSwitch,
    switchTarget: target,
    cases: cases,
    defaultCase: defaultCase,
  )

# =========================== SWITCH EXPRESSION PARSER ============================
# proc parseSwitchExpr(p: Parser): Node =
#   # TODO: Implement expression switches (Zig-style: returns a value)
#   echo "Error: Expression switches not implemented yet"
#   return nil

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
    echo spaces,
      "Function: ",
      node.funcName,
      "() -> ",
      node.returnType,
      if node.returnsError: " error" else: ""
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
  of nkStruct:
    echo spaces, "Struct: ", node.structName
    echo spaces, "  Fields:"
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
    echo spaces, "  Body:"
    printAst(node.forBody, indent + 2)
  of nkSwitch:
    echo spaces, "Switch:"
    printAst(node.switchTarget, indent + 1)
    echo spaces, "  Cases:"
    for caseNode in node.cases:
      printAst(caseNode, indent + 2)
    if node.defaultCase != nil:
      echo spaces, "  Default:"
      printAst(node.defaultCase, indent + 2)
  of nkCase:
    echo spaces, "Case with ", node.caseValues.len, " values"
    for value in node.caseValues:
      printAst(value, indent + 1)
    echo spaces, "  Body:"
    printAst(node.caseBody, indent + 1)
  of nkDefault:
    echo spaces, "Default:"
    printAst(node.defaultBody, indent + 1)
  of nkCall, nkReturn:
    if node.kind == nkCall:
      echo spaces, "Call: ", node.callFunc, "()"
    else:
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
