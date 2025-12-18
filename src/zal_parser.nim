# zal_parser.nim
import std/[strutils]
import zal_lexer

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
  nkEnum
  nkCall
  nkBinaryExpr =      "binary"
  nkIf =              "if"
  nkElse =            "else"
  nkFor =             "for"
  nkGroup =           "group"
  nkStruct =          "struct"
  nkFieldAccess =     "field_access"
  nkStructLiteral =   "struct_literal"
  nkIndexExpr =       "index_expr"
  nkArrayLit =        "array_literal"
  nkArrayType =       "array_type"
  nkSwitch =          "switch"
  nkCase =            "case"
  nkDefault =         "default"
  nkSwitchExpr =      "switch_expr"
  nkInferredVarDecl = "inferred_var_decl"
  nkDefer =           "defer"
  nkForRange =        "for_range"

# ================================= AST NODE ==================================
type Node* = ref object
  kind*:           NodeKind
  line*, col*:     int
  case nodeKind*:  NodeKind
  of nkProgram: functions*:   seq[Node]
  of nkGroup:   groupExpr*:   Node
  of nkPackage: packageName*: string
  
  of nkStruct:
    structName*:    string
    fields*:        seq[Node]
  
  of nkIf:
    ifCondition*:   Node
    ifThen*:        Node
    ifElse*:        Node
  
  of nkFunction:
    funcName*:      string
    params*:        seq[Node]
    body*:          Node
    returnType*:    string
    returnsError*:  bool
  
  of nkBlock: statements*:  seq[Node]
  of nkCBlock: cCode*:      string
  
  of nkBinaryExpr, nkAssignment, nkIndexExpr:
    left*:    Node
    right*:   Node
    op*:      string
  
  of nkVarDecl:
    varName*:   string
    varType*:   string
    varValue*:  Node
  
  of nkConstDecl:
    constName*:  string
    constType*:  string
    constValue*: Node
  
  of nkIdentifier: identName*:              string
  of nkLiteral, nkStringLit: literalValue*: string
  
  of nkCall, nkReturn:
    callFunc*:  string
    callArgs*:  seq[Node]
    target*:    Node
  
  of nkFieldAccess:
    base*:      Node
    field*:     Node
  
  of nkStructLiteral:
    structType*:  string
    fieldValues*: seq[Node]
  
  of nkArrayLit: elements*: seq[Node]
  
  of nkArrayType:
    elemType*:  string
    size*:      Node

  of nkEnum:
    enumName*: string
    enumValues*: seq[string] 
  
  of nkSwitch:
    switchTarget*:  Node
    cases*:         seq[Node]
    defaultCase*:   Node
  
  of nkCase:
    caseValues*:    seq[Node]
    caseBody*:      Node
  
  of nkDefault: defaultBody*: Node
  
  of nkSwitchExpr:
    switchExprTarget*:  Node
    switchExprCases*:   seq[Node]
    switchExprElse*:    Node
  
  of nkFor:
    forInit*:       Node
    forCondition*:  Node
    forUpdate*:     Node
    forBody*:       Node
  
  of nkForRange:
    rangeIndex*:    Node 
    rangeValue*:    Node 
    rangeTarget*:   Node 
    rangeBody*:     Node 
  
  of nkDefer: deferExpr*: Node
  
  else: discard

# =========================== PARSER STATE ============================
type Parser* = ref object
  tokens*:  seq[Token]
  pos*:     int
  current*: Token

# =========================== PRECEDENCE TABLE ============================
proc getPrecedence(kind: TokenKind): int =
  case kind
  of tkAssign, tkColonAssign: 1
  of tkOr: 2
  of tkAnd: 3
  of tkEq, tkNe: 4
  of tkLt, tkGt, tkLe, tkGe: 5
  of tkPlus, tkMinus: 6
  of tkStar, tkSlash, tkModulus: 7
  of tkDotDot: 8
  else: 0

# =========================== FORWARD DECLARATIONS ============================
proc parseForRange(p: Parser): Node
proc parseReturn(p: Parser): Node
proc parsePrimary(p: Parser): Node
proc parseExpression(p: Parser, minPrecedence: int = 0): Node
proc parseFor(p: Parser): Node
proc parseIf(p: Parser): Node
proc parseAssignmentStatement(p: Parser): Node
proc parseSwitch(p: Parser): Node
proc parseStructLiteral(p: Parser, structName: string): Node
proc parseEnum(p: Parser): Node


# =========================== PARSER UTILITIES ============================
proc newParser*(tokens: seq[Token]): Parser =
  result = Parser(tokens: tokens, pos: 0)
  if tokens.len > 0: result.current = tokens[0]
  else: result.current = Token(kind: tkEOF, lexeme: "", line: 0, col: 0, isLiteral: false)

# =========================== TOKEN UTILITIES ============================
proc peek*(p: Parser, offset: int = 0): Token =
  let idx = p.pos + offset
  if  idx < p.tokens.len: return p.tokens[idx]
  else: return Token(kind: tkEOF, lexeme: "", line: 0, col: 0, isLiteral: false)

# =========================== ADVANCE ============================
proc advance*(p: Parser) =
  inc(p.pos)
  if p.pos < p.tokens.len: p.current = p.tokens[p.pos]
  else: p.current = Token(kind: tkEOF, lexeme: "", line: 0, col: 0, isLiteral: false)

# =========================== EXPECT ============================
proc expect*(p: Parser, kind: TokenKind): bool =
  if p.current.kind == kind:
    p.advance()
    return true
  return false

# =========================== EXPECT OR ERROR ============================
proc expectOrError*(p: Parser, kind: TokenKind, message: string): bool =
  if p.expect(kind): return true
  else:
    echo "Error: ", message, " at line ", p.current.line, ":", p.current.col
    return false

# =========================== BASIC NODE PARSERS ============================
proc parseIdentifier(p: Parser): Node =
  if p.current.kind == tkIdent:
    result = Node( kind: nkIdentifier, line: p.current.line, col: p.current.col,
      nodeKind: nkIdentifier, identName: p.current.lexeme, )
    p.advance()
  else: result = nil

# =========================== INFERRED VAR DECL PARSER ============================
proc parseInferredVarDecl(p: Parser): Node =
  let 
    line = p.current.line
    col  = p.current.col
    firstIdent = parseIdentifier(p)

  if firstIdent == nil:
    return nil

  var 
    varName = firstIdent.identName
    isMultiVar = false
    secondIdent: Node = nil
  
  if p.current.kind == tkComma:
    p.advance() 
    
    secondIdent = parseIdentifier(p)
    if secondIdent == nil: return nil
    
    varName = firstIdent.identName & "," & secondIdent.identName
    isMultiVar = true

  if not p.expect(tkColonAssign): return nil

  let value = parseExpression(p)
  if value == nil: return nil

  if isMultiVar and value.kind == nkCall:
    return Node(kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
      varName: varName, varType: "int,char*", varValue: value) 
  
  var varType = ""
  if value.kind == nkLiteral:
    let val = value.literalValue
    if val == "NULL": varType = "void*"
    elif val.contains('.') or val.contains('e') or val.contains('E'): varType = "double"
    else: varType = "int"
  
  return Node(kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
    varName: varName, varType: varType, varValue: value)

# =========================== LITERAL PARSER ============================
proc parseLiteral(p: Parser): Node =
  case p.current.kind

  of tkIntLit, tkFloatLit:
    result = Node( kind: nkLiteral, line: p.current.line, col: p.current.col,
      nodeKind: nkLiteral, literalValue: p.current.lexeme, )
    p.advance()
  
  of tkStringLit:
    result = Node(kind: nkStringLit, line: p.current.line, col: p.current.col, nodeKind: nkStringLit, 
    literalValue: if p.current.isLiteral: p.current.strVal else: p.current.lexeme, )
    p.advance()
  
  of tkCharLit:
    result = Node( kind: nkLiteral, line: p.current.line, col: p.current.col,
      nodeKind: nkLiteral, literalValue: p.current.lexeme, )
    p.advance()
  
  of tkNil:
    result = Node( kind: nkLiteral, line: p.current.line, col: p.current.col,
      nodeKind: nkLiteral, literalValue: "NULL", )
    p.advance()
  of tkIdent:
    if p.current.lexeme == "NULL":
      result = Node(kind: nkLiteral, line: p.current.line, col: p.current.col,
        nodeKind: nkLiteral, literalValue: "NULL")
      p.advance()
  else: result = nil

# =========================== C BLOCK PARSER ============================
proc parseCBlock(p: Parser): Node =
  if p.current.kind != tkCBlock: return nil

  let
    line  = p.current.line
    col   = p.current.col
    cCode = p.current.lexeme

  p.advance()
  return Node( kind: nkCBlock, line: line, col: col, nodeKind: nkCBlock, cCode: cCode )

# =========================== EXPRESSION PARSER ============================
proc parseExpression(p: Parser, minPrecedence: int = 0): Node =
  var left = parsePrimary(p)
  if left == nil: return nil

  while true:
    let curr = p.current
    if curr.kind == tkEOF: break

    let currPrec = getPrecedence(curr.kind)
    if currPrec < minPrecedence: break
    if curr.kind in {tkAssign, tkColonAssign}:
      let isInferred = (curr.kind == tkColonAssign)
      p.advance()

      let right = parseExpression(p, currPrec - 1)
      left = Node( kind: nkBinaryExpr, line: left.line, col: left.col, nodeKind: nkBinaryExpr, 
      left: left, right: right, op: if isInferred: ":=" else: "=", )
      continue

    if currPrec > 0:
      let op = curr.lexeme
      p.advance()

      let right = parseExpression(p, currPrec + 1)
      if right == nil: break

      left = Node( kind: nkBinaryExpr, line: left.line, col: left.col, 
           nodeKind: nkBinaryExpr, left: left, right: right, op: op, )
      continue
    break

  return left

# =========================== CALL ARGUMENTS ============================
proc parseCallArguments(p: Parser): seq[Node] =
  var args: seq[Node] = @[]

  if p.current.kind != tkRParen:
    let arg = parseExpression(p)
    if arg != nil: args.add(arg)

    while p.current.kind == tkComma:
      p.advance()
      let nextArg = parseExpression(p)
      if nextArg != nil: args.add(nextArg)
      else: break 

  return args

# =========================== DEFER PARSER ============================
proc parseDefer(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkDefer): return nil

  let expr = parseExpression(p)
  if expr == nil:
    return nil

  return Node(kind: nkDefer, line: line, col: col, nodeKind: nkDefer, deferExpr: expr)

# =========================== TYPE PARSER ============================
proc parseType(p: Parser): string =

  if p.current.kind == tkStar:
    p.advance()
    let baseType = parseType(p)
    return baseType & "*"

  elif p.current.kind == tkLBracket:
    p.advance()
    let sizeNode = parseExpression(p)
    var sizeStr = ""
    if sizeNode != nil:
      case sizeNode.kind
      of nkLiteral: sizeStr = sizeNode.literalValue
      else: sizeStr = "0"

    if not p.expect(tkRBracket): return ""

    let elemType = parseType(p) 
    return "[" & sizeStr & "]" & elemType 

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
    else: return ""

# =========================== ARRAY LITERAL PARSER ============================
proc parseArrayLiteral(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkLBracket): return nil
  var elements: seq[Node] = @[]

  if p.current.kind == tkRBracket:
    p.advance()
    return Node(kind: nkArrayLit, line: line, col: col, nodeKind: nkArrayLit, elements: elements)

  var elem = parseExpression(p) 
  if elem != nil: elements.add(elem)

  while p.current.kind == tkComma:
    p.advance()
    elem = parseExpression(p)
    if elem != nil: elements.add(elem)
    else: break

  if not p.expect(tkRBracket): return nil
  return Node(kind: nkArrayLit, line: line, col: col, nodeKind: nkArrayLit, elements: elements)

# =========================== STRUCT PARSER ============================
proc parseStruct(p: Parser): Node =
  let
    line = p.current.line
    col  = p.current.col

  if not p.expect(tkStruct): return nil

  let nameIdent = parseIdentifier(p)
  if nameIdent == nil: return nil

  if not p.expectOrError(tkLBrace, "Expected '{' after struct name"): return nil
  var fields: seq[Node] = @[]

  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    var fieldNames: seq[string] = @[]
    let firstField = parseIdentifier(p)

    if firstField == nil: return nil
    fieldNames.add(firstField.identName)

    while p.current.kind == tkComma:
      p.advance()
      let nextField = parseIdentifier(p)
      if nextField == nil: return nil
      fieldNames.add(nextField.identName)

    if not p.expectOrError(tkColon, "Expected ':' after field names"): return nil

    var fieldType = ""
    case p.current.kind

    of tkIntType:
      fieldType =     "int"
      p.advance()
    
    of tkFloatType:
      fieldType =     "double"
      p.advance()
    
    of tkStringType:
      fieldType =     "char*"
      p.advance()
    
    of tkBoolType:
      fieldType =     "bool"
      p.advance()
    
    of tkSizeTType:
      fieldType =     "size_t"
      p.advance()
    
    of tkIdent:
      let typeIdent = parseIdentifier(p)
      if typeIdent != nil:
        fieldType = typeIdent.identName
    else:
      return nil

    for fieldName in fieldNames:
      let fieldNode = Node( kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
        varName: fieldName, varType: fieldType, varValue: nil, )
      fields.add(fieldNode)

    if p.current.kind == tkComma: p.advance()
  if not p.expectOrError(tkRBrace, "Expected '}' at end of struct"): return nil

  return Node(kind: nkStruct, line: line, col: col, nodeKind: nkStruct,
    structName: nameIdent.identName, fields: fields,)

# =========================== STRUCT LITERAL PARSER ============================
proc parseStructLiteral(p: Parser, structName: string): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkLBrace): return nil

  var fieldValues: seq[Node] = @[]

  if p.current.kind == tkRBrace:
    p.advance() 
    return Node(kind: nkStructLiteral, line: line, col: col, nodeKind: nkStructLiteral,
      structType: structName, fieldValues: fieldValues,)

  # Check if this looks like enum initialization (no field names)
  let savedPos = p.pos
  
  # Try to parse as enum initialization (just a value)
  let value = parseExpression(p)
  if value != nil:
    # Check if next token is RBrace or comma
    if p.current.kind == tkRBrace or p.current.kind == tkComma:
      # This is enum-style initialization
      p.pos = savedPos  # Reset to parse properly
      p.current = p.tokens[savedPos]
      
      # Parse enum values
      while p.current.kind != tkRBrace and p.current.kind != tkEOF:
        let enumValue = parseExpression(p)
        if enumValue == nil: return nil
        
        # Create a dummy assignment for consistency
        let fieldAssign = Node(kind: nkAssignment, line: line, col: col,
          nodeKind: nkAssignment, left: Node(kind: nkIdentifier, line: line, col: col,
            nodeKind: nkIdentifier, identName: "value"), right: enumValue)
        fieldValues.add(fieldAssign)
        
        if p.current.kind == tkComma: 
          p.advance()
        elif p.current.kind != tkRBrace: 
          return nil
      
      if not p.expect(tkRBrace): return nil
      
      # For enums with single value, return just the value
      if fieldValues.len == 1:
        return fieldValues[0].right
      else:
        # Multiple enum values not supported
        return nil
    else:
      # Not enum style, reset and parse as struct
      p.pos = savedPos
      p.current = p.tokens[savedPos]
  
  # Parse as regular struct literal with field names
  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    let fieldName = parseIdentifier(p)
    if fieldName == nil: return nil
    if not p.expect(tkColon): return nil

    let value = parseExpression(p)
    if value == nil: return nil

    let fieldAssign = Node(kind: nkAssignment, line: fieldName.line, col: fieldName.col,
      nodeKind: nkAssignment, left: fieldName, right: value)
    fieldValues.add(fieldAssign)

    if p.current.kind == tkComma: p.advance()
    elif p.current.kind != tkRBrace: return nil

  if not p.expect(tkRBrace): return nil

  return Node(kind: nkStructLiteral, line: line, col: col, nodeKind: nkStructLiteral,
    structType: structName, fieldValues: fieldValues,)
# =========================== CALL PARSER ============================
proc parseCall(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if p.current.kind notin {tkPrint, tkGetMem, tkFreeMem, tkIdent, tkLen, tkAlloc}: return nil

  let funcName = p.current.lexeme
  p.advance()

  if not p.expectOrError(tkLParen, "Expected '(' after " & funcName): return nil
  let args = parseCallArguments(p)
  if not p.expectOrError(tkRParen, "Expected ')'"): return nil

  return Node( kind: nkCall, line: line, col: col, nodeKind: nkCall,
    callFunc: funcName, callArgs: args, )

# =========================== CALL STATEMENT PARSER ============================
proc parseCallStatement(p: Parser): Node =
  let    callNode = parseCall(p)
  if     callNode != nil: discard p.expect(tkSemicolon)
  return callNode

# =========================== CALL EXPRESSION PARSER ===========================
proc parseCallExpr(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if p.current.kind notin {tkIdent, tkPrint, tkGetMem, tkFreeMem, tkLen, tkAlloc}: return nil
  let funcName = p.current.lexeme
  p.advance()
  if not p.expectOrError(tkLParen, "Expected '(' after " & funcName): return nil

  let args = parseCallArguments(p)
  if not p.expectOrError(tkRParen, "Expected ')'"): return nil

  return Node( kind: nkCall, line: line, col: col, nodeKind: nkCall,
    callFunc: funcName, callArgs: args, )

# =========================== PRIMARY PARSER ============================
proc parsePrimary(p: Parser): Node =
  case p.current.kind
  of tkIdent, tkPrint, tkGetMem, tkFreeMem, tkLen, tkAlloc:
    if p.peek(1).kind == tkLParen: 
      return parseCallExpr(p) 
    if p.peek(1).kind == tkLBrace:
      let 
        savedPos = p.pos
        typeNameNode = parseIdentifier(p)
      if typeNameNode != nil:
        let name = typeNameNode.identName
        # Allow both structs and enums to use struct literal syntax
        if name.len > 0 and name[0] in {'A'..'Z'} and name != "NULL":
          let structLit = parseStructLiteral(p, name)
          if structLit != nil: 
            return structLit
      
      p.pos = savedPos
      p.current = p.tokens[savedPos]

    # Just parse as identifier (could be enum value, variable name, etc.)
    let baseNode = parseIdentifier(p)
    if baseNode == nil: return nil

    # Handle array indexing
    if p.current.kind == tkLBracket:
      p.advance()
      let index = parseExpression(p)
      if index == nil: return nil
      if not p.expect(tkRBracket): return nil
      return Node(kind: nkIndexExpr, line: baseNode.line, col: baseNode.col,
        nodeKind: nkIndexExpr, left: baseNode, right: index)

    # Handle field access
    var currentNode = baseNode
    while p.current.kind == tkDot:
      p.advance() 
      let field = parseIdentifier(p)
      if field == nil: return nil
      currentNode = Node(kind: nkFieldAccess, line: currentNode.line, col: currentNode.col, 
        nodeKind: nkFieldAccess, base: currentNode, field: field)
    
    return currentNode

  of tkIntType, tkFloatType, tkStringType, tkBoolType, tkSizeTType:
    let
      line =      p.current.line
      col =       p.current.col
      typeName =  p.current.lexeme
    p.advance()

    return Node(kind: nkIdentifier, line: line, col: col, nodeKind: nkIdentifier, identName: typeName,)

  of tkLBracket:                        return parseArrayLiteral(p)
  of tkIntLit, tkFloatLit, tkStringLit: return parseLiteral(p)
  of tkNil:                             return parseLiteral(p)
  of tkLParen:
    let
      line = p.current.line
      col = p.current.col
    p.advance()

    let expr = parseExpression(p)
    if expr == nil: return nil
    if not p.expect(tkRParen): return nil

    var 
      baseNode = Node(kind: nkGroup, line: line, col: col, nodeKind: nkGroup, groupExpr: expr)
      currentNode = parseIdentifier(p) 

    while p.current.kind == tkDot:
      p.advance() 
      let field = parseIdentifier(p)
      if field == nil: return nil

      currentNode = Node(kind: nkFieldAccess, line: currentNode.line, col: currentNode.col,
        nodeKind: nkFieldAccess, base: currentNode, field: field,)

    if p.current.kind == tkLBracket:
      p.advance() 
      let index = parseExpression(p)
      if index == nil: return nil
      if not p.expect(tkRBracket): return nil

      return Node(kind: nkIndexExpr, line: baseNode.line, col: baseNode.col,
        nodeKind: nkIndexExpr, left: baseNode, right: index,)
    return baseNode

  of tkMinus, tkPlus:
    let
      op = p.current.lexeme
      line = p.current.line
      col = p.current.col
    p.advance()

    let operand = parsePrimary(p)
    if operand == nil: return nil

    return Node(kind: nkBinaryExpr, line: line, col: col, nodeKind: nkBinaryExpr,
      left: Node(kind: nkLiteral, line: line, col: col, nodeKind: nkLiteral, literalValue: "0"),
      right: operand, op: op,)

  of tkCBlock: return parseCBlock(p)
  else: return nil


# =========================== STATEMENT VAR DECL ============================
proc parseVarDecl(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkVar): return nil
  let ident = parseIdentifier(p)
  if ident == nil: return nil

  var varType = ""

  if p.current.kind == tkColon:
    p.advance()

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
      if typeIdent != nil: varType = typeIdent.identName
    else:
      return nil

  if not p.expectOrError(tkAssign, "Expected '=' after variable declaration"): return nil

  let value = parseExpression(p)
  if value == nil: return nil

  return Node(kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
    varName: ident.identName, varType: varType, varValue: value)

# =========================== STATEMENT MULTI VAR DECL ===========================
proc parseMultiVarDecl(p: Parser): Node =
  let
    line = p.current.line
    col  = p.current.col

  if not p.expect(tkVar): return nil
  let firstIdent = parseIdentifier(p)
  if firstIdent == nil: return nil

  if not p.expect(tkComma): return nil
  let secondIdent = parseIdentifier(p)
  if secondIdent == nil: return nil

  var varType = ""
  if p.current.kind == tkColon:
    p.advance()
    varType = parseType(p)

  if not p.expectOrError(tkAssign, "Expected '=' in multi-variable declaration"): return nil

  let value = parseExpression(p)
  if value == nil: return nil
  let combinedName = firstIdent.identName & "," & secondIdent.identName

  return Node(kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
    varName: combinedName, varType: varType, varValue: value)

# =========================== STATEMENT PARSERS ============================
proc parseConstDecl(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkConst): return nil

  let ident = parseIdentifier(p)
  if ident == nil: return nil

  var constType = ""
  if p.current.kind == tkColon:
    p.advance()

    case p.current.kind
    of tkIntType:
      constType =     "int"
      p.advance()

    of tkFloatType:
      constType =     "double"
      p.advance()
    
    of tkStringType:
      constType =     "char*"
      p.advance()
    
    of tkBoolType:
      constType =     "bool"
      p.advance()
    
    of tkIdent:
      let typeIdent = parseIdentifier(p)
      if typeIdent != nil: constType = typeIdent.identName
    else:
      return nil

  if not p.expectOrError(tkAssign, "Expected '=' after constant name"): return nil

  let value = parseExpression(p)
  if value == nil: return nil

  return Node(kind: nkConstDecl, line: line, col: col, nodeKind: nkConstDecl,
    constName: ident.identName, constType: constType, constValue: value,)

# =========================== STATEMENT VAR DECL ============================
proc parseVarDeclNoSemi(p: Parser): Node =
  let
    line = p.current.line
    col  = p.current.col

  if not p.expect(tkVar): return nil

  let ident = parseIdentifier(p)
  if ident == nil: return nil

  var varType = ""

  if p.current.kind == tkColon:
    p.advance()

    case p.current.kind
    of tkIntType:
      varType =       "int"
      p.advance()
    
    of tkFloatType:
      varType =       "double"
      p.advance()
    
    of tkStringType:
      varType =       "char*"
      p.advance()
    
    of tkBoolType:
      varType =       "bool"
      p.advance()
    
    of tkSizeTType:
      varType =       "size_t"
      p.advance()
    
    of tkIdent:
      let typeIdent = parseIdentifier(p)
      if typeIdent != nil: varType = typeIdent.identName
    else:
      return nil

  if not p.expectOrError(tkAssign, "Expected '=' after variable declaration"): return nil

  let value = parseExpression(p)
  if value == nil: return nil

  return Node(kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
    varName: ident.identName, varType: varType, varValue: value,)

# =========================== STATEMENT PARSERS ============================
proc parseStatement(p: Parser): Node =
  
  case p.current.kind
  of tkIdent:
    if p.peek(1).kind == tkColonAssign: return parseInferredVarDecl(p)
    
    if p.peek(1).kind == tkComma and p.peek(2).kind == tkIdent and p.peek(3).kind == tkColonAssign:
      return parseInferredVarDecl(p)
    
    var assignment = parseAssignmentStatement(p)
    if assignment != nil: return assignment
    
    let call = parseCallStatement(p)
    if call != nil: return call
    else: return nil
  
  of tkPrint:
    let callNode = parseCall(p)
    if callNode != nil and p.current.kind == tkSemicolon: p.advance()
    return callNode

  of tkGetMem, tkFreeMem, tkAlloc: 
    let callNode = parseCall(p)
    if callNode != nil and p.current.kind == tkSemicolon: p.advance()
    return callNode

  of tkVar:    
    let savedPos = p.pos
    let multiVar = parseMultiVarDecl(p)
    if multiVar != nil: return multiVar
    else:
      p.pos = savedPos
      p.current = p.tokens[savedPos]
      return parseVarDecl(p)

  of tkConst:  return parseConstDecl(p)
  of tkCBlock: return parseCBlock(p)
  of tkIf:     return parseIf(p)

  of tkFor:
    let 
      savedPos       = p.pos
      forRangeResult = parseForRange(p)
    
    if forRangeResult != nil: return forRangeResult
    else:
      p.pos = savedPos
      p.current = p.tokens[savedPos]
      return parseFor(p)

  of tkReturn: return parseReturn(p)
  of tkSwitch: return parseSwitch(p)
  of tkDefer:  return parseDefer(p)
  else:        return nil

# =========================== BLOCK AND FUNCTION PARSERS ============================
proc parseBlock(p: Parser): Node =
  if not p.expect(tkLBrace): return nil

  let
    line = p.current.line
    col = p.current.col
  var statements: seq[Node] = @[]

  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    let stmt = parseStatement(p)
    if stmt != nil: statements.add(stmt)
    else: p.advance()
  if not p.expectOrError(tkRBrace, "Expected '}'"): return nil

  return
    Node(kind: nkBlock, line: line, col: col, nodeKind: nkBlock, statements: statements)

# =========================== FUNCTION PARSER ============================
proc parseFunction(p: Parser): Node =
  let
    line = p.current.line
    col  = p.current.col

  if not p.expect(tkFunc): return nil

  let ident = parseIdentifier(p)
  if ident == nil: return nil

  if not p.expectOrError(tkLParen, "Expected '(' after function name"): return nil
  var params: seq[Node] = @[]

  if p.current.kind != tkRParen:
    let paramName = parseIdentifier(p)
    if paramName == nil: return nil

    if not p.expectOrError(tkColon, "Expected ':' after parameter name"): return nil

    var paramType =     "int"
    case p.current.kind
    of tkIntType:
      paramType =       "int"
      p.advance()

    of tkFloatType:
      paramType =       "double"
      p.advance()
    
    of tkStringType:
      paramType =       "char*"
      p.advance()
    
    of tkBoolType:
      paramType =       "bool"
      p.advance()
    
    of tkSizeTType:
      paramType =       "size_t"
      p.advance()
    
    of tkIdent:
      let typeIdent = parseIdentifier(p)
      if typeIdent != nil: paramType = typeIdent.identName
    else:
      return nil

    let paramNode = Node(kind: nkVarDecl, line: paramName.line, col: paramName.col,
      nodeKind: nkVarDecl, varName: paramName.identName, varType: paramType, varValue: nil, )
    params.add(paramNode)

    while p.current.kind == tkComma:
      p.advance()

      let nextParamName = parseIdentifier(p)
      if nextParamName == nil: return nil
      if not p.expectOrError(tkColon, "Expected ':' after parameter name"): return nil

      var nextParamType =   "int"
      case p.current.kind
      of tkIntType:
        nextParamType =     "int"
        p.advance()
        
      of tkFloatType:
        nextParamType =     "double"
        p.advance()
      
      of tkStringType:
        nextParamType =     "char*"
        p.advance()
      
      of tkBoolType:
        nextParamType =     "bool"
        p.advance()
      
      of tkSizeTType:
        nextParamType =     "size_t"
        p.advance()
      
      of tkIdent:
        let typeIdent = parseIdentifier(p)
        if typeIdent != nil: nextParamType = typeIdent.identName
      else:
        return nil

      let nextParamNode = Node(kind: nkVarDecl, line: nextParamName.line,
        col: nextParamName.col, nodeKind: nkVarDecl, varName: nextParamName.identName,
        varType: nextParamType, varValue: nil,)
      params.add(nextParamNode)

  if not p.expect(tkRParen): return nil

  var 
    returnType = "void"
    returnsError = false

  if p.current.kind == tkColon:
    p.advance()
    
    if p.current.kind == tkLParen:
        p.advance()
        
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
            let typeIdent = parseIdentifier(p)
            if typeIdent != nil: returnType = typeIdent.identName
        else: return nil
      
        if not p.expect(tkComma): return nil
        if p.current.kind != tkError: return nil
        p.advance()
        
        if not p.expect(tkRParen): return nil
        returnsError = true
        
    else:
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
            if returnTypeIdent != nil: returnType = returnTypeIdent.identName
        else:
            echo "Warning: Expected return type after ':'"
  elif p.current.kind in {tkIdent, tkIntType, tkFloatType, tkStringType, tkBoolType}:
    
    case p.current.kind
    of tkIntType:
      returnType =    "int"
      p.advance()
    
    of tkFloatType:
      returnType =    "double"
      p.advance()
    
    of tkStringType:
      returnType =    "char*"
      p.advance()
    
    of tkBoolType:
      returnType =    "bool"
      p.advance()
    
    of tkIdent:
      let returnTypeIdent = parseIdentifier(p)
      if returnTypeIdent != nil: returnType = returnTypeIdent.identName
    else: discard

  let body = parseBlock(p)
  if body == nil: return nil

  return Node(kind: nkFunction, line: line, col: col, nodeKind: nkFunction,
    funcName: ident.identName, params: params, body: body, returnType: returnType,
    returnsError: returnsError,)

# =========================== PACKAGE PARSER ============================
proc parseReturn(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkReturn): return nil
  var returnValues: seq[Node] = @[]
  
  if p.current.kind != tkSemicolon and p.current.kind != tkEOF:
    let firstValue = parseExpression(p)
    if firstValue != nil:
      returnValues.add(firstValue)
      
      if p.current.kind == tkComma:
        p.advance() 
        let secondValue = parseExpression(p)
        if secondValue != nil:
          returnValues.add(secondValue)

  return Node(kind: nkReturn, line: line, col: col, nodeKind: nkReturn,
    callArgs: returnValues)

# =========================== TOP-LEVEL PARSERS ============================
proc parseTopLevel(p: Parser): Node =
  case p.current.kind
  of tkFunc:
    let    funcNode = parseFunction(p)
    return funcNode
  of tkCBlock:  return parseCBlock(p)
  of tkConst:   return parseConstDecl(p)
  of tkVar:     return parseVarDecl(p)
  of tkStruct:  return parseStruct(p)
  of tkEnum:    return parseEnum(p)
  else: return nil

# =========================== PROGRAM PARSER ============================
proc parseProgram*(p: Parser): Node =
  var allNodes: seq[Node] = @[]

  while p.current.kind != tkEOF:
    let node = parseTopLevel(p)
    if node != nil: allNodes.add(node)
    else: p.advance()

  return
    Node(kind: nkProgram, line: 1, col: 1, nodeKind: nkProgram, functions: allNodes)

# =========================== FOR LOOP PARSER ============================
proc parseFor(p: Parser): Node =
  let
    line = p.current.line
    col  = p.current.col

  if not p.expect(tkFor): return nil
  if p.current.kind == tkLParen:
    if not p.expect(tkLParen): return nil

    var init: Node = nil
    if p.current.kind == tkSemicolon: p.advance()
    elif p.current.kind == tkVar:
      init = parseVarDeclNoSemi(p)
      if not p.expect(tkSemicolon): return nil
    else:
      init = parseExpression(p)
      if init != nil:
        if not p.expect(tkSemicolon): return nil
      else: discard p.expect(tkSemicolon)

    var condition: Node = nil
    if p.current.kind == tkSemicolon: p.advance()
    else:
      condition = parseExpression(p)
      if condition != nil:
        if not p.expect(tkSemicolon): return nil
      else: discard p.expect(tkSemicolon)

    var update: Node = nil
    if p.current.kind == tkRParen: discard
    else:
      update = parseExpression(p)
      if update == nil: discard

    if not p.expectOrError(tkRParen, "Expected ')' after for clauses"): return nil
    let body = parseBlock(p)
    if body == nil: return nil

    return Node(kind: nkFor, line: line, col: col, nodeKind: nkFor, forInit: init,
      forCondition: condition, forUpdate: update, forBody: body,)
  else:
    var condition: Node = nil

    if p.current.kind != tkLBrace:
      condition = parseExpression(p)
      if condition == nil: return nil

    let body = parseBlock(p)
    if body == nil: return nil

    return Node(kind: nkFor, line: line, col: col, nodeKind: nkFor, forInit: nil,
      forCondition: condition, forUpdate: nil, forBody: body,)

# =========================== ENUM PARSER ============================
proc parseEnum(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col
  
  if not p.expect(tkEnum): return nil
  
  let nameIdent = parseIdentifier(p)
  if nameIdent == nil: return nil
  
  if not p.expectOrError(tkLBrace, "Expected '{' after enum name"): return nil
  
  var enumValues: seq[string] = @[]
  
  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    if p.current.kind == tkIdent:
      # Store the identifier name as a string
      enumValues.add(p.current.lexeme)
      p.advance()
      
      if p.current.kind == tkComma:
        p.advance()
    else:
      # Skip unexpected tokens
      p.advance()
  
  if not p.expectOrError(tkRBrace, "Expected '}' at end of enum"): return nil
  
  return Node(
    kind: nkEnum,
    line: line,
    col: col,
    nodeKind: nkEnum,
    enumName: nameIdent.identName,
    enumValues: enumValues
  )

# =========================== FOR RANGE LOOP PARSER ============================
proc parseForRange(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkFor): return nil

  var
    indexVar: Node = nil
    valueVar: Node = nil

  let firstIdent = parseIdentifier(p)
  if firstIdent == nil: return nil

  if p.current.kind == tkComma:
    p.advance()
    let secondIdent = parseIdentifier(p)
    if secondIdent == nil: return nil
    indexVar     = firstIdent
    valueVar     = secondIdent
  else: valueVar = firstIdent

  if not p.expectOrError(tkIn, "Expected 'in' in for loop"): return nil

  let target = parseExpression(p)
  if target == nil: return nil

  let body = parseBlock(p)
  if body == nil: return nil

  return Node(kind: nkForRange, line: line, col: col, nodeKind: nkForRange,
    rangeIndex: indexVar, rangeValue: valueVar, rangeTarget: target, rangeBody: body,)

# =========================== ASSIGNMENT PARSER ============================
proc parseAssignmentStatement(p: Parser): Node =
  let 
    startPos = p.pos
    left = parsePrimary(p)

  if left == nil: return nil

  if p.current.kind != tkAssign:
    p.pos = startPos
    p.current = p.tokens[startPos]
    return nil

  p.advance()

  let value = parseExpression(p)
  if value == nil: return nil
  return Node(kind: nkAssignment, line: left.line, col: left.col, nodeKind: nkAssignment,
    left: left, right: value,)

# =========================== CONTROL FLOW PARSERS ============================
proc parseIf(p: Parser): Node =
  let
    line = p.current.line
    col  = p.current.col

  if not p.expect(tkIf): return nil

  let condition = parseExpression(p)
  if condition == nil: return nil
  
  let thenBlock = parseBlock(p)
  if thenBlock == nil: return nil
  
  var elseBlock: Node = nil
  if p.current.kind == tkElse:
    p.advance()
    if p.current.kind == tkIf: elseBlock = parseIf(p)
    else:
      elseBlock = parseBlock(p)
      if elseBlock == nil: echo "Error: Expected block after 'else' at line ", line, ":", col

  return Node(kind: nkIf, line: line, col: col, nodeKind: nkIf, ifCondition: condition,
    ifThen: thenBlock, ifElse: elseBlock,)

# =========================== SWITCH PARSER ============================
proc parseSwitch(p: Parser): Node =
  let 
    line = p.current.line
    col = p.current.col

  if not p.expect(tkSwitch): return nil
  if not p.expectOrError(tkLParen, "Expected '(' after 'switch'"): return nil

  let target = parseExpression(p)
  if target == nil: return nil

  if not p.expectOrError(tkRParen, "Expected ')' after switch expression"): return nil
  if not p.expectOrError(tkLBrace, "Expected '{' after switch"): return nil

  var 
    cases: seq[Node] = @[]
    defaultCase: Node = nil

  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    if p.current.kind == tkCase:
      p.advance()

      var 
        caseValues: seq[Node] = @[]
        value = parseExpression(p)
      if value == nil: return nil
      caseValues.add(value)

      while p.current.kind == tkComma:
        p.advance()
        value = parseExpression(p)
        if value == nil: return nil
        caseValues.add(value)

      if not p.expectOrError(tkColon, "Expected ':' after case values"): return nil

      var body = parseBlock(p)
      if body == nil:
        let stmt = parseStatement(p)
        if stmt != nil:
          let stmtNode = Node(kind: nkBlock, line: stmt.line, col: stmt.col,
            nodeKind: nkBlock, statements: @[stmt],)
          body = stmtNode
        else: return nil

      let caseNode = Node(kind: nkCase, line: line, col: col, nodeKind: nkCase,
        caseValues: caseValues, caseBody: body,)
      cases.add(caseNode)
    elif p.current.kind == tkDefault:
      p.advance()

      if not p.expectOrError(tkColon, "Expected ':' after default"): return nil

      var body = parseBlock(p)
      if body == nil:
        let stmt = parseStatement(p)
        if stmt != nil:
          let stmtNode = Node( kind: nkBlock, line: stmt.line, col: stmt.col, 
            nodeKind: nkBlock, statements: @[stmt],)
          body = stmtNode
        else: return nil

      defaultCase = Node(kind: nkDefault, line: line, col: col, nodeKind: nkDefault, defaultBody: body)
    else: return nil

  if not p.expect(tkRBrace): return nil
  return Node( kind: nkSwitch, line: line, col: col, nodeKind: nkSwitch, 
    switchTarget: target, cases: cases, defaultCase: defaultCase,)

# =========================== AST PRINTING ============================
proc printAst*(node: Node, indent: int = 0) =
  let spaces = "  ".repeat(indent)

  case node.kind
  of nkProgram:
    echo spaces, "Program:"
    for fn in node.functions: printAst(fn, indent + 1)
  
  of nkPackage: echo spaces, "Package: ", node.packageName
  of nkFunction:
    echo spaces,
      "Function: ",
      node.funcName,
      "() -> ",
      node.returnType,
      if node.returnsError: " error" else: ""
    printAst(node.body, indent + 1)
  
  of nkEnum:
    echo spaces, "Enum: ", node.enumName
    echo spaces, "  Values: ", node.enumValues.join(", ")

  of nkBlock:
    echo spaces, "Block:"
    for stmt in node.statements: printAst(stmt, indent + 1)
  of nkCBlock: echo spaces, "CBlock: ", node.cCode
  
  of nkVarDecl:
    echo spaces, "VarDecl: ", node.varName
    printAst(node.varValue, indent + 1)
  
  of nkStruct:
    echo spaces, "Struct: ", node.structName
    echo spaces, "  Fields:"
  
  of nkIdentifier:  echo spaces, "Identifier: ", node.identName
  of nkLiteral:     echo spaces, "Literal: ", node.literalValue
  of nkStringLit:   echo spaces, "String: \"", node.literalValue, "\""

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
  
  of nkForRange:
    echo spaces, "ForRange:"
    if node.rangeIndex != nil: echo spaces, "  Index: ", node.rangeIndex.identName
    if node.rangeValue != nil: echo spaces, "  Value: ", node.rangeValue.identName
    if node.rangeTarget != nil:
      echo spaces, "  Target:"
      printAst(node.rangeTarget, indent + 2)
    if node.rangeBody != nil:
      echo spaces, "  Body:"
      printAst(node.rangeBody, indent + 2)
  
  of nkArrayLit:
    echo spaces, "ArrayLiteral with ", node.elements.len, " elements"
    for elem in node.elements:
      printAst(elem, indent + 1)

  of nkSwitch:
    echo spaces, "Switch:"
    printAst(node.switchTarget, indent + 1)
    echo spaces, "  Cases:"
    for caseNode in node.cases: printAst(caseNode, indent + 2)
    if node.defaultCase != nil:
      echo spaces, "  Default:"
      printAst(node.defaultCase, indent + 2)
  
  of nkCase:
    echo spaces, "Case with ", node.caseValues.len, " values"
    for value in node.caseValues: printAst(value, indent + 1)
    echo spaces, "  Body:"
    printAst(node.caseBody, indent + 1)
  
  of nkDefault:
    echo spaces, "Default:"
    printAst(node.defaultBody, indent + 1)
  
  of nkCall, nkReturn:
    if node.kind == nkCall: echo spaces, "Call: ", node.callFunc, "()"
    else: echo spaces, "Return:"
    if node.callArgs.len > 0:
      echo spaces, "  Args/Value:"
      for arg in node.callArgs: printAst(arg, indent + 2)
  
  of nkBinaryExpr:
    echo spaces, "BinaryExpr: ", node.op
    printAst(node.left, indent + 1)
    printAst(node.right, indent + 1)
  
  else: echo spaces, "Unknown node: ", node.kind
