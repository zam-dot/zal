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
  nkAddressOf =       "address_of"
  nkDeref =           "deref"          # *ptr
  nkPtrType =         "ptr_type"       # int*
  nkRefType =         "ref_type"       # &int
  # Reference counting nodes
  nkRcNew =           "rc_new"
  nkRcRetain =        "rc_retain"
  nkRcRelease =       "rc_release"
  nkRcInit =          "rc_init"
  nkWeakRef =         "weak_ref"
  nkStrongRef =       "strong_ref"
  nkArena =           "arena"
  nkArenaAlloc =      "arena_alloc"
  nkArenaArrayLit =   "arena_array_literal"

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
    structFields*:  seq[Node]
  
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
  
  of nkVarDecl, nkArena:
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
  
  of nkFieldAccess:
    base*:      Node
    field*:     Node
  
  of nkStructLiteral:
    structType*:  string
    fieldValues*: seq[Node]
  
  of nkArrayLit, nkArenaArrayLit: elements*: seq[Node]
  
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
  
  of nkAddressOf, nkDeref: 
    operand*: Node
  
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
  
  of nkRcNew, nkRcInit:
    rcType*:        string
    rcArgs*:        seq[Node]
  
  of nkRcRetain, nkRcRelease:
    rcTarget*:      Node
  
  of nkWeakRef, nkStrongRef:
    refTarget*:     Node

  of nkArenaAlloc:
    arenaType*:     string
    arenaArgs*:     seq[Node]

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
proc parseTypeWithConst(p: Parser): string
proc parseStruct(p: Parser): Node

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
 #   echo "Error: ", message, " at line ", p.current.line, ":", p.current.col
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

  if firstIdent == nil: return nil

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
  

# =========================== ARENA DECL PARSER ============================
proc parseArenaDecl(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col
    tokenStr = p.current.lexeme
  
  var arenaSizeBytes: int = 262144 # Default
  
  if tokenStr.startsWith("@arena(") and tokenStr.endsWith(")"):
    let sizeWithSuffix = tokenStr[7..^2].strip().toLowerAscii()
    
    if sizeWithSuffix.len > 0:
      var multiplier = 1
      var numStr = sizeWithSuffix

      if sizeWithSuffix.endsWith("gb") or sizeWithSuffix.endsWith("g"):
        multiplier = 1024 * 1024 * 1024
        numStr = sizeWithSuffix.replace("gb", "").replace("g", "")
      elif sizeWithSuffix.endsWith("mb") or sizeWithSuffix.endsWith("m"):
        multiplier = 1024 * 1024
        numStr = sizeWithSuffix.replace("mb", "").replace("m", "")
      elif sizeWithSuffix.endsWith("kb") or sizeWithSuffix.endsWith("k"):
        multiplier = 1024
        numStr = sizeWithSuffix.replace("kb", "").replace("k", "")

      try: arenaSizeBytes = parseInt(numStr.strip()) * multiplier
      except: echo "WARNING: Invalid arena size, using default"

  p.advance() 
  
  if p.current.kind == tkIdent:
    let identNode = parseIdentifier(p)
    if identNode == nil: 
      return nil
    
    if p.current.kind != tkAssign:
      return nil
    
    p.advance()
    
    let valueNode = parseExpression(p)
    if valueNode == nil:
      return nil
    
    if valueNode.kind == nkArrayLit:
      let arenaArrayNode = Node(kind: nkArenaArrayLit, nodeKind: nkArenaArrayLit,
                              line: valueNode.line, col: valueNode.col,
                              elements: valueNode.elements)
      
      return Node(kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
                  varName: identNode.identName,
                  varType: "arena:" & $arenaSizeBytes,
                  varValue: arenaArrayNode)
    else:
      return Node(kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
                  varName: identNode.identName,
                  varType: "arena:" & $arenaSizeBytes,
                  varValue: valueNode)
  return nil

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
# In parseType procedure, add support for multi-dimensional array types
proc parseType(p: Parser): string =
  var 
    typeStr = ""
  
  # Parse base type
  case p.current.kind
  of tkIntType:
    typeStr = "int"
    p.advance()
  of tkFloatType:
    typeStr = "double"
    p.advance()
  of tkStringType:
    typeStr = "char"
    p.advance()
    typeStr &= "*"
  of tkBoolType:
    typeStr = "bool"
    p.advance()
  of tkSizeTType:
    typeStr = "size_t"
    p.advance()
  of tkIdent:
    let ident = parseIdentifier(p)
    if ident != nil: typeStr = ident.identName
    else: return ""
  else: return ""
  
  # Parse array dimensions
  while p.current.kind == tkLBracket:
    p.advance()
    
    var size = ""
    if p.current.kind != tkRBracket:
      let sizeExpr = parseExpression(p)
      if sizeExpr != nil:
        case sizeExpr.kind
        of nkLiteral: size = sizeExpr.literalValue
        of nkIdentifier: size = sizeExpr.identName
        else: discard
    
    if not p.expect(tkRBracket): return ""
    
    typeStr &= "[" & size & "]"
  
  # Handle pointer stars
  while p.current.kind == tkStar:
    p.advance()
    if not typeStr.endsWith("*"):
      typeStr &= "*"
  
  return typeStr

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
    col  = p.current.col

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
  of tkIdent:
    let baseNode = parseIdentifier(p)
    if baseNode == nil: return nil

    var currentNode = baseNode
    
    # Handle field access and index expressions
    while true:
      if p.current.kind == tkDot:
        p.advance()
        let field = parseIdentifier(p)
        if field == nil: return nil
        currentNode = Node(kind: nkFieldAccess, line: currentNode.line, col: currentNode.col,
          nodeKind: nkFieldAccess, base: currentNode, field: field)
      
      elif p.current.kind == tkLBracket:
        p.advance()
        let index = parseExpression(p)
        if index == nil: return nil
        if not p.expect(tkRBracket): return nil
        currentNode = Node(kind: nkIndexExpr, line: currentNode.line, col: currentNode.col,
          nodeKind: nkIndexExpr, left: currentNode, right: index)
      
      else:
        break
    
    return currentNode
  
  of tkPrint, tkGetMem, tkFreeMem, tkLen, tkAlloc:
    if p.peek(1).kind == tkLParen:
      if p.current.lexeme == "String" and p.peek(1).kind == tkLParen:
        let line = p.current.line
        let col = p.current.col
        
        p.advance()
        if not p.expect(tkLParen): return nil
        let arg = parseExpression(p)
        if arg == nil: return nil
        if not p.expect(tkRParen): return nil
        
        return Node(kind: nkRcNew, line: line, col: col,
          nodeKind: nkRcNew, rcType: "string", rcArgs: @[arg])
      
      if p.current.lexeme == "Array" and p.peek(1).kind == tkLBracket:
        let line = p.current.line
        let col = p.current.col
        
        p.advance()
        
        if not p.expect(tkLBracket): return nil
        let elemTypeNode = parseIdentifier(p)
        if elemTypeNode == nil: return nil
        if not p.expect(tkRBracket): return nil
        if not p.expect(tkLParen): return nil
        
        let sizeArg = parseExpression(p)
        if sizeArg == nil: return nil
        if not p.expect(tkRParen): return nil
        
        let sizeStr = 
          case sizeArg.kind
          of nkLiteral: sizeArg.literalValue
          of nkIdentifier: sizeArg.identName
          else: "0"
        
        let arrayType = elemTypeNode.identName & "[" & sizeStr & "]"
        
        return Node(kind: nkRcNew, line: line, col: col,
          nodeKind: nkRcNew, rcType: arrayType, rcArgs: @[])
      
      return parseCallExpr(p)
    
    if p.peek(1).kind == tkLBrace:
      let 
        savedPos = p.pos
        typeNameNode = p.parseIdentifier()
      if typeNameNode != nil:
        let name = typeNameNode.identName
        if name.len > 0:
          let structLit = parseStructLiteral(p, name)
          if structLit != nil: return structLit
      
      p.pos = savedPos
      p.current = p.tokens[savedPos]

    let baseNode = parseIdentifier(p)
    if baseNode == nil: return nil

    var currentNode = baseNode
    while p.current.kind == tkDot:
      p.advance()
      let field = parseIdentifier(p)
      if field == nil: return nil
      currentNode = Node(kind: nkFieldAccess, line: currentNode.line, col: currentNode.col,
        nodeKind: nkFieldAccess, base: currentNode, field: field)

    if p.current.kind == tkLBracket:
      p.advance()
      let index = parseExpression(p)
      if index == nil: return nil
      if not p.expect(tkRBracket): return nil
      return Node(kind: nkIndexExpr, line: currentNode.line, col: currentNode.col,
        nodeKind: nkIndexExpr, left: currentNode, right: index)
  
    return currentNode
  
  of tkIntType, tkFloatType, tkStringType, tkBoolType, tkSizeTType:
    let
      line =      p.current.line
      col =       p.current.col
      typeName =  p.current.lexeme
    p.advance()
    return Node(kind: nkIdentifier, line: line, col: col, nodeKind: nkIdentifier, identName: typeName,)

  of tkLBracket:                        
    let line = p.current.line
    let col = p.current.col
    
    p.advance()
    
    var elements: seq[Node] = @[]
    
    if p.current.kind != tkRBracket:
      let first = parseExpression(p)
      if first != nil: 
        elements.add(first)
      
      while p.current.kind == tkComma:
        p.advance()
        let next = parseExpression(p)
        if next != nil: 
          elements.add(next)
        else: break
    
    if not p.expect(tkRBracket): return nil
    
    return Node(kind: nkArrayLit, line: line, col: col,
      nodeKind: nkArrayLit, elements: elements)
  
  of tkIntLit, tkFloatLit, tkStringLit: return parseLiteral(p)
  of tkNil: return parseLiteral(p)
  
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
      currentNode = baseNode

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

  of tkAmpersand:
    let
      line = p.current.line
      col = p.current.col
    p.advance()
    
    let operand = parsePrimary(p)
    if operand == nil: return nil
    return Node(kind: nkAddressOf, line: line, col: col, nodeKind: nkAddressOf,
      operand: operand)

  of tkStar:
    let
      line = p.current.line
      col = p.current.col
    p.advance()
    
    let operand = parsePrimary(p)
    if operand == nil:
      return nil
    return Node(kind: nkDeref, line: line, col: col, nodeKind: nkDeref, operand: operand)

  of tkRc:
    let 
      line = p.current.line
      col = p.current.col
      savedPos = p.pos
    
    p.advance()
    
    if p.current.kind == tkIdent and p.current.lexeme in ["new", "init"]:
      let op = p.current.lexeme
      p.advance()
      
      var typeName = ""
      
      case p.current.kind
      of tkIdent:
        typeName = p.current.lexeme
        p.advance()
        
        if p.current.kind == tkLBracket:
          p.advance()
          
          var size = ""
          if p.current.kind != tkRBracket:
            let sizeExpr = parseExpression(p)
            if sizeExpr != nil:
              case sizeExpr.kind
              of nkLiteral: size = sizeExpr.literalValue
              of nkIdentifier: size = sizeExpr.identName
              else: discard
          
          if not p.expect(tkRBracket): return nil
          typeName = typeName & "[" & size & "]"
      
      of tkStringLit:
        typeName = p.current.strVal
        p.advance()
      
      else:
        p.pos = savedPos
        p.current = p.tokens[savedPos]
        return nil
      
      if typeName.len == 0:
        p.pos = savedPos
        p.current = p.tokens[savedPos]
        return nil
      
      if not p.expect(tkLParen): return nil
      
      var args: seq[Node] = @[]
      if p.current.kind != tkRParen:
        let firstArg = parseExpression(p)
        if firstArg != nil: args.add(firstArg)
        
        while p.current.kind == tkComma:
          p.advance()
          let nextArg = parseExpression(p)
          if nextArg != nil: args.add(nextArg)
          else: break
      
      if not p.expect(tkRParen): return nil
      
      if op == "init": 
        return Node(kind: nkRcInit, line: line, col: col, 
          nodeKind: nkRcInit, rcType: typeName, rcArgs: args)
      else: 
        return Node(kind: nkRcNew, line: line, col: col, 
          nodeKind: nkRcNew, rcType: typeName, rcArgs: args)
    else:
      p.pos = savedPos
      p.current = p.tokens[savedPos]
      return parseIdentifier(p)

  of tkLBrace:
    let line = p.current.line
    let col = p.current.col
    
    p.advance()
    
    var elements: seq[Node] = @[]
    
    if p.current.kind != tkRBrace:
      let first = parseExpression(p)
      if first != nil: 
        elements.add(first)
      
      while p.current.kind == tkComma:
        p.advance()
        let next = parseExpression(p)
        if next != nil: 
          elements.add(next)
        else: break
    
    if not p.expect(tkRBrace): return nil
    
    return Node(kind: nkArrayLit, line: line, col: col,
      nodeKind: nkArrayLit, elements: elements)

  of tkRcRetain:
    let 
      line = p.current.line
      col = p.current.col
    p.advance()
    
    if not p.expect(tkLParen): return nil
    let target = parseExpression(p)
    if target == nil: return nil
    if not p.expect(tkRParen): return nil
    return Node(kind: nkRcRetain, line: line, col: col, nodeKind: nkRcRetain, rcTarget: target)

  of tkRcRelease:
    let 
      line = p.current.line
      col = p.current.col
    p.advance()
    
    if not p.expect(tkLParen): return nil
    let target = parseExpression(p)
    if target == nil: return nil
    if not p.expect(tkRParen): return nil
    return Node(kind: nkRcRelease, line: line, col: col, nodeKind: nkRcRelease, rcTarget: target)

  of tkWeak:
    let 
      line = p.current.line
      col = p.current.col
    p.advance()
    
    let target = parseExpression(p)
    if target == nil: return nil
    return Node(kind: nkWeakRef, line: line, col: col, nodeKind: nkWeakRef, refTarget: target)

  of tkStrong:
    let 
      line = p.current.line
      col = p.current.col
    p.advance()
    
    let target = parseExpression(p)
    if target == nil: return nil
    return Node(kind: nkStrongRef, line: line, col: col, nodeKind: nkStrongRef, refTarget: target)

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

# =========================== ARRAY LITERAL PARSER ============================
proc parseArrayLiteral(p: Parser): Node =
  let line = p.current.line
  let col = p.current.col
  
  if not p.expect(tkLBrace): return nil
  
  var elements: seq[Node] = @[]
  
  if p.current.kind != tkRBrace:
    # Check if next token starts another array literal
    if p.current.kind == tkLBrace:
      # Nested array (matrix)
      let first = parseArrayLiteral(p)
      if first != nil:
        elements.add(first)
    else:
      let first = parseExpression(p)
      if first != nil:
        elements.add(first)
    
    while p.current.kind == tkComma:
      p.advance()
      
      if p.current.kind == tkLBrace:
        # Nested array
        let next = parseArrayLiteral(p)
        if next != nil:
          elements.add(next)
        else: break
      else:
        let next = parseExpression(p)
        if next != nil:
          elements.add(next)
        else: break
  
  if not p.expect(tkRBrace): return nil
  
  return Node(kind: nkArrayLit, line: line, col: col,
    nodeKind: nkArrayLit, elements: elements)

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
    varType = parseType(p)
    if varType.len == 0: return nil
  var varValue: Node = nil
  
  if p.current.kind == tkAssign:
    p.advance()
    varValue = parseExpression(p)
    if varValue == nil: return nil

  return Node(kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
    varName: ident.identName, varType: varType, varValue: varValue)

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

  if not p.expect(tkAssign): return nil

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
    else: return nil

  if not p.expectOrError(tkAssign, "Expected '=' after constant name"): return nil

  let value = parseExpression(p)
  if value == nil: return nil

  return Node(kind: nkConstDecl, line: line, col: col, nodeKind: nkConstDecl,
    constName: ident.identName, constType: constType, constValue: value,)

# =========================== STATEMENT VAR DECL ============================
proc parseVarDeclNoSemi(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col

  if not p.expect(tkVar): return nil
  let ident = parseIdentifier(p)
  if ident == nil: return nil

  var varType = ""

  if p.current.kind == tkColon:
    p.advance()
    varType = parseType(p)
    if varType.len == 0: return nil

  var varValue: Node = nil
  
  if p.current.kind == tkAssign:
    p.advance()
    varValue = parseExpression(p)
    if varValue == nil: return nil

  return Node(kind: nkVarDecl, line: line, col: col, nodeKind: nkVarDecl,
    varName: ident.identName, varType: varType, varValue: varValue)

# =========================== STATEMENT PARSERS ============================
proc parseStatement(p: Parser): Node =
  case p.current.kind

  of tkFor:
    let savedPos = p.pos
    let forRangeResult = parseForRange(p)
    
    if forRangeResult != nil: 
      return forRangeResult
    else:
      p.pos = savedPos
      p.current = p.tokens[savedPos]
      return parseFor(p)

  of tkIf:     return parseIf(p)
  of tkConst:  return parseConstDecl(p)

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

  of tkCBlock: return parseCBlock(p)

  of tkArena:
    return parseArenaDecl(p)  # <-- Make sure it's calling parseArenaDecl

  of tkReturn: return parseReturn(p)
  of tkSwitch: return parseSwitch(p)
  of tkDefer:  return parseDefer(p)
  of tkIdent:
    if p.peek(1).kind == tkColonAssign: return parseInferredVarDecl(p)
    if p.peek(1).kind == tkComma and p.peek(2).kind == tkIdent and p.peek(3).kind == tkColonAssign:
      return parseInferredVarDecl(p)
    
    var assignment = parseAssignmentStatement(p)
    if assignment != nil: return assignment
    
    let call = parseCallStatement(p)
    if call != nil: return call
    else: return nil

  else: return nil

# =========================== ARENA DECLARATION PARSER ============================
proc parseArenaDeclaration(p: Parser): Node {.used.} =
  p.advance()

  let name = parseIdentifier(p)
  if name == nil: return nil

  if not p.expect(tkAssign): return nil

  let sizeExpr = parseExpression(p)
  if sizeExpr == nil: return nil

  var sizeStr = "65536"
  if sizeExpr.kind == nkLiteral:
    sizeStr = sizeExpr.literalValue

  return nil

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

  if not p.expect(tkLParen): return nil
  var params: seq[Node] = @[]

  if p.current.kind != tkRParen:
    let paramName = parseIdentifier(p)
    if paramName == nil: return nil

    if not p.expect(tkColon): return nil
    let paramType = parseTypeWithConst(p)
    if paramType.len == 0: return nil

    let paramNode = Node(kind: nkVarDecl, line: paramName.line, col: paramName.col,
      nodeKind: nkVarDecl, varName: paramName.identName, varType: paramType, varValue: nil, )
    params.add(paramNode)

    while p.current.kind == tkComma:
      p.advance()

      let nextParamName = parseIdentifier(p)
      if nextParamName == nil: return nil
      if not p.expect(tkColon): return nil

      let nextParamType = parseTypeWithConst(p)
      if nextParamType.len == 0: return nil

      let nextParamNode = Node(kind: nkVarDecl, line: nextParamName.line, col: nextParamName.col, 
        nodeKind: nkVarDecl, varName: nextParamName.identName, varType: nextParamType, varValue: nil,)
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

# =========================== TYPE PARSER WITH CONST ============================
proc parseTypeWithConst(p: Parser): string =
  var isConst = false
  
  if p.current.kind == tkConst:
    isConst = true
    p.advance()
  
  var baseType = parseType(p)
  if baseType.len == 0: return ""
  if isConst: return "const " & baseType
  else: return baseType

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
  of tkArena:
    return parseArenaDecl(p)
  of tkFunc:
    let    funcNode = parseFunction(p)
    return funcNode
  of tkCBlock:  return parseCBlock(p)
  of tkConst:   return parseConstDecl(p)
  of tkVar:     return parseVarDecl(p)
  of tkStruct:
    return parseStruct(p)
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

    if not p.expect(tkRParen): return nil
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
  if not p.expect(tkLBrace): return nil
  
  var enumValues: seq[string] = @[]
  
  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    if p.current.kind == tkIdent:
      let valueName = p.current.lexeme
      p.advance()
      
      if p.current.kind == tkAssign:
        p.advance()
        if p.current.kind == tkIntLit:
          let value = p.current.lexeme
          p.advance()
          enumValues.add(valueName & " = " & value)
        elif p.current.kind == tkIdent:
          let refName = p.current.lexeme
          p.advance()
          enumValues.add(valueName & " = " & refName)
        else: return nil
      else: enumValues.add(valueName)
      
      if p.current.kind == tkComma:
        p.advance()
      
    elif p.current.kind == tkComma:
      p.advance()
    else: break
  
  if not p.expectOrError(tkRBrace, "Expected '}' at end of enum"): return nil
  
  return Node(kind: nkEnum, line: line, col: col, nodeKind: nkEnum,
    enumName: nameIdent.identName, enumValues: enumValues)

# =========================== ARRAY LITERAL PARSER ============================
proc parseStructLiteral(p: Parser, structName: string): Node =
  let
    line = p.current.line
    col = p.current.col
  
  if not p.expect(tkLBrace): return nil
  var fieldValues: seq[Node] = @[]
  
  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    let fieldName = p.parseIdentifier()
    if fieldName == nil: return nil
    if p.current.kind == tkColon:
      p.advance()
    elif p.current.kind == tkAssign:
      p.advance()
    else:
      echo "Error: Expected ':' or '=' after field name at line ", p.current.line
      return nil
    
    let value = parseExpression(p)
    if value == nil:
      return nil
    
    let assignment = Node(kind: nkBinaryExpr, line: fieldName.line, col: fieldName.col,
      nodeKind: nkBinaryExpr, left: fieldName, right: value, op: ":")
    fieldValues.add(assignment)
    
    if p.expect(tkComma): discard
    elif p.current.kind != tkRBrace:
      if p.current.kind == tkIdent: continue
      else: break
  
  if not p.expect(tkRBrace): return nil
  
  return Node(kind: nkStructLiteral, line: line, col: col, nodeKind: nkStructLiteral,
    structType: structName, fieldValues: fieldValues)

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
    indexVar = firstIdent
    valueVar = secondIdent
  else: valueVar = firstIdent

  if not p.expect(tkIn): return nil
  let target = parseExpression(p)
  if target == nil: return nil

  let body = parseBlock(p)
  if body == nil: return nil

  return Node(kind: nkForRange, line: line, col: col, nodeKind: nkForRange,
    rangeIndex: indexVar, rangeValue: valueVar, rangeTarget: target, rangeBody: body)

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
  if not p.expect(tkLParen): return nil

  let target = parseExpression(p)
  if target == nil: return nil

  if not p.expect(tkRParen): return nil
  if not p.expect(tkLBrace): return nil

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

      if not p.expect(tkColon): return nil
      var body: Node = nil
      
      if p.current.kind == tkLBrace: body = parseBlock(p)
      else:
        let stmt = parseStatement(p)
        if stmt != nil:
          body = Node(kind: nkBlock, line: stmt.line, col: stmt.col,
            nodeKind: nkBlock, statements: @[stmt])
      
      if body == nil:
        return nil

      let caseNode = Node(kind: nkCase, line: line, col: col, nodeKind: nkCase,
        caseValues: caseValues, caseBody: body)
      cases.add(caseNode)
    
    elif p.current.kind == tkDefault:
      p.advance()

      if not p.expect(tkColon): return nil
      var body: Node = nil
      
      if p.current.kind == tkLBrace:
        body = parseBlock(p)
      else:
        let stmt = parseStatement(p)
        if stmt != nil:
          body = Node(kind: nkBlock, line: stmt.line, col: stmt.col,
            nodeKind: nkBlock, statements: @[stmt])
      
      if body == nil:
        return nil

      defaultCase = Node(kind: nkDefault, line: line, col: col, 
        nodeKind: nkDefault, defaultBody: body)
    
    else:
      return nil

  if not p.expect(tkRBrace):
    return nil

  return Node(kind: nkSwitch, line: line, col: col, nodeKind: nkSwitch,
    switchTarget: target, cases: cases, defaultCase: defaultCase)

# =========================== STRUCT PARSER ============================
proc parseStruct(p: Parser): Node =
  let
    line = p.current.line
    col = p.current.col
  
  if not p.expect(tkStruct): return nil
  
  let nameIdent = p.parseIdentifier()
  if nameIdent == nil: return nil
  if not p.expect(tkLBrace): return nil
  
  var fields: seq[Node] = @[]
  
  while p.current.kind != tkRBrace and p.current.kind != tkEOF:
    let fieldName = p.parseIdentifier()
    if fieldName == nil:
      if p.current.kind == tkComma:
        p.advance()
        continue
      elif p.current.kind == tkSemicolon:
        p.advance() 
        continue
      else: break
    
    if not p.expect(tkColon): return nil
    let fieldType = p.parseType()
    if fieldType.len == 0: return nil
    
    let fieldNode = Node(kind: nkVarDecl, line: fieldName.line, col: fieldName.col, 
      nodeKind: nkVarDecl, varName: fieldName.identName, varType: fieldType, varValue: nil)
    fields.add(fieldNode)
    
    if p.current.kind == tkSemicolon:
      p.advance()
    elif p.current.kind == tkComma:
      p.advance()
  
  if not p.expect(tkRBrace): return nil
  
  return Node(kind: nkStruct, line: line, col: col, nodeKind: nkStruct, 
    structName: nameIdent.identName, structFields: fields)

# =========================== AST PRINTING ============================
proc printAst*(node: Node, indent: int = 0) =
  let spaces = "  ".repeat(indent)

  case node.kind
  of nkProgram:
    echo spaces, "Program:"
    for fn in node.functions: printAst(fn, indent + 1)
  
  of nkPackage: echo spaces, "Package: ", node.packageName
  of nkFunction:
    echo spaces, "Function: ", node.funcName, "() -> ", node.returnType,
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
    if node.varValue != nil: printAst(node.varValue, indent + 1)
    else: echo spaces, "  (no value)"
  
  of nkStruct:
    echo spaces, "Struct: ", node.structName
    echo spaces, "  Fields:"
    for field in node.structFields:
      printAst(field, indent + 2)
  
  of nkIdentifier:  echo spaces, "Identifier: ", node.identName
  of nkLiteral:     echo spaces, "Literal: ", node.literalValue
  of nkStringLit:   echo spaces, "String: \"", node.literalValue, "\""
  
  of nkAddressOf:
    echo spaces, "AddressOf (&):"
    printAst(node.operand, indent + 1)
  
  of nkDeref:
    echo spaces, "Dereference (*):"
    printAst(node.operand, indent + 1)

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
