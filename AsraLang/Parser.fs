module Parser

open FParsec
open Ast
open System

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    if Config.currentConfig.parserTracing then
        fun stream ->
            System.Diagnostics.Debug.WriteLine (sprintf "%A: Entering %s" stream.Position label)
            let reply = p stream
            System.Diagnostics.Debug.WriteLine (sprintf "%A: Leaving %s (%A)" stream.Position label reply.Status)
            reply
    else p

let createParser (data: Parser<'data, unit>) =

    let (expressionParser: Parser<Expression<'data>, unit>, expressionParserRef) = createParserForwardedToRef ()
    
    let (valueExpressionParser: Parser<Expression<'data>, unit>, valueExpressionParserRef) = createParserForwardedToRef ()
    
    let (typeParser: Parser<TypeDeclaration, unit>, typeParserRef) = createParserForwardedToRef ()

    let isSeparator (c: char) = Char.IsWhiteSpace c || c = ';' || c = '(' || c = ')' || c = '[' || c = ']' || c = ',' || c = ':' || c = '#'
    
    let isIdentifierStart (c: char) = (not (isDigit c)) && not (isSeparator c)
    
    let isIdentifierContinue (c: char) = not (isSeparator c)
    
    let identifierOptions = IdentifierOptions(isAsciiIdStart = isIdentifierStart, isAsciiIdContinue = isIdentifierContinue)
    
    let identifierParser: Parser<string, unit> = identifier identifierOptions <?> "Identifier"
    
    let floatLiteralParser: Parser<LiteralValue, unit> = numberLiteral (NumberLiteralOptions.DefaultFloat) "Float literal" |>> fun f -> 
        match f.IsInteger with
            | true -> int64 f.String |> LiteralValue.Int
            | false -> float f.String |> LiteralValue.Float
    
    let intLiteralParser = pint64 |>> LiteralValue.Int
    
    let quoteParser = skipChar '"'
    
    let stringLiteralParser = quoteParser >>. (manyCharsTill anyChar quoteParser) |>> (fun s -> LiteralValue.String s) <?> "String literal"
    
    let literalExpressionParser = data .>>. choiceL [ stringLiteralParser; floatLiteralParser; intLiteralParser ] "Literal" |>> (fun (data, lit) -> LiteralExpression { literalValue = lit; data = data }) <!> "Literal expression parser"
    
    let variableExpressionParser = data .>>. identifierParser |>> (fun (data, s) -> VariableExpression (s, data)) <?> "Variable expression" <!> "Variable expression parser"
    
    let commentParser: Parser<unit, unit> = skipChar '#' >>. (skipManyTill anyChar (skipNewline <|> eof)) <!> "Comment parser"
    
    let separatorParser = skipMany1 (skipChar '\n' <|> skipChar ';' <|> commentParser) <!> "Separator parser" <?> "Separator"
    
    let ws = skipMany (pchar ' ' <|> pchar '\t')
    
    let openParensParser: Parser<char, unit> = pchar '('
    
    let closeParensParser: Parser<char, unit> = pchar ')'
    
    let groupExpressionParser = between openParensParser closeParensParser expressionParser |>> GroupExpression <?> "Group expression" <!> "Group expression parser"
    
    let typeArrowParser = skipString "=>"

    let namedTypeParser = identifierParser |>> Name

    let genericTypeParser = skipChar '\'' >>. identifierParser |>> Generic

    let functionTypeParser = namedTypeParser .>>? ws .>>? typeArrowParser .>> ws .>>. typeParser |>> Function

    typeParserRef := functionTypeParser <|> genericTypeParser <|> namedTypeParser
    
    let equalsParser = pchar '='
    
    let simpleDeclarationParser = ws >>. identifierParser .>> ws |>> Simple
    
    let annotatedDeclarationParser = ws >>. identifierParser .>> skipChar ':' .>> ws .>>. typeParser .>> ws |>> (fun (name, tp) -> Annotated { varName = name; typeName = tp }) |> attempt
    
    let declarationParser = annotatedDeclarationParser <|> simpleDeclarationParser <!> "Declaration parser" <?> "Declaration"
    
    let variableDefinitionParser = data .>>. declarationParser .>> equalsParser .>> ws .>>. valueExpressionParser .>> ws |>> (fun ((data, decl), expr) -> VariableBindingExpression { varName = decl; value = expr; varData = data }) <!> "Variable definition parser"
    
    let commaParser = skipChar ','
    
    let arrowParser = skipString "->"
    
    let blockParameterParser = ((sepBy (ws >>. declarationParser .>> ws) commaParser) .>> ws .>> arrowParser) |> attempt |> opt <?> "Block parameters" <!> "Block parameter parser"
    
    let blockParser = data .>> skipChar '[' .>>. blockParameterParser .>> spaces .>>. (sepEndBy expressionParser separatorParser) .>> spaces .>> skipChar ']' |>> (fun ((data, parameters), exprs) -> BlockExpression { parameters = Option.defaultValue [] parameters; body = exprs; data = data }) <!> "Block expression parser"
    
    let primitiveExpressionParser = 
        choiceL [
            blockParser
            literalExpressionParser
            groupExpressionParser
            variableExpressionParser ] "Primitive expression" <!> "Primitive expression parser"
    
    let functionCallParser = data .>>. primitiveExpressionParser .>>? ws .>>.? (sepEndBy1 primitiveExpressionParser ws) |>> (fun ((data, first), exprs) -> FunctionCallExpression { func = first; args = exprs; data = data }) <!> "Function call parser"
    
    valueExpressionParserRef := choiceL [
        functionCallParser
        primitiveExpressionParser
    ] "Value expression" <!> "Value expression parser"
    
    
    let singleExpressionParser = (choiceL [
        variableDefinitionParser |> attempt
        valueExpressionParser
    ] "Expression" <!> "Expression parser")
    
    expressionParserRef := ws >>? singleExpressionParser .>> ws
    
    let programParser =  data .>> spaces .>> (opt commentParser) .>> spaces .>>. sepEndBy expressionParser separatorParser .>> spaces .>> eof |>> (fun (data, exprs) -> BlockExpression { parameters = []; body = exprs; data = data })
    
    let parse (name: string) (code: string) = match CharParsers.runParserOnString programParser () name code with
                                                | Success (res, a, b) -> Result.Ok res
                                                | Failure (es, e, _) -> Result.Error es

    parse

let testParser = (createParser (preturn ())) "Test"

let defaultParser = createParser getPosition