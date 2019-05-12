module Parser

open FParsec
open UntypedAST
open System

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    if Config.currentConfig.parserTracing then
        fun stream ->
            System.Diagnostics.Debug.WriteLine (sprintf "%A: Entering %s" stream.Position label)
            let reply = p stream
            System.Diagnostics.Debug.WriteLine (sprintf "%A: Leaving %s (%A)" stream.Position label reply.Status)
            reply
    else p

let (expressionParser: Parser<Expression, unit>, expressionParserRef) = createParserForwardedToRef ()

let (valueExpressionParser: Parser<Expression, unit>, valueExpressionParserRef) = createParserForwardedToRef ()

let isSeparator (c: char) = Char.IsWhiteSpace c || c = ';' || c = '(' || c = ')' || c = '[' || c = ']' || c = ',' || c = ':' || c = '#'

let isIdentifierStart (c: char) = (not (isDigit c)) && not (isSeparator c)

let isIdentifierContinue (c: char) = not (isSeparator c)

let identifierOptions = IdentifierOptions(isAsciiIdStart = isIdentifierStart, isAsciiIdContinue = isIdentifierContinue)

let identifierParser: Parser<string, unit> = identifier identifierOptions <?> "Identifier"

let floatLiteralParser: Parser<Literal, unit> = numberLiteral (NumberLiteralOptions.DefaultFloat) "Float literal" |>> fun f -> 
    match f.IsInteger with
        | true -> int64 f.String |> IntLiteral
        | false -> float f.String |> FloatLiteral

let intLiteralParser = pint64 |>> IntLiteral

let quoteParser = skipChar '"'

let stringLiteralParser = quoteParser >>. (manyCharsTill anyChar quoteParser) |>> (fun s -> StringLiteral s) <?> "String literal"

let literalExpressionParser = choiceL [ stringLiteralParser; floatLiteralParser; intLiteralParser ] "Literal" |>> (fun lit -> LiteralExpression lit) <!> "Literal expression parser"

let variableExpressionParser = identifierParser |>> VariableExpression <?> "Variable expression" <!> "Variable expression parser"

let commentParser: Parser<unit, unit> = skipChar '#' >>. (skipManyTill anyChar (skipNewline <|> eof)) <!> "Comment parser"

let separatorParser = skipMany1 (skipChar '\n' <|> skipChar ';' <|> commentParser) <!> "Separator parser" <?> "Separator"

let ws = skipMany (pchar ' ' <|> pchar '\t')

let openParensParser: Parser<char, unit> = pchar '('

let closeParensParser: Parser<char, unit> = pchar ')'

let groupExpressionParser = between openParensParser closeParensParser expressionParser |>> GroupExpression <?> "Group expression" <!> "Group expression parser"

let typeParser = identifierParser

let equalsParser = pchar '='

let simpleDeclarationParser = ws >>. identifierParser .>> ws |>> Simple

let annotatedDeclarationParser = ws >>. identifierParser .>> skipChar ':' .>> ws .>>. typeParser .>> ws |>> (fun (name, tp) -> Annotated { varName = name; typeName = tp }) |> attempt

let declarationParser = annotatedDeclarationParser <|> simpleDeclarationParser

let variableDefinitionParser = declarationParser .>> equalsParser .>> ws .>>. valueExpressionParser .>> ws |>> (fun (decl, expr) -> DefineVariableExpression { variableName = decl; value = expr }) <!> "Variable definition parser"

let commaParser = skipChar ','

let arrowParser = skipString "->"

let blockParameterParser = ((sepBy (ws >>. declarationParser .>> ws) commaParser) .>> ws .>> arrowParser) |> attempt |> opt <?> "Block parameters" <!> "Block parameter parser"

let blockParser = skipChar '[' >>. blockParameterParser .>> spaces .>>. (sepEndBy expressionParser separatorParser) .>> spaces .>> skipChar ']' |>> (fun (paramsOpt, exprs) -> BlockExpression { parameters = paramsOpt; body = exprs }) <!> "Block expression parser"

let primitiveExpressionParser = 
    choiceL [
        blockParser
        literalExpressionParser
        groupExpressionParser
        variableExpressionParser ] "Primitive expression" <!> "Primitive expression parser"

let functionCallParser = primitiveExpressionParser .>>? ws .>>.? (sepEndBy1 primitiveExpressionParser ws) |>> (fun (first, exprs) -> FunctionCallExpression { func = first; arguments = exprs }) <!> "Function call parser"

valueExpressionParserRef := choiceL [
    functionCallParser
    primitiveExpressionParser
] "Value expression" <!> "Value expression parser"


let singleExpressionParser = (choiceL [
    variableDefinitionParser |> attempt
    valueExpressionParser
] "Expression" <!> "Expression parser")

expressionParserRef := ws >>? singleExpressionParser .>> ws

let programParser =  spaces >>. (opt commentParser) >>. spaces >>. sepEndBy expressionParser separatorParser .>> spaces .>> eof

let parse (code: string) = match CharParsers.run programParser code with
                                | Success (res, a, b) -> Result.Ok res
                                | Failure (es, e, _) -> Result.Error e