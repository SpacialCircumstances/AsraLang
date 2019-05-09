module Parser

open FParsec
open UntypedAST
open System

let (expressionParser: Parser<Expression, unit>, expressionParserRef) = createParserForwardedToRef ()

let (primitiveExpressionParser: Parser<Expression, unit>, primitiveExpressionParserRef) = createParserForwardedToRef ()

let isSeparator (c: char) = Char.IsWhiteSpace c || c = ';' || c = '(' || c = ')' || c = '[' || c = ']' || c = ',' || c = ':' || c = '#'

let isIdentifierStart (c: char) = (not (isDigit c)) && not (isSeparator c)

let isIdentifierContinue (c: char) = not (isSeparator c)

let identifierOptions = IdentifierOptions(isAsciiIdStart = isIdentifierStart, isAsciiIdContinue = isIdentifierContinue)

let identifierParser: Parser<string, unit> = identifier identifierOptions

let floatLiteralParser: Parser<Literal, unit> = numberLiteral (NumberLiteralOptions.DefaultFloat) "Float literal" |>> fun f -> 
    match f.IsInteger with
        | true -> int64 f.String |> IntLiteral
        | false -> float f.String |> FloatLiteral

let intLiteralParser = pint64 |>> IntLiteral

let quoteParser = skipChar '"'

let stringLiteralParser = quoteParser >>. (manyCharsTill anyChar quoteParser) |>> (fun s -> StringLiteral s)

let literalExpressionParser = choice [ stringLiteralParser; floatLiteralParser; intLiteralParser ] |>> (fun lit -> LiteralExpression lit)

let variableExpressionParser = identifierParser |>> VariableExpression

let separatorParser = skipMany1 (skipChar '\n' <|> skipChar ';')

let ws = skipMany (pchar ' ' <|> pchar '\t')

let openParensParser: Parser<char, unit> = pchar '('

let closeParensParser: Parser<char, unit> = pchar ')'

let groupExpressionParser = between openParensParser closeParensParser expressionParser |>> GroupExpression

let typeParser = identifierParser

let equalsParser = pchar '='

let simpleDeclarationParser = ws >>. identifierParser .>> ws |>> Simple

let annotatedDeclarationParser = ws >>. identifierParser .>> skipChar ':' .>> ws .>>. typeParser .>> ws |>> (fun (name, tp) -> Annotated { varName = name; typeName = tp }) |> attempt

let declarationParser = annotatedDeclarationParser <|> simpleDeclarationParser

let variableDefinitionParser = declarationParser .>> equalsParser .>> ws .>>. primitiveExpressionParser .>> ws |>> (fun (decl, expr) -> DefineVariableExpression { variableName = decl; value = expr })

primitiveExpressionParserRef := choice [
    literalExpressionParser
    groupExpressionParser
    variableExpressionParser
]

let singleExpressionParser = choice [
    variableDefinitionParser |> attempt
    primitiveExpressionParser
]

expressionParserRef := ws >>? singleExpressionParser .>> ws

let programParser = spaces >>. sepEndBy expressionParser separatorParser .>> spaces .>> eof

let parse (code: string) = match CharParsers.run programParser code with
                                | Success (res, a, b) -> Result.Ok res
                                | Failure (es, e, _) -> Result.Error e