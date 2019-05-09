module Parser

open FParsec
open UntypedAST
open System

let floatLiteralParser: Parser<Literal, unit> = numberLiteral (NumberLiteralOptions.DefaultFloat) "Float literal" |>> fun f -> 
    match f.IsInteger with
        | true -> int64 f.String |> IntLiteral
        | false -> float f.String |> FloatLiteral

let intLiteralParser = pint64 |>> IntLiteral

let quoteParser = skipChar '"'

let stringLiteralParser = quoteParser >>. (manyCharsTill anyChar quoteParser) |>> (fun s -> StringLiteral s)

let literalParser = choice [
    stringLiteralParser 
    floatLiteralParser
    intLiteralParser ] |>> (fun lit -> LiteralExpression lit)

let separatorParser = skipMany1 (skipChar '\n' <|> skipChar ';')

let ws = skipMany (pchar ' ' <|> pchar '\t')

let singleExpressionParser = choice [
    literalParser
]

let expressionParser = ws >>. singleExpressionParser .>> ws

let programParser = sepEndBy expressionParser separatorParser .>> eof

let parse (code: string) = match CharParsers.run programParser code with
                                | Success (res, a, b) -> Result.Ok res
                                | Failure (es, e, _) -> Result.Error e