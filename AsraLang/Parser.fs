module Parser

open FParsec
open UntypedAST

let expressionParser = anyString 2 |>> fun _ -> LiteralExpression (StringLiteral "Test")

let programParser = many expressionParser

let parse (code: string) = match CharParsers.run programParser code with
                                | Success (res, a, b) -> Result.Ok res
                                | Failure (es, _, _) -> Result.Error es