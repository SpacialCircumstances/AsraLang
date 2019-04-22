module Parser

open Pidgin
open Token
open System

let mutable parseValueExpression = Unchecked.defaultof<Parser<Token, ParseTree.Expression>>

let token (f: Token -> 'a option) = //TODO: Make more efficient, do not run f twice
    Parser<Token>.Token(Func<Token, bool>(fun t -> match f t with
                                                        | None -> false
                                                        | Some _ -> true)).Select(Func<Token, 'a>(fun t -> match f t with
                                                                                                            | Some a -> a
                                                                                                            | None -> invalidOp "Can never happen"))

let choice (parsers: Parser<Token, 'a> seq) = Parser.OneOf parsers

let map (parser: Parser<Token, 'a>) (mapper: 'a -> 'b) = parser.Select(Func<'a, 'b>(mapper))

let (<!>) = map

let map2 (parser1: Parser<Token, 'a>) (parser2: Parser<Token, 'b>) (mapper: 'a -> 'b -> 'c) = Parser.Map(Func<'a, 'b, 'c>(mapper), parser1, parser2)

let sepBy (parser: Parser<Token, 'a>) (sep: Parser<Token, 'b>) = parser.SeparatedAtLeastOnce sep

let prec (p: unit -> Parser<Token, 'a>) = Parser.Rec(Func<Parser<Token, 'a>>(p))

let parseStringLiteral = token (fun t ->
                                    match t.token with
                                        | StringLiteral str -> Some (ParseTree.StringLiteral str)
                                        | _ -> None)

let parseIntLiteral = token (fun t ->
                                match t.token with
                                    | IntLiteral i -> Some (ParseTree.IntLiteral i)
                                    | _ -> None)

let parseFloatLiteral = token (fun t ->
                                match t.token with
                                    | FloatLiteral f -> Some (ParseTree.FloatLiteral f)
                                    | _ -> None)

let parseLiteralExpression = choice [ parseStringLiteral; parseFloatLiteral; parseIntLiteral ] <!> ParseTree.LiteralExpression

let parseIdentifier = token (fun t -> match t.token with
                                                | Identifier i -> Some i
                                                | _ -> None)

let parseVariableExpression = parseIdentifier <!> ParseTree.VariableExpression

let parseLeftParen = token (fun t -> match t.token with
                                        | LeftParen -> Some ()
                                        | _ -> None)

let parseRightParen = token (fun t -> match t.token with
                                        | RightParen -> Some ()
                                        | _ -> None)

let parseGroupExpression = parseLeftParen.Then(prec (fun () -> parseValueExpression)).Before(parseRightParen) <!> ParseTree.GroupExpression

let parseEqual = token (fun t -> match t.token with
                                        | Equal -> Some ()
                                        | _ -> None)

let parseSimpleDeclaration = parseIdentifier <!> ParseTree.Simple

let parseDeclaration = parseSimpleDeclaration //TODO: Declaration with type annotation

let parseVariableDefinitionExpression = map2 (parseDeclaration.Before(parseEqual)) (prec (fun () -> parseValueExpression)) (fun d e -> ParseTree.DefineVariableExpression { variableName = d; value = e })

parseValueExpression <- choice [
    parseLiteralExpression
    parseVariableExpression
    parseGroupExpression
]

let parseExpression = choice [ 
    parseVariableDefinitionExpression
    parseValueExpression
]

let parseDot = token (fun t ->
                            match t.token with
                                | Dot -> Some ()
                                | _ -> None)

let programParser = sepBy parseExpression parseDot

let parse (tokens: Token seq) = 
    let res = programParser.Parse(tokens)
    match res.Success with
        | true -> Ok res.Value
        | false -> Error res.Error