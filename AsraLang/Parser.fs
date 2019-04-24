module Parser

open Pidgin
open Token
open System

let mutable parseValueExpression = Unchecked.defaultof<Parser<Token, ParseTree.Expression>>

let mutable parseExpression = Unchecked.defaultof<Parser<Token, ParseTree.Expression>>

let mutable parsePrimitiveExpression = Unchecked.defaultof<Parser<Token, ParseTree.Expression>>

let token (f: Token -> 'a option) = //TODO: Make more efficient, do not run f twice
    Parser<Token>.Token(Func<Token, bool>(fun t -> match f t with
                                                        | None -> false
                                                        | Some _ -> true)).Select(Func<Token, 'a>(fun t -> match f t with
                                                                                                            | Some a -> a
                                                                                                            | None -> invalidOp "Can never happen"))

let choice (parsers: Parser<Token, 'a> seq) = Parser.OneOf parsers

let map (parser: Parser<Token, 'a>) (mapper: 'a -> 'b) = parser.Select(Func<'a, 'b>(mapper))

let (<!>) = map

let label (lbl: string) (parser: Parser<Token, 'a>) = parser.Labelled lbl

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

let parseLiteralExpression = choice [ parseStringLiteral; parseFloatLiteral; parseIntLiteral ] <!> ParseTree.LiteralExpression |> label "Literal"

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

let parseVariableDefinitionExpression = Parser.Try (map2 (parseDeclaration.Before(parseEqual)) (prec (fun () -> parsePrimitiveExpression)) (fun d e -> ParseTree.DefineVariableExpression { variableName = d; value = e })) |> label "Variable definition"

let parseDot = token (fun t ->
                            match t.token with
                                | Dot -> Some ()
                                | _ -> None)

let parseBlockOpen = token (fun t -> match t.token with
                                        | BlockOpen -> Some ()
                                        | _ -> None)

let parseBlockClose = token (fun t -> match t.token with
                                        | BlockClose -> Some ()
                                        | _ -> None)

let parseExpressions = sepBy (prec (fun () -> parseExpression)) parseDot

let parseBlock = parseExpressions.Between(parseBlockOpen, parseBlockClose) <!> (fun expressions -> ParseTree.BlockExpression { parameters = None; body = List.ofSeq expressions })

let pve = prec (fun () -> parsePrimitiveExpression)

//TODO: Rethink parser order so we do not have to backtrack on fun calls

let parseFunCall = map2 pve (pve.AtLeastOnce()) (fun first exprs -> ParseTree.FunctionCallExpression { func = first; arguments = List.ofSeq exprs }) |> Parser.Try

parsePrimitiveExpression <- choice [
    parseLiteralExpression
    parseVariableExpression
    parseGroupExpression
    parseBlock
]

parseValueExpression <- choice [
    parseFunCall
    parsePrimitiveExpression
]

parseExpression <- choice [ 
    parseVariableDefinitionExpression
    parseValueExpression
]


let programParser = parseExpressions

let parse (tokens: Token seq) = 
    let res = programParser.Parse(tokens)
    match res.Success with
        | true -> Ok res.Value
        | false -> Error res.Error