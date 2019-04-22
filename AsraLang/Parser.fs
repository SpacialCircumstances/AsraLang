module Parser

open Pidgin
open Token
open System

let token (f: Token -> 'a option) = //TODO: Make more efficient, do not run f twice
    Parser<Token>.Token(Func<Token, bool>(fun t -> match f t with
                                                        | None -> false
                                                        | Some _ -> true)).Select(Func<Token, 'a>(fun t -> match f t with
                                                                                                            | Some a -> a
                                                                                                            | None -> invalidOp "Can never happen"))

let choice (parsers: Parser<Token, 'a> seq) = Parser.OneOf parsers

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

let parseLiteral = choice [
    parseStringLiteral
    parseFloatLiteral
    parseIntLiteral
]