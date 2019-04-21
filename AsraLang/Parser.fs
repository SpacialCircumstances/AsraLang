module Parser

open Pidgin
open Token
open System

let token (f: Token -> bool) = Parser<Token>.Token(Func<Token, bool>(f))

let map (p: Parser<Token, Token>) (f: Token -> 'a) = p.Select(Func<Token, 'a>(f))

let (<!>) = map

let parseStringLiteral = token (fun t ->
                                    match t.token with
                                        | StringLiteral _ -> true
                                        | _ -> false) <!> (fun l -> 
                                            match l.token with
                                                | StringLiteral s -> ParseTree.StringLiteral s
                                                | _ -> invalidOp "Error")