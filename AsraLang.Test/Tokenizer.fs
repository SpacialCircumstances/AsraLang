module Tokenizer

open System
open Xunit
open Tokenizer
open Token

let tokensMatch (expected: TokenType seq) (got: Token seq) =
    if Seq.length expected <> Seq.length got then
        false
    else
        Seq.zip expected got |> Seq.forall (fun (e, gt) -> e = gt.token)

[<Fact>]
let ``Single char tokens`` () =
    let input = ". : () [ ][] - > + / *"
    let tokens = tokenize input
    let expected = [
        Dot
        Colon
        BlockOpen
        BlockClose
        BlockOpen
        BlockClose
        Identifier("-")
        Identifier(">")
        Identifier("+")
        Identifier("/")
        Identifier("*")
    ]
    Assert.True (tokensMatch expected tokens)
