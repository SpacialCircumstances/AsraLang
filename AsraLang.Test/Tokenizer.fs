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
    let input = ". : ( ) [ ] - > + / *"
    let tokens = tokenize input
    let expected = [
        Dot
        Colon
        LeftParen
        RightParen
        BlockOpen
        BlockClose
        Identifier "-"
        Identifier ">"
        Identifier "+"
        Identifier "/"
        Identifier "*"
    ]
    Assert.True (tokensMatch expected tokens)

[<Fact>]
let ``Number literals`` () =
    let input = "123 1 2.4 29.0 658.34 7777 12."
    let tokens = tokenize input
    let expected = [
        IntLiteral 123L
        IntLiteral 1L
        FloatLiteral 2.4 
        FloatLiteral 29.0
        FloatLiteral 658.34
        IntLiteral 7777L
        IntLiteral 12L
        Dot
    ]
    Assert.True (tokensMatch expected tokens)
