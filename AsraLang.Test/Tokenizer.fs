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
    let input = "123 1 2.4 29.0 658.34 7777 12. -1 -3.0 -4."
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
        IntLiteral -1L
        FloatLiteral -3.0
        IntLiteral -4L
        Dot
    ]
    Assert.True (tokensMatch expected tokens)

[<Fact>]
let ``String literals`` () =
    let input = """ Test "Test" "" "123" """
    let tokens = tokenize input
    let expected = [
        Identifier "Test"
        StringLiteral "Test"
        StringLiteral ""
        StringLiteral "123"
    ]
    Assert.True (tokensMatch expected tokens)

[<Fact>]
let ``Identifiers`` () =
    let input = "abc ++ (test) (vvb == %) ,asdf. a->b"
    let tokens = tokenize input
    let expected = [
        Identifier "abc"
        Identifier "++"
        LeftParen
        Identifier "test"
        RightParen
        LeftParen
        Identifier "vvb"
        Identifier "=="
        Identifier "%"
        RightParen
        Comma
        Identifier "asdf"
        Dot
        Identifier "a"
        Arrow
        Identifier "b"
    ]
    Assert.True (tokensMatch expected tokens)

[<Fact>]
let ``Hello world`` () =
    let input = """"Hello World" printLn"""
    let tokens = tokenize input
    let expected = [
        StringLiteral "Hello World"
        Identifier "printLn"
    ]
    Assert.True (tokensMatch expected tokens)

[<Fact>]
let ``Math`` () =
    let input = "(2 + 2) / ((12.0 / 3.0) toInt)"
    let tokens = tokenize input
    let expected = [
        LeftParen
        IntLiteral 2L
        Identifier "+"
        IntLiteral 2L
        RightParen
        Identifier "/"
        LeftParen
        LeftParen
        FloatLiteral 12.0
        Identifier "/"
        FloatLiteral 3.0
        RightParen
        Identifier "toInt"
        RightParen
    ]
    Assert.True (tokensMatch expected tokens)
