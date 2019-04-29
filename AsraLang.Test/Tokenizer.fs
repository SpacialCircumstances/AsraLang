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
    let input = ": ( ) [ ] - > + / * ."
    let tokens = tokenizer input
    let expected = [
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
        Identifier "."
    ]
    Assert.True (tokensMatch expected tokens)

[<Fact>]
let ``Number literals`` () =
    let input = "123 1 2.4 29.0 658.34 7777 12; -1 -3. -4;"
    let tokens = tokenizer input
    let expected = [
        IntLiteral 123L
        IntLiteral 1L
        FloatLiteral 2.4 
        FloatLiteral 29.0
        FloatLiteral 658.34
        IntLiteral 7777L
        IntLiteral 12L
        Separator
        IntLiteral -1L
        FloatLiteral -3.0
        IntLiteral -4L
        Separator
    ]
    Assert.True (tokensMatch expected tokens)

[<Fact>]
let ``String literals`` () =
    let input = """ Test "Test" "" "123" """
    let tokens = tokenizer input
    let expected = [
        Identifier "Test"
        StringLiteral "Test"
        StringLiteral ""
        StringLiteral "123"
    ]
    Assert.True (tokensMatch expected tokens)

[<Fact>]
let ``Identifiers`` () =
    let input = "abc ++ (test) (vvb == %) ,asdf; a -> b"
    let tokens = tokenizer input
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
        Separator
        Identifier "a"
        Arrow
        Identifier "b"
    ]
    Assert.True (tokensMatch expected tokens)

[<Fact>]
let ``Code with blocks`` () =
    let input = """
    z = [ Int a, String b, foo c ->
        x = + a b
        * x -2
    ]"""
    let tokens = tokenizer input
    let expected = [
        Identifier "z"
        BlockOpen
        Identifier "Int"
        Identifier "a"
        Comma
        Identifier "String"
        Identifier "b"
        Comma
        Identifier "foo"
        Identifier "c"
        Arrow
        Separator
        Identifier "x"
        Equal
        Identifier "+"
        Identifier "a"
        Identifier "b"
        Separator
        Identifier "*"
        Identifier "x"
        IntLiteral -2L
        Separator
        BlockClose
    ]
    Assert.True (tokensMatch expected tokens)