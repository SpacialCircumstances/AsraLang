module Parser

open Xunit
open ParseTree

let astMatch (expected: Expression seq) (got: Expression seq) =
    if Seq.length expected <> Seq.length got then
        false
    else
        Seq.zip expected got |> Seq.forall (fun (a, b) -> a = b)

let isOk (result: Result<'a, 'b>) (f: 'a -> unit) = 
    Assert.True(match result with
                    | Ok _ -> true
                    | Error _ -> false)
    match result with
        | Ok a -> f a
        | Error e -> () 

[<Fact>]
let ``Parse literals`` () =
    let input = """12. 234.5. "test". 123.4"""
    let tokens = Tokenizer.tokenize input
    let parsed = Parser.parse tokens
    let expected = [
        LiteralExpression (IntLiteral 12L)
        LiteralExpression (FloatLiteral 234.5)
        LiteralExpression (StringLiteral "test")
        LiteralExpression (FloatLiteral 123.4)
    ]
    isOk parsed (fun result -> Assert.True (astMatch expected result))

[<Fact>]
let ``Parse literals and variables`` () =
    let input = """"test". test. 123. t123"""
    let tokens = Tokenizer.tokenize input
    let parsed = Parser.parse tokens
    let expected = [
        LiteralExpression (StringLiteral "test")
        VariableExpression "test"
        LiteralExpression (IntLiteral 123L)
        VariableExpression "t123"
    ]
    isOk parsed (fun result -> Assert.True (astMatch expected result))

[<Fact>]
let ``Parse groups`` () =
    let input = """(123). ("test"). (foo). ((42.6))"""
    let tokens = Tokenizer.tokenize input
    let parsed = Parser.parse tokens
    let expected = [
        GroupExpression (LiteralExpression (IntLiteral 123L))
        GroupExpression (LiteralExpression (StringLiteral "test"))
        GroupExpression (VariableExpression "foo")
        GroupExpression (GroupExpression (LiteralExpression (FloatLiteral 42.6)))
    ]
    isOk parsed (fun result -> Assert.True (astMatch expected result))