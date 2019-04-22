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