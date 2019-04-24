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

[<Fact>]
let ``Parse variable definitions`` () =
    let input = """
    test = 42.
    foo = "test"
    """
    let tokens = Tokenizer.tokenize input
    let parsed = Parser.parse tokens
    let expected = [
        DefineVariableExpression { variableName = Simple "test"; value = (LiteralExpression (IntLiteral 42L)) }
        DefineVariableExpression { variableName = Simple "foo"; value = (LiteralExpression (StringLiteral "test")) }
    ]
    isOk parsed (fun result -> Assert.True (astMatch expected result))

[<Fact>]
let ``Parse block expressions`` () =
    let input = """
    testBlock = [
        "asdf".
        123.
        a = 233.
        [
            456
        ]
    ]
    """    
    let tokens = Tokenizer.tokenize input
    let parsed = Parser.parse tokens
    let block = BlockExpression { parameters = None; body = [
        LiteralExpression (StringLiteral "asdf")
        LiteralExpression (IntLiteral 123L)
        DefineVariableExpression { variableName = Simple "a"; value = LiteralExpression (IntLiteral 233L) }
        BlockExpression { parameters = None; body = [
            LiteralExpression (IntLiteral 456L)
        ]}
    ]}
    let expected = [ DefineVariableExpression { variableName = Simple "testBlock"; value = block } ]
    isOk parsed (fun result -> Assert.True (astMatch expected result))
    
[<Fact>]
let ``Parse function calls`` () = 
    let input = """
    + 2 2.
    [ test ] [ 324. "asdf" ].
    (test 23 234.3) 12.1 [ x ]
    """    
    let tokens = Tokenizer.tokenize input
    let parsed = Parser.parse tokens
    let expected = [ 
        FunctionCallExpression { func = VariableExpression "+"; arguments = [
            LiteralExpression (IntLiteral 2L)
            LiteralExpression (IntLiteral 2L)
        ]}
        FunctionCallExpression { func = BlockExpression { parameters = None; body = [
            VariableExpression "test"
        ]};
        arguments = [
                    BlockExpression { 
                        parameters = None; 
                        body = [
                            LiteralExpression (IntLiteral 324L)
                            LiteralExpression (StringLiteral "asdf")
                        ]}   
        ]}
        FunctionCallExpression {
            func = GroupExpression (FunctionCallExpression { 
            func = VariableExpression "test"; 
            arguments = [
                LiteralExpression (IntLiteral 23L)
                LiteralExpression (FloatLiteral 234.3)
            ]});
            arguments = [
                LiteralExpression (FloatLiteral 12.1)
                BlockExpression { parameters = None; body = [ VariableExpression "x" ]}
            
        ]}
    ]
    isOk parsed (fun result -> Assert.True (astMatch expected result))