module Parser

open Xunit
open Ast

let astMatch (expected: UntypedExpression seq) (got: UntypedExpression) =
    match got with
        | BlockExpression bl ->
            let got = bl.body
            if Seq.length expected <> Seq.length got then
                Assert.True (false, sprintf "Expected length: %i, but got: %i" (Seq.length expected) (Seq.length got))
            else
                Seq.iter2 (fun (e: UntypedExpression) (g: UntypedExpression) -> Assert.Equal(e, g)) expected got
        | _ -> invalidOp "Parser needs to return one top-level block"

let isOk (result: Result<'a, 'b>) (f: 'a -> unit) = 
    let ok = match result with
                    | Ok _ -> true
                    | Error _ -> false
    Assert.True (ok, "Expected: Ok, got: Error")
    match result with
        | Ok a -> f a
        | Error e -> () 

[<Fact>]
let ``Parse literals`` () =
    let input = """12; 234.5; "test"; 123.4"""
    let parsed = Parser.defaultParser input
    let expected = [
        LiteralExpression ({ literalValue = LiteralValue.Int 12L; data = () })
        LiteralExpression ({ literalValue = LiteralValue.Float 234.5; data = () })
        LiteralExpression ({ literalValue = LiteralValue.String "test"; data = () })
        LiteralExpression ({ literalValue = LiteralValue.Float 123.4; data = () })
    ]
    isOk parsed (fun result -> astMatch expected result)

[<Fact>]
let ``Parse literals and variables`` () =
    let input = """"test"; test; 123; t123"""
    let parsed = Parser.defaultParser input
    let expected = [
        LiteralExpression { data = (); literalValue = LiteralValue.String "test" }
        VariableExpression ("test", ())
        LiteralExpression { data = (); literalValue = LiteralValue.Int 123L }
        VariableExpression ("t123", ())
    ]
    isOk parsed (fun result -> astMatch expected result)

[<Fact>]
let ``Parse groups`` () =
    let input = """(123); ("test"); (foo); ((42.6))"""
    let parsed = Parser.defaultParser input
    let expected = [
        GroupExpression (LiteralExpression { data = (); literalValue = LiteralValue.Int 123L })
        GroupExpression (LiteralExpression { data = (); literalValue = LiteralValue.String "test" })
        GroupExpression (VariableExpression ("foo", ()))
        GroupExpression (GroupExpression (LiteralExpression { data = (); literalValue = LiteralValue.Float 42.6 }))
    ]
    isOk parsed (fun result -> astMatch expected result)

[<Fact>]
let ``Parse variable definitions`` () =
    let input = """
    test = 42
    fooBar = "test" 
    """
    let parsed = Parser.defaultParser input
    let expected = [
        VariableBindingExpression { varName = Simple "test"; value = (LiteralExpression { data = (); literalValue = LiteralValue.Int 42L }); varData = () }
        VariableBindingExpression { varName = Simple "fooBar"; value = (LiteralExpression { data = (); literalValue = LiteralValue.String "test" }); varData = () }
    ]
    isOk parsed (fun result -> astMatch expected result)

[<Fact>]
let ``Parse block expressions`` () =
    let input = """
    testBlock = [
        "asdf"
        123
        a = 233
        [
            456
        ]
    ]
    """    
    let parsed = Parser.defaultParser input
    let block = BlockExpression { parameters = []; data = (); body = [
        LiteralExpression { data = (); literalValue = LiteralValue.String "asdf" }
        LiteralExpression { data = (); literalValue = LiteralValue.Int 123L }
        VariableBindingExpression { varName = Simple "a"; varData = (); value = LiteralExpression { data = (); literalValue = LiteralValue.Int 233L } }
        BlockExpression { parameters = []; data = (); body = [
            LiteralExpression { data = (); literalValue = LiteralValue.Int 456L }
        ]}
    ]}
    let expected = [ VariableBindingExpression { varName = Simple "testBlock"; varData = (); value = block } ]
    isOk parsed (fun result -> astMatch expected result)
    
[<Fact>]
let ``Parse function calls`` () = 
    let input = """
    + 2 2
    [ test ] [ 324; "asdf" ]
    (test 23 234.3) 12.1 [ x ]
    """    
    let parsed = Parser.defaultParser input
    let expected = [ 
        FunctionCallExpression { func = VariableExpression ("+", ()); data = (); args = [
            LiteralExpression { data = (); literalValue = Int 2L }
            LiteralExpression { data = (); literalValue = Int 2L }
        ]}
        FunctionCallExpression { data = (); func = BlockExpression { data = (); parameters = []; body = [
            VariableExpression ("test", ())
        ]};
        args = [
                    BlockExpression { 
                        parameters = [];
                        data = ();
                        body = [
                            LiteralExpression { data = (); literalValue = Int 324L }
                            LiteralExpression { data = (); literalValue = String "asdf" }
                        ]}   
        ]}
        FunctionCallExpression {
            data = ();
            func = GroupExpression (FunctionCallExpression { 
                data = ();
                func = VariableExpression ("test", ()); 
                args = [
                    LiteralExpression { data = (); literalValue = Int 23L }
                    LiteralExpression { data = (); literalValue = Float 234.3 }
                ]});
            args = [
                LiteralExpression { data = (); literalValue = Float 12.1 }
                BlockExpression { parameters = []; data = (); body = [ VariableExpression ("x", ()) ]}
            
        ]}
    ]
    isOk parsed (fun result -> astMatch expected result)

[<Fact>]
let ``Type annotated declarations`` () =
    let input = """
    t1: int = 3
    t2: string = "test"
    t5: Foo = 42.2
    """    
    let parsed = Parser.defaultParser input
    let expected = [ 
        VariableBindingExpression { 
            varData = ();
            varName = Annotated { varName = "t1"; typeName = "int" };
            value = LiteralExpression { data = (); literalValue = Int 3L }
        }
        VariableBindingExpression {
            varData = ();
            varName = Annotated { varName = "t2"; typeName = "string" };
            value = LiteralExpression { data = (); literalValue = String "test" }
        }
        VariableBindingExpression {
            varData = ();
            varName = Annotated { varName = "t5"; typeName = "Foo" };
            value = LiteralExpression { data = (); literalValue = Float 42.2 }
        }
    ]
    isOk parsed (fun result -> astMatch expected result)

[<Fact>]
let ``Block with identifier`` () =
    let input = "[ test ]"
    let parsed = Parser.defaultParser input
    let expected = [
        BlockExpression {
            parameters = []
            data = ()
            body = [
                VariableExpression ("test", ())
            ]
        }
    ]
    isOk parsed (fun result -> astMatch expected result)

[<Fact>]
let ``Block with parameters`` () =
    let input = """
    block = [ a: Int, b: Int ->
        + a b
    ]
    [ x: String -> "test" ]
    """
    let parsed = Parser.defaultParser input
    let expected = [
        VariableBindingExpression {
            varName = Simple "block"
            varData = ()
            value = BlockExpression {
                data = ()
                parameters = [
                    Annotated { varName = "a"; typeName = "Int" }
                    Annotated { varName = "b"; typeName = "Int" }
                ]
                body = [
                    FunctionCallExpression {
                        data = ()
                        func = VariableExpression ("+", ())
                        args = [
                            VariableExpression ("a", ())
                            VariableExpression ("b", ())
                        ]
                    }
                ]
            }
        }
        BlockExpression {
            data = ()
            parameters = [
                Annotated { varName = "x"; typeName = "String" }
            ]
            body = [
                LiteralExpression { data = (); literalValue = String "test" }
            ]
        }
        
    ]
    isOk parsed (fun result -> astMatch expected result)

[<Fact>]
let ``Parsing with comments`` () =
    let input = """
    #asdf
    test = 42 # foo bar 42; single line comment
    fooBar = "test" 
    """
    let parsed = Parser.defaultParser input
    let expected = [
        VariableBindingExpression { varData = (); varName = Simple "test"; value = (LiteralExpression { literalValue = Int 42L; data = () }) }
        VariableBindingExpression { varData = (); varName = Simple "fooBar"; value = (LiteralExpression { literalValue = String "test"; data = () }) }
    ]
    isOk parsed (fun result -> astMatch expected result)