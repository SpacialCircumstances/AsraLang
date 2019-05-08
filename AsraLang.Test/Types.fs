module Types

open TypedAST
open Xunit

let assertEqResult (expected: 'a) (got: Result<'a, 'b>) =
    let gotR = match got with
                    | Error e -> 
                        Assert.True(false, e.ToString())
                        invalidOp "Unreachable"
                    | Ok g -> g
    Assert.Equal<'a>(expected, gotR)

[<Fact>]
let ``Call types`` () =
    let i = Native "Int"
    let f = Native "Float"
    let s = Native "String"
    let funcT = FunctionType {
        input = i;
        output = FunctionType {
            input = i;
            output = FunctionType {
                input = i;
                output = s;
            }
        }
    }
    let p1 = [ i; i ]
    let p2 = [ i; i; i ]
    let p3 = [ i; f; s ]
    let pr1 = FunctionType {
        input = i;
        output = s;
    }
    assertEqResult pr1 (returnType funcT p1)
    assertEqResult s (returnType funcT p2)
    Assert.True(match returnType funcT p3 with
                | Error _ -> true
                | Ok _ -> false)

[<Fact>]
let ``Block types`` () =
    let i = Native "Int"
    let s = Native "String"
    let f = Native "Float"
    let p1 = [
        s
        FunctionType {
            input = i;
            output = s;
        }
        i
    ]
    let bt = genFunType p1 f
    let expected = FunctionType {
        input = s;
        output = FunctionType {
            input = FunctionType {
                input = i;
                output = s;
            }
            output = FunctionType {
                input = i;
                output = f;
            }
        }
    }
    Assert.Equal(expected, bt)

[<Fact>]
let ``Type printing`` () =
    let t1 = Native "String"
    let t2 = Native "Unit"
    let t3 = Native "Int"
    Assert.Equal("String", t1.ToString())
    Assert.Equal("Unit", t2.ToString())
    Assert.Equal("Int", t3.ToString())
    let ft1 = FunctionType {
        input = t1;
        output = t3;
    }
    Assert.Equal("String -> Int", ft1.ToString())
    let ft2 = FunctionType {
        input = t2;
        output = t3;
    }
    Assert.Equal("Unit -> Int", ft2.ToString())
    let ft3 = FunctionType {
        input = t1;
        output = ft2;
    }
    Assert.Equal("String -> Unit -> Int", ft3.ToString())
    let ft4 = FunctionType {
        input = ft1;
        output = t2;
    }
    Assert.Equal("(String -> Int) -> Unit", ft4.ToString())
    ()