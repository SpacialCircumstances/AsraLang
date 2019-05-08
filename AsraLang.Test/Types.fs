module Types

open TypedAST
open Xunit

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