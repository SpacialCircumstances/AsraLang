module Types

open Ast
open Types
open Xunit
open Asserts

[<Fact>]
let ``Call types`` () =
    let funcT = FunctionType {
        input = anumber;
        output = FunctionType {
            input = anumber;
            output = FunctionType {
                input = anumber;
                output = astring;
            }
        }
    }
    let p1 = [ anumber; anumber ]
    let p2 = [ anumber; anumber; anumber ]
    let p3 = [ anumber; anumber; astring ]
    let pr1 = FunctionType {
        input = anumber;
        output = astring;
    }
    assertEqResult pr1 (returnType funcT p1 |> fst)
    assertEqResult astring (returnType funcT p2 |> fst)
    Assert.True(match returnType funcT p3 |> fst with
                | Error _ -> true
                | Ok _ -> false)

[<Fact>]
let ``Block types`` () =
    let p1 = [
        astring
        FunctionType {
            input = anumber;
            output = astring;
        }
        anumber
    ]
    let bt = genFunType p1 anumber
    let expected = FunctionType {
        input = astring;
        output = FunctionType {
            input = FunctionType {
                input = anumber;
                output = astring;
            }
            output = FunctionType {
                input = anumber;
                output = anumber;
            }
        }
    }
    Assert.Equal(expected, bt)

[<Fact>]
let ``Type printing`` () =
    let t1 = astring
    let t2 = aunit
    let t3 = anumber
    let t4 = Generic "a"
    Assert.Equal("String", t1.ToString())
    Assert.Equal("Unit", t2.ToString())
    Assert.Equal("Number", t3.ToString())
    Assert.Equal("'a", t4.ToString())
    let ft1 = FunctionType {
        input = t1;
        output = t3;
    }
    Assert.Equal("String -> Number", ft1.ToString())
    let ft2 = FunctionType {
        input = t2;
        output = t3;
    }
    Assert.Equal("Unit -> Number", ft2.ToString())
    let ft3 = FunctionType {
        input = t1;
        output = ft2;
    }
    Assert.Equal("String -> Unit -> Number", ft3.ToString())
    let ft4 = FunctionType {
        input = ft1;
        output = t2;
    }
    Assert.Equal("(String -> Number) -> Unit", ft4.ToString())
    let ft5 = FunctionType {
        input = ft1
        output = t4
    }
    Assert.Equal("(String -> Number) -> 'a", ft5.ToString())
    ()

[<Fact>]
let ``Apply by arg count`` () =
    let ft = FunctionType {
        input = astring;
        output = FunctionType {
            input = FunctionType {
                input = anumber;
                output = astring;
            }
            output = FunctionType {
                input = anumber;
                output = anumber;
            }
        }
    }
    let e1 = FunctionType {
        input = FunctionType {
            input = anumber;
            output = astring;
        }
        output = FunctionType {
            input = anumber;
            output = anumber } } |> Some

    let e2 = FunctionType {
        input = anumber
        output = anumber } |> Some
    let e3 = anumber |> Some
    let e4 = None
    Assert.Equal(e1, appliedType ft 1)
    Assert.Equal(e2, appliedType ft 2)
    Assert.Equal(e3, appliedType ft 3)
    Assert.Equal(e4, appliedType ft 4)