module Types

open Ast
open Types
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
    let funcT = FunctionType {
        input = aint;
        output = FunctionType {
            input = aint;
            output = FunctionType {
                input = aint;
                output = astring;
            }
        }
    }
    let p1 = [ aint; aint ]
    let p2 = [ aint; aint; aint ]
    let p3 = [ aint; afloat; astring ]
    let pr1 = FunctionType {
        input = aint;
        output = astring;
    }
    assertEqResult pr1 (returnType funcT p1)
    assertEqResult astring (returnType funcT p2)
    Assert.True(match returnType funcT p3 with
                | Error _ -> true
                | Ok _ -> false)

[<Fact>]
let ``Block types`` () =
    let p1 = [
        astring
        FunctionType {
            input = aint;
            output = astring;
        }
        aint
    ]
    let bt = genFunType p1 afloat
    let expected = FunctionType {
        input = astring;
        output = FunctionType {
            input = FunctionType {
                input = aint;
                output = astring;
            }
            output = FunctionType {
                input = aint;
                output = afloat;
            }
        }
    }
    Assert.Equal(expected, bt)

[<Fact>]
let ``Type printing`` () =
    let t1 = astring
    let t2 = aunit
    let t3 = aint
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