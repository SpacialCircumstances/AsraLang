module Generics

open Types
open Xunit

let assertError = fun res -> Assert.True (match res with
                                            | Ok _ -> false
                                            | Error _ -> true)

let assertEqResult (expected: 'a) (got: Result<'a, 'b>) =
    let gotR = match got with
                    | Error e -> 
                        Assert.True(false, e.ToString())
                        invalidOp "Unreachable"
                    | Ok g -> g
    Assert.Equal<'a>(expected, gotR)

[<Fact>]
let ``Generic 1`` () =
    let ft = genFunType [ astring; astring ] afloat
    let pt = genFunType [ Generic "a" ] astring
    let res = genericUnification ft pt
    assertError res

let ``Generic 2`` () =
    let ft = genFunType [ Generic "a"; astring ] afloat
    let pt = genFunType [ astring ] astring
    let res = genericUnification ft pt
    assertEqResult afloat res