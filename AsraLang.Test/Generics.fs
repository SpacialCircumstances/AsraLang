module Generics

open Types
open Xunit
open Asserts

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

let ``Generic 3`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = genFunType [ astring ] (genFunType [ Generic "a"; Generic "a" ] abool)
    let res = genericUnification ft pt
    assertEqResult astring res

let ``Generic 4`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = genFunType [ astring ] (genFunType [ astring; astring ] abool)
    let res = genericUnification ft pt
    assertEqResult astring res
