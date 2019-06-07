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