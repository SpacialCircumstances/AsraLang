module Generics

open Types
open Xunit
open Asserts

[<Fact>]
let ``Generic 1`` () =
    let ft = genFunType [ astring; astring ] afloat
    let pt = genFunType [ Generic "a" ] astring
    let (res, ctx) = genericUnification ft pt
    assertError res

let ``Generic 2`` () =
    let ft = genFunType [ Generic "a"; astring ] afloat
    let pt = genFunType [ astring ] astring
    let (res, ctx) = genericUnification ft pt
    assertEqResult afloat res

let ``Generic 3`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = genFunType [ astring ] (genFunType [ Generic "a"; Generic "a" ] abool)
    let (res, ctx) = genericUnification ft pt
    assertEqResult astring res

let ``Generic 4`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = genFunType [ astring ] (genFunType [ astring; astring ] abool)
    let (res, ctx) = genericUnification ft pt
    assertEqResult astring res

let ``Generic 5`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = genFunType [ astring ] (genFunType [ Generic "b"; astring ] abool)
    let (res, ctx) = genericUnification ft pt
    assertEqResult astring res

let ``Generic 6`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = genFunType [ astring ] (genFunType [ aint; astring ] abool)
    let (res, ctx) = genericUnification ft pt
    assertError res

let ``Generic 7`` () =
    let ft = genFunType [ Generic "a"; Generic "a" ] abool
    let pt = genFunType [ astring ] (Generic "b")
    let (res, ctx) = genericUnification ft pt
    assertError res

let ``Generic 8`` () =
    let p = genFunType [ astring ] (Generic "a")
    let ft = genFunType [ p; p ] abool
    let pt = genFunType [
        genFunType [ astring ] astring ] (genFunType [ astring ] (Generic "b"))
    let (res, ctx) = genericUnification ft pt
    assertEqResult abool res

let ``Generic 9`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let p1 = genFunType [ aunit ] astring
    let pt = genFunType [ abool; p1 ] p1
    let (res, ctx) = genericUnification ft pt
    assertEqResult astring res

let ``Generic 10`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let pt = genFunType [ 
        abool
        (genFunType [ aunit ] (Generic "b")) ] (genFunType [ aunit ] astring)
    let (res, ctx) = genericUnification ft pt
    assertEqResult astring res

let ``Generic 11`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let pt = genFunType [
        abool
        genFunType [ aunit ] (Generic "b") ] (genFunType [ aunit ] (Generic "b"))
    let (res, ctx) = genericUnification ft pt
    assertEqResult (Generic "b") res

let ``Generic 12`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let pt = genFunType [ 
        abool
        (genFunType [ aunit ] astring) ] (genFunType [ aunit ] aint)
    let (res, ctx) = genericUnification ft pt
    assertError res

let ``Generic 13`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "b") ] (Generic "a")
    let pt = genFunType [ 
        abool
        (genFunType [ aunit ] astring) ] (genFunType [ aunit ] aint)
    let (res, ctx) = genericUnification ft pt
    assertEqResult astring res

let ``Generic 14`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let pt = genFunType [ abool ] (genFunType [ aunit ] astring)
    let exp = genFunType [ (genFunType [ aunit ] astring) ] astring
    let (res, ctx) = genericUnification ft pt
    assertEqResult exp res