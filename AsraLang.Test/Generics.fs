module Generics

open Types
open Xunit
open Asserts

[<Fact>]
let ``Generic 1`` () =
    let ft = genFunType [ astring; astring ] afloat
    let pt = [ Generic "a"; astring ]
    let (res, ctx) = returnType ft pt
    assertError res

[<Fact>]
let ``Generic 2`` () =
    let ft = genFunType [ Generic "a"; astring ] afloat
    let pt = [ astring; astring ]
    let (res, ctx) = returnType ft pt
    assertEqResult afloat res
    assertGenericContext ctx "a" astring

[<Fact>]
let ``Generic 3`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = [ astring; (genFunType [ Generic "a"; Generic "a" ] abool) ]
    let (res, ctx) = returnType ft pt
    assertEqResult astring res
    assertGenericContextEmpty ctx "a"

[<Fact>]
let ``Generic 4`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = [ astring; (genFunType [ astring; astring ] abool) ]
    let (res, ctx) = returnType ft pt
    assertEqResult astring res
    assertGenericContext ctx "a" astring
    
[<Fact>]
let ``Generic 5`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = [ astring; (genFunType [ Generic "b"; astring ] abool) ]
    let (res, ctx) = returnType ft pt
    assertEqResult astring res
    assertGenericContext ctx "a" astring

[<Fact>]
let ``Generic 6`` () =
    let ft = genFunType [ 
        astring
        (genFunType [ Generic "a"; Generic "a" ] abool) ] astring
    let pt = [ astring; (genFunType [ aint; astring ] abool) ]
    let (res, ctx) = returnType ft pt
    assertError res

[<Fact>]
let ``Generic 7`` () =
    let ft = genFunType [ Generic "a"; Generic "a" ] abool
    let pt = [ astring; (Generic "b") ]
    let (res, ctx) = returnType ft pt
    assertError res

[<Fact>]
let ``Generic 8`` () =
    let p = genFunType [ astring ] (Generic "a")
    let ft = genFunType [ p; p ] abool
    let pt = [
        genFunType [ astring ] astring
        genFunType [ astring ] (Generic "b") ]
    let (res, ctx) = returnType ft pt
    assertEqResult abool res
    assertGenericContext ctx "a" astring

[<Fact>]
let ``Generic 9`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let p1 = genFunType [ aunit ] astring
    let pt = [ abool; p1; p1 ]
    let (res, ctx) = returnType ft pt
    assertEqResult astring res
    assertGenericContext ctx "a" astring

[<Fact>]
let ``Generic 10`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let pt = [ 
        abool
        (genFunType [ aunit ] (Generic "b"))
        (genFunType [ aunit ] astring) ]
    let (res, ctx) = returnType ft pt
    assertEqResult astring res
    assertGenericContext ctx "a" astring

[<Fact>]
let ``Generic 11`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let pt = [
        abool
        genFunType [ aunit ] (Generic "b")
        (genFunType [ aunit ] (Generic "b")) ]
    let (res, ctx) = returnType ft pt
    assertEqResult (Generic "b") res
    assertGenericContext ctx "a" (Generic "b") //Can fail

[<Fact>]
let ``Generic 12`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let pt = [ 
        abool
        (genFunType [ aunit ] astring)
        (genFunType [ aunit ] aint) ]
    let (res, ctx) = returnType ft pt
    assertError res

[<Fact>]
let ``Generic 13`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "b") ] (Generic "a")
    let pt = [ 
        abool
        (genFunType [ aunit ] astring)
        (genFunType [ aunit ] aint) ]
    let (res, ctx) = returnType ft pt
    assertEqResult astring res
    assertGenericContext ctx "a" astring
    assertGenericContext ctx "b" aint

[<Fact>]
let ``Generic 14`` () =
    let ft = genFunType [
        abool
        genFunType [ aunit ] (Generic "a")
        genFunType [ aunit ] (Generic "a") ] (Generic "a")
    let pt = [ abool; (genFunType [ aunit ] astring) ]
    let exp = genFunType [ (genFunType [ aunit ] astring) ] astring
    let (res, ctx) = returnType ft pt
    assertEqResult exp res
    assertGenericContext ctx "a" astring

[<Fact>]
let ``Generic 15`` () =
    let p = genFunType [ astring ] (Generic "a")
    let ft = genFunType [ p; p ] abool
    let pt = [
        genFunType [ astring ] (Generic "b")
        genFunType [ astring ] astring ]
    let (res, ctx) = returnType ft pt
    assertEqResult abool res
    assertGenericContext ctx "a" astring