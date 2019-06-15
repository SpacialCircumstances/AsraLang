module Asserts

open Xunit
open Ast
open Types

let assertError = fun res -> Assert.True (match res with
                                            | Ok _ -> false
                                            | Error _ -> true)

let assertGenericContext (ctx: Context) (key: string) (expected: AType) =
    match resolveGeneric key ctx with
        | None -> Assert.True(false)
        | Some g -> Assert.Equal(expected, g)

let assertGenericContextEmpty (ctx: Context) (key: string) = Assert.False (Map.containsKey key ctx.resolvedGenerics)

let assertEqResult (expected: 'a) (got: Result<'a, 'b>) =
    let gotR = match got with
                    | Error e -> 
                        Assert.True(false, e.ToString())
                        invalidOp "Unreachable"
                    | Ok g -> g
    Assert.Equal<'a>(expected, gotR)

let astMatch (expected: UntypedTestExpression seq) (got: UntypedTestExpression) =
    match got with
        | BlockExpression bl ->
            let got = bl.body
            if Seq.length expected <> Seq.length got then
                Assert.True (false, sprintf "Expected length: %i, but got: %i" (Seq.length expected) (Seq.length got))
            else
                Seq.iter2 (fun (e: UntypedTestExpression) (g: UntypedTestExpression) -> Assert.Equal(e, g)) expected got
        | _ -> invalidOp "Parser needs to return one top-level block"

let isOk (result: Result<'a, 'b>) (f: 'a -> unit) = 
    let ok = match result with
                    | Ok _ -> true
                    | Error _ -> false
    Assert.True (ok, "Expected: Ok, got: Error")
    match result with
        | Ok a -> f a
        | Error e -> () 