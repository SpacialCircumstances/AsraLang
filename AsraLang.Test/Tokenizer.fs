module Tokenizer

open System
open Xunit
open Tokenizer

[<Fact>]
let ``Basic Tokenization`` () =
    let input = " .   . .  .."
    let tokens = tokenize input |> Seq.toList
    printfn "%A" tokens
    ()
