open System
open System.IO
open Parser
open Typechecker

[<EntryPoint>]
let main argv =
    let inFile = argv.[0]
    let outFile = argv.[1]
    let ast = File.ReadAllText inFile |> parse

    match ast with
        | Error e -> printfn "%A" e
        | Ok ast ->
            let typedAst, _ = typecheck ast
            ()
    0
