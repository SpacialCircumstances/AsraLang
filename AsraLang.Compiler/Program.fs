open System
open System.IO
open Parser
open Typechecker
open JsGenerator

[<EntryPoint>]
let main argv =
    let inFile = argv.[0]
    let outFile = argv.[1]
    let ast = File.ReadAllText inFile |> parse

    match ast with
        | Error e -> printfn "%A" e
        | Ok ast ->
            let externs = []
            let typedAst, _ = typecheck ast externs
            let jsGen = genState "" externs
            let generatedJs = generateJs jsGen typedAst
            if (File.Exists outFile) then File.Delete outFile
            File.WriteAllText (outFile, generatedJs)
            ()
    0
