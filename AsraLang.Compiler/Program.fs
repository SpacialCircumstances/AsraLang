open System
open System.IO
open Parser
open Typechecker
open JsGenerator
open Types

[<EntryPoint>]
let main argv =
    let inFile = argv.[0]
    let outFile = argv.[1]
    let ast = File.ReadAllText inFile |> Parser.defaultParser

    match ast with
        | Error e -> printfn "%A" e
        | Ok ast ->
            
            let typedAst, _ = typecheck ast JsLibrary.externs
            let jsGen = genState (File.ReadAllText Config.currentConfig.preludePath) JsLibrary.externs
            let generatedJs = generateJs jsGen typedAst
            if (File.Exists outFile) then File.Delete outFile
            File.WriteAllText (outFile, generatedJs)
            ()
    0
