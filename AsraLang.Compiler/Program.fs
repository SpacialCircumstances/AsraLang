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
    let ast = File.ReadAllText inFile |> (Parser.defaultParser)

    match ast with
        | Error e -> printfn "%A" e
        | Ok ast ->
            let externs = [
                { asraName = "println"; asraType = genFunType [ Native "String" ] (Native "Unit"); externName = "println" }
                { asraName = "+"; asraType = genFunType [ Native "Int"; Native "Int" ] (Native "Int"); externName = "add" }
                { asraName = "-"; asraType = genFunType [ Native "Int"; Native "Int" ] (Native "Int"); externName = "subtract" }
                { asraName = "*"; asraType = genFunType [ Native "Int"; Native "Int" ] (Native "Int"); externName = "multiply" }
                { asraName = "/"; asraType = genFunType [ Native "Int"; Native "Int" ] (Native "Int"); externName = "divide" }
                { asraName = "if"; asraType = genFunType [
                    genFunType [ Native "Unit" ] (Native "Bool")
                    genFunType [ Native "Unit" ] (Native "Unit")
                    genFunType [ Native "Unit" ] (Native "Unit")
                ] (Native "Unit"); externName = "iffn" }
                { asraName = "=="; asraType = genFunType [ Native "Int"; Native "Int" ] (Native "Bool"); externName = "eq" }
                { asraName = "!="; asraType = genFunType [ Native "Int"; Native "Int" ] (Native "Bool"); externName = "neq" }
                { asraName = "not"; asraType = genFunType [ Native "Bool" ] (Native "Bool"); externName = "not" }
                { asraName = "&"; asraType = genFunType [ Native "Bool"; Native "Bool" ] (Native "Bool"); externName = "and" }
                { asraName = "|"; asraType = genFunType [ Native "Bool"; Native "Bool" ] (Native "Bool"); externName = "or" }
            ]
            let typedAst, _ = typecheck ast externs
            let jsGen = genState (File.ReadAllText Config.currentConfig.preludePath) externs
            let generatedJs = generateJs jsGen typedAst
            if (File.Exists outFile) then File.Delete outFile
            File.WriteAllText (outFile, generatedJs)
            ()
    0
