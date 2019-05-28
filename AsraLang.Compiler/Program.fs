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
            let externs = [
                { asraName = "println"; asraType = genFunType [ astring ] (aunit); externName = "println" }
                { asraName = "+"; asraType = genFunType [ aint; aint ] (aint); externName = "add" }
                { asraName = "-"; asraType = genFunType [ aint; aint ] (aint); externName = "subtract" }
                { asraName = "*"; asraType = genFunType [ aint; aint ] (aint); externName = "multiply" }
                { asraName = "/"; asraType = genFunType [ aint; aint ] (aint); externName = "divide" }
                { asraName = "if"; asraType = genFunType [
                    genFunType [ aunit ] (abool)
                    genFunType [ aunit ] (aunit)
                    genFunType [ aunit ] (aunit)
                ] (aunit); externName = "iffn" }
                { asraName = "=="; asraType = genFunType [ aint; aint ] (abool); externName = "eq" }
                { asraName = "!="; asraType = genFunType [ aint; aint ] (abool); externName = "neq" }
                { asraName = "not"; asraType = genFunType [ abool ] (abool); externName = "not" }
                { asraName = "&"; asraType = genFunType [ abool; abool ] (abool); externName = "and" }
                { asraName = "|"; asraType = genFunType [ abool; abool ] (abool); externName = "or" }
            ]
            let typedAst, _ = typecheck ast externs
            let jsGen = genState (File.ReadAllText Config.currentConfig.preludePath) externs
            let generatedJs = generateJs jsGen typedAst
            if (File.Exists outFile) then File.Delete outFile
            File.WriteAllText (outFile, generatedJs)
            ()
    0
