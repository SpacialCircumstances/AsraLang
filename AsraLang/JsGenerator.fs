module JsGenerator

open System.Text
open TypedAST
open Typechecker

type GenerationState = {
    externFunctionsMapper: Map<string, string>
    prelude: string
}

let genState (prelude: string) (externs: Extern list) = {
    prelude = prelude
    externFunctionsMapper = Map.ofList (List.map (fun ext -> ext.asraName, ext.externName) externs)
}

let writeJs (state: GenerationState) (writer: StringBuilder) (expr: Expression) =
    writer

let generateJs (state: GenerationState) (ast: Expression seq) =
    let sb = StringBuilder().Append(state.prelude)
    (Seq.fold (writeJs state) sb ast).ToString ()
