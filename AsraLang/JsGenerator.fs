module JsGenerator

open System.Text
open TypedAST
open Typechecker
open System

type GenerationState = {
    externFunctionsMapper: Map<string, string>
    prelude: string
}

let genState (prelude: string) (externs: Extern list) = {
    prelude = prelude
    externFunctionsMapper = Map.ofList (List.map (fun ext -> ext.asraName, ext.externName) externs)
}

let rec writeJs (state: GenerationState) (writer: StringBuilder) (expr: Expression) =
    match expr with
        | VariableBindingExpression binding ->
            do writer.Append (sprintf "let %s = " binding.varName) |> ignore
            do writeJs state writer binding.value |> ignore
            do writer.AppendLine ";" |> ignore
            writer
        | LiteralExpression lit ->
            match lit.literalValue with
                | LiteralValue.String str -> writer.Append(sprintf "\"%s\"" str)
                | LiteralValue.Int i -> writer.Append i
                | LiteralValue.Float f -> writer.Append f

        | _ -> raise (NotImplementedException())

let generateJs (state: GenerationState) (ast: Expression seq) =
    let sb = StringBuilder().Append(state.prelude)
    (Seq.fold (writeJs state) sb ast).ToString ()
