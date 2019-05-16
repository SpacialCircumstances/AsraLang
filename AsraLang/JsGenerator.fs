module JsGenerator

open System.Text
open TypedAST
open Typechecker
open System
open System.Globalization

type GenerationState = {
    externFunctionsMapper: Map<string, string>
    prelude: string
}

let genState (prelude: string) (externs: Extern list) = {
    prelude = prelude
    externFunctionsMapper = Map.ofList (List.map (fun ext -> ext.asraName, ext.externName) externs)
}

let rec private writeArguments (writeJs: StringBuilder -> Expression -> StringBuilder) (writer: StringBuilder) (arg: Expression) (rest: Expression list) =
    writeJs writer arg |> ignore 
    match List.tryHead rest with
        | None -> ()
        | Some head ->
            writer.Append(", ") |> ignore
            writeArguments writeJs writer head (List.tail rest)

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
                | LiteralValue.Float f -> writer.Append(Convert.ToString(f, CultureInfo.InvariantCulture))
        | VariableExpression (var, _) -> 
            if Map.containsKey var state.externFunctionsMapper then
                writer.Append (Map.find var state.externFunctionsMapper)
            else writer.Append(var)
        | GroupExpression expr -> 
            writer.Append("(") |> ignore
            writeJs state writer expr |> ignore
            writer.Append(")")
        | FunctionCallExpression funCall ->
            match funCall.func with
                | VariableExpression _ ->
                    writeJs state writer funCall.func |> ignore
                | BlockExpression _ ->
                    writer.Append "(" |> ignore
                    writeJs state writer funCall.func |> ignore
                    writer.Append ")" |> ignore
                | _ -> invalidOp "Invalid expression for function call"
            writer.Append "(" |> ignore
            let args = List.map (fun (e, _) -> e) funCall.args
            if not (List.isEmpty args) then
                writeArguments (writeJs state) writer (List.head args) (List.tail args)
            else ()
            writer.AppendLine ");"
        | _ -> raise (NotImplementedException())

let generateJs (state: GenerationState) (ast: Expression seq) =
    let sb = StringBuilder().Append(state.prelude)
    (Seq.fold (writeJs state) sb ast).ToString ()
