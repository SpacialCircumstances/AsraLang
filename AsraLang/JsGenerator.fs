module JsGenerator

open System.Text
open Ast
open Types
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

let rec private writeArguments (writeJs: StringBuilder -> TypedExpression -> StringBuilder) (writer: StringBuilder) (arg: TypedExpression) (rest: TypedExpression list) =
    writeJs writer arg |> ignore 
    match List.tryHead rest with
        | None -> ()
        | Some head ->
            writer.Append(", ") |> ignore
            writeArguments writeJs writer head (List.tail rest)

let rec writeBlockBody (writeJs: StringBuilder -> TypedExpression -> StringBuilder) (block: Block<AType>) (writer: StringBuilder) (doesReturn: bool) =
    match doesReturn with
        | false ->
            List.iter (fun expr ->
                writeJs writer expr |> ignore
                writer.AppendLine(";") |> ignore
            ) block.body
            writer
        | true ->
            let noReturnExpressions = block.body.[ 0..(List.length block.body - 2) ]
            List.iter (fun expr ->
                writeJs writer expr |> ignore
                writer.AppendLine(";") |> ignore
            ) noReturnExpressions
            writer.Append "return " |> ignore
            let last = (List.last block.body)
            writeJs writer last |> ignore
            writer.AppendLine ";"

let rec writeJs (state: GenerationState) (writer: StringBuilder) (expr: TypedExpression) =
    match expr with
        | VariableBindingExpression binding ->
            do writer.Append (sprintf "const %s = " (toVarName binding.varName)) |> ignore
            do writeJs state writer binding.value |> ignore
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
            writeJs state writer funCall.func |> ignore
            List.iter (fun a ->
                writer.Append "(" |> ignore
                writeJs state writer a |> ignore
                writer.Append ")" |> ignore
            ) funCall.args
            writer
        | BlockExpression block ->
            if List.isEmpty block.parameters then
                writer.Append "() =>" |> ignore
            else 
                List.iter (fun (name: Declaration) -> 
                    writer.Append "(" |> ignore
                    writer.Append(toVarName name) |> ignore
                    writer.Append ") =>" |> ignore
                ) block.parameters
            writer.AppendLine "{" |> ignore
            let returns = match appliedType block.data (List.length block.parameters) with
                                | Some (Primitive (Native "Unit")) -> false
                                | _ -> true
            writeBlockBody (writeJs state) block writer returns |> ignore
            writer.AppendLine "}"

let generateJs (state: GenerationState) (ast: TypedExpression) =
    let sb = StringBuilder().Append(state.prelude)
    match ast with
        | BlockExpression block ->
            (writeBlockBody (writeJs state) block sb false).ToString ()
        | _ -> invalidOp "AST must be a top-level block node"
