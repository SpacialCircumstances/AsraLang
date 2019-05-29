module Typechecker

open Types
open Ast

type Extern = {
    asraName: string
    asraType: AType
    externName: string
}

type Error = {
    message: string
}

type State = {
    context: Map<string, AType>
    types: Map<string, AType>
}

let resolveType (state: State) (typeName: string) = Map.find typeName state.types

let rec typeExpr (state: State) (expr: UntypedExpression) =
    match expr with
        | LiteralExpression lit ->
            let literal, litType = match lit.literalValue with
                                    | LiteralValue.String str ->
                                        String str, astring
                                    | LiteralValue.Int i ->
                                        Int i, aint
                                    | LiteralValue.Float f ->
                                        Float f, afloat
            LiteralExpression { data = litType; literalValue = literal }, state
        | GroupExpression e ->
            let subExpr, newState = typeExpr state e
            GroupExpression subExpr, newState
        | VariableBindingExpression def ->
            let result, _ = typeExpr state def.value
            let valueType = getType result
            let name = match def.varName with
                        | Simple s -> s
                        | Annotated a -> a.varName
            let binding: VariableBinding<AType> = { varName = Simple name; varData = valueType; value = result }
            let newContext = Map.add name valueType state.context
            //TODO: Check type annotation if it matches
            let newState = { state with context = newContext }
            VariableBindingExpression binding, newState
        | VariableExpression (var, _) ->
            let varType = Map.tryFind var state.context
            match varType with
                | Some varType ->
                    VariableExpression (var, varType), state
                | None ->
                    //TODO: We need error handling
                    invalidOp (sprintf "Variable %s not found" var)
        | FunctionCallExpression fc ->
            let args = List.map (fun a -> typeExpr state a |> fst) fc.args
            let funExp, _ = typeExpr state fc.func
            let funType = getType funExp
            match returnType funType (List.map getType args) with
                | Ok retType ->
                    let call: FunctionCall<AType> = { func = funExp; args = args; data = retType }
                    FunctionCallExpression call, state
                | Error e -> invalidOp e
        | BlockExpression block ->
            match List.isEmpty block.parameters with
                | true ->
                    let body, _ = List.mapFold typeExpr state block.body
                    let rt = getType (List.last body)
                    let bt = genFunType [] rt
                    let tblock: Block<AType> = { parameters = []; body = body; data = bt }
                    BlockExpression tblock, state
                | false ->
                    let parameters = block.parameters
                    let typedParams = List.map (fun d -> match d with
                                                            | Simple _ -> invalidOp "You need to specify type names in block parameters"
                                                            | Annotated t -> (t.varName, resolveType state t.typeName)) parameters
                    let blockContext = List.fold (fun ctx (p, pt) -> Map.add p pt ctx) state.context typedParams
                    let blockState = { state with context = blockContext }
                    let body, _ = List.mapFold typeExpr blockState block.body
                    let rt = getType (List.last body)
                    let bt = genFunType (List.map getType body) rt
                    let tblock: Block<AType> = { parameters = (List.map (fst >> Simple) typedParams); body = body; data = bt }
                    BlockExpression tblock, state

let typecheck (program: UntypedExpression) (externs: Extern list) =
    let init = { 
        context = Map.ofList (List.map (fun ext -> ext.asraName, ext.asraType) externs); 
        types = Map.empty 
    }
    typeExpr init program
