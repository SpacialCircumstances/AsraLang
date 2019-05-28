module Typechecker

module T = Ast
open Types

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

let rec typeExpr (state: State) (expr: T.UntypedExpression) =
    match expr with
        | T.LiteralExpression lit ->
            let literal, litType = match lit.literalValue with
                                    | T.LiteralValue.String str ->
                                        T.String str, astring
                                    | T.LiteralValue.Int i ->
                                        T.Int i, aint
                                    | T.LiteralValue.Float f ->
                                        T.Float f, afloat
            T.LiteralExpression { data = litType; literalValue = literal }, state
        | T.GroupExpression e ->
            let subExpr, newState = typeExpr state e
            T.GroupExpression subExpr, newState
        | T.VariableBindingExpression def ->
            let result, _ = typeExpr state def.value
            let valueType = T.getType result
            let name = match def.varName with
                        | T.Simple s -> s
                        | T.Annotated a -> a.varName
            let binding: T.VariableBinding<AType> = { varName = T.Simple name; varData = valueType; value = result }
            let newContext = Map.add name valueType state.context
            //TODO: Check type annotation if it matches
            let newState = { state with context = newContext }
            T.VariableBindingExpression binding, newState
        | T.VariableExpression (var, _) ->
            let varType = Map.tryFind var state.context
            match varType with
                | Some varType ->
                    T.VariableExpression (var, varType), state
                | None ->
                    //TODO: We need error handling
                    invalidOp (sprintf "Variable %s not found" var)
        | T.FunctionCallExpression fc ->
            let args = List.map (fun a -> typeExpr state a |> fst) fc.args
            let funExp, _ = typeExpr state fc.func
            let funType = T.getType funExp
            match returnType funType (List.map T.getType args) with
                | Ok retType ->
                    let call: T.FunctionCall<AType> = { func = funExp; args = args; data = retType }
                    T.FunctionCallExpression call, state
                | Error e -> invalidOp e
        | T.BlockExpression block ->
            match List.isEmpty block.parameters with
                | true ->
                    let body, _ = List.mapFold typeExpr state block.body
                    let rt = T.getType (List.last body)
                    let bt = genFunType [] rt
                    let tblock: T.Block<AType> = { parameters = []; body = body; data = bt }
                    T.BlockExpression tblock, state
                | false ->
                    let parameters = block.parameters
                    let typedParams = List.map (fun d -> match d with
                                                            | T.Simple _ -> invalidOp "You need to specify type names in block parameters"
                                                            | T.Annotated t -> (t.varName, resolveType state t.typeName)) parameters
                    let blockContext = List.fold (fun ctx (p, pt) -> Map.add p pt ctx) state.context typedParams
                    let blockState = { state with context = blockContext }
                    let body, _ = List.mapFold typeExpr blockState block.body
                    let rt = T.getType (List.last body)
                    let bt = genFunType (List.map T.getType body) rt
                    let tblock: T.Block<AType> = { parameters = (List.map (fst >> T.Simple) typedParams); body = body; data = bt }
                    T.BlockExpression tblock, state

let typecheck (program: T.UntypedExpression) (externs: Extern list) =
    let init = { 
        context = Map.ofList (List.map (fun ext -> ext.asraName, ext.asraType) externs); 
        types = Map.empty 
    }
    typeExpr init program
