module Typechecker

module U = UntypedAST
module T = TypedAST

type Extern = {
    asraName: string
    asraType: T.AType
    externName: string
}

type Error = {
    message: string
}

type State = {
    context: Map<string, T.AType>
    types: Map<string, T.AType>
}

let resolveType (state: State) (typeName: string) = Map.find typeName state.types

let rec typeExpr (state: State) (expr: U.Expression) =
    match expr with
        | U.LiteralExpression lit ->
            let literal, litType = match lit with
                                    | U.StringLiteral str ->
                                        T.String str, T.Native "String"
                                    | U.IntLiteral i ->
                                        T.Int i, T.Native "Int"
                                    | U.FloatLiteral f ->
                                        T.Float f, T.Native "Float"
            T.LiteralExpression { atype = litType; literalValue = literal }, state
        | U.GroupExpression e ->
            let subExpr, newState = typeExpr state e
            T.GroupExpression subExpr, newState
        | U.DefineVariableExpression def ->
            let result, _ = typeExpr state def.value
            let valueType = T.getType result
            let name = match def.variableName with
                        | U.Simple s -> s
                        | U.Annotated a -> a.varName
            let binding: T.VariableBinding = { varName = name; varType = valueType; value = result }
            let newContext = Map.add name valueType state.context
            //TODO: Check type annotation if it matches
            let newState = { state with context = newContext }
            T.VariableBindingExpression binding, newState
        | U.VariableExpression var ->
            let varType = Map.tryFind var state.context
            match varType with
                | Some varType ->
                    T.VariableExpression (var, varType), state
                | None ->
                    //TODO: We need error handling
                    invalidOp (sprintf "Variable %s not found" var)
        | U.FunctionCallExpression fc ->
            let args = List.map (fun a -> 
                                    let e, _ = typeExpr state a
                                    e, T.getType e) fc.arguments
            let funExp, _ = typeExpr state fc.func
            let funType = T.getType funExp
            match T.returnType funType (List.map (fun (_, t) -> t) args) with
                | Ok retType ->
                    let call: T.FunctionCall = { func = funExp; funcType = funType; args = args; returnType = retType }
                    T.FunctionCallExpression call, state
                | Error e -> invalidOp e
        | U.BlockExpression block ->
            match block.parameters with
                | None ->
                    let body, _ = List.mapFold typeExpr state block.body
                    let tbody = List.map (fun e -> e, T.getType e) body
                    let _, rt = List.last tbody
                    let bt = T.genFunType [] rt
                    let tblock: T.Block = { parameters = []; body = tbody; blockType = bt }
                    T.BlockExpression tblock, state
                | Some parameters ->
                    let typedParams = List.map (fun d -> match d with
                                                            | U.Simple _ -> invalidOp "You need to specify type names in block parameters"
                                                            | U.Annotated t -> (t.varName, resolveType state t.typeName)) parameters
                    let blockContext = List.fold (fun ctx (p, pt) -> Map.add p pt ctx) state.context typedParams
                    let blockState = { state with context = blockContext }
                    let body, _ = List.mapFold typeExpr blockState block.body
                    let tbody = List.map (fun e -> e, T.getType e) body
                    let _, rt = List.last tbody
                    let bt = T.genFunType (List.map (fun (_, t) -> t) tbody) rt
                    let tblock: T.Block = { parameters = typedParams; body = tbody; blockType = bt }
                    T.BlockExpression tblock, state

let typecheck (program: U.Expression) (externs: Extern list) =
    let init = { 
        context = Map.ofList (List.map (fun ext -> ext.asraName, ext.asraType) externs); 
        types = Map.empty 
    }
    typeExpr init program
