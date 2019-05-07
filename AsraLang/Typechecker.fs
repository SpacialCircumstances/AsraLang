module Typechecker

module U = UntypedAST
module T = TypedAST

type Error = {
    message: string
}

type State = {
    context: Map<string, T.AType>
    types: Map<string, T.AType>
    errors: Error list
}

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
                    let bt = T.FunctionType { input = T.Native "Unit"; output = rt } 
                    let tblock: T.Block = { parameters = []; body = tbody; blockType = bt }
                    T.BlockExpression tblock, state
                | Some parameters ->
                    invalidOp ""

let typecheck (program: U.Expression seq) =
    let init = { context = Map.empty; errors = []; types = Map.empty }
    Seq.mapFold typeExpr init program