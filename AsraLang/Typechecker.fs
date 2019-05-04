module Typechecker

module U = UntypedAST
module T = TypedAST

type Error = {
    message: string
}

type State = {
    context: Map<string, T.AType>
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
        | _ -> raise(System.NotImplementedException())

let typecheck (program: U.Expression seq) =
    let init = { context = Map.empty; errors = [] }
    Seq.mapFold typeExpr init program