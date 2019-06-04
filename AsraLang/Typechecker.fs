module Typechecker

open Types
open Ast

type Extern = {
    asraName: string
    asraType: AType
    externName: string
}

type Message =
    | TypeError of string
    | Warning of string

type State = {
    context: Map<string, AType>
    types: Map<string, AType>
}

let formatPosition (pos: FParsec.Position) =
    sprintf "File: %s Line: %i Col: %i" pos.StreamName pos.Line pos.Column

let resolveType (state: State) (typeName: string) = Map.find typeName state.types

let rec typeExpr (state: State) (expr: UntypedExpression): TypedExpression option * State * Message list =
    match expr with
        | LiteralExpression lit ->
            let literal, litType = match lit.literalValue with
                                    | LiteralValue.String str ->
                                        String str, astring
                                    | LiteralValue.Int i ->
                                        Int i, aint 
                                    | LiteralValue.Float f ->
                                        Float f, afloat
            Some (LiteralExpression { data = litType; literalValue = literal }), state, []
        | GroupExpression e ->
            let subExpr, newState, errors = typeExpr state e
            match subExpr with
                | None -> None, newState, errors
                | Some subExpr -> Some (GroupExpression subExpr), newState, errors
        | VariableBindingExpression def ->
            let result, _, errors = typeExpr state def.value
            match result with
                | Some result ->
                    let valueType = getType result
                    let name = match def.varName with
                                | Simple s -> s
                                | Annotated a -> a.varName
                    let binding: VariableBinding<AType> = { varName = Simple name; varData = valueType; value = result }
                    let newContext = Map.add name valueType state.context
                    //TODO: Check type annotation if it matches
                    let newState = { state with context = newContext }
                    Some (VariableBindingExpression binding), newState, errors
                | None -> None, state, errors
        | VariableExpression (var, pos) ->
            let varType = Map.tryFind var state.context
            match varType with
                | Some varType ->
                    Some (VariableExpression (var, varType)), state, []
                | None ->
                    let error = sprintf "%s: Variable \"%s\" not found" (formatPosition pos) var |> TypeError
                    None, state, [error]
        | FunctionCallExpression fc ->
            let args, errors = List.mapFold (fun errors a -> 
                                        let x, _, newErrors = typeExpr state a
                                        (x, errors @ newErrors)) [] fc.args
            match List.length errors with
                 | 0 -> 
                        let funExp, _, err = typeExpr state fc.func
                        let args = List.choose id args //No errors - we can unwrap the arguments
                        match funExp with
                            | Some funExp ->
                                let funType = getType funExp
                                match returnType funType (List.map getType args) with
                                    | Ok retType ->
                                        let call: FunctionCall<AType> = { func = funExp; args = args; data = retType }
                                        Some (FunctionCallExpression call), state, []
                                    | Error e -> 
                                        let error = sprintf "%s: %s" (formatPosition fc.data) e |> TypeError
                                        None, state, [error]
                            | None -> None, state, err
                 | _ -> None, state, errors
        | BlockExpression block ->
            let foldSubExprs = (fun (state, errs) expr ->
                let te, st, es = typeExpr state expr
                te, (st, errs @ es))
            match List.isEmpty block.parameters with
                | true ->
                    let body, (_, errors) = List.mapFold foldSubExprs (state, []) block.body
                    match List.length errors with
                        | 0 ->
                            let body = List.choose id body
                            let rt = getType (List.last body)
                            let bt = genFunType [] rt
                            let tblock: Block<AType> = { parameters = []; body = body; data = bt }
                            Some(BlockExpression tblock), state, errors
                        | _ -> None, state, errors
                | false ->
                    //TODO: Type inference
                    let parameters = block.parameters
                    let typedParams = List.choose (fun d -> match d with
                                                                | Simple _ -> None
                                                                | Annotated t -> (t.varName, resolveType state t.typeName) |> Some) parameters

                    if (List.length typedParams) = (List.length parameters) then
                        let blockContext = List.fold (fun ctx (p, pt) -> Map.add p pt ctx) state.context typedParams
                        let blockState = { state with context = blockContext }
                        let body, (_, errors) = List.mapFold foldSubExprs (blockState, []) block.body
                        match errors with
                            | [] ->
                                let body = List.choose id body
                                let rt = getType (List.last body)
                                let bt = genFunType (List.map getType body) rt
                                let tblock: Block<AType> = { parameters = (List.map (fst >> Simple) typedParams); body = body; data = bt }
                                Some (BlockExpression tblock), state, errors
                            | _ -> None, state, errors
                    else
                        let error = sprintf "%s: Function parameter types not specified" (formatPosition block.data)
                        None, state, [TypeError error]
                    

let typecheck (program: UntypedExpression) (externs: Extern list) =
    let init = { 
        context = Map.ofList (List.map (fun ext -> ext.asraName, ext.asraType) externs); 
        types = Map.ofList [ 
            "Int", aint
            "Float", afloat
            "Bool", abool
            "Unit", aunit
            "String", astring
        ]
    }
    typeExpr init program
