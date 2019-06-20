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

//TODO: Use nested context for different scopes
type Context = {
    parent: Context option
    context: Map<string, AType>
}

type State = {
    context: Context
    types: Map<string, AType>
}

let formatPosition (pos: FParsec.Position) =
    sprintf "File: %s Line: %i Col: %i" pos.StreamName pos.Line pos.Column

let rec resolveType (state: State) (typeName: TypeDeclaration) = 
    match typeName with
        | Name typeName -> Map.tryFind typeName state.types
        | Parameterized pt ->
            match Map.tryFind pt.name state.types with
                | Some (Primitive rbt) ->
                    let resolveTried = List.map (fun tp -> resolveType state tp) pt.genericParameters
                    let resolved = List.choose id resolveTried
                    match List.length resolved = List.length resolveTried with
                        | true -> TypeParameterized {
                                typeParameters = resolved
                                baseType = rbt } |> Some
                        | false -> None
                | _ -> None
        | Generic typeName -> Some (AType.Generic typeName)
        | Function (inp, out) -> 
            match (resolveType state inp, resolveType state out) with
                | Some inp, Some out ->
                    FunctionType { input = inp; output = out } |> Some
                | _ -> None


let rec typeExpr (state: State) (expr: UntypedExpression): TypedExpression option * State * Message list =
    match expr with
        | LiteralExpression lit ->
            let literal, litType = match lit.literalValue with
                                    | LiteralValue.String str ->
                                        String str, astring
                                    | LiteralValue.Int i ->
                                        Int i, anumber 
                                    | LiteralValue.Float f ->
                                        Float f, anumber
            Some (LiteralExpression { data = litType; literalValue = literal }), state, []
        | GroupExpression e ->
            let subExpr, newState, errors = typeExpr state e
            match subExpr with
                | None -> None, newState, errors
                | Some subExpr -> Some (GroupExpression subExpr), newState, errors
        | VariableBindingExpression def ->
            let name, newContext, errors = match def.varName with
                                            | Simple s -> 
                                                let typed, _, errors = typeExpr state def.value
                                                match typed with
                                                    | Some te ->
                                                        Some (s, te), { state.context with context = Map.add s (getType te) state.context.context }, errors
                                                    | None ->
                                                        None, state.context, errors
                                            | Annotated at ->
                                                match resolveType state at.typeName with
                                                    | Some tp ->
                                                        let ctx = { state.context with context = Map.add at.varName tp state.context.context }
                                                        let typed, _, errors = typeExpr { state with context = ctx } def.value
                                                        match typed with
                                                            | Some te ->
                                                                let infT = getType te
                                                                match infT = tp with
                                                                    | true ->
                                                                        Some (at.varName, te), ctx, errors
                                                                    | false ->
                                                                        None, ctx, (sprintf "%s: Annotated type: %O, but inferred: %O" (formatPosition def.varData) tp infT |> TypeError) :: errors
                                                            | None ->
                                                                None, ctx, errors
                                                    | None ->
                                                        None, state.context, [sprintf "%s: Undefined type: %O" (formatPosition def.varData) at.typeName |> TypeError]
            match name with
                | None ->
                    None, state, errors
                | Some (name, typedExpr) ->
                    let binding: VariableBinding<AType> = { varName = Simple name; varData = getType typedExpr; value = typedExpr }
                    let newState = { state with context = newContext }
                    Some (VariableBindingExpression binding), newState, errors
        | VariableExpression (var, pos) ->
            let varType = Map.tryFind var state.context.context
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
                                match returnType funType (List.map getType args) |> fst with
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
                                                                | Annotated t -> 
                                                                    match resolveType state t.typeName with
                                                                        | Some tp -> Some (t.varName, tp)
                                                                        | None -> None) parameters

                    if (List.length typedParams) = (List.length parameters) then
                        let blockContext = { state.context with context = List.fold (fun ctx (p, pt) -> Map.add p pt ctx) state.context.context typedParams }
                        let blockState = { state with context = blockContext }
                        let body, (_, errors) = List.mapFold foldSubExprs (blockState, []) block.body
                        match errors with
                            | [] ->
                                let body = List.choose id body
                                let rt = getType (List.last body)
                                let bt = genFunType (List.map snd typedParams) rt
                                let tblock: Block<AType> = { parameters = (List.map (fst >> Simple) typedParams); body = body; data = bt }
                                Some (BlockExpression tblock), state, errors
                            | _ -> None, state, errors
                    else
                        let error = sprintf "%s: Function parameter types not specified" (formatPosition block.data)
                        None, state, [TypeError error]
                    

let typecheck (program: UntypedExpression) (externs: Extern list) =
    let init = { 
        context = {
            parent = None
            context = Map.ofList (List.map (fun ext -> ext.asraName, ext.asraType) externs)
        }
        types = Map.ofList [ 
            "Number", anumber
            "Bool", abool
            "Unit", aunit
            "String", astring
            "Array", aarray
        ]
    }
    typeExpr init program
