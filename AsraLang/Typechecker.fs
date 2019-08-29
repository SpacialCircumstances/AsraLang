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

module Simple =
    
    //TODO: Use nested context for different scopes
    type Context = {
        parent: Context option
        variables: Map<string, AType>
        types: Map<string, AType>
    }
    
    type State = {
        context: Context
    }
    
    let formatPosition (pos: FParsec.Position) =
        sprintf "File: %s Line: %i Col: %i" pos.StreamName pos.Line pos.Column
    
    let rec resolveTypeFromContext (ctx: Context) (name: string) =
        match Map.tryFind name ctx.types with
            | None ->
                match ctx.parent with
                    | None -> None
                    | Some parent ->
                        resolveTypeFromContext parent name
            | Some t -> Some t
    
    let rec resolveVariable (ctx: Context) (name: string) =
        match Map.tryFind name ctx.variables with
            | None ->
                match ctx.parent with
                    | None -> None
                    | Some parent ->
                        resolveVariable parent name
            | Some t -> Some t
    
    let rec resolveType (state: State) (typeName: TypeDeclaration) = 
        match typeName with
            | Name typeName -> resolveTypeFromContext state.context typeName
            | Parameterized pt ->
                match resolveTypeFromContext state.context pt.name with
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
                                        | LiteralValue.Unit ->
                                            Unit, aunit
                Some (LiteralExpression { data = litType; literalValue = literal }), state, []
            | GroupExpression e ->
                let subExpr, newState, errors = typeExpr state e
                match subExpr with
                    | None -> None, newState, errors
                    | Some subExpr -> Some (GroupExpression subExpr), newState, errors
            | VariableBindingExpression def ->
                let addToContext varName tp context = 
                    match Map.tryFind varName context.variables with
                        | None -> { context with variables = Map.add varName tp context.variables }, []
                        | Some _ ->
                            let err = sprintf "%s: Variable %s (type: %O) is already defined in this scope and shadowing is currently not allowed due to naive JS generation" (formatPosition def.varData) varName tp
                            { context with variables = Map.add varName tp context.variables }, [TypeError err]
                let name, newContext, errors = match def.varName with
                                                | Simple s -> 
                                                    let typed, _, errors = typeExpr state def.value
                                                    match typed with
                                                        | Some te ->
                                                            let ctx, errs2 = addToContext s (getData te) state.context
                                                            Some (s, te), ctx, errs2 @ errors
                                                        | None ->
                                                            None, state.context, errors
                                                | Annotated at ->
                                                    match resolveType state at.typeName with
                                                        | Some tp ->
                                                            let ctx, errs1 = addToContext at.varName tp state.context
                                                            let typed, _, errs2 = typeExpr { state with context = ctx } def.value
                                                            let errors = errs1 @ errs2
                                                            match typed with
                                                                | Some te ->
                                                                    let infT = getData te
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
                        let errs = match getData typedExpr with
                                        | Primitive (Native "Unit") ->
                                            (Warning (sprintf "%s: Attempting to set %s to a unit expression, which will result in undefined" (formatPosition def.varData) name)) :: errors
                                        | _ -> errors
                        let binding: VariableBinding<AType> = { varName = Simple name; varData = getData typedExpr; value = typedExpr }
                        let newState = { state with context = newContext }
                        Some (VariableBindingExpression binding), newState, errs
            | VariableExpression (var, pos) ->
                let varType = resolveVariable state.context var
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
                                    let funType = getData funExp
                                    match returnType funType (List.map getData args) |> fst with
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
                        let blockContext = { 
                            parent = Some state.context
                            variables = Map.empty
                            types = Map.empty
                        }
                        let body, (_, errors) = List.mapFold foldSubExprs ({ state with context = blockContext}, []) block.body
                        match List.length errors with
                            | 0 ->
                                let body = List.choose id body
                                let rt = getData (List.last body)
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
                            let blockContext = { 
                                parent = Some state.context
                                variables = List.fold (fun ctx (p, pt) -> Map.add p pt ctx) state.context.variables typedParams
                                types = Map.empty
                            }
                            let blockState = { state with context = blockContext }
                            let body, (_, errors) = List.mapFold foldSubExprs (blockState, []) block.body
                            match errors with
                                | [] ->
                                    let body = List.choose id body
                                    let rt = getData (List.last body)
                                    let bt = genFunType (List.map snd typedParams) rt
                                    let tblock: Block<AType> = { parameters = (List.map (fst >> Simple) typedParams); body = body; data = bt }
                                    Some (BlockExpression tblock), state, errors
                                | _ -> None, state, errors
                        else
                            let error = sprintf "%s: Function parameter types not specified" (formatPosition block.data)
                            None, state, [TypeError error]
    
            | ArrayLiteralExpression (data, subexprs) ->
                match subexprs with
                    | [] ->
                        Some (ArrayLiteralExpression (tArray "x", [])), state, []
                    | _ ->
                        let typedSubExprs, errs = List.fold (fun (exprs, errs) untyped -> 
                                                        let typedOpt, _, newErrs = typeExpr state untyped
                                                        match typedOpt with
                                                            | Some t->
                                                                t :: exprs, newErrs @ errs
                                                            | None ->
                                                                exprs, newErrs @ errs) ([], []) subexprs
                        match List.length typedSubExprs = List.length subexprs with
                            | true ->
                                let arrayConstructionType = genFunType (List.map (fun _ -> AType.Generic "element") typedSubExprs) (tArray "element")
                                let arrayType, _ = returnType arrayConstructionType (List.map getData typedSubExprs)
                                match arrayType with
                                    | Ok at ->
                                        Some (ArrayLiteralExpression (at, typedSubExprs)), state, errs
                                    | Error err ->
                                        let error = sprintf "%s: Array literal: %s" (formatPosition data) err
                                        None, state, TypeError error :: errs
                            | false ->
                                None, state, errs

    let typecheck (program: UntypedExpression) (externs: Extern list) =
        let init = { 
            context = {
                parent = None
                variables = Map.ofList (List.map (fun ext -> ext.asraName, ext.asraType) externs)
                types = Map.ofList [ 
                    "Number", anumber
                    "Bool", abool
                    "Unit", aunit
                    "String", astring
                    "Array", aarray
                ]
            }
        }
        let typedAst, _, msgs = typeExpr init program
        typedAst, msgs
                        
module Improved =
    type TypeRef = string

    type Context = {
        parent: Context option
        varTypes: Map<string, TypeRef>
    }

    type State = {
        typeBindings: Map<TypeRef, AType>
        typeNameCount: int
    }

    let bindTypeRef (state: State ref) tref typ = 
        state := { state.contents with typeBindings = Map.add tref typ state.contents.typeBindings }
        ()

    let newTypeRef (state: State ref) = 
        let tnc = state.contents.typeNameCount
        let refn = (sprintf "t%i" tnc)
        state := { state.contents with typeNameCount = tnc + 1 }
        refn

    let resolveVar name ctx = Map.tryFind name ctx.varTypes

    let resolveRef tr state = Map.tryFind tr state.typeBindings

    let declaredType decl ctx: AType option = None

    let rec typeExpr (expr: UntypedExpression) (ctx: Context) (state: State ref) =
        match expr with
            | LiteralExpression litExpr ->
                let tr = newTypeRef state
                let litType = match litExpr.literalValue with
                                            | LiteralValue.String _ -> astring
                                            | LiteralValue.Int _ -> anumber 
                                            | LiteralValue.Float _ -> anumber
                                            | LiteralValue.Unit -> aunit
                bindTypeRef state tr litType
                Some (LiteralExpression { data = tr; literalValue = litExpr.literalValue }), ctx, []
            | VariableBindingExpression bindExpr ->
                let subExpr, newCtx, errs = typeExpr bindExpr.value ctx state
                match subExpr with
                    | None ->
                        None, newCtx, errs
                    | Some subExpr ->
                        let valueTypeRef = getData subExpr
                        match resolveRef valueTypeRef state.contents, declaredType bindExpr.varName newCtx with
                            | Some refT, Some tp ->
                                match refT = tp with
                                    | true ->
                                        Some (VariableBindingExpression { value = subExpr; varName = bindExpr.varName; varData = valueTypeRef }), newCtx, errs
                                    | false ->
                                        let msg = sprintf "Expected annotated type: %O, but got %O instead" tp refT |> TypeError
                                        None, newCtx, msg :: errs
                            | None, Some tp ->
                                bindTypeRef state valueTypeRef tp
                                Some (VariableBindingExpression { value = subExpr; varName = bindExpr.varName; varData = valueTypeRef }), newCtx, errs
                            | _, None -> 
                                Some (VariableBindingExpression { value = subExpr; varName = bindExpr.varName; varData = valueTypeRef }), newCtx, errs
            | VariableExpression (name, data) ->
                match resolveVar name ctx with
                    | None ->
                        let msg = sprintf "Variable %s does not exist" name |> TypeError
                        None, ctx, [msg]
                    | Some tr ->
                        Some (VariableExpression (name, tr)), ctx, []
            | GroupExpression expr -> typeExpr expr ctx state            
            | _ -> None, ctx, []

    let typecheck (program: UntypedExpression) (externs: Extern list) =
        let state = ref {
            typeBindings = Map.empty
            typeNameCount = 0
        }
        let types = externs |>
                                List.map (fun ext -> 
                                    let tr = newTypeRef state
                                    bindTypeRef state tr ext.asraType
                                    ext.asraName, tr)
                            |> Map.ofList
        let ctx = {
            parent = None
            varTypes = types
        }
        
        let typed, _, errs = typeExpr program ctx state
        //TODO: Use state to convert typeRefs into actual types
        typed, errs

let typecheck (program: UntypedExpression) (externs: Extern list) = Simple.typecheck program externs
