module Typechecker

open Xunit
open Types
module T = Ast

let assertContextContains (state: Typechecker.State) (name: string) (atype: AType) =
    match Map.tryFind name state.context with
        | None -> Assert.True(false, sprintf "Variable %s not found" name)
        | Some tp -> Assert.Equal(atype, tp)
