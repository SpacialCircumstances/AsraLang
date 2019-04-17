module Tokenizer

open Token
open System

type State = {
    source: string
    start: int
    current: int
    line: int
    col: int
}

let hasEnded (state: State) = state.current >= state.source.Length

let advance (state: State) = ({ state with current = state.current + 1 }, state.source.[state.current])

let skipWhitespace (state: State) =
    let sourceAfterCurrent = Seq.skip state.current state.source
    let nextNonWs = Seq.findIndex (fun c -> not (Char.IsWhiteSpace c)) sourceAfterCurrent + state.current
    { state with start = nextNonWs; current = nextNonWs }

let rec nextToken (state: State) =
    let state, nextChar = state |> skipWhitespace |> advance //Get first character after whitespace
    match nextChar with
        | _ -> ({ token = Comma; position = state.start }, state)

let tokenize (code: string): Token seq =
    let init = {
        source = code;
        start = 0;
        current = 0;
        line = 1;
        col = 1;
    }
    Seq.unfold (fun state -> 
        match hasEnded state with
            | true -> None
            | false -> Some (nextToken state)
    ) init
    