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

let singleCharTokens = dict [
    '.', Dot
    ',', Comma
    '=', Equal
    ':', Colon
    '[', BlockOpen
    ']', BlockClose
    '(', LeftParen
    ')', RightParen
]

let keywords = dict [
    "->", Arrow
]

let hasEnded (state: State) = state.current >= state.source.Length

let advance (state: State) = ({ state with current = state.current + 1 }, state.source.[state.current])

let peek (state: State) = match hasEnded state with
                            | true -> char 0
                            | false -> state.source.[state.current]

let skipWhitespace (state: State) =
    let sourceAfterCurrent = Seq.skip state.current state.source
    let nextNonWs = Seq.findIndex (fun c -> not (Char.IsWhiteSpace c)) sourceAfterCurrent + state.current
    { state with start = nextNonWs; current = nextNonWs }

let makeToken (state: State) (t: TokenType) = ({ token = t; position = state.start }, state)

let nextToken (state: State) =
    let state, curr = state |> skipWhitespace |> advance //Get first character after whitespace
    let nextChar = peek state
    match Char.IsWhiteSpace nextChar with
        | true -> //Single char token
            match singleCharTokens.ContainsKey curr with
                | true -> makeToken state (singleCharTokens.Item curr)
                | false -> makeToken state (Identifier (string curr))
        | false ->
            match curr with
                | _ -> ({ token = Comment; position = state.start }, state)

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
    