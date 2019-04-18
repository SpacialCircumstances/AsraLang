﻿module Tokenizer

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

let findNext (source: string) (start: int) (pred: char -> bool) =
    let afterStart = Seq.skip start source
    Seq.findIndex pred afterStart + start

let skipWhitespace (state: State) =
    let nextNonWs = findNext state.source state.current (fun c -> not (Char.IsWhiteSpace c))
    { state with start = nextNonWs; current = nextNonWs }

let makeToken (state: State) (t: TokenType) = ({ token = t; position = state.start }, state)

let comment (state: State) =
    let nextLinebreak = findNext state.source state.current (fun c -> c = '\n' || c = char 0)
    { token = Comment; position = state.start }, { state with start = nextLinebreak; current = nextLinebreak }

let stringLiteral (state: State) =
    let nextQuote = findNext state.source state.current (fun c -> c = '"')
    let literal = state.source.Substring(state.start, nextQuote)
    let newState, _ = advance state //Consume quote
    { token = StringLiteral literal; position = newState.current }, newState

let numberLiteral (state: State) =
    raise(NotImplementedException())

let identifier (state: State) =
    let stop = findNext state.source state.current Char.IsWhiteSpace
    let literal = state.source.Substring(state.start, stop)
    { token = Identifier literal; position = state.start }, { state with start = state.current + stop; current = state.current + stop }

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
                | '#' -> comment state
                | '"' -> stringLiteral state
                | _ -> 
                    if Char.IsNumber curr then
                        numberLiteral state
                    else
                        identifier state

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
    