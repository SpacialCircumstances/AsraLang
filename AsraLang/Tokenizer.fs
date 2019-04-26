module Tokenizer

open Token
open System
open System.Globalization

type State = {
    source: string
    start: int
    current: int
}

let keywords = dict [
    "->", Arrow
    ".", Dot
    ",", Comma
    "=", Equal
    ":", Colon
    "[", BlockOpen
    "]", BlockClose
    "(", LeftParen
    ")", RightParen
]

let isDelimiter (c: Char) = Char.IsWhiteSpace c || c = '.' || c = ')' || c = ']' || c = ',' || c = '-' || c = ':'

let hasEnded (state: State) = state.current >= state.source.Length

let advance (state: State) = ({ state with current = state.current + 1 }, state.source.[state.current])

let peek (state: State) = match hasEnded state with
                            | true -> char 0
                            | false -> state.source.[state.current]

let findNext (source: string) (start: int) (pred: char -> bool) =
    let afterStart = Seq.skip start source
    match (Seq.tryFindIndex pred afterStart) with
        | Some index -> index + start
        | None -> source.Length //Not found, therefore token must end at source end

let nextChar (source: string) (after: int) =
    match after + 1 >= source.Length with
        | true -> char 0
        | false -> source.[after + 1]

let skipWhitespace (state: State) =
    let nextNonWs = findNext state.source state.current (fun c -> not (Char.IsWhiteSpace c))
    { state with start = nextNonWs; current = nextNonWs }

let makeToken (state: State) (t: TokenType) = ({ token = t; position = state.start }, state)

let comment (state: State) =
    let nextLinebreak = findNext state.source state.current (fun c -> c = '\n' || c = char 0)
    { token = Comment; position = state.start }, { state with start = nextLinebreak; current = nextLinebreak }

let stringLiteral (state: State) =
    let nextQuote = findNext state.source state.current (fun c -> c = '"')
    let literal = state.source.Substring(state.current, nextQuote - state.current)
    let newState = { state with start = nextQuote + 1; current = nextQuote + 1 } //Consume the second quote
    { token = StringLiteral literal; position = state.current }, newState

let numberLiteral (state: State) =
    let nextNonDigit = findNext state.source state.current (fun c -> not (Char.IsNumber c))
    let afterThat = nextChar state.source nextNonDigit
    let tt, final = if Char.IsNumber afterThat && state.source.[nextNonDigit] = '.' then
                        let stop = findNext state.source (nextNonDigit + 1) (fun c -> not (Char.IsNumber c))
                        let literal = state.source.Substring(state.start, stop - state.start)
                        FloatLiteral (Double.Parse (literal, CultureInfo.InvariantCulture)), stop
                    else
                        let literal = state.source.Substring(state.start, nextNonDigit - state.start)
                        IntLiteral (Int64.Parse literal), nextNonDigit
    { token = tt; position = state.start }, { state with start = final; current = final }

let identifier (state: State) =
    let stop = findNext state.source state.current isDelimiter
    let literal = state.source.Substring(state.start, stop - state.start)
    let tt = match keywords.ContainsKey literal with
                | true -> keywords.Item literal
                | false -> Identifier literal
    { token = tt; position = state.start }, { state with start = stop; current = stop }

let nextToken (state: State) =
    let state, curr = advance state
    match curr with
        | '#' -> comment state
        | '"' -> stringLiteral state
        | '(' -> makeToken state LeftParen
        | ')' -> makeToken state RightParen
        | '[' -> makeToken state BlockOpen
        | ']' -> makeToken state BlockClose
        | ',' -> makeToken state Comma
        | '-' ->
            let newState, nextChar = advance state
            match nextChar with
                | '>' -> makeToken newState Arrow
                | _ ->
                    if Char.IsNumber nextChar then
                        numberLiteral newState
                    else
                        identifier state
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
    }
    Seq.unfold (fun state -> 
        let skipped = skipWhitespace state
        match hasEnded skipped with
            | true -> None
            | false -> Some (nextToken skipped)
    ) init |> Seq.cache
    