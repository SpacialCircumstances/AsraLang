module Tokenizer

open Token
open System
open System.Globalization

type State = {
    line: int
    col: int
    comment: bool
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

let isDelimiter (c: Char) = Char.IsWhiteSpace c || c = '.' || c = '(' || c = ')' || c = '[' || c = ']' || c = ',' || c = ':'

let findNext (source: string) (start: int) (pred: char -> bool) =
    let afterStart = Seq.skip start source
    match Seq.tryFindIndex pred afterStart with
        | Some index -> index + start
        | None -> source.Length

let split (code: string) =
    Seq.unfold (fun start -> 
        if start >= code.Length then
            None
        else
            let startChar = code.[start]
            if isDelimiter startChar then
                Some (code.Substring (start, 1), start + 1)
            else
                let next = findNext code start isDelimiter
                let lexeme = code.Substring (start, next - start)
                Some (lexeme, next)) 0

let lexemeToToken (state: State) (lexeme: string): (Token option * State) =
    match lexeme with
        | "\n" -> (None, { state with col = 1; line = state.line + 1})
        | " " -> (None, { state with col = state.col + 1 })
        | _ ->
            let token = if keywords.ContainsKey lexeme then
                            token (keywords.[lexeme]) state.col state.line |> Some
                        else
                            None
            (token, { state with col = state.col + lexeme.Length })

let mapToTokens (delimited: string seq) =
    let init = {
        line = 1
        col = 1
        comment = false
    }
    let tokens, _ = Seq.mapFold lexemeToToken init delimited
    Seq.choose id tokens

let private tokenize (code: string): Token seq =
    let tokens = code 
                    |> split
                    |> mapToTokens
    Seq.empty

let stripComments (tokens: Token seq) =
    Seq.filter (fun t -> match t.token with
                            | Comment -> false
                            | _ -> true) tokens
    
let debugTokenizer = fun s -> tokenize s |> Seq.cache

let tokenizer = fun s -> tokenize s |> stripComments |> Seq.cache