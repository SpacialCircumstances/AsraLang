module Tokenizer

open Token
open System
open System.Globalization

type State = {
    start: int
    line: int
    col: int
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

let hasEnded (code: string) (state: State) = state.start >= code.Length

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

let mapToTokens (delimited: string seq) =
    delimited 
            |> Seq.map (fun lexeme -> 
                if keywords.ContainsKey lexeme then
                    let t = keywords.[lexeme]
                    Some (token t 0 0)
                else
                    None
            )
            |> Seq.choose id

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