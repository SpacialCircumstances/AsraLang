module Config

type Config = {
    parserTracing: bool
    preludePath: string
}

let mutable currentConfig = {
    parserTracing = false
    preludePath = "../Stdlib/prelude.js"
}