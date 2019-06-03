module Tests

open System.IO
open Xunit
open Config
open System.Diagnostics
open System

let integrationTestDirectory = "../../../../Tests"
let mainFileName = "main.asra"
let outFileName = "out.js"
let outputFileName = "output.txt"

let assertOutputEqual (expected: string) (got: string) =
    let normExp = System.Text.RegularExpressions.Regex.Replace(expected, @"\r\n|\n\r|\n|\r", "\n").TrimEnd('\n')
    let normGot = System.Text.RegularExpressions.Regex.Replace(got, @"\r\n|\n\r|\n|\r", "\n").TrimEnd('\n')
    Assert.Equal(normExp, normGot)

let getTestCases () =
    if Directory.Exists integrationTestDirectory then
        Directory.EnumerateDirectories (integrationTestDirectory)
            |> Seq.map (fun dir -> [| box dir |])
    else invalidOp "Test directory not found"

let runCompiledScriptWithNode (file: string) =
    use proc = new Process()
    proc.StartInfo.FileName <- "node"
    proc.StartInfo.Arguments <- file
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.RedirectStandardOutput <- true
    proc.Start() |> ignore
    let output = proc.StandardOutput.ReadToEnd();
    proc.WaitForExit();
    output

[<Theory>]
[<MemberData("getTestCases")>]
let integrationTest (testDir: string) =
    //Awful hack until we can resolve modules via compiler parameters
    Config.currentConfig <- { parserTracing = false; preludePath = "../../../../Stdlib/prelude.js" }
    let mainFile = Path.Combine(testDir, mainFileName)
    let outputFile = Path.Combine(testDir, outputFileName)
    Assert.True (File.Exists mainFile)
    Assert.True (File.Exists outputFile)
    let outFile = Path.Combine(testDir, outFileName)
    if File.Exists outFile then File.Delete outFile else ()
    let expectedOutput = File.ReadAllText outputFile
    let parameters = match Compiler.Parameters.create mainFile [] outFile with
                        | Error e -> 
                            Assert.True(false, e)
                            invalidOp "Not reachable" //Satifsy typechecker. Should never be executed.
                        | Ok p -> p
    let compilerOut = Compiler.compile parameters
    Assert.Empty(compilerOut)
    let output = runCompiledScriptWithNode outFile
    assertOutputEqual expectedOutput output
