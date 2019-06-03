module Tests

open System.IO
open Xunit

let integrationTestDirectory = "../../../../Tests"
let mainFileName = "main.asra"
let outputFileName = "output.txt"

let getTestCases () =
    if Directory.Exists integrationTestDirectory then
        Directory.EnumerateDirectories (integrationTestDirectory)
            |> Seq.map (fun dir -> [| box dir |])
    else invalidOp "Test directory not found"

[<Theory>]
[<MemberData("getTestCases")>]
let integrationTest (testDir: string) =
    let mainFile = Path.Combine(testDir, mainFileName)
    let outputFile = Path.Combine(testDir, outputFileName)
    Assert.True (File.Exists mainFile)
    Assert.True (File.Exists outputFile)
    ()