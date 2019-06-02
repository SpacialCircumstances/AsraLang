module Tests

open System.IO
open Xunit

let integrationTestDirectory = "../../../../Tests"

let getTestCases () =
    if Directory.Exists integrationTestDirectory then
        Directory.EnumerateDirectories (integrationTestDirectory)
            |> Seq.map (fun dir -> [| box dir |])
    else invalidOp "Test directory not found"

[<Theory>]
[<MemberData("getTestCases")>]
let integrationTest (testDir: string) =
    printfn "%s" testDir
    ()