module Tests

open NUnit.Framework

[<TestFixture>]
type IntegrationTests () =

    [<Test>]
    member this.RunTests() =
        Assert.True(true)