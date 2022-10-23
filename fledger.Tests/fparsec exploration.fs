module fledger.Tests.fparsec_exploration



open Xunit
open FsCheck

open Xunit.Abstractions

type FParsecExplorationTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``icebreaker``() = true
        