module fledger.Tests.ParsingUtils

open FsCheck

open FParsec
open Xunit.Abstractions

open fledger.Parsing.ParsingBasics

open Swensen.Unquote

let testParser parser (output: ITestOutputHelper) text expectedValue =
    let result =
        runParserOnString parser { Something = 0 } "test stream" text

    match result with
    | Success (value, _, _) ->
        output.WriteLine $"PARSING SUCCESS: {value}"
        test <@ value = expectedValue @>
    | Failure (errorMsg, _, _) ->
        output.WriteLine $"PARSING ERROR: {errorMsg}"
        test <@ false @>
