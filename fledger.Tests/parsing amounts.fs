module fledger.``parsing amounts``


open Xunit
open FsCheck

open FParsec
open Xunit.Abstractions

open fledger.Journal
open fledger.ParsingAmounts

open Swensen.Unquote

// todo 1: clean up the tests, extract the common helper functionality
type AmountsParsingTests(output: ITestOutputHelper) =
    [<Theory>]
    [<InlineData("")>]
    [<InlineData("+")>]
    [<InlineData("-")>]
    member this.``parsing number signs`` text =
        let result =
            runParserOnString pSign { Output = output } "test stream" text

        match result with
        | Success (value, _, _) ->
            output.WriteLine $"PARSING SUCCESS: {value}"
            test <@ value = text @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>

    [<Theory>]
    [<InlineData("12,334", "12334")>]
    [<InlineData("1,334", "1334")>]
    [<InlineData("1335,334,666", "1335334666")>]
    member this.``parsing thousands parts`` text expectedText =
        let result =
            runParserOnString
                pThousandsPart
                { Output = output }
                "test stream"
                text

        match result with
        | Success (value, _, _) ->
            output.WriteLine $"PARSING SUCCESS: {value}"
            test <@ value = expectedText @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>

    [<Theory>]
    [<InlineData("12,334", "12334")>]
    [<InlineData("1,334", "1334")>]
    [<InlineData("1", "1")>]
    [<InlineData("1335", "1335")>]
    member this.``parsing integer parts`` text expectedText =
        let result =
            runParserOnString
                pIntegerPart
                { Output = output }
                "test stream"
                text

        match result with
        | Success (value, _, _) ->
            output.WriteLine $"PARSING SUCCESS: {value}"
            test <@ value = expectedText @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>

    [<Theory>]
    [<InlineData(".")>]
    [<InlineData(".1")>]
    [<InlineData(".1256")>]
    member this.``parsing decimal parts`` text =
        let result =
            runParserOnString
                pDecimalPart
                { Output = output }
                "test stream"
                text

        match result with
        | Success (value, _, _) ->
            output.WriteLine $"PARSING SUCCESS: {value}"
            test <@ value = text @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>

    [<Theory>]
    [<InlineData("12,334", "12334")>]
    [<InlineData("1,334", "1334")>]
    [<InlineData("1335,334,666", "1335334666")>]
    [<InlineData("1,123.45", "1123.45")>]
    [<InlineData("123.45", "123.45")>]
    [<InlineData("0.45", "0.45")>]
    [<InlineData(".45", ".45")>]
    member this.``parsing number literals`` text expectedText =
        let result =
            runParserOnString
                numberLiteral2
                { Output = output }
                "test stream"
                text

        match result with
        | Success (value, _, _) ->
            output.WriteLine $"PARSING SUCCESS: {value}"
            test <@ value = expectedText @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>

    [<Fact>]
    member this.``parsing amounts``() =
        let text = "123.45"

        let result =
            runParserOnString pAmount { Output = output } "test stream" text

        match result with
        | Success (value, _, _) ->
            output.WriteLine $"PARSING SUCCESS: {value}"
            test <@ value = { Value = 123.45m; Currency = None } @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>
