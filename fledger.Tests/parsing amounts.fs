module fledger.``parsing amounts``


open System
open System.Globalization
open Xunit
open FsCheck

open FParsec
open Xunit.Abstractions

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingAmounts

open Swensen.Unquote


let testParser parser (output: ITestOutputHelper) text expectedValue =
    let result =
        runParserOnString parser { Output = output } "test stream" text

    match result with
    | Success (value, _, _) ->
        output.WriteLine $"PARSING SUCCESS: {value}"
        test <@ value = expectedValue @>
    | Failure (errorMsg, _, _) ->
        output.WriteLine $"PARSING ERROR: {errorMsg}"
        test <@ false @>

type AmountsParsingTests(output: ITestOutputHelper) =
    [<Theory>]
    [<InlineData("")>]
    [<InlineData("+")>]
    [<InlineData("-")>]
    member this.``parsing number signs`` text =
        testParser pSign output text text

    [<Theory>]
    [<InlineData("12,334", "12334")>]
    [<InlineData("1,334", "1334")>]
    [<InlineData("1335,334,666", "1335334666")>]
    member this.``parsing thousands parts`` text expectedText =
        testParser pThousandsPart output text expectedText

    [<Theory>]
    [<InlineData("12,334", "12334")>]
    [<InlineData("1,334", "1334")>]
    [<InlineData("1", "1")>]
    [<InlineData("1335", "1335")>]
    member this.``parsing integer parts`` text expectedText =
        testParser pIntegerPart output text expectedText

    [<Theory>]
    [<InlineData(".")>]
    [<InlineData(".1")>]
    [<InlineData(".1256")>]
    member this.``parsing decimal parts`` text =
        testParser pDecimalPart output text text

    [<Theory>]
    [<InlineData("12,334", "12334")>]
    [<InlineData("1,334", "1334")>]
    [<InlineData("1335,334,666", "1335334666")>]
    [<InlineData("1,123.45", "1123.45")>]
    [<InlineData("123.45", "123.45")>]
    [<InlineData("0.45", "0.45")>]
    [<InlineData(".45", ".45")>]
    member this.``parsing number literals`` text expectedText =
        testParser numberLiteral2 output text expectedText

    // todo 2: extend this test with more samples
    [<Theory>]
    [<InlineData("123.45", "123.45", null)>]
    [<InlineData("123.45 EUR", "123.45", "EUR")>]
    [<InlineData("123.45    EUR", "123.45", "EUR")>]
    [<InlineData("123.    EUR", "123.", "EUR")>]
    [<InlineData(".95    EUR", "0.95", "EUR")>]
    member this.``parsing amounts`` text expectedValue expectedCurrency =
        let expectedValue =
            { Value = Decimal.Parse(expectedValue, CultureInfo.InvariantCulture)
              Currency =
                if expectedCurrency <> null then
                    Some expectedCurrency
                else
                    None }

        testParser pAmount output text expectedValue
