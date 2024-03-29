﻿module fledger.Parsing.ParsingAmounts

open System

open System.Globalization
open FParsec

open fledger.Journal
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingUtils

// number sign = ["+" | "-"]
let pSign<'T> : Parser<string, 'T> =
    (pchar '+' |>> fun _ -> "+") <|> (pchar '-' |>> fun _ -> "-") <|>% ""
    <??> "sign"

// thousands digits = ",", digit, digit, digit
let pThousandsDigits<'T> : Parser<string, 'T> =
    pchar ',' >>. parray 3 digit |>> fun digits -> digits |> String
    <??> "thousands digits"

// digits many = {digit}
let pDigitsMany<'T> : Parser<string, 'T> =
    many digit |>> fun digits -> digits |> List.toArray |> String
    <??> "digits 0 or more"

// digits many 1 = digit, {digit}
let pDigitsMany1<'T> : Parser<string, 'T> =
    many1 digit |>> fun digits -> digits |> List.toArray |> String
    <??> "digits 1 or more"

// thousands part = [digits many 1], {thousands digits}
let pThousandsPart<'T> : Parser<string, 'T> =
    pDigitsMany1 .>>. many1 pThousandsDigits
    |>> fun (digits, thousandsParts) ->
            let thousandsStr = thousandsParts |> String.concat ""

            digits + thousandsStr
    <??> "thousands part"

let pIntegerPart<'T> : Parser<string, 'T> =
    (attempt pThousandsPart) <|> pDigitsMany <??> "integer part"

// decimal part = ".", {digit}
let pDecimalPart<'T> : Parser<string, 'T> =
    pchar '.' >>. pDigitsMany <??> "decimal part"
    |>> (fun digits -> "." + digits)

// number = [number sign], thousands part | digits many, [decimal part]
let numberLiteral2<'T> : Parser<string, 'T> =
    pipe3
        pSign
        pIntegerPart
        (opt pDecimalPart)
        (fun sign integerPart decimalPart ->
            match decimalPart with
            | Some decimalPart -> sign + integerPart + decimalPart
            | None -> sign + integerPart)
    <??> "number"

let pAmountValue<'T> : Parser<decimal, 'T> =
    numberLiteral2 |> withPos
    |>> (fun pos ->
        let num = pos.Value

        match
            Decimal.TryParse(
                num,
                NumberStyles.Any,
                CultureInfo.InvariantCulture
            )
        with
        | true, value -> value
        | false, _ ->
            let line = pos.Start.Line
            let column = pos.Start.Column

            failwith
                $"Invalid amount value '{num}', line {line}, column {column}.")
    <??> "amount value"

let pCommodityChar = letter

let pCommodity<'T> : Parser<string, 'T> =
    many1Chars pCommodityChar <??> "commodity"

let pAmountCommodity<'T> : Parser<string, 'T> =
    (whitespace1 >>. pCommodity) |> attempt <??> "amount commodity"

let pAmount<'T> : Parser<JournalAmount, 'T> =
    pipe2 pAmountValue (opt pAmountCommodity) (fun amount commodity ->
        match commodity with
        | Some commodity ->
            { Value = amount
              Commodity = Some commodity }
        | None -> { Value = amount; Commodity = None })
    <??> "amount"
