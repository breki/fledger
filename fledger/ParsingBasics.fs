module fledger.ParsingBasics

open System

open FParsec

let filterOutNone items =
    items
    |> (List.filter Option.isSome)
    |> List.map Option.get

let trimStringOptional (s: string option) =
    match s with
    | Some s ->
        s.Trim()
        |> function
            | "" -> None
            | trimmed -> Some trimmed
    | None -> None

let whitespace: Parser<string, unit> =
    manyChars (pchar ' ' <|> pchar '\t')
    <?> "whitespace"

let whitespace1: Parser<string, unit> =
    many1Chars (pchar ' ' <|> pchar '\t')
    <?> "whitespace 1"

let endOfLineWhitespace: Parser<unit, unit> =
    whitespace >>. newline >>% ()
    <?> "end of line whitespace"

let pEmptyLine =
    endOfLineWhitespace <?> "empty line"

let pDatePartSeparator: Parser<char, unit> =
    pchar '/' <|> pchar '-' <?> "date part separator"

let pYear =
    pint32 .>> pDatePartSeparator <?> "year"

let pMonth =
    pint32 .>> pDatePartSeparator <?> "month"

let pDay = pint32 <?> "day"

let pDate =
    pipe3 pYear pMonth pDay (fun year month day -> DateTime(year, month, day))
    <?> "date"
