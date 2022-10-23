module fledger.ParsingBasics

open System

open FParsec
open Xunit.Abstractions

// Note that adding test output to user state is attempt in despair,
// since I don't know how to effectively debug/trace the parsing.
// And currently I even don't know how to use the user state at all.
type UserState = { Output: ITestOutputHelper }

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

let BP (p: Parser<_, _>) stream = p stream // set a breakpoint here

let whitespace: Parser<string, UserState> =
    manyChars (pchar ' ' <|> pchar '\t')
    <??> "whitespace"

let whitespace1: Parser<string, UserState> =
    many1Chars (pchar ' ' <|> pchar '\t')
    <??> "whitespace 1"

let newlineOrEof: Parser<unit, UserState> =
    (newline |>> fun _ -> ()) <|> eof
    <??> "newline or eof"

let endOfLineWhitespace: Parser<unit, UserState> =
    whitespace >>. newlineOrEof >>% ()
    <??> "end of line whitespace"

let pEmptyLine =
    whitespace >>. newline <??> "empty line"

let pDatePartSeparator: Parser<char, UserState> =
    pchar '/' <|> pchar '-' <??> "date part separator"

let pYear =
    pint32 .>> pDatePartSeparator <??> "year"

let pMonth =
    pint32 .>> pDatePartSeparator <??> "month"

let pDay = pint32 <??> "day"

let pDate =
    pipe3 pYear pMonth pDay (fun year month day -> DateTime(year, month, day))
    <??> "date"
