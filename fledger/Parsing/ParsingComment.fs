module fledger.Parsing.ParsingComment

open FParsec

open fledger.Journal

open fledger.Parsing.ParsingUtils

// comment = "#", comment text
let pComment<'T> : Parser<int64 * JournalItem, 'T> =
    pstring "#" >>. restOfLine true |> withPos
    |>> (fun x -> x.Start.Line, Comment x.Value)
    <??> "comment"
