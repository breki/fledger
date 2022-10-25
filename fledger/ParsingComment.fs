module fledger.ParsingComment

open FParsec

open fledger.Journal

// comment = "#", comment text
let pComment<'T> : Parser<JournalItem, 'T> =
    pstring "#" >>. restOfLine true |>> Comment
    <??> "comment"
