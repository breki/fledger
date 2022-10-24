module fledger.ParsingJournal

open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingDefaultCommodity
open fledger.ParsingTransactions

// todo 10: add support for "commodity EUR" directives
// todo 40: add support for "P 2018-11-01 CAD 0.671 EUR" directives
// todo 70: add support for account directives
// todo 100: add support for account directives

let pJournalItem<'T> : Parser<JournalItem option, 'T> =
    (pTx |>> Transaction |>> Some)
    <|> (pDefaultCommodity |>> Some)
    <|> (pEmptyLine |>> fun _ -> None)
    <??> "journal item"

let pJournal<'T> : Parser<Journal, 'T> =
    many pJournalItem .>> eof
    |>> fun items ->
            let actualItems = filterOutNone items
            { Items = actualItems }
    <??> "journal"
