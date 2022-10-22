module fledger.JournalParsing


open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingTransactions


let pJournalItem = pTx |>> Some

let pJournal: Parser<Journal, unit> =
    many pJournalItem
    |>> fun txsMaybe ->
            filterOutNone txsMaybe
            |> (fun txs -> { Transactions = txs })
    <?> "journal"
