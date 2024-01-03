# fledger Development Diary

## 2024-01-03

Migrated to .NET 8.

What follows is fixing the remaining transactions that cannot be parsed from 
the realistic file. Like this one:

```
2023-03-16 *Paypal - izravnava
    assets:current assets:PayPal      = 10 EUR
    assets:current assets:PayPal      = 10 CAD
    assets:current assets:PayPal      = 10 GBP
    assets:current assets:PayPal      = 10 USD
```

or this one:
```
2023-04-21 *Paypal - izravnava
    assets:current assets:PayPal      = 10 EUR
    assets:current assets:PayPal      = 10 CAD
    assets:current assets:PayPal      = 10 GBP
    income:Other Income       -10 USD
    assets:current assets:PayPal      = 10 USD
```

For both the error reported is
```
Could not balance the transaction - cannot have more than one 
posting without an amount.
```

## 2024-01-02

Got back to the project after a very long time. I've added this diary to keep
everything in a single place instead of Notion.

I've also added the `Run-Build.ps1` build script and written the basic build 
instructions in `README.md`.

The next step would be to migrate the project to .NET 8 and then go back to 
work on the parsing and processing of the hledger journal files.