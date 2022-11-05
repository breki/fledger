# Brainstorming ideas

## Quick & dirty

- automatically defining and entering periodical/scheduled transactions
    - making predictions
    - using machine learning from ledger data?
- reformatting the ledger file into
    - using the unified date format
    - removing the default currency from transactions
    - prettifying the transactions output
- rendering a dashboard from the latest data
- rendering charts (D3?)
- adding transactions via a web interface
    - integration with git - so the changes are automatically recorded in a
      distributed way

## Rendering charts - first try

1. Create a CLI project.
2. Provide a command to generate JSON data for a certain chart type.
3. Create a JS+HTML+CSS for this chart.
4. 