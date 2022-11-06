open System.CommandLine

open System.Reflection
open fledger.tool.Commands.GenerateCharts

[<EntryPoint>]
let main args =
    let root =
        RootCommand(
            $"fledger.tool v{Assembly.GetExecutingAssembly().GetName().Version}"
        )

    root.Add(generateChartsCommand ())

    async {
        let! exitCode = root.InvokeAsync(args) |> Async.AwaitTask
        return exitCode
    }
    |> Async.RunSynchronously
