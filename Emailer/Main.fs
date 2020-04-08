// Learn more about F# at http://fsharp.org

open System
open Serilog
open Serilog.Sinks.SystemConsole
open Ump

[<EntryPoint>]
let main argv =
    let config : Emailer.EffectConfig =
        {
            Logger =
                LoggerConfiguration()
                    .Destructure.FSharpTypes()
                    .WriteTo.Console()
                    .MinimumLevel.Debug()
                    .CreateLogger()
        }
    // string literals for demo purposes.
    // these should be loaded from configuration.
    let settings : Emailer.Settings =
        {
            SendFrom = "Notificator <notify@asdf.com>"
            SubjectTemplate = "[My App] New notifications"
            SendLimitPerSecond = 1
            Today = DateTime.Today
        }
    let ump = Emailer.ump config
    // AKA decorator pattern in OO
    let loggedPerform effect =
        config.Logger.Debug("effect {@Effect}", box effect)
        async {
            let! result = ump.Perform effect
            config.Logger.Debug("result {@Result}", box result)
            return result
        }
    // later declaration of ump shadows previous one
    // helps my simple brain not have to remember 2nd variable
    let ump = { ump with Perform = loggedPerform }

    Ump.run ump settings
    |> Async.RunSynchronously
    |> fun output -> config.Logger.Debug("output {@Output}", box output)

#if DEBUG
    Console.WriteLine("Press ENTER to exit...")
    Console.ReadLine() |> ignore
#endif
    0 // return an integer exit code
