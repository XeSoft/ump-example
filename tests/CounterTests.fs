namespace tests

open DecrementCounter
open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open Ump


[<TestClass>]
type CounterTests () =
    let test = Ump.createTest init (Result.bindUpdate update)

    // test data
    let counterId = new Guid("9E6F6552-DEA9-4D56-AEAB-08EE5EBD54D3")
    let request =
        {
            CounterId = counterId
            Amount = 12
        }
    let model = { Request = request }

    let tests =
        [   // init, msgs, expected model, expected effects
            ( "loads state"
            , request
            , []
            , Ok model
            , [LoadState counterId]
            )
            ( "saves state"
            , request
            , [StateLoaded (Ok (Some 13))]
            , Ok model
            , [SaveState (counterId, 1)]
            )
        ]


    [<TestMethod>]
    member __.``DecrementCounter - loads state`` () =
        let init, msgs = request, []
        let expected = Ok model, [LoadState counterId]
        expected |> equals (test init msgs)

    [<TestMethod>]
    member __.``DecrementCounter tests`` () =
        for (name, init, msgs, model, effects) in tests do
            printfn "%s" name
            let expected = model, effects
            expected |> equals (test init msgs)

    [<TestMethod>]
    member __.``DecrementCounter - failing test`` () =
        let init, msgs = request, []
        let expected = Error CounterNotFound, []
        expected |> equals (test init msgs)

    [<TestMethod>]
    member __.``DecrementCounter - prevent negative`` () =
        let msg = StateLoaded (Ok (Some 0))
        let expected = Error CounterWouldGoNegative, []
        expected |> equals (update msg model)