module DecrementCounter

open System

type Request =
    {
        CounterId: Guid
        Amount: int
    }

type Error =
    | LoadFailed of message: string
    | CounterNotFound
    | CounterWouldGoNegative
    | SaveFailed of message: string

type Effect =
    | LoadState of counterId: Guid
    | SaveState of counterId: Guid * count: int

type Msg =
    | StateLoaded of Result<int option, string>
    | StateSaved of Result<unit, string>

type Model =
    {
        Request: Request
    }


let init request =
    Ok { Request = request }
    , [LoadState request.CounterId]


let update msg model =
    match msg with
    | StateLoaded (Error s) ->
        Error (LoadFailed s), []
    | StateLoaded (Ok None) ->
        Error CounterNotFound, []
    | StateLoaded (Ok (Some oldCount)) ->
        let count = oldCount - model.Request.Amount
        if count < 0 then
            Error CounterWouldGoNegative, []
        else
            Ok model
            , [SaveState (model.Request.CounterId, count)]
    | StateSaved (Error s) ->
        Error (SaveFailed s), []
    | StateSaved (Ok ()) ->
        Ok model, []


type EffectConfig =
    {
        ExampleConfig: string
        // other items such as:
        // connection strings
        // endpoint URLs
        // loggers
    }


let perform config effect =
    match effect with
    | LoadState counterId ->
        // simulate db call
        async {
            let rand = new Random()
            do! Async.Sleep 30
            let count = rand.Next(0, 100)
            return StateLoaded (Ok (Some count))
        }
    | SaveState (counterId, count) ->
        async {
            do! Async.Sleep 30
            return StateSaved (Ok ())
        }


open Ump


let toUmp config =
    {
        Init = init
        Update = Result.bindUpdate update
        Perform = perform config
        Output = Result.map ignore
    }


