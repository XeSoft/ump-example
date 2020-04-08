namespace Ump

type Ump<'initArg, 'Model, 'Effect, 'Msg, 'Output> =
    {
        Init : 'initArg -> 'Model * 'Effect list
        Update : 'Msg -> 'Model -> 'Model * 'Effect list
        Perform : 'Effect -> Async<'Msg>
        Output : 'Model -> 'Output
    }


module Result =

    let bindUpdate updatef msg result =
        match result with
        | Ok model ->
            updatef msg model
        | Error err ->
            Error err, []


module Ump =

    [<AutoOpen>]
    module Internal =

        [<Struct>]
        // struct - per iteration: 1 stack allocation + 1 frame copy
        type ProgramState<'Model, 'Effect, 'Msg> =
            {
                Model : 'Model
                Effects : 'Effect list
                Msgs : 'Msg list
            }


        // Msgs are processed before Effects.
        // Msgs are run sequentially.
        // Effects are run in parallel.
        // In practice, Program.Update will return one Effect at a time when it needs to run them sequentially.
        let rec runLoop program state =
            match state.Effects, state.Msgs with
            | [], [] ->
                async.Return (program.Output state.Model)
            | _, msg :: nMsgs ->
                let (nModel, effects) = program.Update msg state.Model
                let nState =
                    {
                        Model = nModel
                        Effects = List.append state.Effects effects
                        Msgs = nMsgs
                    }
                runLoop program nState
            | _, [] ->
                async {
                    let effectsAsync = List.map program.Perform state.Effects
                    let! nMsgsArr = Async.Parallel effectsAsync
                    let nState =
                        {
                            Model = state.Model
                            Effects = []
                            Msgs = List.ofArray nMsgsArr
                        }
                    return! runLoop program nState
                }


    /// Runs a program using the provided initial argument.
    /// The returned Model is the final state of the Model when the program exited.
    /// Infinite loops are possible when Update generates Effects on every iteration.
    /// This allows the program to support interactive applications, for example.
    let run (program: Ump<'initArg, 'Model, 'Effect, 'Msg, 'Output>) (initArg: 'initArg) =
        let (model, effects) = program.Init initArg
        let state =
            {
                Model = model
                Msgs = []
                Effects = effects
            }
        runLoop program state


    /// Creates a test function from the init and update functions.
    /// The returned function takes initArg and msgs and returns
    /// a model and effect list that you can test against expected values.
    let createTest init update =
        let test initArg msgs =
            let init = init initArg
            // ignore previous effects
            let update (model, _) msg =
                update msg model
            msgs
            |> List.fold update init
        test

