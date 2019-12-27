module Ump

type Ump<'initArg, 'resumeArg, 'Model, 'Effect, 'Msg, 'Output> =
    {
        Init : 'initArg -> 'Model * 'Effect list
        Update : 'Msg -> 'Model -> 'Model * 'Effect list
        Perform : 'Effect -> Async<'Msg>
        Output : 'Model -> 'Output
        ResumeMsg : ('resumeArg -> 'Msg) option
    }


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
    let run (program: Ump<'initArg, 'resumeArg, 'Model, 'Effect, 'Msg, 'Output>) (initArg: 'initArg) =
        let (model, effects) = program.Init initArg
        let state =
            {
                Model = model
                Msgs = []
                Effects = effects
            }
        runLoop program state


    /// Resumes a program from a previous state.
    /// The returned Model is the final state of the Model when the program exited.
    /// Infinite loops are possible when Update generates Effects on every iteration.
    /// This allows the program to support interactive applications, for example.
    let resume (program: Ump<'initArg, 'resumeArg, 'Model, 'Effect, 'Msg, 'Model>) (resumeArg: 'resumeArg) (lastModel: 'Model) =
        match program.ResumeMsg with
        | None ->
            async.Return lastModel
        | Some resumeMsg ->
            let state =
                {
                    Model = lastModel
                    Msgs = [resumeMsg resumeArg]
                    Effects = []
                }
            runLoop program state


module Result =

    let bindUpdate updatef msg result =
        match result with
        | Ok model ->
            updatef msg model
        | Error err ->
            Error err, []

