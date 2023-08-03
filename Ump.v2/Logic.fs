namespace Ump

type Logic<'initArg, 'model, 'effect, 'msg> =
    {
        init: 'initArg -> 'model * 'msg
        update: 'model -> 'msg -> 'model * 'effect list
    }


module Logic =

    /// Creates a test function from init and update.
    /// The returned function takes initArg and msgs and returns
    /// a model and effect list that you can test against expected values.
    let createTest logic =
        let test initArg msgs =
            let init = logic.init initArg
            // ignore previous effects
            let update (model, _) msg =
                logic.update model msg
            msgs
            |> List.fold update init
        test

