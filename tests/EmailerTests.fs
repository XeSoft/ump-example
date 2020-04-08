namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Ump

open Emailer

[<TestClass>]
type EmailerTests () =

    let today = DateTime(2019, 1, 1)
    let now = DateTimeOffset(2019, 1, 1, 12, 34, 56, TimeSpan.Zero)
    let nextSend = DateTimeOffset(2019, 1, 1, 12, 34, 57, TimeSpan.Zero)

    let test = Ump.createTest Emailer.init (Result.bindUpdate Emailer.update)
    
    let settings : Emailer.Settings =
        {
            SendFrom = "whatever"
            SubjectTemplate = "whatever"
            SendLimitPerSecond = 10
            Today = today
        }

    let settings1ps =
        { settings with SendLimitPerSecond = 1 }

    let model : Emailer.Model =
        {
            Settings = settings
            CurrentTime = DateTimeOffset.MinValue
            ToSend = []
            ToMark = Set.empty
        }

    let model1ps = 
        { model with Settings = settings1ps }

    let dueItems =
        [
            { ItemId = 1; Email = "asdf"; NotificationHtml = "foo" }
            { ItemId = 2; Email = "asdf"; NotificationHtml = "bar" }
        ]

    let dueItems2 =
        dueItems @
        [
            { ItemId = 3; Email = "fdsa"; NotificationHtml = "baz" }
        ]

    let email =
        {
            From = settings.SendFrom
            Subject = settings.SubjectTemplate
            To = "asdf"
            Body = "&bull; foo<br>&bull; bar"
            Completes = [1; 2]
        }

    let email2 =
        { email with To = "fdsa"; Body = "&bull; baz"; Completes = [3]}

    let toSend = [ [ email ] ]

    let toSend2 = [ [ email2 ] ]

    [<TestMethod>]
    member __.``init - finds new items`` () =
        let msgs = []
        let expected = Ok model, [ FindDueItems today ]
        expected |> equals (test settings msgs)

    [<TestMethod>]
    member __.``due items - error`` () =
        let msgs = [DueItems (Error ())]
        let expected = Error (), []
        expected |> equals (test settings msgs)

    [<TestMethod>]
    member __.``email sent - error`` () =
        let msgs = [EmailSent (Error ())]
        let expected = Error (), []
        expected |> equals (test settings msgs)

    [<TestMethod>]
    member __.``marked- error`` () =
        let msgs = [Marked (Error ())]
        let expected = Error (), []
        expected |> equals (test settings msgs)

    [<TestMethod>]
    member __.``due items - batches and schedules sending now`` () =
        let msgs = [DueItems (Ok dueItems)]
        let expected = Ok { model with ToSend = toSend }, [ ScheduleSendNow ]
        expected |> equals (test settings msgs)

    [<TestMethod>]
    member __.``time to send - keeps track of items to mark and sends emails`` () =
        let msgs = [DueItems (Ok dueItems); TimeToSend now ]
        let expected = Ok { model with CurrentTime = now; ToMark = Set.ofList [1; 2] }, [ SendEmail email ]
        expected |> equals (test settings msgs)

    [<TestMethod>]
    member __.``email sent - marks items`` () =
        let msgs = [DueItems (Ok dueItems); TimeToSend now; EmailSent (Ok [1; 2]) ]
        let expected = Ok { model with CurrentTime = now }, [ MarkComplete [1; 2] ]
        expected |> equals (test settings msgs)

    [<TestMethod>]
    member __.``items marked - early exit when nothing left`` () =
        let msgs = [DueItems (Ok dueItems); TimeToSend now; EmailSent (Ok [1; 2]); Marked (Ok ()) ]
        let expected = Ok { model with CurrentTime = now }, []
        expected |> equals (test settings msgs)

    [<TestMethod>]
    member __.``items marked - schedule next send`` () =
        let msgs = [DueItems (Ok dueItems2); TimeToSend now; EmailSent (Ok [1; 2]); Marked (Ok ()) ]
        let expected = Ok { model1ps with ToSend = toSend2; CurrentTime = now }, [ScheduleSend nextSend]
        expected |> equals (test settings1ps msgs)




