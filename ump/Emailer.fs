module Emailer

open System


type Settings =
    {
        SendFrom: string
        SubjectTemplate: string
        SendLimitPerSecond: int
        Today: DateTime
    }


type DueItem =
    {
        ItemId: int
        Email: string
        NotificationHtml: string
    }

type Email =
    {
        From: string
        To: string
        Subject: string
        Body: string
        Completes: int list
    }

type Effect =
    | FindDueItems of DateTime
    | ScheduleSendNow
    | ScheduleSend of DateTimeOffset
    | SendEmail of Email
    | MarkComplete of int list

type Msg =
    | DueItems of Result<DueItem list, unit>
    | TimeToSend of DateTimeOffset
    | EmailSent of Result<int list, unit>
    | Marked of Result<unit, unit>

type Model =
    {
        Settings: Settings
        CurrentTime: DateTimeOffset
        ToSend: Email list list
        ToMark: Set<int>
    }


module Logic =

    let toEmail settings (email, groupedItems) =
        {
            From = settings.SendFrom
            Subject = settings.SubjectTemplate
            To = email
            Body =
                groupedItems
                |> List.map (fun item -> "&bull; " + item.NotificationHtml)
                |> String.concat "<br>"
            Completes = groupedItems |> List.map (fun item -> item.ItemId)
        }


    let batch settings items =
        items
        |> List.groupBy (fun item -> item.Email)
        |> List.map (toEmail settings)
        |> List.chunkBySize settings.SendLimitPerSecond


let init settings =
    Ok {
        Settings = settings
        CurrentTime = DateTimeOffset.MinValue
        ToSend = []
        ToMark = Set.empty
    }
    , [ FindDueItems settings.Today ]


let update msg model =
    match msg with
    | DueItems (Ok items) ->
        let toSend = Logic.batch model.Settings items
        Ok { model with ToSend = toSend }
        , [ ScheduleSendNow ]
    | TimeToSend now ->
        let model = { model with CurrentTime = now }
        match model.ToSend with
        | [] ->
            Ok model, []
        | batch :: remaining ->
            let toMark =
                batch
                |> List.map (fun x -> x.Completes)
                |> List.concat
                |> Set.ofList
            Ok { model with ToSend = remaining; ToMark = toMark }
            , [ for email in batch do SendEmail email ]
    | EmailSent (Ok itemIds) ->
        let items = Set.ofList itemIds
        Ok { model with ToMark = Set.difference model.ToMark items }
        , [ MarkComplete itemIds ]
        // marking complete after sending is "at least once" delivery
        // before would be "at most once"
    | Marked (Ok ()) ->
        let effects =
            if Set.isEmpty model.ToMark && not (List.isEmpty model.ToSend) then
                let nextSend = model.CurrentTime.AddSeconds(1.0)
                [ ScheduleSend nextSend ]
            else
                []
        Ok model
        , effects

    | DueItems (Error ())
    | EmailSent (Error ())
    | Marked (Error ()) ->
        Error (), []


open Serilog
open Ump

type EffectConfig =
    {
        Logger: ILogger
        // this is also where you would put things like:
        // connection strings
        // endpoint urls
        // credentials
        // anything else your side effects need
    }


let perform config effect =
    let mkItem itemId email notification =
        { ItemId = itemId; Email = email; NotificationHtml = notification }
    match effect with
    | ScheduleSendNow ->
        async { return TimeToSend DateTimeOffset.Now }
    | ScheduleSend time ->
        let span = time - DateTimeOffset.Now
        let sleepTime = Math.Ceiling(span.TotalMilliseconds) |> int
        async {
            do! Async.Sleep sleepTime
            return TimeToSend DateTimeOffset.Now
        }
    | FindDueItems date ->
        async {
            do! Async.Sleep 20 // simulate latency
            return DueItems (
                Ok [
                    mkItem 1234 "paper@asdf.com" "Scissors liked your post."
                    mkItem 1233 "paper@asdf.com" "Rock downvoted your post."
                    mkItem 2345 "trogdor@asdf.com" "Villagers downvoted your post."
                ]
            )
        }
    | SendEmail email ->
        async {
            do! Async.Sleep 50
            return EmailSent (Ok email.Completes)
        }
    | MarkComplete itemIds ->
        async {
            do! Async.Sleep 10
            return Marked (Ok ())
        }


let ump config =
    {
        Init = init
        Update = Result.bindUpdate update
        Perform = perform config
        Output = id
        ResumeMsg = None
    }

