[<AutoOpen>]
module Common

open Microsoft.VisualStudio.TestTools.UnitTesting

let equals expected actual =
    if expected = actual then
        ()
    else
        let msg = sprintf "\r\nActual\r\n------\r\n%A\r\n\r\nExpected\r\n--------\r\n%A" expected actual
        Assert.Fail(msg)
