module TimeOff.Tests

open Expecto
open EventStorage

let Given events = events
let When command events = events, command
let Then expected message (events: RequestEvent list, command) =
    let store = InMemoryStore.Create<UserId, RequestEvent>()
    for event in events do
      let stream = store.GetStream event.Request.UserId
      stream.Append [event]
    let result = Logic.handleCommand store command
    Expect.equal result expected message

open System

let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime.Now.AddDays(-1.); HalfDay = AM }
        End = { Date = DateTime.Now.AddDays(1.); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (ValidateRequest (1, Guid.Empty))
      |> Then (Ok [RequestValidated request]) "The request has been validated"
    }
  ]

let refusalTests =
  testList "Refusal tests" [
    test "A request is refused" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime.Now.AddDays(-1.); HalfDay = AM }
        End = { Date = DateTime.Now.AddDays(1.); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (RefuseRequest (1, Guid.Empty))
      |> Then (Ok [RequestRefused request]) "The request has been refused"
    }
  ]

let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime.Now.AddDays(-1.); HalfDay = AM }
        End = { Date = DateTime.Now.AddDays(1.); HalfDay = PM } }

      Given [ ]
      |> When (RequestTimeOff (request))
      |> Then (Ok [RequestCreated request]) "The request has been created"
    }
  ]

let cancelTests =
  testList "Cancellation tests" [
    test "A pending request is cancelled by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime.Now.AddDays(-1.); HalfDay = AM }
        End = { Date = DateTime.Now.AddDays(1.); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (CancelByManagerRequest (1, Guid.Empty))
      |> Then (Ok [RequestCancelledByManager request]) "The pending request has been cancelled"
    }
  ]

let askCancelTests =
  testList "Ask cancel tests" [
    test "Validated requests with start in past can be asked cancel" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime.Now.AddDays(-1.); HalfDay = AM }
        End = { Date = DateTime.Now.AddDays(1.); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> When (AskingForCancellationRequest (1, Guid.Empty))
      |> Then (Ok [RequestAskingForCancellation request]) "The validated has been asked cancelation"
    }
  ]

let tests =
  testList "All tests" [
    validationTests
    refusalTests
    creationTests
    cancelTests
    askCancelTests
  ]