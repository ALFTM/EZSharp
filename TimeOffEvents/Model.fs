namespace TimeOff

open System
open EventStorage

type User =
    | Employee of int
    | Manager

type HalfDay = | AM | PM

type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

type UserId = int

type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid 
    | RefuseRequest of UserId * Guid
    | CancelByEmployeeRequest of UserId * Guid
    | AskingForCancellationRequest of UserId * Guid 
    | RefuseCancelationRequest of UserId * Guid 
    | CancelByManagerRequest of UserId * Guid with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId

type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest 
    | RequestCancelledByEmployee of TimeOffRequest 
    | RequestAskingForCancellation of TimeOffRequest
    | RequestCancelledByManager of TimeOffRequest with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestRefused request -> request
        | RequestCancelledByEmployee request -> request
        | RequestCancelledByManager request -> request
        | RequestAskingForCancellation request -> request

module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest 
        | Refused of TimeOffRequest
        | CancelledByEmployee of TimeOffRequest 
        | CancelledByManager of TimeOffRequest
        | AskingForCancellation of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Refused request -> request
            | CancelledByEmployee request -> request
            | CancelledByManager request -> request
            | AskingForCancellation request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true
            | Refused _ -> false
            | CancelledByEmployee _ -> false
            | CancelledByManager _ -> false
            | AskingForCancellation _ -> false

    let evolve _ event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestRefused request -> Refused request
        | RequestCancelledByEmployee request -> CancelledByEmployee request
        | RequestCancelledByManager request -> CancelledByManager request
        | RequestAskingForCancellation request -> AskingForCancellation request

    let getRequestState events =
        events |> Seq.fold evolve NotCreated

    let getAllRequests events =
        let folder requests (event: RequestEvent) =
            let state = defaultArg (Map.tryFind event.Request.RequestId requests) NotCreated
            let newState = evolve state event
            requests.Add (event.Request.RequestId, newState)

        events |> Seq.fold folder Map.empty

    let overlapWithAnyRequest (previousRequests: TimeOffRequest seq) request =
        false //TODO

    let createRequest previousRequests request =
        if overlapWithAnyRequest previousRequests request then
            Error "Overlapping request"
        elif request.Start.Date <= DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"        

    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | Refused _ ->
            Error "State is already refused"
        | _ ->
            Error "Request cannot be validated"  

    let cancelByEmployeeRequest requestState =
        match requestState with
            | PendingValidation request
            | Validated request ->
                if requestState.Request.Start.Date > DateTime.Now then
                    Ok [RequestCancelledByEmployee request]
                else
                    Error "Already started TimeOff"
            | _ ->
                Error "Request cannot be cancelled"

    let askingForCancellationRequest requestState =
            match requestState with
            | Validated request ->
                if requestState.Request.Start.Date < DateTime.Now ||
                   requestState.Request.Start.Date.Equals  DateTime.Now then
                   Ok [RequestAskingForCancellation request]
                else
                    Error "Request cannot be cancelled"
            | PendingValidation _ ->
                Error "TimeOff is in pending"
            | _ ->
                Error "Request cannot be cancelled"

    let refuseCancelationRequest requestState =
        match requestState with
        | AskingForCancellation request ->
            Ok [RequestValidated request]
        | CancelledByManager _ ->
            Error "Already cancelled by Manager"        
        | _ ->
            Error "Cannot refuse cancellation"

    let cancelByManagerRequest (requestState: RequestState) =
        if requestState.IsActive then
            Ok [RequestCancelledByManager requestState.Request]
        else
            Error "Request cannot be cancelled"


    let handleCommand (store: IStore<UserId, RequestEvent>) (command: Command) =
        let userId = command.UserId
        let stream = store.GetStream userId
        let events = stream.ReadAll()
        let userRequests = getAllRequests events

        match command with
        | RequestTimeOff request ->
            let activeRequests =
                userRequests
                |> Map.toSeq
                |> Seq.map (fun (_, state) -> state)
                |> Seq.where (fun state -> state.IsActive)
                |> Seq.map (fun state -> state.Request)

            createRequest activeRequests request

        | ValidateRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            validateRequest requestState 

        | RefuseRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            refuseRequest requestState

        | AskingForCancellationRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            askingForCancellationRequest requestState

        | RefuseCancelationRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            refuseCancelationRequest requestState

        | CancelByManagerRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            cancelByManagerRequest requestState

        | CancelByEmployeeRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            cancelByEmployeeRequest requestState