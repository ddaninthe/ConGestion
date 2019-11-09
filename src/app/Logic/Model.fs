namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | DenyRequest of UserId * Guid
    | WantCancelRequest of UserId * Guid  // Demande d'annulation
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | DenyRequest (userId, _) -> userId
        | WantCancelRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestDenied of TimeOffRequest
    | CancelWanted of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestDenied request -> request
        | CancelWanted request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest 
        | CanceledAsked of TimeOffRequest // Demande d'annulation
        | Canceled of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Canceled request -> request
            | CanceledAsked request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | CanceledAsked _
            | Validated _ -> true
            | Canceled _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestDenied request -> Canceled request
        | CancelWanted request -> CanceledAsked request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith request1 request2 =
        not (request1.Start > request2.End || request1.End < request2.Start)

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        otherRequests
        |> Seq.exists (overlapsWith request)

    let createRequest activeUserRequests request boundary =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start <= boundary then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let denyRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestDenied request]
        | _ ->
            Error "Request cannot be canceled"

    let wantCancel requestState =
        match requestState with
        //| ValidatedByManager // TODO
        | PendingValidation request ->
            if request.Start <= Boundary.Now then
                Error "It's too late to cancel this request"
            else
                Ok [CancelWanted request]
        | _ -> 
            Error "Not yet implemented" // TODO


    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized : Employee should be the same"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request Boundary.Now

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState

            | DenyRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                denyRequest requestState
            | WantCancelRequest (_, requestId) ->
                if user <> Manager then
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    wantCancel requestState
                else
                    Error "Not yet implemented"
                    // TODO: Annulée par manager
