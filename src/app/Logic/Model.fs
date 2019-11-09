namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | DenyRequest of UserId * Guid
    | WantCancelRequest of UserId * Guid  // Demande d'annulation
    | CancelRequestByEmployee of UserId * TimeOffRequest // Requête utilisé pour qu'un employé annule sa requête
    | CancelRequestByManager of UserId * TimeOffRequest // Requête utilisé pour qu'un manager annule une requête
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | DenyRequest (userId, _) -> userId
        | WantCancelRequest (userId, _) -> userId
        | CancelRequestByEmployee (_, request) -> request.UserId
        | CancelRequestByManager (_, request) -> request.UserId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestDenied of TimeOffRequest
    | CancelWanted of TimeOffRequest
    | RequestCanceledByUser of TimeOffRequest
    | RequestCanceledByManager of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestDenied request -> request
        | CancelWanted request -> request
        | RequestCanceledByUser request -> request
        | RequestCanceledByManager request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest 
        | CanceledAsked of TimeOffRequest // Demande d'annulation
        | Canceled of TimeOffRequest
        | Denied of TimeOffRequest 
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Canceled request -> request
            | CanceledAsked request -> request
            | Denied request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | CanceledAsked _
            | Validated _ -> true
            | Canceled _ -> false
            | Denied _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | CancelWanted request -> CanceledAsked request
        | RequestDenied request -> Denied request
        | RequestCanceledByUser request -> Canceled request
        | RequestCanceledByManager request -> Canceled request

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
            Error "Request cannot be denied"

    let cancelRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCanceledByUser request]
        | Validated request ->
            Ok [RequestCanceledByUser request]        
        | _ ->
            Error "Request cannot be canceled" 

    let cancelRequestByManager requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCanceledByManager request]
        | Validated request ->
            Ok [RequestCanceledByManager request]
        | Denied request ->
            Ok [RequestCanceledByManager request]        
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
            Error "Not yet implemented" // TODO: 


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
            | CancelRequestByEmployee (_, request) ->
                if user = Manager then
                    Error "Unauthorized"
                else 
                    if request.End.Date < DateTime.Now then
                        Error "Error: Cannot cancel a request in past"
                    else if request.Start.Date.Day = DateTime.Now.Day then
                        Error "Error: Cannot cancel request where the start date is equal to today"                    
                    else 
                        let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                        cancelRequest requestState   
            | CancelRequestByManager (_, request) ->
                if request.End.Date < DateTime.Now then
                    Error "Error: Cannot cancel a request in past"
                else 
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    cancelRequestByManager requestState
            | WantCancelRequest (_, requestId) ->
                if user <> Manager then
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    wantCancel requestState
                else
                    Error "Not yet implemented"
                    // TODO: Annulée par manager ?                                 
