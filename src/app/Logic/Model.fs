namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | DenyRequest of UserId * Guid
    | CancelRequestByEmployee of UserId * TimeOffRequest // Requête utilisé pour qu'un employé annule sa requête
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | DenyRequest (userId, _) -> userId
        | CancelRequestByEmployee (_, request) -> request.UserId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestDenied of TimeOffRequest
    | RequestCanceledByUser of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestDenied request -> request
        | RequestCanceledByUser request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest 
        | Canceled of TimeOffRequest
        | Denied of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Canceled request -> request
            | Denied request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true
            | Canceled _ -> false
            | Denied _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestDenied request -> Canceled request
        | RequestCanceledByUser request -> Canceled request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith request1 request2 =
        let compare boundary boundary2 =
            let value = DateTime.Compare(boundary.Date, boundary2.Date)
            match value with
            | 0 -> 
                if boundary.HalfDay = AM && boundary2.HalfDay = PM then
                    -1
                elif boundary.HalfDay = PM && boundary2.HalfDay = AM then
                    1
                else
                    0
            | _ -> value

        not (request1.Start > request2.End || request1.End < request2.Start)

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        otherRequests
        |> Seq.exists (overlapsWith request)

    let createRequest activeUserRequests request date =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= date then
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

                createRequest activeUserRequests request DateTime.Today

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
            | DenyRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    denyRequest requestState
            | CancelRequestByEmployee (_, request) ->
                if user = Manager then
                    Error "Unauthorized"
                else 
                    if request.End.Date < DateTime.Now then
                        Error "Error: Cannot cancel a request in past"
                    else 
                        let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                        cancelRequest requestState                    
