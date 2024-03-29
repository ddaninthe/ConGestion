module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }

    test "A Request in the past cannot be created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2010, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2010, 12, 27); HalfDay = PM }
      }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Error "The request starts in the past") "The request should not have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated by a manager" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }

    test "A request is refused by a manager" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (DenyRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestDenied request]) "The request should have been canceled"
    }

    test "A request cannot be denied by another user" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }  

      let user = Employee "janedoe"

      Given [ RequestCreated request ]
      |> ConnectedAs user
      |> When (DenyRequest ("jdoe", request.RequestId))
      |> Then (Error "Unauthorized : Employee should be the same") "Error should have been thrown"
    }
  ]
[<Tests>]
let cancelationByEmployeeTests = 
  testList "Cancelation by employee tests" [
    test "A request starting in the future is canceled by an employee" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } 
      }  

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (CancelRequestByEmployee request)
      |> Then (Ok [RequestCanceledByUser request]) "The request should have been canceled"
    }

    test "I try to cancel a request as Manager" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } 
      }  

        Given [ RequestCreated request ]
        |> ConnectedAs Manager
        |> When (CancelRequestByEmployee request)
        |> Then (Error "Cannot cancel as Employee when the user is a Manager") "Error should have been thrown"
    }
    test "I try to cancel a request in past" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(1992, 12, 27); HalfDay = AM }
        End = { Date = DateTime(1992, 12, 27); HalfDay = PM } 
      }  

        Given [ RequestCreated request ]
        |> ConnectedAs (Employee "jdoe")
        |> When (CancelRequestByEmployee request)
        |> Then (Error "Cannot cancel a request in past") "Error should have been thrown"
    }  

    test "I try to cancel a validated request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } 
      }  

        Given [ RequestValidated request ]
        |> ConnectedAs (Employee "jdoe")
        |> When (CancelRequestByEmployee request)
        |> Then (Ok [RequestCanceledByUser request]) "The request should have been canceled"
    }

    test "I try to cancel a validated request where the start date is equal to today" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime.Now; HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } 
      }  

        Given [ RequestValidated request ]
        |> ConnectedAs (Employee "jdoe")
        |> When (CancelRequestByEmployee request)
        |> Then (Error "Cannot cancel request where the start date is equal to today") "Error should have been thrown"
    }  
  ]

[<Tests>]
let cancelationByManagerTests = 
  testList "Cancelation by manager tests" [
    test "A validated request is canceled by a manager" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When (CancelRequestByManager request)
      |> Then (Ok [RequestCanceledByManager request]) "The request should have been canceled"
    }

    test "A denied request is canceled by a manager" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ RequestDenied request ]
      |> ConnectedAs Manager
      |> When (CancelRequestByManager request)
      |> Then (Ok [RequestCanceledByManager request]) "The request should have been canceled"
    }
  ]

[<Tests>]
let cancelAskedTests = // Tests demande d'annulation
  testList "Cancel Asked tests" [
    test "Cancel a created request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (WantCancelRequest (request.UserId, request.RequestId))
      |> Then (Ok [CancelWanted request]) "The cancelation should have been asked"
    }

    test "Cancel a validated request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (WantCancelRequest (request.UserId, request.RequestId))
      |> Then (Ok [CancelWanted request]) "The cancelation should have been asked"
    }

    test "A Request in the past cannot be canceled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2010, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2010, 12, 27); HalfDay = PM }
      }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (WantCancelRequest (request.UserId, request.RequestId))
      |> Then (Error "It's too late to cancel this request") "The request should not have been canceled"
    }
  ]

[<Tests>]
let denyCancelTests =
  testList "Deny Cancel Request tests" [
    test "A cancel request can be denied" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
      }

      Given [ CancelWanted request ]
      |> ConnectedAs (Manager)
      |> When (DenyCancelRequest (request.UserId, request.RequestId))
      |> Then (Ok [CancelRequestDenied request]) "The cancel request should have been denied"
    }

    test "A created request cannot be cancel denied" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
      }

      Given [ RequestCreated request ]
      |> ConnectedAs (Manager)
      |> When (DenyCancelRequest (request.UserId, request.RequestId))
      |> Then (Error "Cannot denied this request") "The cancel request should not have been denied"
    }
  ]