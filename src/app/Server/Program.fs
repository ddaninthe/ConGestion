module ServerCode.App

open TimeOff
open Storage.Events

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks

// ---------------------------------
// Handlers
// ---------------------------------

module HttpHandlers =

    open Microsoft.AspNetCore.Http

    [<CLIMutable>]
    type UserAndRequestId = {
        UserId: UserId
        RequestId: Guid
    }

    let requestTimeOff (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! timeOffRequest = ctx.BindJsonAsync<TimeOffRequest>()
                let command = RequestTimeOff timeOffRequest
                let result = handleCommand command
                match result with
                | Ok _ -> return! json timeOffRequest next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let validateRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindJsonAsync<UserAndRequestId>()
                let command = ValidateRequest (userAndRequestId.Result.UserId, userAndRequestId.Result.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestValidated timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let denyRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindJsonAsync<UserAndRequestId>()
                let command = DenyRequest (userAndRequestId.Result.UserId, userAndRequestId.Result.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestDenied timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let wantCancelRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindJsonAsync<UserAndRequestId>()
                let command = WantCancelRequest (userAndRequestId.Result.UserId, userAndRequestId.Result.RequestId)
                let result = handleCommand command
                match result with
                | Ok [CancelWanted timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let cancelRequestByEmployee (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! timeOffRequest = ctx.BindJsonAsync<TimeOffRequest>()
                let command = CancelRequestByEmployee timeOffRequest
                let result = handleCommand command
                match result with
                | Ok [RequestCanceledByUser timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let cancelRequestByManager (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! timeOffRequest = ctx.BindJsonAsync<TimeOffRequest>()
                let command = CancelRequestByManager timeOffRequest
                let result = handleCommand command
                match result with
                | Ok [RequestCanceledByManager timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let denyCancelRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindJsonAsync<UserAndRequestId>()
                let command = DenyCancelRequest (userAndRequestId.Result.UserId, userAndRequestId.Result.RequestId)
                let result = handleCommand command
                match result with
                | Ok [CancelRequestDenied timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let currentBalance (handleCurrentBalance: UserId -> seq<RequestEvent>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! userId = ctx.BindJsonAsync<UserId>()
                let result = handleCurrentBalance userId

                // Completer les 5 fonctions ci dessous
                let cumulTimeOff = 20.0
                let lastYearTimeOff = 0.0
                let takenTimeOff = 
                    Seq.choose(fun x ->
                                    match x with
                                    | RequestValidated req -> Some(req)
                                    | _ -> None) result
                    |> Seq.filter(fun x -> 
                        let firstJanuaryCurrentYear = {
                            Date = new DateTime(Boundary.Now.Date.Year, 01, 01)
                            HalfDay = AM
                        }
                        let afterBeginingYear = Boundary.Compare x.Start firstJanuaryCurrentYear
                        let beforeToday = Boundary.Compare x.Start Boundary.Now

                        afterBeginingYear > 0 && beforeToday < 0
                    )
                    |> Seq.map(fun x -> Boundary.Days x.Start x.End)
                    |> Seq.sum

                let plannedTimeOff =
                    Seq.choose(fun x ->
                                    match x with
                                    | RequestValidated req -> Some(req)
                                    | _ -> None) result
                    |> Seq.filter(fun x -> 
                        let lastDayCurrentYear = {
                            Date = new DateTime(Boundary.Now.Date.Year, 12, 31)
                            HalfDay = PM
                        }
                        let beforeEndYear = Boundary.Compare x.Start lastDayCurrentYear
                        let afterToday = Boundary.Compare x.Start (Boundary.Now)

                        beforeEndYear < 0 && afterToday > 0
                    )
                    |> Seq.map(fun x -> Boundary.Days x.Start x.End)
                    |> Seq.sum
                    
                let currentBalance = 0.0

                let finalResult = {
                    UserId = userId
                    AttribueTimeOff = cumulTimeOff
                    LastYearTimeOff = lastYearTimeOff
                    TakenToDateTimeOff = takenTimeOff
                    PlannedTimeOff = plannedTimeOff
                    CurrentBalance = currentBalance
                }

                return! json finalResult next ctx
            }

// ---------------------------------
// Web app
// ---------------------------------

let webApp (eventStore: IStore<UserId, RequestEvent>) =
    let handleCommand (user: User) (command: Command) =
        let userId = command.UserId

        let eventStream = eventStore.GetStream(userId)
        let state = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty

        // Decide how to handle the command
        let result = Logic.decide state user command

        // Save events in case of success
        match result with
        | Ok events -> eventStream.Append(events)
        | _ -> ()

        // Finally, return the result
        result

    let handleCurrentBalance(userId: UserId): seq<RequestEvent> =
        let eventStream = eventStore.GetStream(userId)
        let items = eventStream.ReadAll()
        eventStream.ReadAll()

    choose [
        subRoute "/api"
            (choose [
                route "/users/login" >=> POST >=> Auth.Handlers.login
                subRoute "/timeoff"
                    (Auth.Handlers.requiresJwtTokenForAPI (fun user ->
                        choose [
                            POST >=> route "/request" >=> HttpHandlers.requestTimeOff (handleCommand user)
                            POST >=> route "/validate-request" >=> HttpHandlers.validateRequest (handleCommand user)
                            POST >=> route "/deny-request" >=> HttpHandlers.denyRequest(handleCommand user)
                            POST >=> route "/want-cancel-request" >=> HttpHandlers.wantCancelRequest(handleCommand user)
                            POST >=> route "/cancel-request-by-employee" >=> HttpHandlers.cancelRequestByEmployee(handleCommand user)
                            POST >=> route "/cancel-request-by-manager" >=> HttpHandlers.cancelRequestByManager(handleCommand user)
                            POST >=> route "/deny-cancel-request" >=> HttpHandlers.denyCancelRequest(handleCommand user)
                            GET >=> route "/current-balance" >=> HttpHandlers.currentBalance(handleCurrentBalance)
                        ]
                    ))
            ])
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (eventStore: IStore<UserId, RequestEvent>) (app: IApplicationBuilder) =
    let webApp = webApp eventStore
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder: ILoggingBuilder) =
    let filter (l: LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()

    //let eventStore = InMemoryStore.Create<UserId, RequestEvent>()
    let storagePath = System.IO.Path.Combine(contentRoot, "./.storage", "userRequests")
    let eventStore = FileSystemStore.Create<UserId, RequestEvent>(storagePath, sprintf "%s")

    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp eventStore))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0