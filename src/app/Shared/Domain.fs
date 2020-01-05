namespace TimeOff

open System

// First, we define our domain
type UserId = string

type User =
    | Employee of UserId
    | Manager

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}
with 
    static member Now = 
        {
            Date = DateTime.Today
            HalfDay = if DateTime.Now.Hour <= 12 then AM else PM
        }
    static member Compare boundary boundary2 =
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
    static member Days (boundary: Boundary) (boundary2: Boundary) =
        (boundary.Date - boundary2.Date).TotalDays //TODO: Prends pas encore en compte le HalfDay
        |> abs

[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

[<CLIMutable>]
type TimeOffDetails = {
    UserId: UserId
    AttribueTimeOff: float
    LastYearTimeOff: float
    TakenToDateTimeOff: float
    PlannedTimeOff: float
    CurrentBalance: float
}