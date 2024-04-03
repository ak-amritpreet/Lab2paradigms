// Define the Coach record
type Coach = { Name: string; FormerPlayer: bool }

// Define the Stats record
type Stats = { Wins: int; Losses: int }

// Define the Team record
type Team = { Name: string; Coach: Coach; Stats: Stats }

// Create function to calculate success percentage
let successPercentage (stats: Stats) =
    let wins = float stats.Wins
    let losses = float stats.Losses
    (wins / (wins + losses)) * 100.0

// Define the list of teams
let teams =
    [
        { Name = "Boston Celtics"; Coach = { Name = "Joe Mazzulla"; FormerPlayer = true }; Stats = { Wins = 57; Losses = 15 } }
        { Name = "Chicago Bulls"; Coach = { Name = "Billy Donovan"; FormerPlayer = false }; Stats = { Wins = 34; Losses = 38 } }
        { Name = "Golden State Warriors"; Coach = { Name = "Steve Kerr"; FormerPlayer = true }; Stats = { Wins = 37; Losses = 34 } }
        { Name = "Minnesota Timberwolves"; Coach = { Name = "Chris Finch"; FormerPlayer = false }; Stats = { Wins = 49; Losses = 22 } }
        { Name = "Philadelphia 76ers"; Coach = { Name = "Nick Nurse"; FormerPlayer = false }; Stats = { Wins = 39; Losses = 33 } }
    ]

// Filter the list of successful teams
let successfulTeams =
    teams
    |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)

// Map the list of successful teams to include success percentage
let successful_TeamsWith_SuccessPercentage =
    successfulTeams
    |> List.map (fun team -> { team with Stats = { team.Stats with Wins = int (successPercentage team.Stats) } })

// Output
printfn "List of successful teams:"
successful_TeamsWith_SuccessPercentage |> List.iter (fun team ->
    printfn "Team: %s, Success Percentage: %.2f%%" team.Name (successPercentage team.Stats))

printfn""
printfn""
printfn "Valentine's Budget"

printfn""
printfn""

// Define the Cuisine discriminated union
type Food =
    | Korean
    | Turkish

// Define the MovieType discriminated union
type Movie_Type =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

// Define the Activity discriminated union
type Activity =
    | BoardGame
    | Chill
    | Movie of Movie_Type
    | Restaurant of Food
    | LongDrive of int * float

// Define the function to calculate the budget
let calculateAmount (activity: Activity) =
    match activity with
    | BoardGame -> 0.0
    | Chill -> 0.0
    | Movie movieType ->
        match movieType with
        | Regular -> 12.0
        | IMAX -> 17.0
        | DBOX -> 20.0
        | RegularWithSnacks -> 12.0 + 5.0
        | IMAXWithSnacks -> 17.0 + 5.0
        | DBOXWithSnacks -> 20.0 + 5.0
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (kilometres, fuelChargePerKm) -> float kilometres * fuelChargePerKm

// Example usage for every possible activity
let activity1 = BoardGame
let activity2 = Chill
let activity3 = Movie Regular
let activity4 = Movie IMAX
let activity5 = Movie DBOX
let activity6 = Movie RegularWithSnacks
let activity7 = Movie IMAXWithSnacks
let activity8 = Movie DBOXWithSnacks
let activity9 = Restaurant Korean
let activity10 = Restaurant Turkish
let activity11 = LongDrive (100, 0.1)

// Output
printfn "Budget for Playing a Board Game: %.2f CAD" (calculateAmount activity1)
printfn "Budget for Chilling Out: %.2f CAD" (calculateAmount activity2)
printfn "Budget for Movie (Regular): %.2f CAD" (calculateAmount activity3)
printfn "Budget for Movie (IMAX): %.2f CAD" (calculateAmount activity4)
printfn "Budget for Movie (DBOX): %.2f CAD" (calculateAmount activity5)
printfn "Budget for Movie (Regular with Snacks): %.2f CAD" (calculateAmount activity6)
printfn "Budget for Movie (IMAX with Snacks): %.2f CAD" (calculateAmount activity7)
printfn "Budget for Movie (DBOX with Snacks): %.2f CAD" (calculateAmount activity8)
printfn "Budget for Restaurant (Korean): %.2f CAD" (calculateAmount activity9)
printfn "Budget for Restaurant (Turkish): %.2f CAD" (calculateAmount activity10)
printfn "Budget for Long Drive (100 km, fuel charge 0.1 CAD/km): %.2f CAD" (calculateAmount activity11)
