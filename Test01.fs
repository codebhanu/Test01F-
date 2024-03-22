module Test01

//defining list with float to avaoid type mismatch letetr in the code

let givenSalaries =
    [ 75000.0
      48000.0
      120000.0
      190000.0
      300113.0
      92000.0
      36000.0 ]

//Filtering the salaries that are above 100000
let salariesmorethan1lakh =
    givenSalaries
    |> List.filter (fun x -> x > 100000.0)

// Calculating the tax based on the given table
let salarywithtax salaryamount =
    match salaryamount with
    | _ when salaryamount <= 49020.0 -> salaryamount * 0.15
    | _ when salaryamount <= 98040.0 -> salaryamount * 0.205
    | _ when salaryamount <= 151978.0 -> salaryamount * 0.26
    | _ when salaryamount <= 216511.0 -> salaryamount * 0.29
    | _ -> salaryamount * 0.33

// Calculate tax for all salaries
let taxesOfgivenSalaries = givenSalaries |> List.map salarywithtax

// Adjust salaries less than $49,020 by adding $20,000
let changedsalries =
    givenSalaries
    |> List.map (fun x -> if x < 49020.0 then x + 20000.0 else x)

// Filter salaries between $50,000 and $100,000 and sum them
let totalsalriesbetween50Kand100k =
    givenSalaries
    |> List.filter (fun x -> x >= 50000.0 && x <= 100000.0)
    |> List.reduce (fun acc x -> acc + x)
// Print results
printfn "Salaries that are More than 100k: %A" salariesmorethan1lakh
printfn "Taxes: %A" taxesOfgivenSalaries
printfn "Changed Salaries for those who are earning less than $49,020: %A" changedsalries
printfn "Total Salaries  Between $50,000 and $100,000: %f" totalsalriesbetween50Kand100k

let totalofMultipleofThree max =
    // Define a tail-recursive helper function
    let rec assistingSum current acc =
        if current > max then
            acc
        else
            assistingSum (current + 3) (acc + current)
    //  we are providing helper for the main funciton
    assistingSum 3 0

// Example usage
let result = totalofMultipleofThree 27
printfn "The sum of all multiples of 3 up to 27 is: %d" result
