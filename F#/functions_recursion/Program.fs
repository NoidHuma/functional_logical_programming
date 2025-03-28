open System


// task 2
type SolveResult =
    None
    | Linear of float
    | Quadratic of float*float

let solve a b c = 
    let D = b * b - 4. * a * c
    if a = 0. then
        if b = 0. then None
        else Linear(-c / b)
    else
        if D < 0 then None
        else Quadratic(((-b + sqrt(D)) / (2. * a), (-b - sqrt(D)) / (2. * a)))


// task 3
let circleArea r = 
    Math.PI * r * r

let multiplyAreaHeight area height =
    area * height

let cylinderVolumeSuperPos =
    circleArea >> multiplyAreaHeight

let cylinderVolumeCurr r height =
    (circleArea r) * height


// task 4
let rec sumDigitsUp num =
    if num = 0 then 0
    else (num % 10) + sumDigitsUp(num / 10)


// task 5
let rec sumDigitsTailHelper num acc =
    if num = 0 then acc
    else sumDigitsTailHelper (num / 10) (acc + (num % 10))

let rec sumDigitsTail num =
    sumDigitsTailHelper num 0


[<EntryPoint>]
let main argv = 
    Console.Write("Введите число: ")
    let number = Console.ReadLine() |> int
    Console.WriteLine($"Сумма цифр (рекурсия вверх): {sumDigitsUp number}")
    Console.WriteLine($"Сумма цифр (хвостовая рекурсия): {sumDigitsTail number}")


    0
