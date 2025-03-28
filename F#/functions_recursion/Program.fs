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


[<EntryPoint>]
let main argv = 
    Console.WriteLine("Task 2")
    let res = solve 4 -24 36
    match res with
         None -> Console.WriteLine("Нет решений")
         | Linear(x) -> Console.WriteLine($"Линейное уравнение, корень: {x}.")
         | Quadratic(x1, x2) when x1 = x2 -> Console.WriteLine($"Квадратичное уравнение, 1 корень: {x1}.")
         | Quadratic(x1, x2) -> Console.WriteLine($"Квадратичное уравнение, 2 корня: {x1}, {x2}.")
    
    Console.WriteLine("Task 3")
    Console.Write("Радиус = ")
    let radius = Console.ReadLine() |> float
    Console.Write("Высота = ")
    let height = Console.ReadLine() |> float
    Console.WriteLine($"Площадь круга: {circleArea radius}")
    Console.WriteLine($"Объем цилиндра (суперпозиция): {cylinderVolumeSuperPos radius height}")
    Console.WriteLine($"Объем цилиндра (каррирование): {cylinderVolumeCurr radius height}")


    0
