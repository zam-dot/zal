@include "math_lib.mg"
@c { #include <stdio.h> }
const PI = 3.14159

struct Person {
    age: int
    weight: int
} 

func main() {
    var p: Person = Person{
        age = 25, 
        weight = 42
    }
    if p.age == 30 {
        print("Age is 30\n")
    } else if p.age == 25 {
        print("Age is 25\n")
    } else {
        print("Age is not 30 or 25\n")
    }

    print("\nPI: %.2f\n\n", PI)

    var numss: double = (23 * 3) * (34 * 0.4)
    print("numss: %.2f\n", numss)

    print("Age: %d\n", p.age)
    print("Weight: %d\n", p.weight)
    print("\nFloat division: %.2f\n\n", divide(100.0, 3.0))

    var myNumbers = [1, 2, 3, 4, 5]

    for (var i = 0; i < 5; i = i + 1) {
        print("Array[%d]: %d\n", i, myNumbers[i])
    }
}
