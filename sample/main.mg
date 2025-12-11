@c { #include <stdio.h> }

func add(a: int, b: int) int { return a + b }
func sub(a: int, b: int) int { return a - b }
func mul(a: int, b: int) int { return a * b }
func div(a: int, b: int) int { return a / b }

const PI = 3.14

func main() {
    print("addition = %d\n", add(32, 2))
    print("subtraction = %d\n", sub(32, 2))
    print("multiplication = %d\n", mul(32, 2))
    print("division = %d\n", div(32, 2))

    for (var i = 0; i < 10; i = i + 1) {
        print("i = %d\n", i)
    }
    @c {
        for (int i = 0; i < 10; i++) {
            printf("i = %d\n", i);
        }
    }

    var i = 9
    if (i % 2 == 0) {
        print("i \% 2 = %d\n", i % 2)
    } else {
        print("i \% 2 = %d\n", i % 2)
    }
    print("number = %.2lf\n", PI)

    const myName: string = "HOla Pedor"
    print("myName = %s\n", myName)
}
