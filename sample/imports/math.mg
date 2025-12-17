const PI = 3.14159
const E = 2.71828
const GRAVITY = 9.81

func square(x: float): float {
    return x * x
}

func cube(x: float): float {
    return x * x * x
}

func sqrt(x: float): float {
    @c {
        return sqrt(x);
    }
}
