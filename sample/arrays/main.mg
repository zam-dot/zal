@c {
    #include <stdio.h>
}

func main() {
    arr := [1, 2, 3, 4, 5]
    for i, v in arr {
        if v > 2 && v < 5 {  // Using new && operator
            printf("Value %d at index %d\n", v, i)
        }
    }
}
