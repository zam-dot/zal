@c {
    #include <stdio.h>
}

func main() {
    i := 5

    switch (i) {
        case 0, 2, 4, 5: print("7 is even\n")
        case 1: print("7 is odd\n")
        default: print("shit\n")
    }
}
