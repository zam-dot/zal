@c {
    #include <stdio.h>
    #include <stdlib.h>
}

func main() {
    defer print("World\n")
    print("Hello\n")
}
