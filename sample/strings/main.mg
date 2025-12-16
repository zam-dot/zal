@c {
    #include <stdio.h>
}

func main() {
    s := "hello"
    for i, v in s {
        print("%d: %c\n", i, v)
    }
}
