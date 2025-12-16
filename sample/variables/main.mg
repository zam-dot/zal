@c {
    #include <stdio.h>
    #include <stdbool.h>
}

func main() {
    a := "initial"
    print("%s\n", a)

    b := 12
    c := 13
    print("%d %d\n", b, c)

    d := true
    print("%d\n",d)

    e := nil         // var e int 
    print("%p\n", e) // fmt.Println(e)

    f := "apple"
    print("%s\n", f)
}
