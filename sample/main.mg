@c { #include <stdio.h> }

func main() {
    @c {
        var a = 20;
    }
    print("a: %d\n", a)
   
    var sum = a + b
    var diff = a - b
    var product = a * b
    var quotient = a / b
    
    print("Sum: %d\n", sum)
    print("Difference: %d\n", diff)
    print("Product: %d\n", product)
    print("Quotient: %d\n", quotient)
    
    if product > 80 {
        print("Product is greater than 80\n")
    } else if quotient < 10 {
        print("Quotient is less than 10\n")
    }
}
