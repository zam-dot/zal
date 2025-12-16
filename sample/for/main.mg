@c { #include <stdio.h> }

func main() {
    i := 1
    for i <= 3 {
        print("first loop: %d\n", i)
        i = i + 1
    }
    
     for j in 1..3 {
         print("second loop: %d\n", j)
     }

   for n in 0..6 {
       if n == 0 {
           print("third...\n")
       }
       print("third loop: %d\n", n)
   }
}
