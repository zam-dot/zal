@c {
    #include <stdio.h>
}

struct Person {
    id: int
    age: int
}

struct Employee {
    person: Person  
    salary: float
    department: string
}

func main() {
    emp := Employee{
        person: Person{
            id: 1001,
            age: 30
        },
        salary: 75000.0,
        department: "Engineering"
    }
    
    print("Employee %d:\n", emp.person.id)
    print("  Age: %d\n", emp.person.age)
    print("  Salary: $%.2f\n", emp.salary)
    print("  Dept: %s\n", emp.department)
    
    emp.person.age = 31
    emp.salary = 80000.0
    
    print("After raise: %d years old, $%.2f\n", 
           emp.person.age, emp.salary)
}
