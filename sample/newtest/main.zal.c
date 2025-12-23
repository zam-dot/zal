#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    size_t refcount;
    size_t weak_count;
    size_t array_count; // <--- The missing link
} RCHeader;

#define RC_HEADER_SIZE sizeof(RCHeader)
#define RC_GET_HEADER(ptr) ((RCHeader *)((char *)(ptr) - RC_HEADER_SIZE))
#define ZAL_RELEASE(ptr)                                                                           \
    do {                                                                                           \
        rc_release(ptr);                                                                           \
        ptr = NULL;                                                                                \
    } while (0)

// Increments the weak reference count
static inline void rc_weak_retain(void *ptr) {
    if (ptr) {
        RCHeader *header = RC_GET_HEADER(ptr);
        header->weak_count++;
    }
}

static inline void *rc_alloc(size_t size) {
    RCHeader *header = (RCHeader *)malloc(RC_HEADER_SIZE + size);
    if (header) {
        header->refcount = 1;
        header->weak_count = 0;
        header->array_count = 0; // Default
    }
    return header ? (char *)header + RC_HEADER_SIZE : NULL;
}

static inline void *rc_alloc_array(size_t elem_size, size_t count) {
    RCHeader *header = (RCHeader *)malloc(RC_HEADER_SIZE + (elem_size * count));
    if (header) {
        header->refcount = 1;
        header->weak_count = 0;
        header->array_count = count; // <--- Store it here!
        memset((char *)header + RC_HEADER_SIZE, 0, elem_size * count);
    }
    return header ? (char *)header + RC_HEADER_SIZE : NULL;
}

// Strong release: Kills the object data, but keeps header if weak pointers exist
static inline void rc_release(void *ptr) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);

    if (--header->refcount == 0) {
        // Here you would normally call a destructor for the object's contents
        // if this was a complex struct rather than just a string.

        if (header->weak_count == 0) {
            free(header);
        }
        // If weak_count > 0, we leave the header allocated so weak pointers
        // can still check (refcount == 0) to know the object is dead.
    }
}


#define rc_new_array(type, count) (type *)rc_alloc_array(sizeof(type), count)
#define rc_string_new(str)                                                                         \
    ({                                                                                             \
        const char *_s = (str);                                                                    \
        size_t      _len = _s ? strlen(_s) : 0;                                                    \
        char       *_d = (char *)rc_alloc(_len + 1);                                               \
        if (_d) {                                                                                  \
            strcpy(_d, _s);                                                                        \
        }                                                                                          \
        _d;                                                                                        \
    })

static inline void rc_retain(void *ptr) {
    if (ptr) {
        RCHeader *header = RC_GET_HEADER(ptr);
        header->refcount++; // [cite: 6]
    }
}

// Weak release: The "Cleanup Crew"
static inline void rc_weak_release(void *ptr) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);

    if (--header->weak_count == 0) {
        // If the object is already dead and this was the last weak reference,
        // the header is finally no longer needed.
        if (header->refcount == 0) {
            free(header);
        }
    }
}


#define PI 3.14159
typedef struct {
    int id;
    int age;
} Person;

typedef struct {
    Person person;
    double salary;
    char  *department;
} Employee;

typedef enum { RED, GREEN } Colors;

void loop(void) {
    int i = 1;
    while (i <= 3) {
        printf("first loop: %d\n", i);
        i = i + 1;
    }
    for (int j = 1; j <= 3; j++) {
        printf("second loop: %d\n", j);
    }
    for (int n = 0; n <= 6; n++) {
        if (n == 0) {
            printf("third...\n");
        }
        printf("third loop: %d\n", n);
    }
}
int main() {
    char *str = rc_string_new("hello world");
    printf("%s\n", str);
    int *arr = rc_new_array(int, 5);
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;
    arr[3] = 4;
    arr[4] = 5;
    for (int _i = 0; _i < RC_GET_HEADER(arr)->array_count; _i++) {
        int i = arr[_i];
        printf("%d\n", i);
    }
    printf("%d\n", GREEN);
    const int MAX_SIZE = 100;
    printf("%d\n", MAX_SIZE);
    Employee tei = {
        .person = {.id = 1001, .age = 30}, .salary = 75000.0, .department = "Engineering"};
    printf("Employee %d:\n", tei.person.id);
    printf("Age: %d\n", tei.person.age);
    printf("Salary: $%.2f\n", tei.salary);
    printf("Dept: %s\n", tei.department);
    loop();
    int i = 5;
    switch (i) {
        case 0:
            // fallthrough
        case 2:
            // fallthrough
        case 4:
            // fallthrough
        case 5:  printf("7 is even\n"); break;
        case 1:  printf("7 is odd\n"); break;
        default: printf("shit\n"); break;
    }

    // Block scope cleanup
    if (str) rc_release(str);
    if (arr) rc_release(arr);
    return 0;
}
