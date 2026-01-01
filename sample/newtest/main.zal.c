#include <stdio.h>
#ifndef ZAL_ARENA_H
#define ZAL_ARENA_H
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
typedef struct {
    size_t refcount;
    size_t weak_count;
    size_t array_count;
} RCHeader;
#define RC_HEADER_SIZE sizeof(RCHeader)
#define RC_GET_HEADER(ptr) ((RCHeader *)((char *)(ptr) - RC_HEADER_SIZE))
#define ZAL_RELEASE(ptr)                                                                           \
    do {                                                                                           \
        rc_release(ptr);                                                                           \
        ptr = NULL;                                                                                \
    } while (0)
static inline void rc_weak_retain(void *ptr) {
    if (ptr) {
        RCHeader *header = RC_GET_HEADER(ptr);
        header->weak_count++;
    }
}
static inline void *rc_alloc(size_t size) {
    RCHeader *header = (RCHeader *)calloc(1, RC_HEADER_SIZE + size);
    if (header) {
        header->refcount = 1;
        header->weak_count = 0;
        header->array_count = 0; // Default
    }
    return header ? (char *)header + RC_HEADER_SIZE : NULL;
}
static inline void *rc_alloc_array(size_t elem_size, size_t count) {
    RCHeader *header = (RCHeader *)calloc(1, sizeof(RCHeader) + (elem_size * count));
    if (header) {
        header->refcount = 1;
        header->weak_count = 0;
        header->array_count = count; // <--- Store it here!
        memset((char *)header + RC_HEADER_SIZE, 0, elem_size * count);
    }
    return header ? (char *)header + RC_HEADER_SIZE : NULL;
}
static inline void rc_release(void *ptr) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);

    if (--header->refcount == 0) {
        if (header->weak_count == 0) {
            free(header);
        }
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
static inline void rc_release_array(void *ptr, void (*destructor)(void *)) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);
    if (--header->refcount == 0) {
        if (destructor) {
            void **array = (void **)ptr;
            for (size_t i = 0; i < header->array_count; i++) {
                destructor(array[i]);
            }
        }
        if (header->weak_count == 0) {
            free(header);
        }
    }
}
static inline void rc_weak_release(void *ptr) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);
    if (--header->weak_count == 0) {
        if (header->refcount == 0) {
            free(header);
        }
    }
}
#endif


typedef struct {
    int id;
    int age;
} Person;

typedef struct {
    Person person;
    double salary;
    char  *department;
} Employee;

int main() {
    Employee emp = {
        .person = {.id = 1001, .age = 30}, .salary = 75000.0, .department = "Engineering"};
    printf("Employee %d:\n", emp.person.id);
    printf("  Age: %d\n", emp.person.age);
    printf("  Salary: $%.2f\n", emp.salary);
    printf("  Dept: %s\n", emp.department);
    return 0;
}
