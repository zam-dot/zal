
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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


void simpleTest() {
    char *s = rc_string_new("test");
    int  *a = rc_new_array(int, 3);
    a[0] = 1;
    a[1] = 2;
    a[2] = 3;
    char **sa = rc_new_array(char *, 1);
    sa[0] = rc_string_new("x");
    printf("Testing...\n");

    // Block scope cleanup
    if (s) rc_release(s);
    if (a) rc_release(a);
    if (sa) rc_release_array(sa, (void (*)(void *))rc_release);
}
void nestingTest() {
    char *s = rc_string_new("outer");
    if (true) {
        char **tmp = rc_new_array(char *, 2);
        tmp[0] = rc_string_new("inner1");
        tmp[1] = rc_string_new("inner2");
        printf("%s\n", tmp[0]);
        // Block scope cleanup
        if (tmp) rc_release_array(tmp, (void (*)(void *))rc_release);
    }
    printf("%s\n", s);

    // Block scope cleanup
    if (s) rc_release(s);
}
void test() { printf("hello"); }
int  main() {
    simpleTest();
    nestingTest();
    for (int i = 0; i <= 5; i++) {
        char *msg = rc_string_new("looping");
        printf("%s\n", msg);
        // Block scope cleanup
        if (msg) rc_release(msg);
    }
    test();
    return 0;
}
