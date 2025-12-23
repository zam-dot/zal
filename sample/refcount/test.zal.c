
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

// Deep release for arrays of RC objects (like strings)
static inline void rc_release_array(void *ptr, void (*release_element)(void *)) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);
    if (--header->refcount == 0) {
        if (release_element && header->array_count > 0) {
            void **elements = (void **)ptr;
            for (size_t i = 0; i < header->array_count; i++) {
                if (elements[i]) {
                    release_element(elements[i]);
                    elements[i] = NULL; // Set to NULL after release
                }
            }
        }
        free(header);
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

static inline void rc_release(void *ptr) {
    if (!ptr) return;
    RCHeader *header = RC_GET_HEADER(ptr);
    if (--header->refcount == 0) {     // [cite: 7]
        if (header->weak_count == 0) { // [cite: 7]
            free(header);              // [cite: 7]
        } else {
            header->refcount = 0; // [cite: 8]
        }
    }
}


int main() {
    char *s = rc_string_new("test");
    int  *a = rc_new_array(int, 3);
    a[0] = 1;
    a[1] = 2;
    a[2] = 3;
    char **sa = rc_new_array(char *, 2);
    sa[0] = rc_string_new("x");
    sa[1] = rc_string_new("y");
    printf("Testing...\n");

    // Block scope cleanup
    if (s) rc_release(s);
    if (a) rc_release(a);
    if (sa) rc_release_array(sa, (void (*)(void *))rc_release);
    return 0;
}
