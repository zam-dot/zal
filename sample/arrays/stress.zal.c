
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


#ifndef ARENA_H
#define ARENA_H
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint8_t *buffer;
    size_t   offset;
    size_t   capacity;
} Arena;

typedef struct {
    size_t size; // Store array size!
} ArenaArrayHeader;

#define ARENA_ARRAY_HEADER_SIZE sizeof(ArenaArrayHeader)
#define ARENA_GET_HEADER(ptr) ((ArenaArrayHeader *)((char *)(ptr) - ARENA_ARRAY_HEADER_SIZE))

static inline void *arena_alloc_with_size(Arena *a, size_t size, size_t array_size) {
    if (size == 0) return NULL;

    size_t total_size = ARENA_ARRAY_HEADER_SIZE + size;
    size_t aligned_size = (total_size + 7) & ~7;

    if (a->offset + aligned_size <= a->capacity) {
        void             *ptr = &a->buffer[a->offset + ARENA_ARRAY_HEADER_SIZE];
        ArenaArrayHeader *header = (ArenaArrayHeader *)&a->buffer[a->offset];
        header->size = array_size;
        a->offset += aligned_size;
        return ptr;
    }
    return NULL;
}

static inline void *arena_alloc_array_with_size(Arena *a, size_t elem_size, size_t count) {
    size_t total_size = elem_size * count;
    void  *ptr = arena_alloc_with_size(a, total_size, count);
    if (ptr) {
        memset(ptr, 0, total_size);
    }
    return ptr;
}

static inline size_t arena_array_len(void *ptr) {
    if (!ptr) return 0;
    return ARENA_GET_HEADER(ptr)->size;
}

// Keep old functions for backward compatibility
static inline void *arena_alloc(Arena *a, size_t size) { return arena_alloc_with_size(a, size, 0); }

static inline void *arena_alloc_array(Arena *a, size_t elem_size, size_t count) {
    return arena_alloc_array_with_size(a, elem_size, count);
}

static inline void  arena_reset(Arena *a) { a->offset = 0; }
static inline char *arena_string_new(Arena *a, const char *str) {
    if (!str) return NULL;
    size_t len = strlen(str);
    char  *result = (char *)arena_alloc_with_size(a, len + 1, len + 1);
    if (result) {
        strcpy(result, str);
    }
    return result;
}
static inline Arena arena_init_dynamic(size_t capacity) {
    uint8_t *buffer = (uint8_t *)calloc(1, capacity);
    if (!buffer) {
        fprintf(stderr, "ERROR: Failed to allocate %zu bytes for arena\n", capacity);
        exit(1);
    }
    return (Arena){.buffer = buffer, .offset = 0, .capacity = capacity};
}
static inline void arena_free(Arena *a) {
    if (a->buffer) {
        free(a->buffer);
        a->buffer = NULL;
    }
    a->capacity = 0;
    a->offset = 0;
}
static inline Arena arena_init(void *backing_buffer, size_t capacity) {
    return (Arena){.buffer = (uint8_t *)backing_buffer, .offset = 0, .capacity = capacity};
}
#endif


static Arena global_arena;

void stress_test() {
    printf("Starting Arena stress test...\n");
    for (int k = 0; k <= 10000; k++) {
        int **temp_matrix = (int **)arena_alloc_array(&global_arena, sizeof(int *), 3);
        temp_matrix[0] = (int *)arena_alloc_array(&global_arena, sizeof(int), 3);
        temp_matrix[0][0] = k;
        temp_matrix[0][1] = k + 1;
        temp_matrix[0][2] = k + 2;
        temp_matrix[1] = (int *)arena_alloc_array(&global_arena, sizeof(int), 3);
        temp_matrix[1][0] = k + 3;
        temp_matrix[1][1] = k + 4;
        temp_matrix[1][2] = k + 5;
        temp_matrix[2] = (int *)arena_alloc_array(&global_arena, sizeof(int), 3);
        temp_matrix[2][0] = k + 6;
        temp_matrix[2][1] = k + 7;
        temp_matrix[2][2] = k + 8;
        int sum = 0;
        for (int i = 0; i <= arena_array_len(temp_matrix) - 1; i++) {
            for (int j = 0; j <= RC_GET_HEADER(temp_matrix[i])->array_count - 1; j++) {
                sum = sum + temp_matrix[i][j];
            }
        }
        if (k % 1000 == 0) {
            printf("Processed %d matrices...\n", k);
        }
    }
    printf("Test complete. Check your system monitor!\n");
}
int main() {
    // Initialize arena
    global_arena = arena_init_dynamic(10485760);
    stress_test();
    // Clean up arena
    arena_free(&global_arena);
    return 0;
}
