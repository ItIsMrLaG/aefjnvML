#include <stdio.h>
#include <stdint.h>

#define REMOVE_INT_TAG(integer) (((integer) >> 1) << 1)

#define rt_(name) (rt_##name) // add specific runtime prefix

void rt_(print_int)(int64_t value) {
    printf("%lld\n", value);
}

int64_t rt_(add_int)(int64_t a, int64_t b) {
    return a + b;
}

int64_t rt_(mul_int)(int64_t a, int64_t b) {
    return a * b;
}

int64_t rt_(div_int)(int64_t a, int64_t b) {
    return a / b;
}

int64_t rt_(minus_int)(int64_t a, int64_t b) {
    return a - b;
}

struct rt_closure
{
    int64_t args_n;
    int64_t applied_args;
    int64_t* args;
    int64_t (*code)();
};

// TODO: mb something for pattern matching (block of functions + start with [int] and [names])

// (--> use var_args and  <--)

// TODO: ptr(int) = closure_eval()

// TODO: ptr(int) = closure_create()

// TODO: closure_add_args()

// TODO: ptr(int) = list_create([el], [tail])

// TODO: tuple_create([el])

