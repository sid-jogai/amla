#include <stdbool.h>  // bool
#include <inttypes.h> // PRId64, etc.
#include <stdint.h>   // int64_t, etc.
#include <stdio.h>    // printf

char *
bool_string(bool x)
{
	return x ? "true" : "false";
}
