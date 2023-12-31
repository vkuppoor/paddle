#ifndef VALUES_H
#define VALUES_H

#include <stdint.h>
#include <inttypes.h>

typedef struct {
  int type_tag;  // Corresponds to i32 in LLVM IR
  int64_t value; // Corresponds to i64 in LLVM IR
} llvm_val_t;

typedef llvm_val_t val_t;

typedef enum type_t {
  T_INVALID = -1,
  /* immediates */
  T_INT,
  T_BOOL,
} type_t;

/* return the type of x */
type_t val_typeof(val_t *x);

/**
 * Wrap/unwrap values
 *
 * The behavior of unwrap functions are undefined on type mismatch.
 */
int64_t val_unwrap_int(val_t *x);
val_t val_wrap_int(int64_t i);

int val_unwrap_bool(val_t *x);
val_t val_wrap_bool(int b);

#endif
