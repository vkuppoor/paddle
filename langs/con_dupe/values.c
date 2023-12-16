#include "values.h"
#include "types.h"

type_t val_typeof(val_t *x)
{
  if (x->type_tag == TAG_INT)
    return T_INT;
  if (x->type_tag == TAG_BOOL)
    return T_BOOL;
  
  return T_INVALID;
}

int64_t val_unwrap_int(val_t *x)
{
  return x->value;
}
val_t val_wrap_int(int64_t i)
{
  val_t wrapped_int = {TAG_INT, i};
  return wrapped_int;
}
int val_unwrap_bool(val_t *x)
{
  return x->value;
}
val_t val_wrap_bool(int b)
{
  val_t wrapped_int = {TAG_BOOL, b ? val_true : val_false};

  return wrapped_int;
}