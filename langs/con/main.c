#include "values.h"
#include <stdio.h>
#include <stdlib.h>
#include "print.h"

val_t *entry(); // entry returns a pointer to val_t

int main(int argc, char **argv) {
  val_t *result_ptr = entry(); // Store the returned pointer in result_ptr

  if (result_ptr != NULL) {
    print_result(result_ptr);
  } else {
    printf("Null pointer received from entry function\n");
  }

  // Free the allocated memory if it's no longer needed
  free(result_ptr);

  putchar('\n');
  return 0;
}
// #include <stdio.h>
// #include "values.h"
// #include "print.h"

// val_t entry();

// int main(int argc, char** argv)
// {
//   val_t result;

//   result = entry();
//   printf("%" PRId64, result.value);
//   // print_result(result);
//   putchar('\n');
//   return 0;
// }