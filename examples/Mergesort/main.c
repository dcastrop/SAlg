#include "Mergesort.h"
#include <inttypes.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

vec_int_t merge(pair_int_pair_vec_int_vec_int_t in){
  vec_int_t out;
  int *tmp = (int *)malloc(in.fst * sizeof(int));
  out = in.snd.fst;
  for (int z = 0; z < in.fst; z++) {
    if (in.snd.fst.size > 0 && in.snd.snd.size > 0) {
      if (in.snd.fst.elems[0] <= in.snd.snd.elems[0]) {
        tmp[z] = in.snd.fst.elems[0];
        in.snd.fst.size--;
        in.snd.fst.elems++;
      } else {
        tmp[z] = in.snd.snd.elems[0];
        in.snd.snd.size--;
        in.snd.snd.elems++;
      }
    }
    else if (in.snd.fst.size > 0) {
      tmp[z] = in.snd.fst.elems[0];
      in.snd.fst.size--;
      in.snd.fst.elems++;
    }
    else if (in.snd.snd.size > 0) {
      tmp[z] = in.snd.snd.elems[0];
      in.snd.snd.size--;
      in.snd.snd.elems++;
    }
  }
  out.size = in.fst;
  for (int r=0; r < in.fst; r++) {
    out.elems[r] = tmp[r];
  }
  free(tmp);
  return out;
}

void usage(const char *nm){
  printf("Usage: %s <input_size>\n", nm);
  exit(-1);
}

void test(size_t sz){
  printf("Testing...");
  int tmp[sz];
  printf("Done\n");
  return;
}

int main(int argc, const char *argv[]) {
  vec_int_t in;
  if (argc <= 1) {
    usage(argv[0]);
  }
  char *endptr = NULL;
  errno = 0;
  size_t size = strtoimax(argv[1],&endptr,10);
  if (errno != 0) {
    printf("%s", strerror(errno));
    usage(argv[0]);
  }
  if (endptr != NULL && *endptr != 0) {
    usage(argv[0]);
  }

  //test(size);
  in.elems = (int *)calloc(size, sizeof(int));
  in.size = size;

  srandom(time(NULL));

  for (int i = 0; i < size; i++) {
    in.elems[i] = (int)random() % size;
  }

  vec_int_t out = parMsort(in);
  //for (int i = 0; i < out.size; i++) {
  //  printf("%d ", out.elems[i]);
  //}
  //printf("\n");
  free(in.elems);
}
