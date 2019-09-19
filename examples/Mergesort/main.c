#include "Mergesort.h"

vec_int_t merge(pair_vec_int_vec_int_t in){
  vec_int_t out;
  int sz = in.fst.size + in.snd.size;
  int *tmp = (int *)malloc(sz * sizeof(int));
  int i = 0;
  int j = 0;
  for (int z = 0; z < sz; z++) {
    if (i < in.fst.size && j < in.snd.size) {
      if (in.fst.elems[i] <= in.snd.elems[j]) {
        tmp[z] = in.fst.elems[i++];
      } else {
        tmp[z] = in.snd.elems[j++];
      }
    }
    else if (i < in.fst.size) tmp[z] = in.fst.elems[i++];
    else if (j < in.snd.size) tmp[z] = in.snd.elems[j++];
  }
  out = in.fst;
  out.size = sz;
  for (int r=0; r < sz; r++) {
    out.elems[r] = tmp[r];
  }
  free (tmp);
  return out;
}

int main(void) {
  vec_int_t in;
  int inv[] = {3,1,2,31,4,1,4,5,6,1,6,1,2,1,2,3,4,5,1,5,77,5,2,4};
  in.elems = (int *)inv;
  in.size = sizeof(inv) / sizeof(int);

  vec_int_t out = parMsort(in);
  for (int i = 0; i < out.size; i++) {
    printf("%d ", out.elems[i]);
  }
  printf("\n");
}
