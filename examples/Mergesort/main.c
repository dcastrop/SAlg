#include "Mergesort.h"

either_vec_int_pair_vec_int_vec_int_t split(vec_int_t in){
  either_vec_int_pair_vec_int_vec_int_t out;
  if (in.size <= 1) {
    out.tag = Inl;
    out.val.inl = in;
  } else {
    out.tag = Inr;
    vec_int_t in1;
    vec_int_t in2;
    in2.size = in.size - in1.size;
    in2.elems = in.elems + in1.size;
    out.val.inr.fst.size = in.size / 2;
    out.val.inr.fst.elems = in.elems;
    out.val.inr.snd.size = in.size - out.val.inr.fst.size;
    out.val.inr.snd.elems = in.elems + out.val.inr.fst.size;
  }
  return out;
}

vec_int_t do_merge(int sz, vec_int_t in1, vec_int_t in2){
  vec_int_t out;
  int tmp[sz];
  int i = 0;
  int j = 0;
  int k = 0;
  while (i < in1.size && j < in2.size) {
    if (in1.elems[i] <= in2.elems[j]) {
      tmp[k++] = in1.elems[i++];
    } else {
      tmp[k++] = in2.elems[j++];
    }
  }
  while (i < in1.size) tmp[k++] = in1.elems[i++];
  while (j < in2.size) tmp[k++] = in2.elems[j++];
  out = in1;
  out.size = sz;
  for (int r=0; r < sz; r++) {
    out.elems[r] = tmp[r];
  }
  return out;
}

vec_int_t merge(either_vec_int_pair_vec_int_vec_int_t in){
  vec_int_t out;
  switch (in.tag) {
    case Inl:
      { out = in.val.inl;
        break;
      }
    case Inr:
      { int sz = in.val.inr.fst.size + in.val.inr.snd.size;
        out = do_merge(sz, in.val.inr.fst, in.val.inr.snd);
      }
  }
  return out;
}

int main(void) {
  vec_int_t in;
  int inv[] = {3,1,2,31,4,1,4,5,6,1,6,1,2,1,2,3,4,5,1,5,77,5,2,4};
  in.elems = (int *)inv;
  in.size = sizeof(inv) / sizeof(int);

  // vec_int_t sout = fix_fun(in);
  // for (int i = 0; i < sout.size; i++) {
  //   printf("%d ", sout.elems[i]);
  // }
  // printf("\n");

  vec_int_t out = pms(in);
  for (int i = 0; i < out.size; i++) {
    printf("%d ", out.elems[i]);
  }
  printf("\n");
}
