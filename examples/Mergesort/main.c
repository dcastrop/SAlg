#include "Mergesort.h"

//either_vec_int_pair_vec_int_vec_int_t split(vec_int_t in){
//  either_vec_int_pair_vec_int_vec_int_t out;
//  if (in.size <= 1) {
//    out.tag = Inl;
//    out.val.inl = in;
//  } else {
//    out.tag = Inr;
//    vec_int_t in1;
//    vec_int_t in2;
//    in2.size = in.size - in1.size;
//    in2.elems = in.elems + in1.size;
//    out.val.inr.fst.size = in.size / 2;
//    out.val.inr.fst.elems = in.elems;
//    out.val.inr.snd.size = in.size - out.val.inr.fst.size;
//    out.val.inr.snd.elems = in.elems + out.val.inr.fst.size;
//  }
//  return out;
//}
//
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
    if (i < in.fst.size) tmp[z] = in.fst.elems[i++];
    if (j < in.snd.size) tmp[z] = in.snd.elems[j++];
  }
  out = in.fst;
  out.size = sz;
  for (int r=0; r < sz; r++) {
    out.elems[r] = tmp[r];
  }
  free (tmp);
  return out;
}

//vec_int_t merge(either_vec_int_pair_vec_int_vec_int_t in){
//  vec_int_t out;
//  switch (in.tag) {
//    case Inl:
//      { out = in.val.inl;
//        break;
//      }
//    case Inr:
//      { int sz = in.val.inr.fst.size + in.val.inr.snd.size;
//        out = do_merge(sz, in.val.inr.fst, in.val.inr.snd);
//      }
//  }
//  return out;
//}
//
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

  vec_int_t out = parMsort(in);
  for (int i = 0; i < out.size; i++) {
    printf("%d ", out.elems[i]);
  }
  printf("\n");
}
