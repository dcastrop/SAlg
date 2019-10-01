#include "Quicksort.h"
#include <inttypes.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>

#define REPETITIONS 1

#define BENCHMARK(s, f) { \
  time = 0; \
  time_diff = 0; \
  time_old = 0; \
  var = 0; \
  for(int i=0; i<REPETITIONS; i++){ \
    in = randvec(size); \
    start = get_time(); \
    out = f(in); \
    end = get_time(); \
    time_diff = end - start; \
    time_old = time; \
    time += (time_diff - time)/(i+1); \
    var += (time_diff - time) * (time_diff - time_old); \
    free(in.elems); \
  } \
  printf("\tK: %s\n", s); \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", REPETITIONS <= 1? 0 : sqrt(var / (REPETITIONS - 1))); \
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

vec_int_t randvec(size_t s){
  vec_int_t in;
  in.elems = (int *)calloc(s, sizeof(int));
  in.size = s;

  srand(get_time());

  for (int i = 0; i < s; i++) {
    in.elems[i] = (int)rand() % 100;
  }

  return in;
}

pair_vec_int_vec_int_t filter(vec_int_t in){
  pair_vec_int_vec_int_t out;
  size_t i = 0;
  size_t j = in.size - 1;
  int tmp;
  int pivot = in.elems[i];
  do {
    while (i < j && pivot >= in.elems[i]) { i++; }
    while (j >= 0 && pivot < in.elems[j]) { j--; }
    if (i < j) {
      tmp = in.elems[i];
      in.elems[i] = in.elems[j];
      in.elems[j] = tmp;
    }
  } while (i < j);
  tmp = in.elems[0];
  in.elems[0] = in.elems[j];
  in.elems[j] = tmp;
  out.fst = in;
  out.fst.size = j + 1;
  if (out.fst.size == in.size) {
    out.fst.size = in.size / 2;
  }
  out.snd.elems = in.elems + out.fst.size;
  out.snd.size = in.size - out.fst.size;
  return out;
}


vec_int_t cat(pair_vec_int_vec_int_t in){
  in.fst.size += in.snd.size;
  return in.fst;
}

void usage(const char *nm){
  printf("Usage: %s <input_size>\n", nm);
  exit(-1);
}


long tspec2ms(struct timespec *tv)
{
	return tv->tv_sec * 1.0e9 + tv->tv_nsec;
}

int main(int argc, const char *argv[]) {

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

  vec_int_t in, out;
  double start, end;
  // Warmup
  for(int i=0; i<10; i++){
    in = randvec(size);
    // printf("In %d: ", in.size);
    // for (int i = 0; i < in.size; i++) {
    //   printf("%d ", in.elems[i]);
    // }
    //printf("\n");
    out = parMsort1(in);
    //printf("Out %d: ", out.size);
    //for (int i = 0; i < out.size; i++) {
    //  printf("%d ", out.elems[i]);
    //}
    //printf("\n \n");
    free(in.elems);
  }


  double time = 0;
  double time_diff = 0;
  double time_old = 0;
  double var = 0;
  BENCHMARK("qs0", parMsort0)
  BENCHMARK("qs1", parMsort1)
  BENCHMARK("qs2", parMsort2)
  BENCHMARK("qs3", parMsort3)
  BENCHMARK("qs4", parMsort4)
  BENCHMARK("qs5", parMsort5)
  BENCHMARK("qs6", parMsort6)
  BENCHMARK("qs7", parMsort7)
  BENCHMARK("qs8", parMsort8)
}