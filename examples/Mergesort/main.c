#include "Mergesort.h"
#include <inttypes.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>

#define REPETITIONS 50

#define BENCHMARK(s, f) { \
  time = 0; \
  time_diff = 0; \
  time_old = 0; \
  var = 0; \
  for(int i=0; i<REPETITIONS; i++){ \
    in = randvec(size); \
    clock_gettime(CLOCK_MONOTONIC, &start); \
    out = f(in); \
    clock_gettime(CLOCK_MONOTONIC, &end); \
    time_diff = tspec2ms(&end) - tspec2ms(&start); \
    time_old = time; \
    time += (time_diff - time)/(i+1); \
    var += (time_diff - time) * (time_diff - time_old); \
    free(in.elems); \
  } \
  printf("\tK: %s\n", s); \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", sqrt(var / (REPETITIONS - 1))); \
}

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

vec_int_t randvec(ssize_t s){
  vec_int_t in;
  in.elems = (int *)calloc(s, sizeof(int));
  in.size = s;

  srandom(time(NULL));

  for (int i = 0; i < s; i++) {
    in.elems[i] = (int)random() % s;
  }

  return in;
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
  struct timespec start, end;
  // Warmup
  for(int i=0; i<REPETITIONS; i++){
    in = randvec(size);
    out = parMsort0(in);
    free(in.elems);
  }

  double time = 0;
  double time_diff = 0;
  double time_old = 0;
  double var = 0;
  BENCHMARK("ms0", parMsort0)
  BENCHMARK("ms1", parMsort1)
  //BENCHMARK("ms1a", parMsort1a)
  BENCHMARK("ms2", parMsort2)
  BENCHMARK("ms3", parMsort3)
  BENCHMARK("ms4", parMsort4)
  BENCHMARK("ms5", parMsort5)
  BENCHMARK("ms6", parMsort6)
  BENCHMARK("ms7", parMsort7)
  BENCHMARK("ms8", parMsort8)

  // time = 0;
  // for(int i=0; i<REPETITIONS; i++){
    // in = randvec(size);
    // clock_gettime(CLOCK_MONOTONIC, &start);
    // out = parMsort1(in);
    // clock_gettime(CLOCK_MONOTONIC, &end);
    // time += (tspec2ms(&end) - tspec2ms(&start))/REPETITIONS;
    // free(in.elems);
  // }
  // printf("ms1: %d\n", time);

//#ifdef DEBUG
//    if (start.tv_nsec > end.tv_nsec ){
//  for (int i = 0; i < out.size; i++) {
//    printf("%d ", out.elems[i]);
//  }
//  printf("\n");
//    }
//#endif
}
