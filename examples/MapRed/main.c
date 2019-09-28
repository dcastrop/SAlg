#include "ParMap.h"
#include <inttypes.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>

#define REPETITIONS 50

#define BENCHMARKSEQ(s, f) { \
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
  } \
  printf("\tK: %s\n", s); \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", sqrt(var / (REPETITIONS - 1))); \
}


#define BENCHMARKPAR(s, fi, f) { \
  time = 0; \
  time_diff = 0; \
  time_old = 0; \
  var = 0; \
  for(int i=0; i<REPETITIONS; i++){ \
    in = randvec(size); \
    fi(); \
    start = get_time(); \
    out = f(in); \
    end = get_time(); \
    time_diff = end - start; \
    time_old = time; \
    time += (time_diff - time)/(i+1); \
    var += (time_diff - time) * (time_diff - time_old); \
  } \
  printf("\tK: %s\n", s); \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", sqrt(var / (REPETITIONS - 1))); \
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

vec_vec_int_t randvec(size_t s){
  vec_vec_int_t in;
  in.elems = (vec_int_t *)calloc(s, sizeof(vec_int_t));
  in.size = s;

  srand(time(NULL));

  for (int i = 0; i < s; i++) {
    in.elems[i].size = s;
    in.elems[i].elems = (int *)calloc(s, sizeof(int));
    for(int j = 0; j < s; j++){
      in.elems[i].elems[j] = (int)rand() % 100;
    }
  }

  return in;
}

vec_int_t cat(pair_vec_int_vec_int_t in){
  vec_int_t out;
  out.size = in.fst.size + in.snd.size;
  out.elems = (int *)malloc(out.size * sizeof(int));
  memcpy(out.elems, in.fst.elems, in.fst.size);
  memcpy(out.elems + in.fst.size, in.snd.elems, in.snd.size);
  free(in.fst.elems);
  free(in.snd.elems);
  return out;
}

vec_int_t prod(vec_vec_int_t v){
  vec_int_t out;
  out.size = v.size;
  out.elems = (int *) malloc (v.size * sizeof(int));
  for(int i = 0; i < v.size; i++){
    out.elems[i] = 1;
    for (int j = 0; j < v.elems[i].size; j++){
      out.elems[i] *= v.elems[i].elems[j];
    }
  }
  for(int i = 0; i < v.size; i++){
    free(v.elems[i].elems);
  }
  // free(v.elems);
  return out;
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


  vec_vec_int_t in;
  vec_int_t out;
  double start, end, time, time_diff, time_old, var;
  // Warmup
  for(int i=0; i<REPETITIONS; i++){
    in = randvec(size);
    out = prod(in);
  }

  BENCHMARKSEQ("seq", prod)
  BENCHMARKSEQ("ms0", parProd)

  // BENCHMARKPAR("ms0", scalarProdInit, scalarProd)

  // time = 0;
  // for(int i=0; i<REPETITIONS; i++){
    // in = randvec(size);
    // clock_gettime(CLOCK_MONOTONIC, &start);
    // out = parMsort1(in);
    // clock_gettime(CLOCK_MONOTONIC, &end);
    // time += (end - start)/REPETITIONS;
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
