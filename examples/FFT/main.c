#include "FFT.h"
#include <inttypes.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>

#define REPETITIONS 1

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
    show("Res:", in.fst); \
    free_mat(in); \
    time_diff = end - start; \
    time_old = time; \
    time += (time_diff - time)/(i+1); \
    var += (time_diff - time) * (time_diff - time_old); \
  } \
  printf("\tK: %s\n", s); \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", REPETITIONS<=1? 0: sqrt(var / (REPETITIONS - 1))); \
}

#define WARMUP(f) { \
  for(int i=0; i<REPETITIONS; i++){ \
    in = randvec(size); \
    out = f(in); \
    free_mat(in); \
  } \
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
    free_mat(in); \
    time_diff = end - start; \
    time_old = time; \
    time += (time_diff - time)/(i+1); \
    var += (time_diff - time) * (time_diff - time_old); \
  } \
  printf("\tK: %s\n", s); \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", REPETITIONS<=1? 0: sqrt(var / (REPETITIONS - 1))); \
}

/* Primitive functions */
// pair_vec_cplx_vec_cplx_t add_padding(vec_cplx_t v) {
//  int size = 2 ^ (int)ceil(log2(v.size));
//  if (v.size == size){
//    return v;
//  }
//  vec_cplx_t out;
//  out.elems = (cplx_t *)realloc(v.elems, size * sizeof(cplx_t));
//  out.size = size;
//  for(int i = v.size; i < size; i++) out.elems[i] = 0;
//  return v;
//}

double PI = atan2(1, 1) * 4;

int total_step;

pair_vec_cplx_vec_cplx_t map_exp(pair_int_pair_vec_cplx_vec_cplx_t in){
  int lvl = in.fst;
  pair_vec_cplx_vec_cplx_t vec = in.snd;
  int e = vec.fst.size;
  for (int i = 0; i < vec.fst.size - lvl; i+=total_step){
    vec.fst.elems[i] = cexp(-I * PI * i / e) * vec.fst.elems[i];
  }
  return vec;
}

pair_vec_cplx_vec_cplx_t zip_add(pair_pair_vec_cplx_vec_cplx_pair_vec_cplx_vec_cplx_t in){
  vec_cplx_t l = in.snd.fst;
  vec_cplx_t r = in.fst.fst;
  vec_cplx_t o = in.snd.snd;
  for(int i = 0; i < in.fst.fst.size && i < in.snd.fst.size; i+=total_step){
    o.elems[i] = l.elems[i] + r.elems[i];
  }
  pair_vec_cplx_vec_cplx_t out = {o, l};
  return out;
}

pair_vec_cplx_vec_cplx_t zip_sub(pair_pair_vec_cplx_vec_cplx_pair_vec_cplx_vec_cplx_t in){
  vec_cplx_t l = in.fst.fst;
  vec_cplx_t r = in.snd.fst;
  vec_cplx_t o = in.snd.snd;
  for(int i = 0; i < in.fst.fst.size && i < in.snd.fst.size; i+=total_step){
    o.elems[i] = l.elems[i] - r.elems[i];
  }
  pair_vec_cplx_vec_cplx_t out = {o, r};
  return out;
}

pair_vec_cplx_vec_cplx_t cat(pair_pair_vec_cplx_vec_cplx_pair_vec_cplx_vec_cplx_t in){
  pair_vec_cplx_vec_cplx_t out = { in.fst.fst, in.fst.snd };
  // out.fst.size += in.snd.fst.size;
  // out.snd.size += in.snd.snd.size;
  return out;
}

void _fft(cplx_t buf[], cplx_t out[], int n, int step)
{
  if (step < n) {
    _fft(out, buf, n, step * 2);
    _fft(out + step, buf + step, n, step * 2);

    for (int i = 0; i < n; i += 2 * step) {
      cplx_t t = cexp(-I * PI * i / n) * out[i + step];
      buf[i / 2]     = out[i] + t;
      buf[(i + n)/2] = out[i] - t;
    }
  }
}

vec_cplx_t udrop(pair_int_vec_cplx_t i){
  i.snd.elems += i.fst;
  return i.snd;
}

void showstep(int stp, const char * s, vec_cplx_t in) {
	printf("%s", s);
	for (int i = 0; i < in.size; i+=stp)
		if (!cimag(in.elems[i]))
			printf("%g ", creal(in.elems[i]));
		else
			printf("(%g, %g) ", creal(in.elems[i]), cimag(in.elems[i]));
    printf("\n");
}

pair_vec_cplx_vec_cplx_t baseFFT(pair_int_pair_vec_cplx_vec_cplx_t in)
{
    cplx_t *buf = in.snd.fst.elems;
    int n = in.snd.fst.size;
    int step = in.fst;
    total_step = step;

	_fft(buf, in.snd.snd.elems, n, step);

    return in.snd;
}

void show(const char * s, vec_cplx_t in) {
	printf("%s", s);
	for (int i = 0; i < in.size; i++)
		if (!cimag(in.elems[i]))
			printf("%g ", creal(in.elems[i]));
		else
			printf("(%g, %g) ", creal(in.elems[i]), cimag(in.elems[i]));
    printf("\n");
}

pair_vec_cplx_vec_cplx_t seqfft(pair_vec_cplx_vec_cplx_t v){
  pair_int_pair_vec_cplx_vec_cplx_t in = {1, v};
  return baseFFT(in);
}

void free_mat(pair_vec_cplx_vec_cplx_t v){
  free(v.fst.elems);
  free(v.snd.elems);
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

pair_vec_cplx_vec_cplx_t randvec(size_t s){
  vec_cplx_t in;
  in.elems = (cplx_t *)calloc(s, sizeof(cplx_t));
  in.size = s;

  srand(time(NULL));

  for (int i = 0; i < s; i++) {
    double rand_r = (double)rand() / (double)RAND_MAX;
    double rand_i = (double)rand() / (double)RAND_MAX;
    in.elems[i] = rand_r + rand_i * I;
  }

  vec_cplx_t copy;
  copy.elems = (cplx_t *)calloc(s, sizeof(cplx_t));
  copy.size = s;
  memcpy(copy.elems, in.elems, s * sizeof(cplx_t));

  return (pair_vec_cplx_vec_cplx_t){in, copy};
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
  size = (size_t) 1 << (long)ceil(log2(size));
  size = size < 256? 256:size;
  if (errno != 0) {
    printf("%s", strerror(errno));
    usage(argv[0]);
  }
  if (endptr != NULL && *endptr != 0) {
    usage(argv[0]);
  }


  pair_vec_cplx_vec_cplx_t in, out;
  double start, end, time, time_diff, time_old, var;

  // TEST
  // cplx_t i[] = {1, 1, 1, 1, 0, 0, 0, 0};
  // cplx_t o[] = {1, 1, 1, 1, 0, 0, 0, 0};
  // pair_vec_cplx_vec_cplx_t iin = {{i, 8}, {o, 8}};

  // out = fft3(iin);
  // show("Out-fst:", out.fst); printf("\n");
  // show("Out-snd:", out.snd); printf("\n");

//   WARMUP(seqfft)
//
  BENCHMARKSEQ("seq", seqfft)
  BENCHMARKSEQ("0", fft0)
  BENCHMARKSEQ("1", fft1)
  BENCHMARKSEQ("2", fft2)
  BENCHMARKSEQ("3", fft3)
  BENCHMARKSEQ("4", fft4)
  BENCHMARKSEQ("5", fft5)
  BENCHMARKSEQ("6", fft6)
//   // BENCHMARKSEQ("7", fft7)
//   // BENCHMARKSEQ("8", fft8)
}
