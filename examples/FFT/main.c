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
vec_cplx_t add_padding(vec_cplx_t v) {
//  int size = 2 ^ (int)ceil(log2(v.size));
//  if (v.size == size){
//    return v;
//  }
//  vec_cplx_t out;
//  out.elems = (cplx_t *)realloc(v.elems, size * sizeof(cplx_t));
//  out.size = size;
//  for(int i = v.size; i < size; i++) out.elems[i] = 0;
  return v;
}

double PI = atan2(1, 1) * 4;

vec_cplx_t map_exp(pair_int_pair_int_vec_cplx_t in){
  int lvl = in.fst;
  int idx = in.snd.fst;
  vec_cplx_t vec = in.snd.snd;
  int k = idx * vec.size;
  int e = lvl * vec.size;
  for (int i = 0; i < vec.size; i++){
    vec.elems[i] = cexp(-I * PI * (k + i) / e) * vec.elems[i];
  }
  return vec;
}

vec_cplx_t zip_add(pair_vec_cplx_vec_cplx_t in){
  vec_cplx_t out;
  out.elems = (cplx_t *)malloc(in.fst.size * sizeof(cplx_t));
  out.size = in.fst.size;
  for(int i = 0; i < in.fst.size && i < in.snd.size; i++){
    out.elems[i] = in.fst.elems[i] + in.snd.elems[i];
  }
  return out;
}

vec_cplx_t zip_sub(pair_vec_cplx_vec_cplx_t in){
  vec_cplx_t out;
  out.elems = (cplx_t *)malloc(in.fst.size * sizeof(cplx_t));
  out.size = in.fst.size;
  for(int i = 0; i < in.fst.size && i < in.snd.size; i++){
    out.elems[i] = in.fst.elems[i] - in.snd.elems[i];
  }
  return out;
}

vec_cplx_t cat(pair_vec_cplx_vec_cplx_t in){
  vec_cplx_t out;
  out.size = in.fst.size + in.snd.size;
  out.elems = (cplx_t *)malloc((in.fst.size + in.snd.size) * sizeof(cplx_t));
  memcpy(out.elems, in.fst.elems, in.fst.size * sizeof(cplx_t));
  memcpy(out.elems + in.fst.size, in.snd.elems, in.snd.size * sizeof(cplx_t));
  free(in.fst.elems);
  free(in.snd.elems);
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

vec_cplx_t baseFFT(pair_int_vec_cplx_t in)
{
    cplx_t *buf = in.snd.elems;
    int n = in.snd.size;
    int step = in.fst;
	vec_cplx_t out;
    out.elems = (cplx_t *)malloc(n * sizeof(cplx_t));
    out.size = n;
	for (int i = 0; i < n; i++) out.elems[i] = buf[i];

	_fft(out.elems, buf, n, step);
    return out;
}


void show(const char * s, vec_cplx_t in) {
	printf("%s", s);
	for (int i = 0; i < in.size; i++)
		if (!cimag(in.elems[i]))
			printf("%g ", creal(in.elems[i]));
		else
			printf("(%g, %g) ", creal(in.elems[i]), cimag(in.elems[i]));
}


vec_cplx_t seqfft(vec_cplx_t v){
  vec_cplx_t v1 = add_padding(v);
  pair_int_vec_cplx_t in = {1, v1};
  return baseFFT(in);
}

void free_mat(vec_cplx_t v){
  free(v.elems);
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

vec_cplx_t randvec(size_t s){
  vec_cplx_t in;
  in.elems = (cplx_t *)calloc(s, sizeof(cplx_t));
  in.size = s;

  srand(time(NULL));

  for (int i = 0; i < s; i++) {
    double rand_r = (double)rand() / (double)RAND_MAX;
    double rand_i = (double)rand() / (double)RAND_MAX;
    in.elems[i] = rand_r + rand_i * I;
  }

  return in;
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


  vec_cplx_t in, out;
  double start, end, time, time_diff, time_old, var;

  WARMUP(seqfft)

  BENCHMARKSEQ("seq", seqfft)
  BENCHMARKSEQ("1", fft1)
  BENCHMARKSEQ("2", fft2)
  BENCHMARKSEQ("3", fft3)
  BENCHMARKSEQ("4", fft4)
  BENCHMARKSEQ("5", fft5)
  BENCHMARKSEQ("6", fft6)
  BENCHMARKSEQ("7", fft7)
  BENCHMARKSEQ("8", fft8)
}
