#ifndef __PARMAP__
#define __PARMAP__

#include<stdio.h>
#include<stdlib.h>
#include<pthread.h>

typedef struct vec_int {
            int * elems; size_t size;
        } vec_int_t;

typedef struct q_vec_int {
            volatile unsigned int q_size;
            int q_head;
            int q_tail;
            vec_int_t q_mem[16];
        } q_vec_int_t;

void q_vec_int_put(q_vec_int_t *, vec_int_t);

vec_int_t q_vec_int_get(q_vec_int_t *);

typedef struct pair_vec_int_vec_int {
            vec_int_t fst; vec_int_t snd;
        } pair_vec_int_vec_int_t;

vec_int_t cat(pair_vec_int_vec_int_t);

typedef enum unit {
            Unit
        } unit_t;

vec_int_t prod(vec_int_t);

vec_int_t scalarProd(vec_int_t);

#endif

