#ifndef __MAPRED__
#define __MAPRED__

#include<stdio.h>
#include<stdlib.h>
#include<pthread.h>

typedef struct vec_int {
            int * elems; size_t size;
        } vec_int_t;

typedef struct pair_vec_int_vec_int {
            vec_int_t fst; vec_int_t snd;
        } pair_vec_int_vec_int_t;

typedef enum tag {
            Inl, Inr
        } tag_t;

typedef struct pair_int_pair_vec_int_vec_int {
            int fst; pair_vec_int_vec_int_t snd;
        } pair_int_pair_vec_int_vec_int_t;

typedef struct either_pair_int_pair_vec_int_vec_int_pair_int_pair_vec_int_vec_int {
            tag_t tag;
            union {
                pair_int_pair_vec_int_vec_int_t inl;
                pair_int_pair_vec_int_vec_int_t inr;
            } val;
        } either_pair_int_pair_vec_int_vec_int_pair_int_pair_vec_int_vec_int_t;

typedef enum unit {
            Unit
        } unit_t;

typedef struct q_either_unit_unit {
            volatile unsigned int q_size;
            int q_head;
            int q_tail;
            tag_t q_mem[16];
        } q_either_unit_unit_t;

void q_either_unit_unit_put(q_either_unit_unit_t *, tag_t);

tag_t q_either_unit_unit_get(q_either_unit_unit_t *);

vec_int_t dot(pair_vec_int_vec_int_t);

typedef struct q_vec_int {
            volatile unsigned int q_size;
            int q_head;
            int q_tail;
            vec_int_t q_mem[16];
        } q_vec_int_t;

void q_vec_int_put(q_vec_int_t *, vec_int_t);

vec_int_t q_vec_int_get(q_vec_int_t *);

vec_int_t sum(pair_vec_int_vec_int_t);

vec_int_t dotProd(pair_vec_int_vec_int_t);

#endif

