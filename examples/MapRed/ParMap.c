#include "ParMap.h"

q_vec_vec_int_t ch0 = { 0, 0, 0, { } };

void q_vec_vec_int_put(q_vec_vec_int_t * ch, vec_vec_int_t v_m)
{
    while (1)
    {
        pthread_mutex_lock(&ch->q_mutex);
        while (ch->q_size >= 16)
        {
            pthread_cond_wait(&ch->q_full, &ch->q_mutex);
        }
        if (ch->q_size < 16)
        {
            ch->q_mem[ch->q_head] = v_m;
            ch->q_head = (ch->q_head + 1) % 16;
            ch->q_size++;
            pthread_cond_signal(&ch->q_empty);
            pthread_mutex_unlock(&ch->q_mutex);
            return;
        }
        pthread_mutex_unlock(&ch->q_mutex);
    }
}

vec_vec_int_t q_vec_vec_int_get(q_vec_vec_int_t * ch)
{
    vec_vec_int_t v_n;
    while (1)
    {
        pthread_mutex_lock(&ch->q_mutex);
        while (ch->q_size <= 0)
        {
            pthread_cond_wait(&ch->q_empty, &ch->q_mutex);
        }
        if (ch->q_size > 0)
        {
            v_n = ch->q_mem[ch->q_tail];
            ch->q_tail = (ch->q_tail + 1) % 16;
            ch->q_size--;
            pthread_cond_signal(&ch->q_full);
            pthread_mutex_unlock(&ch->q_mutex);
            return v_n;
        }
        pthread_mutex_unlock(&ch->q_mutex);
    }
}

q_vec_vec_int_t ch1 = { 0, 0, 0, { } };

q_vec_vec_int_t ch2 = { 0, 0, 0, { } };

q_vec_vec_int_t ch3 = { 0, 0, 0, { } };

q_vec_vec_int_t ch4 = { 0, 0, 0, { } };

q_vec_vec_int_t ch5 = { 0, 0, 0, { } };

q_vec_int_t ch6 = { 0, 0, 0, { } };

void q_vec_int_put(q_vec_int_t * ch, vec_int_t v_p)
{
    while (1)
    {
        pthread_mutex_lock(&ch->q_mutex);
        while (ch->q_size >= 16)
        {
            pthread_cond_wait(&ch->q_full, &ch->q_mutex);
        }
        if (ch->q_size < 16)
        {
            ch->q_mem[ch->q_head] = v_p;
            ch->q_head = (ch->q_head + 1) % 16;
            ch->q_size++;
            pthread_cond_signal(&ch->q_empty);
            pthread_mutex_unlock(&ch->q_mutex);
            return;
        }
        pthread_mutex_unlock(&ch->q_mutex);
    }
}

vec_int_t q_vec_int_get(q_vec_int_t * ch)
{
    vec_int_t v_q;
    while (1)
    {
        pthread_mutex_lock(&ch->q_mutex);
        while (ch->q_size <= 0)
        {
            pthread_cond_wait(&ch->q_empty, &ch->q_mutex);
        }
        if (ch->q_size > 0)
        {
            v_q = ch->q_mem[ch->q_tail];
            ch->q_tail = (ch->q_tail + 1) % 16;
            ch->q_size--;
            pthread_cond_signal(&ch->q_full);
            pthread_mutex_unlock(&ch->q_mutex);
            return v_q;
        }
        pthread_mutex_unlock(&ch->q_mutex);
    }
}

q_vec_int_t ch7 = { 0, 0, 0, { } };

q_vec_int_t ch8 = { 0, 0, 0, { } };

q_vec_int_t ch9 = { 0, 0, 0, { } };

q_vec_int_t ch10 = { 0, 0, 0, { } };

q_vec_int_t ch11 = { 0, 0, 0, { } };

vec_int_t parProd_part_0(vec_vec_int_t v_a)
{
    int v_b;
    v_b = v_a.size / 5;
    vec_vec_int_t v_c;
    v_c.size = v_b;
    v_c.elems = v_a.elems;
    vec_vec_int_t v_d;
    v_d.size = v_a.size - v_b;
    v_d.elems = v_a.elems + v_b;
    vec_vec_int_t v_e;
    v_e.size = v_b;
    v_e.elems = v_d.elems;
    vec_vec_int_t v_f;
    v_f.size = v_d.size - v_b;
    v_f.elems = v_d.elems + v_b;
    vec_vec_int_t v_g;
    v_g.size = v_b;
    v_g.elems = v_f.elems;
    vec_vec_int_t v_h;
    v_h.size = v_f.size - v_b;
    v_h.elems = v_f.elems + v_b;
    vec_vec_int_t v_i;
    v_i.size = v_b;
    v_i.elems = v_h.elems;
    vec_vec_int_t v_j;
    v_j.size = v_h.size - v_b;
    v_j.elems = v_h.elems + v_b;
    vec_vec_int_t v_k;
    v_k.size = v_b;
    v_k.elems = v_j.elems;
    vec_vec_int_t v_l;
    v_l.size = v_j.size - v_b;
    v_l.elems = v_j.elems + v_b;
    q_vec_vec_int_put(&ch0, v_c);
    q_vec_vec_int_put(&ch1, v_e);
    q_vec_vec_int_put(&ch2, v_g);
    q_vec_vec_int_put(&ch3, v_i);
    q_vec_vec_int_put(&ch4, v_k);
    q_vec_vec_int_put(&ch5, v_l);
    vec_int_t v_o;
    v_o = q_vec_int_get(&ch6);
    vec_int_t v_r;
    v_r = q_vec_int_get(&ch7);
    pair_vec_int_vec_int_t v_s;
    v_s.fst = v_o;
    v_s.snd = v_r;
    vec_int_t v_t;
    v_t = q_vec_int_get(&ch8);
    pair_vec_int_vec_int_t v_u;
    v_u.fst = v_t;
    v_u.snd = cat(v_s);
    vec_int_t v_v;
    v_v = q_vec_int_get(&ch9);
    pair_vec_int_vec_int_t v_w;
    v_w.fst = v_v;
    v_w.snd = cat(v_u);
    vec_int_t v_x;
    v_x = q_vec_int_get(&ch10);
    pair_vec_int_vec_int_t v_y;
    v_y.fst = v_x;
    v_y.snd = cat(v_w);
    vec_int_t v_z;
    v_z = q_vec_int_get(&ch11);
    pair_vec_int_vec_int_t v_aa;
    v_aa.fst = v_z;
    v_aa.snd = cat(v_y);
    return cat(v_aa);
}

unit_t parProd_part_1()
{
    vec_vec_int_t v_ba;
    v_ba = q_vec_vec_int_get(&ch0);
    q_vec_int_put(&ch11, prod(v_ba));
    return Unit;
}

unit_t parProd_part_2()
{
    vec_vec_int_t v_ca;
    v_ca = q_vec_vec_int_get(&ch1);
    q_vec_int_put(&ch10, prod(v_ca));
    return Unit;
}

unit_t parProd_part_3()
{
    vec_vec_int_t v_da;
    v_da = q_vec_vec_int_get(&ch2);
    q_vec_int_put(&ch9, prod(v_da));
    return Unit;
}

unit_t parProd_part_4()
{
    vec_vec_int_t v_ea;
    v_ea = q_vec_vec_int_get(&ch3);
    q_vec_int_put(&ch8, prod(v_ea));
    return Unit;
}

unit_t parProd_part_5()
{
    vec_vec_int_t v_fa;
    v_fa = q_vec_vec_int_get(&ch4);
    q_vec_int_put(&ch6, prod(v_fa));
    return Unit;
}

unit_t parProd_part_6()
{
    vec_vec_int_t v_ga;
    v_ga = q_vec_vec_int_get(&ch5);
    q_vec_int_put(&ch7, prod(v_ga));
    return Unit;
}

void * fun_thread_1(void * arg)
{
    parProd_part_1();
    return NULL;
}

void * fun_thread_2(void * arg)
{
    parProd_part_2();
    return NULL;
}

void * fun_thread_3(void * arg)
{
    parProd_part_3();
    return NULL;
}

void * fun_thread_4(void * arg)
{
    parProd_part_4();
    return NULL;
}

void * fun_thread_5(void * arg)
{
    parProd_part_5();
    return NULL;
}

void * fun_thread_6(void * arg)
{
    parProd_part_6();
    return NULL;
}

vec_int_t parProd(vec_vec_int_t v_ha)
{
    pthread_t thread1;
    pthread_t thread2;
    pthread_t thread3;
    pthread_t thread4;
    pthread_t thread5;
    pthread_t thread6;
    pthread_create(&thread1, NULL, fun_thread_1, NULL);
    pthread_create(&thread2, NULL, fun_thread_2, NULL);
    pthread_create(&thread3, NULL, fun_thread_3, NULL);
    pthread_create(&thread4, NULL, fun_thread_4, NULL);
    pthread_create(&thread5, NULL, fun_thread_5, NULL);
    pthread_create(&thread6, NULL, fun_thread_6, NULL);
    return parProd_part_0(v_ha);
}
