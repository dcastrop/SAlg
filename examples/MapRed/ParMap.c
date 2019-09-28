#include "ParMap.h"

q_vec_int_t ch0 = { 0, 0, 0, { } };

void q_vec_int_put(q_vec_int_t * ch, vec_int_t v_u)
{
    while (1)
    {
        while (ch->q_size >= 16)
        {
        }
        if (ch->q_size < 16)
        {
            ch->q_mem[ch->q_head] = v_u;
            ch->q_head = (ch->q_head + 1) % 16;
            ch->q_size++;
            return;
        }
    }
}

vec_int_t q_vec_int_get(q_vec_int_t * ch)
{
    vec_int_t v_v;
    while (1)
    {
        while (ch->q_size <= 0)
        {
        }
        if (ch->q_size > 0)
        {
            v_v = ch->q_mem[ch->q_tail];
            ch->q_tail = (ch->q_tail + 1) % 16;
            ch->q_size--;
            return v_v;
        }
    }
}

q_vec_int_t ch1 = { 0, 0, 0, { } };

q_vec_int_t ch2 = { 0, 0, 0, { } };

q_vec_int_t ch3 = { 0, 0, 0, { } };

q_vec_int_t ch4 = { 0, 0, 0, { } };

q_vec_int_t ch5 = { 0, 0, 0, { } };

q_vec_int_t ch6 = { 0, 0, 0, { } };

vec_int_t scalarProd_part_0(vec_int_t v_a)
{
    int v_b;
    v_b = v_a.size / 32;
    int v_c;
    v_c = 0 * v_b;
    vec_int_t v_d;
    v_d.size = v_a.size - v_c;
    v_d.elems = v_a.elems + v_c;
    vec_int_t v_e;
    v_e.size = v_b;
    v_e.elems = v_d.elems;
    int v_f;
    v_f = 1 * v_b;
    vec_int_t v_g;
    v_g.size = v_a.size - v_f;
    v_g.elems = v_a.elems + v_f;
    vec_int_t v_h;
    v_h.size = v_b;
    v_h.elems = v_g.elems;
    int v_i;
    v_i = 2 * v_b;
    vec_int_t v_j;
    v_j.size = v_a.size - v_i;
    v_j.elems = v_a.elems + v_i;
    vec_int_t v_k;
    v_k.size = v_b;
    v_k.elems = v_j.elems;
    int v_l;
    v_l = 3 * v_b;
    vec_int_t v_m;
    v_m.size = v_a.size - v_l;
    v_m.elems = v_a.elems + v_l;
    vec_int_t v_n;
    v_n.size = v_b;
    v_n.elems = v_m.elems;
    int v_o;
    v_o = 4 * v_b;
    vec_int_t v_p;
    v_p.size = v_a.size - v_o;
    v_p.elems = v_a.elems + v_o;
    vec_int_t v_q;
    v_q.size = v_b;
    v_q.elems = v_p.elems;
    int v_r;
    v_r = 5 * v_b;
    vec_int_t v_s;
    v_s.size = v_a.size - v_r;
    v_s.elems = v_a.elems + v_r;
    vec_int_t v_t;
    v_t.size = v_b;
    v_t.elems = v_s.elems;
    q_vec_int_put(&ch0, v_e);
    q_vec_int_put(&ch1, v_h);
    q_vec_int_put(&ch2, v_k);
    q_vec_int_put(&ch3, v_n);
    q_vec_int_put(&ch4, v_q);
    q_vec_int_put(&ch5, v_t);
    vec_int_t v_w;
    v_w = q_vec_int_get(&ch6);
    return v_w;
}

q_vec_int_t ch7 = { 0, 0, 0, { } };

q_vec_int_t ch8 = { 0, 0, 0, { } };

q_vec_int_t ch9 = { 0, 0, 0, { } };

unit_t scalarProd_part_1()
{
    vec_int_t v_x;
    v_x = q_vec_int_get(&ch0);
    vec_int_t v_y;
    v_y = q_vec_int_get(&ch7);
    pair_vec_int_vec_int_t v_z;
    v_z.fst = prod(v_x);
    v_z.snd = v_y;
    vec_int_t v_aa;
    v_aa = q_vec_int_get(&ch8);
    pair_vec_int_vec_int_t v_ba;
    v_ba.fst = cat(v_z);
    v_ba.snd = v_aa;
    vec_int_t v_ca;
    v_ca = q_vec_int_get(&ch7);
    pair_vec_int_vec_int_t v_da;
    v_da.fst = prod(v_x);
    v_da.snd = v_ca;
    vec_int_t v_ea;
    v_ea = q_vec_int_get(&ch9);
    pair_vec_int_vec_int_t v_fa;
    v_fa.fst = cat(v_ba);
    v_fa.snd = v_ea;
    q_vec_int_put(&ch6, cat(v_fa));
    return Unit;
}

unit_t scalarProd_part_2()
{
    vec_int_t v_ga;
    v_ga = q_vec_int_get(&ch1);
    q_vec_int_put(&ch7, prod(v_ga));
    q_vec_int_put(&ch7, prod(v_ga));
    return Unit;
}

q_vec_int_t ch10 = { 0, 0, 0, { } };

unit_t scalarProd_part_3()
{
    vec_int_t v_ha;
    v_ha = q_vec_int_get(&ch2);
    vec_int_t v_ia;
    v_ia = q_vec_int_get(&ch10);
    pair_vec_int_vec_int_t v_ja;
    v_ja.fst = prod(v_ha);
    v_ja.snd = v_ia;
    q_vec_int_put(&ch8, cat(v_ja));
    vec_int_t v_ka;
    v_ka = q_vec_int_get(&ch10);
    pair_vec_int_vec_int_t v_la;
    v_la.fst = prod(v_ha);
    v_la.snd = v_ka;
    return Unit;
}

unit_t scalarProd_part_4()
{
    vec_int_t v_ma;
    v_ma = q_vec_int_get(&ch3);
    q_vec_int_put(&ch10, prod(v_ma));
    q_vec_int_put(&ch10, prod(v_ma));
    return Unit;
}

q_vec_int_t ch11 = { 0, 0, 0, { } };

unit_t scalarProd_part_5()
{
    vec_int_t v_na;
    v_na = q_vec_int_get(&ch4);
    vec_int_t v_oa;
    v_oa = q_vec_int_get(&ch11);
    pair_vec_int_vec_int_t v_pa;
    v_pa.fst = prod(v_na);
    v_pa.snd = v_oa;
    vec_int_t v_qa;
    v_qa = q_vec_int_get(&ch11);
    pair_vec_int_vec_int_t v_ra;
    v_ra.fst = prod(v_na);
    v_ra.snd = v_qa;
    q_vec_int_put(&ch9, cat(v_ra));
    return Unit;
}

unit_t scalarProd_part_6()
{
    vec_int_t v_sa;
    v_sa = q_vec_int_get(&ch5);
    q_vec_int_put(&ch11, prod(v_sa));
    q_vec_int_put(&ch11, prod(v_sa));
    return Unit;
}

void * fun_thread_1(void * arg)
{
    scalarProd_part_1();
    return NULL;
}

void * fun_thread_2(void * arg)
{
    scalarProd_part_2();
    return NULL;
}

void * fun_thread_3(void * arg)
{
    scalarProd_part_3();
    return NULL;
}

void * fun_thread_4(void * arg)
{
    scalarProd_part_4();
    return NULL;
}

void * fun_thread_5(void * arg)
{
    scalarProd_part_5();
    return NULL;
}

void * fun_thread_6(void * arg)
{
    scalarProd_part_6();
    return NULL;
}

vec_int_t scalarProd(vec_int_t v_ta)
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
    return scalarProd_part_0(v_ta);
}
