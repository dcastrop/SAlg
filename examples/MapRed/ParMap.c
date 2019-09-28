#include "ParMap.h"

q_vec_vec_int_t ch0 = { 0, 0, 0, { } };

void q_vec_vec_int_put(q_vec_vec_int_t * ch, vec_vec_int_t v_y)
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
            ch->q_mem[ch->q_head] = v_y;
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
    vec_vec_int_t v_z;
    while (1)
    {
        pthread_mutex_lock(&ch->q_mutex);
        while (ch->q_size <= 0)
        {
            pthread_cond_wait(&ch->q_empty, &ch->q_mutex);
        }
        if (ch->q_size > 0)
        {
            v_z = ch->q_mem[ch->q_tail];
            ch->q_tail = (ch->q_tail + 1) % 16;
            ch->q_size--;
            pthread_cond_signal(&ch->q_full);
            pthread_mutex_unlock(&ch->q_mutex);
            return v_z;
        }
        pthread_mutex_unlock(&ch->q_mutex);
    }
}

q_vec_vec_int_t ch1 = { 0, 0, 0, { } };

q_vec_vec_int_t ch2 = { 0, 0, 0, { } };

q_vec_vec_int_t ch3 = { 0, 0, 0, { } };

q_vec_vec_int_t ch4 = { 0, 0, 0, { } };

q_vec_vec_int_t ch5 = { 0, 0, 0, { } };

q_vec_vec_int_t ch6 = { 0, 0, 0, { } };

q_vec_vec_int_t ch7 = { 0, 0, 0, { } };

q_vec_vec_int_t ch8 = { 0, 0, 0, { } };

q_vec_vec_int_t ch9 = { 0, 0, 0, { } };

q_vec_vec_int_t ch10 = { 0, 0, 0, { } };

q_vec_vec_int_t ch11 = { 0, 0, 0, { } };

q_vec_vec_int_t ch12 = { 0, 0, 0, { } };

q_vec_vec_int_t ch13 = { 0, 0, 0, { } };

q_vec_vec_int_t ch14 = { 0, 0, 0, { } };

q_vec_vec_int_t ch15 = { 0, 0, 0, { } };

q_vec_vec_int_t ch16 = { 0, 0, 0, { } };

q_vec_vec_int_t ch17 = { 0, 0, 0, { } };

q_vec_vec_int_t ch18 = { 0, 0, 0, { } };

q_vec_vec_int_t ch19 = { 0, 0, 0, { } };

q_vec_vec_int_t ch20 = { 0, 0, 0, { } };

q_vec_vec_int_t ch21 = { 0, 0, 0, { } };

q_vec_vec_int_t ch22 = { 0, 0, 0, { } };

q_vec_vec_int_t ch23 = { 0, 0, 0, { } };

vec_vec_int_t parProd_part_0(vec_vec_int_t v_a)
{
    int v_b;
    v_b = v_a.size / 11;
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
    vec_vec_int_t v_m;
    v_m.size = v_b;
    v_m.elems = v_l.elems;
    vec_vec_int_t v_n;
    v_n.size = v_l.size - v_b;
    v_n.elems = v_l.elems + v_b;
    vec_vec_int_t v_o;
    v_o.size = v_b;
    v_o.elems = v_n.elems;
    vec_vec_int_t v_p;
    v_p.size = v_n.size - v_b;
    v_p.elems = v_n.elems + v_b;
    vec_vec_int_t v_q;
    v_q.size = v_b;
    v_q.elems = v_p.elems;
    vec_vec_int_t v_r;
    v_r.size = v_p.size - v_b;
    v_r.elems = v_p.elems + v_b;
    vec_vec_int_t v_s;
    v_s.size = v_b;
    v_s.elems = v_r.elems;
    vec_vec_int_t v_t;
    v_t.size = v_r.size - v_b;
    v_t.elems = v_r.elems + v_b;
    vec_vec_int_t v_u;
    v_u.size = v_b;
    v_u.elems = v_t.elems;
    vec_vec_int_t v_v;
    v_v.size = v_t.size - v_b;
    v_v.elems = v_t.elems + v_b;
    vec_vec_int_t v_w;
    v_w.size = v_b;
    v_w.elems = v_v.elems;
    vec_vec_int_t v_x;
    v_x.size = v_v.size - v_b;
    v_x.elems = v_v.elems + v_b;
    q_vec_vec_int_put(&ch0, v_c);
    q_vec_vec_int_put(&ch1, v_e);
    q_vec_vec_int_put(&ch2, v_g);
    q_vec_vec_int_put(&ch3, v_i);
    q_vec_vec_int_put(&ch4, v_k);
    q_vec_vec_int_put(&ch5, v_m);
    q_vec_vec_int_put(&ch6, v_o);
    q_vec_vec_int_put(&ch7, v_q);
    q_vec_vec_int_put(&ch8, v_s);
    q_vec_vec_int_put(&ch9, v_u);
    q_vec_vec_int_put(&ch10, v_w);
    q_vec_vec_int_put(&ch11, v_x);
    vec_vec_int_t v_aa;
    v_aa = q_vec_vec_int_get(&ch12);
    vec_vec_int_t v_ba;
    v_ba = q_vec_vec_int_get(&ch13);
    pair_vec_vec_int_vec_vec_int_t v_ca;
    v_ca.fst = v_aa;
    v_ca.snd = v_ba;
    vec_vec_int_t v_da;
    v_da = q_vec_vec_int_get(&ch14);
    pair_vec_vec_int_vec_vec_int_t v_ea;
    v_ea.fst = v_da;
    v_ea.snd = cat(v_ca);
    vec_vec_int_t v_fa;
    v_fa = q_vec_vec_int_get(&ch15);
    pair_vec_vec_int_vec_vec_int_t v_ga;
    v_ga.fst = v_fa;
    v_ga.snd = cat(v_ea);
    vec_vec_int_t v_ha;
    v_ha = q_vec_vec_int_get(&ch16);
    pair_vec_vec_int_vec_vec_int_t v_ia;
    v_ia.fst = v_ha;
    v_ia.snd = cat(v_ga);
    vec_vec_int_t v_ja;
    v_ja = q_vec_vec_int_get(&ch17);
    pair_vec_vec_int_vec_vec_int_t v_ka;
    v_ka.fst = v_ja;
    v_ka.snd = cat(v_ia);
    vec_vec_int_t v_la;
    v_la = q_vec_vec_int_get(&ch18);
    pair_vec_vec_int_vec_vec_int_t v_ma;
    v_ma.fst = v_la;
    v_ma.snd = cat(v_ka);
    vec_vec_int_t v_na;
    v_na = q_vec_vec_int_get(&ch19);
    pair_vec_vec_int_vec_vec_int_t v_oa;
    v_oa.fst = v_na;
    v_oa.snd = cat(v_ma);
    vec_vec_int_t v_pa;
    v_pa = q_vec_vec_int_get(&ch20);
    pair_vec_vec_int_vec_vec_int_t v_qa;
    v_qa.fst = v_pa;
    v_qa.snd = cat(v_oa);
    vec_vec_int_t v_ra;
    v_ra = q_vec_vec_int_get(&ch21);
    pair_vec_vec_int_vec_vec_int_t v_sa;
    v_sa.fst = v_ra;
    v_sa.snd = cat(v_qa);
    vec_vec_int_t v_ta;
    v_ta = q_vec_vec_int_get(&ch22);
    pair_vec_vec_int_vec_vec_int_t v_ua;
    v_ua.fst = v_ta;
    v_ua.snd = cat(v_sa);
    vec_vec_int_t v_va;
    v_va = q_vec_vec_int_get(&ch23);
    pair_vec_vec_int_vec_vec_int_t v_wa;
    v_wa.fst = v_va;
    v_wa.snd = cat(v_ua);
    return cat(v_wa);
}

unit_t parProd_part_1()
{
    vec_vec_int_t v_xa;
    v_xa = q_vec_vec_int_get(&ch0);
    q_vec_vec_int_put(&ch23, prod(v_xa));
    return Unit;
}

unit_t parProd_part_2()
{
    vec_vec_int_t v_ya;
    v_ya = q_vec_vec_int_get(&ch1);
    q_vec_vec_int_put(&ch22, prod(v_ya));
    return Unit;
}

unit_t parProd_part_3()
{
    vec_vec_int_t v_za;
    v_za = q_vec_vec_int_get(&ch2);
    q_vec_vec_int_put(&ch21, prod(v_za));
    return Unit;
}

unit_t parProd_part_4()
{
    vec_vec_int_t v_ab;
    v_ab = q_vec_vec_int_get(&ch3);
    q_vec_vec_int_put(&ch20, prod(v_ab));
    return Unit;
}

unit_t parProd_part_5()
{
    vec_vec_int_t v_bb;
    v_bb = q_vec_vec_int_get(&ch4);
    q_vec_vec_int_put(&ch19, prod(v_bb));
    return Unit;
}

unit_t parProd_part_6()
{
    vec_vec_int_t v_cb;
    v_cb = q_vec_vec_int_get(&ch5);
    q_vec_vec_int_put(&ch18, prod(v_cb));
    return Unit;
}

unit_t parProd_part_7()
{
    vec_vec_int_t v_db;
    v_db = q_vec_vec_int_get(&ch6);
    q_vec_vec_int_put(&ch17, prod(v_db));
    return Unit;
}

unit_t parProd_part_8()
{
    vec_vec_int_t v_eb;
    v_eb = q_vec_vec_int_get(&ch7);
    q_vec_vec_int_put(&ch16, prod(v_eb));
    return Unit;
}

unit_t parProd_part_9()
{
    vec_vec_int_t v_fb;
    v_fb = q_vec_vec_int_get(&ch8);
    q_vec_vec_int_put(&ch15, prod(v_fb));
    return Unit;
}

unit_t parProd_part_10()
{
    vec_vec_int_t v_gb;
    v_gb = q_vec_vec_int_get(&ch9);
    q_vec_vec_int_put(&ch14, prod(v_gb));
    return Unit;
}

unit_t parProd_part_11()
{
    vec_vec_int_t v_hb;
    v_hb = q_vec_vec_int_get(&ch10);
    q_vec_vec_int_put(&ch12, prod(v_hb));
    return Unit;
}

unit_t parProd_part_12()
{
    vec_vec_int_t v_ib;
    v_ib = q_vec_vec_int_get(&ch11);
    q_vec_vec_int_put(&ch13, prod(v_ib));
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

void * fun_thread_7(void * arg)
{
    parProd_part_7();
    return NULL;
}

void * fun_thread_8(void * arg)
{
    parProd_part_8();
    return NULL;
}

void * fun_thread_9(void * arg)
{
    parProd_part_9();
    return NULL;
}

void * fun_thread_10(void * arg)
{
    parProd_part_10();
    return NULL;
}

void * fun_thread_11(void * arg)
{
    parProd_part_11();
    return NULL;
}

void * fun_thread_12(void * arg)
{
    parProd_part_12();
    return NULL;
}

vec_vec_int_t parProd(vec_vec_int_t v_jb)
{
    pthread_t thread1;
    pthread_t thread2;
    pthread_t thread3;
    pthread_t thread4;
    pthread_t thread5;
    pthread_t thread6;
    pthread_t thread7;
    pthread_t thread8;
    pthread_t thread9;
    pthread_t thread10;
    pthread_t thread11;
    pthread_t thread12;
    pthread_create(&thread1, NULL, fun_thread_1, NULL);
    pthread_create(&thread2, NULL, fun_thread_2, NULL);
    pthread_create(&thread3, NULL, fun_thread_3, NULL);
    pthread_create(&thread4, NULL, fun_thread_4, NULL);
    pthread_create(&thread5, NULL, fun_thread_5, NULL);
    pthread_create(&thread6, NULL, fun_thread_6, NULL);
    pthread_create(&thread7, NULL, fun_thread_7, NULL);
    pthread_create(&thread8, NULL, fun_thread_8, NULL);
    pthread_create(&thread9, NULL, fun_thread_9, NULL);
    pthread_create(&thread10, NULL, fun_thread_10, NULL);
    pthread_create(&thread11, NULL, fun_thread_11, NULL);
    pthread_create(&thread12, NULL, fun_thread_12, NULL);
    return parProd_part_0(v_jb);
}
