#include "MapRed.h"

q_either_unit_unit_t ch0 = { 0, 0, 0, { } };

void q_either_unit_unit_put(q_either_unit_unit_t * ch, tag_t v_k)
{
    while (1)
    {
        while (ch->q_size >= 16)
        {
        }
        if (ch->q_size < 16)
        {
            ch->q_mem[ch->q_head] = v_k;
            ch->q_head = (ch->q_head + 1) % 16;
            ch->q_size++;
            return;
        }
    }
}

tag_t q_either_unit_unit_get(q_either_unit_unit_t * ch)
{
    tag_t v_l;
    while (1)
    {
        while (ch->q_size <= 0)
        {
        }
        if (ch->q_size > 0)
        {
            v_l = ch->q_mem[ch->q_tail];
            ch->q_tail = (ch->q_tail + 1) % 16;
            ch->q_size--;
            return v_l;
        }
    }
}

q_either_unit_unit_t ch1 = { 0, 0, 0, { } };

q_either_unit_unit_t ch2 = { 0, 0, 0, { } };

q_either_unit_unit_t ch3 = { 0, 0, 0, { } };

q_either_unit_unit_t ch4 = { 0, 0, 0, { } };

q_either_unit_unit_t ch5 = { 0, 0, 0, { } };

q_either_unit_unit_t ch6 = { 0, 0, 0, { } };

q_vec_int_t ch7 = { 0, 0, 0, { } };

void q_vec_int_put(q_vec_int_t * ch, vec_int_t v_sb)
{
    while (1)
    {
        while (ch->q_size >= 16)
        {
        }
        if (ch->q_size < 16)
        {
            ch->q_mem[ch->q_head] = v_sb;
            ch->q_head = (ch->q_head + 1) % 16;
            ch->q_size++;
            return;
        }
    }
}

vec_int_t q_vec_int_get(q_vec_int_t * ch)
{
    vec_int_t v_tb;
    while (1)
    {
        while (ch->q_size <= 0)
        {
        }
        if (ch->q_size > 0)
        {
            v_tb = ch->q_mem[ch->q_tail];
            ch->q_tail = (ch->q_tail + 1) % 16;
            ch->q_size--;
            return v_tb;
        }
    }
}

q_vec_int_t ch8 = { 0, 0, 0, { } };

q_vec_int_t ch9 = { 0, 0, 0, { } };

q_vec_int_t ch10 = { 0, 0, 0, { } };

q_vec_int_t ch11 = { 0, 0, 0, { } };

vec_int_t dotProd_part_0(pair_vec_int_vec_int_t v_a)
{
    vec_int_t v_b;
    v_b = v_a.fst;
    int v_c;
    v_c = v_b.size / 4;
    int v_d;
    v_d = v_c == 0;
    either_pair_int_pair_vec_int_vec_int_pair_int_pair_vec_int_vec_int_t v_e;
    if (v_d)
    {
        either_pair_int_pair_vec_int_vec_int_pair_int_pair_vec_int_vec_int_t v_f;
        v_f.tag = Inl;
        pair_int_pair_vec_int_vec_int_t v_g;
        v_g.fst = v_c;
        v_g.snd = v_a;
        v_f.val.inl = v_g;
        v_e = v_f;
    }
    else
    {
        either_pair_int_pair_vec_int_vec_int_pair_int_pair_vec_int_vec_int_t v_h;
        v_h.tag = Inr;
        pair_int_pair_vec_int_vec_int_t v_i;
        v_i.fst = v_c;
        v_i.snd = v_a;
        v_h.val.inr = v_i;
        v_e = v_h;
    }
    unit_t v_j;
    switch (v_e.tag)
    {
    case Inl:
        {
            q_either_unit_unit_put(&ch0, Inl);
            q_either_unit_unit_put(&ch1, Inl);
            q_either_unit_unit_put(&ch2, Inl);
            q_either_unit_unit_put(&ch3, Inl);
            q_either_unit_unit_put(&ch4, Inl);
            q_either_unit_unit_put(&ch5, Inl);
            q_either_unit_unit_put(&ch6, Inl);
            v_j = Unit;
            break;
        }
    case Inr:
        {
            q_either_unit_unit_put(&ch0, Inr);
            q_either_unit_unit_put(&ch1, Inr);
            q_either_unit_unit_put(&ch2, Inr);
            q_either_unit_unit_put(&ch3, Inr);
            q_either_unit_unit_put(&ch4, Inr);
            q_either_unit_unit_put(&ch5, Inr);
            q_either_unit_unit_put(&ch6, Inr);
            v_j = Unit;
        }
    }
    vec_int_t v_m;
    switch (v_e.tag)
    {
    case Inl:
        {
            pair_vec_int_vec_int_t v_n;
            v_n = v_e.val.inl.snd;
            v_m = dot(v_n);
            break;
        }
    case Inr:
        {
            int v_o;
            int v_p;
            v_p = v_e.val.inr.fst;
            v_o = 0 * v_p;
            vec_int_t v_q;
            vec_int_t v_r;
            pair_vec_int_vec_int_t v_s;
            v_s = v_e.val.inr.snd;
            v_r = v_s.fst;
            v_q.size = v_r.size - v_o;
            v_q.elems = v_r.elems + v_o;
            vec_int_t v_t;
            int v_u;
            v_u = v_e.val.inr.fst;
            v_t.size = v_u;
            v_t.elems = v_q.elems;
            int v_v;
            int v_w;
            v_w = v_e.val.inr.fst;
            v_v = 0 * v_w;
            vec_int_t v_x;
            vec_int_t v_y;
            pair_vec_int_vec_int_t v_z;
            v_z = v_e.val.inr.snd;
            v_y = v_z.snd;
            v_x.size = v_y.size - v_v;
            v_x.elems = v_y.elems + v_v;
            vec_int_t v_aa;
            int v_ba;
            v_ba = v_e.val.inr.fst;
            v_aa.size = v_ba;
            v_aa.elems = v_x.elems;
            int v_ca;
            int v_da;
            v_da = v_e.val.inr.fst;
            v_ca = 1 * v_da;
            vec_int_t v_ea;
            vec_int_t v_fa;
            pair_vec_int_vec_int_t v_ga;
            v_ga = v_e.val.inr.snd;
            v_fa = v_ga.fst;
            v_ea.size = v_fa.size - v_ca;
            v_ea.elems = v_fa.elems + v_ca;
            vec_int_t v_ha;
            int v_ia;
            v_ia = v_e.val.inr.fst;
            v_ha.size = v_ia;
            v_ha.elems = v_ea.elems;
            int v_ja;
            int v_ka;
            v_ka = v_e.val.inr.fst;
            v_ja = 1 * v_ka;
            vec_int_t v_la;
            vec_int_t v_ma;
            pair_vec_int_vec_int_t v_na;
            v_na = v_e.val.inr.snd;
            v_ma = v_na.snd;
            v_la.size = v_ma.size - v_ja;
            v_la.elems = v_ma.elems + v_ja;
            vec_int_t v_oa;
            int v_pa;
            v_pa = v_e.val.inr.fst;
            v_oa.size = v_pa;
            v_oa.elems = v_la.elems;
            int v_qa;
            int v_ra;
            v_ra = v_e.val.inr.fst;
            v_qa = 2 * v_ra;
            vec_int_t v_sa;
            vec_int_t v_ta;
            pair_vec_int_vec_int_t v_ua;
            v_ua = v_e.val.inr.snd;
            v_ta = v_ua.fst;
            v_sa.size = v_ta.size - v_qa;
            v_sa.elems = v_ta.elems + v_qa;
            vec_int_t v_va;
            int v_wa;
            v_wa = v_e.val.inr.fst;
            v_va.size = v_wa;
            v_va.elems = v_sa.elems;
            int v_xa;
            int v_ya;
            v_ya = v_e.val.inr.fst;
            v_xa = 2 * v_ya;
            vec_int_t v_za;
            vec_int_t v_ab;
            pair_vec_int_vec_int_t v_bb;
            v_bb = v_e.val.inr.snd;
            v_ab = v_bb.snd;
            v_za.size = v_ab.size - v_xa;
            v_za.elems = v_ab.elems + v_xa;
            vec_int_t v_cb;
            int v_db;
            v_db = v_e.val.inr.fst;
            v_cb.size = v_db;
            v_cb.elems = v_za.elems;
            int v_eb;
            int v_fb;
            v_fb = v_e.val.inr.fst;
            v_eb = 3 * v_fb;
            vec_int_t v_gb;
            vec_int_t v_hb;
            pair_vec_int_vec_int_t v_ib;
            v_ib = v_e.val.inr.snd;
            v_hb = v_ib.fst;
            v_gb.size = v_hb.size - v_eb;
            v_gb.elems = v_hb.elems + v_eb;
            vec_int_t v_jb;
            int v_kb;
            v_kb = v_e.val.inr.fst;
            v_jb.size = v_kb;
            v_jb.elems = v_gb.elems;
            int v_lb;
            int v_mb;
            v_mb = v_e.val.inr.fst;
            v_lb = 3 * v_mb;
            vec_int_t v_nb;
            vec_int_t v_ob;
            pair_vec_int_vec_int_t v_pb;
            v_pb = v_e.val.inr.snd;
            v_ob = v_pb.snd;
            v_nb.size = v_ob.size - v_lb;
            v_nb.elems = v_ob.elems + v_lb;
            vec_int_t v_qb;
            int v_rb;
            v_rb = v_e.val.inr.fst;
            v_qb.size = v_rb;
            v_qb.elems = v_nb.elems;
            q_vec_int_put(&ch7, v_t);
            q_vec_int_put(&ch7, v_aa);
            q_vec_int_put(&ch8, v_ha);
            q_vec_int_put(&ch8, v_oa);
            q_vec_int_put(&ch9, v_va);
            q_vec_int_put(&ch9, v_cb);
            q_vec_int_put(&ch10, v_jb);
            q_vec_int_put(&ch10, v_qb);
            vec_int_t v_ub;
            v_ub = q_vec_int_get(&ch11);
            v_m = v_ub;
        }
    }
    return v_m;
}

q_vec_int_t ch12 = { 0, 0, 0, { } };

unit_t dotProd_part_1()
{
    tag_t v_vb;
    v_vb = q_either_unit_unit_get(&ch6);
    unit_t v_wb;
    switch (v_vb)
    {
    case Inl:
        {
            v_wb = Unit;
            break;
        }
    case Inr:
        {
            vec_int_t v_xb;
            v_xb = q_vec_int_get(&ch7);
            vec_int_t v_yb;
            v_yb = q_vec_int_get(&ch7);
            pair_vec_int_vec_int_t v_zb;
            v_zb.fst = v_xb;
            v_zb.snd = v_yb;
            q_vec_int_put(&ch12, dot(v_zb));
            v_wb = Unit;
        }
    }
    return Unit;
}

q_vec_int_t ch13 = { 0, 0, 0, { } };

unit_t dotProd_part_2()
{
    tag_t v_ac;
    v_ac = q_either_unit_unit_get(&ch5);
    unit_t v_bc;
    switch (v_ac)
    {
    case Inl:
        {
            v_bc = Unit;
            break;
        }
    case Inr:
        {
            vec_int_t v_cc;
            v_cc = q_vec_int_get(&ch8);
            vec_int_t v_dc;
            v_dc = q_vec_int_get(&ch8);
            pair_vec_int_vec_int_t v_ec;
            v_ec.fst = v_cc;
            v_ec.snd = v_dc;
            q_vec_int_put(&ch13, dot(v_ec));
            v_bc = Unit;
        }
    }
    return Unit;
}

q_vec_int_t ch14 = { 0, 0, 0, { } };

unit_t dotProd_part_3()
{
    tag_t v_fc;
    v_fc = q_either_unit_unit_get(&ch4);
    unit_t v_gc;
    switch (v_fc)
    {
    case Inl:
        {
            v_gc = Unit;
            break;
        }
    case Inr:
        {
            vec_int_t v_hc;
            v_hc = q_vec_int_get(&ch9);
            vec_int_t v_ic;
            v_ic = q_vec_int_get(&ch9);
            pair_vec_int_vec_int_t v_jc;
            v_jc.fst = v_hc;
            v_jc.snd = v_ic;
            q_vec_int_put(&ch14, dot(v_jc));
            v_gc = Unit;
        }
    }
    return Unit;
}

q_vec_int_t ch15 = { 0, 0, 0, { } };

unit_t dotProd_part_4()
{
    tag_t v_kc;
    v_kc = q_either_unit_unit_get(&ch3);
    unit_t v_lc;
    switch (v_kc)
    {
    case Inl:
        {
            v_lc = Unit;
            break;
        }
    case Inr:
        {
            vec_int_t v_mc;
            v_mc = q_vec_int_get(&ch10);
            vec_int_t v_nc;
            v_nc = q_vec_int_get(&ch10);
            pair_vec_int_vec_int_t v_oc;
            v_oc.fst = v_mc;
            v_oc.snd = v_nc;
            q_vec_int_put(&ch15, dot(v_oc));
            v_lc = Unit;
        }
    }
    return Unit;
}

q_vec_int_t ch16 = { 0, 0, 0, { } };

unit_t dotProd_part_5()
{
    tag_t v_pc;
    v_pc = q_either_unit_unit_get(&ch2);
    unit_t v_qc;
    switch (v_pc)
    {
    case Inl:
        {
            v_qc = Unit;
            break;
        }
    case Inr:
        {
            vec_int_t v_rc;
            v_rc = q_vec_int_get(&ch12);
            vec_int_t v_sc;
            v_sc = q_vec_int_get(&ch13);
            pair_vec_int_vec_int_t v_tc;
            v_tc.fst = v_rc;
            v_tc.snd = v_sc;
            q_vec_int_put(&ch16, sum(v_tc));
            v_qc = Unit;
        }
    }
    return Unit;
}

q_vec_int_t ch17 = { 0, 0, 0, { } };

unit_t dotProd_part_6()
{
    tag_t v_uc;
    v_uc = q_either_unit_unit_get(&ch1);
    unit_t v_vc;
    switch (v_uc)
    {
    case Inl:
        {
            v_vc = Unit;
            break;
        }
    case Inr:
        {
            vec_int_t v_wc;
            v_wc = q_vec_int_get(&ch14);
            vec_int_t v_xc;
            v_xc = q_vec_int_get(&ch15);
            pair_vec_int_vec_int_t v_yc;
            v_yc.fst = v_wc;
            v_yc.snd = v_xc;
            q_vec_int_put(&ch17, sum(v_yc));
            v_vc = Unit;
        }
    }
    return Unit;
}

unit_t dotProd_part_7()
{
    tag_t v_zc;
    v_zc = q_either_unit_unit_get(&ch0);
    unit_t v_ad;
    switch (v_zc)
    {
    case Inl:
        {
            v_ad = Unit;
            break;
        }
    case Inr:
        {
            vec_int_t v_bd;
            v_bd = q_vec_int_get(&ch16);
            vec_int_t v_cd;
            v_cd = q_vec_int_get(&ch17);
            pair_vec_int_vec_int_t v_dd;
            v_dd.fst = v_bd;
            v_dd.snd = v_cd;
            q_vec_int_put(&ch11, sum(v_dd));
            v_ad = Unit;
        }
    }
    return Unit;
}

void * fun_thread_1(void * arg)
{
    dotProd_part_1();
    return NULL;
}

void * fun_thread_2(void * arg)
{
    dotProd_part_2();
    return NULL;
}

void * fun_thread_3(void * arg)
{
    dotProd_part_3();
    return NULL;
}

void * fun_thread_4(void * arg)
{
    dotProd_part_4();
    return NULL;
}

void * fun_thread_5(void * arg)
{
    dotProd_part_5();
    return NULL;
}

void * fun_thread_6(void * arg)
{
    dotProd_part_6();
    return NULL;
}

void * fun_thread_7(void * arg)
{
    dotProd_part_7();
    return NULL;
}

vec_int_t dotProd(pair_vec_int_vec_int_t v_ed)
{
    pthread_t thread1;
    pthread_t thread2;
    pthread_t thread3;
    pthread_t thread4;
    pthread_t thread5;
    pthread_t thread6;
    pthread_t thread7;
    pthread_create(&thread1, NULL, fun_thread_1, NULL);
    pthread_create(&thread2, NULL, fun_thread_2, NULL);
    pthread_create(&thread3, NULL, fun_thread_3, NULL);
    pthread_create(&thread4, NULL, fun_thread_4, NULL);
    pthread_create(&thread5, NULL, fun_thread_5, NULL);
    pthread_create(&thread6, NULL, fun_thread_6, NULL);
    pthread_create(&thread7, NULL, fun_thread_7, NULL);
    return dotProd_part_0(v_ed);
}
