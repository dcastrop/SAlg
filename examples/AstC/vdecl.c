struct {int tag; union {int x; char *y;} val;} v = {0, { .x = 1 }};

protocol DnC (n : nat, r : role) {
  case n of
    Z -> end
    S m -> {
      new R1;
      new R2;
      msg(Int) from r to R1;
      msg(Int) from r to R2;
      Tree(n-1, R1);
      Tree(n-1, R2);
      msg(Char) from R1 to r;
      msg(Char) from R2 to r;
    }
}

protocol DAG(n : nat, r1 r2 : role) {
  case n of
    Z -> end
    S m -> {
      new R1 R2 R3 R4;
      msg(Int) from r1 to R1;
      msg(Int) from r1 to R2;
      DAG(n-1, R1, R3);
      DAG(n-1, R2, R4);
      msg(Char) from R3 to r2;
      msg(Char) from R4 to r2;
    }
}
