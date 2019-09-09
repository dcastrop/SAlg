#include<stdio.h>

enum Tag {Inl, Inr};
struct pair1 { int proj1; char *proj2; };
struct pair2 {int proj1; int proj2; struct pair1 proj3; };
struct either3 { enum Tag tag; union { char * inl; struct pair1 inr;} val; };

enum Unit { Unit };

enum Unit x = Unit;
struct pair2 ex = {1, 2, {3, "a"}};

struct either3 ex2 = { Inl, { .inl = "EX1" } };
struct either3 ex3 = { Inr, { .inr = {3, "A"} } };

int main(void){
  printf("%s\n", ex.proj3.proj2);
}
