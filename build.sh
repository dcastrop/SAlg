#!/bin/zsh

sbuild () {
  stack build
  stack exec -- session-arrc $1.hs
  gcc $1.c main.c -pthread -lm -o $1:l
}
