#!/bin/bash

# It is assumed that the fast process is the one with mpi rank 0 so it should be started first : 
mpirun -np 1 ./fast_prog : -np 1 ./slow_prog
