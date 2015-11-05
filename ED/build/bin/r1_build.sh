#!/usr/bin/env bash
#
#
module load hdf5_18/1.8.14;
module load openmpi/gcc/64/1.8.5;
./generate_deps.sh;
OPT=opt.gfortran make;
