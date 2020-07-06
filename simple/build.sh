#!/bin/bash

rm -f *.o *.mod check
gfortran interpreter.f90 check.f90 -o check
