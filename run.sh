#! /bin/sh

F=$1

make all opt
make -C etoile all opt

./mince.opt -dot $F.txt
./etoile/etoile.opt $F.txt > $F.dat
./etoile/etoile.opt main.dep.txt > $F.dep.dat
./etoile/etoile.opt main.indep.txt > $F.indep.dat

oplrun opl.mod $F.dat
oplrun opl.mod $F.dep.dat
oplrun opl.mod $F.indep.dat