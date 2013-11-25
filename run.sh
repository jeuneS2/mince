#! /bin/sh

# test case: fas
F=fas
# example latencies for fas:
L1='endOf(s["gnc0_0"]) - startOf(s["gps0_0"])'
L2='endOf(s["gnc0_0"]) - startOf(s["gps0_GPS_Acq85_0"])'

make all opt || exit 1
make -C etoile all opt || exit 1

./mince.opt -dot $F.txt
./etoile/etoile.opt $F.txt > $F.dat
./etoile/etoile.opt main.dep.txt > $F.dep.dat
./etoile/etoile.opt main.indep.txt > $F.indep.dat

sed "s/@LATENCY@/$L1/" opl.mod > opllat1.mod
oplrun opllat1.mod $F.dat
oplrun opllat1.mod $F.indep.dat
sed "s/@LATENCY@/$L2/" opl.mod > opllat2.mod
oplrun opllat2.mod $F.dep.dat
