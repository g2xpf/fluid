#!/bin/sh

ghc --make -O -dynamic ./log.hs
if [ -e ./log.o ]; then
    rm ./log.o
    echo "successfully deleted \"log.o\""
fi
if [ -e ./log.hi ]; then
    rm ./log.hi
    echo "successfully deleted \"log.hi\""
fi
./log
gnuplot -e "set xlabel \"r / h\";
set ylabel \"rho[kg/m^3]\";
plot \"./output1.log\" with lines title \"p1 of particles\", \"./output2.log\" with lines title \"p2 of particles\""
echo "successfully plotted"
