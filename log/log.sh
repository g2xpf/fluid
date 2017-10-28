#!/bin/sh

ghc --make -O -dynamic log.hs
if [ -e log.o ]; then
    rm log.o
    echo "successfully deleted \"log.o\""
fi
if [ -e log.hi ]; then
    rm log.hi
    echo "successfully deleted \"log.hi\""
fi
./log
gnuplot -e "plot \"output.log\" with lines title \"density of particles\""
echo "successfully plotted"
