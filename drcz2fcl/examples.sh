#!/bin/sh

echo "mk-adder"
cat mk-adder.d2
echo ""
echo "mk-adder cps"
guile cps-transformer.scm < map-adder.d2 
echo ""
echo "mk-adder d17n"
guile d17n-proto2.scm < mk-adder.d2 
echo ""
echo "mk-adder cps+d17n"
guile cps-transformer.scm < map-adder.d2 | guile d17n-proto2.scm 
echo ""
echo "->fcl"
./compile.sh mk-adder.d2
./compile.sh mk-adder.d2 > mk-adder.fcl

