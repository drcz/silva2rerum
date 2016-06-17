#!/bin/bash
# converts tmp notation from scheme transformations into "the real" FCL/drcz0 syntax.
cat $1 | sed "s:(LET:(let:g" \
       | sed "s:(GOTO:(goto:g" \
       | sed "s:(IF:(if:g" \
       | sed "s:(EQ:(=:g" \
       | sed "s:(LT:(<:g" \
       | sed "s:(ADD:(+:g" \
       | sed "s:(MUL:(*:g" \
       | sed "s:(SUB:(-:g" \
       | sed "s:(CAR:(.:g" \
       | sed "s:(CDR:(,:g" \
       | sed "s:(CONS:(;:g"
# + maybe some more in the close future...?
