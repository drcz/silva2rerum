#!/bin/sh
cat $1 | guile cps-transformer.scm | guile d17n-proto2.scm | guile apply2fcl.scm