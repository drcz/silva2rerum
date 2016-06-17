#eg

 $ (cat mk-adder.d2 ; echo "(5)") | guile drcz2-fast.scm
 (7 . 8)
 $ ./compile.sh mk-adder.d2 > mk-adder.fcl
 $ (cat mk-adder.fcl ; echo "(5)") | guile fcl.scm
 (7 . 8)
 
