module Main where

import AdventUtil
import P1
import P2
import P3
import P4
import P5
import P6
import P7
import P8
import P9
import P10
import P11
import P12
import P13
import P14
import P15
import P16
import P17

quickProblems = [p1_1, p1_2, p2, p3_1, p3_2, p5_1, p5_2, p7, p8_1, p8_2, p9, p10, p11]
slowProblems = [p4, p6_1]

main :: IO ()
main = sequence_ quickProblems
