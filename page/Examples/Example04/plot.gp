#Tested with Gnuplot 5.2.2

set terminal png size 700,700

set output "example4.png"

set xrange[0:10]
unset key

set label at 0.364,184.084 " 0.364,184.084" point pointtype 7 pointsize 2
set label at 4.111,162.341 " 4.111,162.341" point pointtype 7 pointsize 2

plot "example4.dat" u 1:2 skip 2 lw 2 with lines


