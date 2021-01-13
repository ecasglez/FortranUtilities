#Tested with Gnuplot 5.2.2

set terminal png size 700,700

set output "example3.png"

set xrange[0:10]
unset key

linreg(x) = 0.7948 * x + 0.5358
logreg(x) = 0.9877 * log(x) + 0.5260
expreg(x) = 0.8921 * exp(0.1926 * x)
potreg(x) = 3.6021 * x**0.4330

set multiplot layout 2,2

set title "Linear Regression"
set label 1 "f(x) = 0.7948 * x + 0.5358" at 1,8
plot "example3.dat" u 1:2 pt 7 lc rgb "blue" with points ,\
     linreg(x) lc rgb "red" lw 2 with lines

set title "Logarithmic Regression"
set label 1 "f(x) = 0.9877 * ln(x) + 0.5260" at 2,-1
plot "example3.dat" u 1:3 pt 7 lc rgb "blue" with points ,\
     logreg(x) lc rgb "red" lw 2 with lines

set title "Exponential Regression"
set label 1 "f(x) = 0.8921 * exp(0.1926 * x)" at 1, 6
plot "example3.dat" u 1:4 pt 7 lc rgb "blue" with points ,\
     expreg(x) lc rgb "red" lw 2 with lines

set title "Potential Regression"
set label 1 "f(x) = 3.6021 * x**0.4330" at 2,2
plot "example3.dat" u 1:5 pt 7 lc rgb "blue" with points ,\
     potreg(x) lc rgb "red" lw 2 with lines



