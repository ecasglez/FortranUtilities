avg = mean(x)
res = SUM((x(:) - avg)**2)
res = res / REAL(SIZE(x) - 1,prec)
