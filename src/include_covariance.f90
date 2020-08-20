avg1 = mean(x)
avg2 = mean(y)
res = SUM((x(:) - avg1)*(y(:) - avg2))
res = res / REAL(SIZE(x)-1,prec)
