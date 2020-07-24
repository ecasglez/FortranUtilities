avg = mean(values)
res = SUM((values(:) - avg)**2)
res = res / REAL(SIZE(values) - 1,prec)
