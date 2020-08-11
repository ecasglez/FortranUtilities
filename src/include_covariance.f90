avg1 = mean(values1)
avg2 = mean(values2)
res = SUM((values1(:) - avg1)*(values2(:) - avg2))
res = res / REAL(SIZE(values1)-1,prec)
