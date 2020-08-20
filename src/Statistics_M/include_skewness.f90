  avg = mean(x)
  sd = stdev(x)
  n = SIZE(x)
  res = SUM((x(:)-avg)**3) / sd**3 * n /(n-1)/(n-2)
