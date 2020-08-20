  avg = mean(x)
  sd = pstdev(x)
  n = SIZE(x)
  res = SUM((x(:)-avg)**3) / sd**3  / n
