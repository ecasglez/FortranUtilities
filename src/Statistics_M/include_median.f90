  x_cp = x
  size_x = SIZE(x_cp)
  CALL c_sort(x_cp,size_x)
  IF (MOD(size_x,2) == 0) THEN
     res = (x_cp(size_x / 2) + x_cp(size_x / 2 + 1)) / REAL(2,prec)
  ELSE
     res = x_cp(size_x / 2 + 1)
  END IF
