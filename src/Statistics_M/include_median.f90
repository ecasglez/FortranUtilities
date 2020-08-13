  values_cp = values
  size_values = SIZE(values_cp)
  CALL c_sort(values_cp,size_values)
  IF (MOD(size_values,2) == 0) THEN
     res = (values_cp(size_values / 2) + values_cp(size_values / 2 + 1)) / REAL(2,prec)
  ELSE
     res = values_cp(size_values / 2 + 1)
  END IF
