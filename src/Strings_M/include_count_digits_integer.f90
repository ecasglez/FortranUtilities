IF (i < 0) THEN
   num_digits = two
ELSE
   num_digits = one
END IF
integ = ABS(i)
integ = integ / ten
DO WHILE (integ /= 0)
   num_digits = num_digits + one
   integ = integ / ten
END DO
