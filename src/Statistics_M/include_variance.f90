IF (SIZE(values) <= 0) THEN
   WRITE(*,'(A)') 'ERROR in variance: Input array dimension is lower than 0'
   STOP exit_error_code
END IF
res = 0._prec
avg = mean(values)
DO i = 1, SIZE(values)
   res = res + (values(i) - avg)**2
END DO
res = res / REAL(SIZE(values) - 1,prec)
