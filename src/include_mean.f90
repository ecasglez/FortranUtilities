IF (SIZE(values) <= 0) THEN
   WRITE(*,'(A)') 'ERROR in mean: Input array dimension is lower than 0.'
   STOP exit_error_code
END IF
res = SUM(values) / REAL(SIZE(values),prec)
