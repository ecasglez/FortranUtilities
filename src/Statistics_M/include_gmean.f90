IF (SIZE(values) <= 0) THEN
   WRITE(*,'(A)') 'ERROR in gmean: Input array dimension is lower than 0.'
   STOP exit_error_code
END IF
res = PRODUCT(values)**(1._prec/REAL(SIZE(values),prec))
