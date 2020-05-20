IF (integ < 0) THEN
   WRITE(*,'(A)') 'ERROR in int2str00000: Number is negative'
   STOP exit_error_code
END IF
num_digits = count_digits_integer (integ)
IF (num_digits > total_length) THEN
   WRITE(*,'(A)') 'ERROR in int2str00000: Total length is not enough'
   STOP exit_error_code
END IF
num_zeros = total_length - num_digits

!Automatic allocation is not allowed in the write statements.
ALLOCATE(character(len=total_length) :: str)
str(1:num_zeros) = REPEAT('0',num_zeros)
WRITE(str(num_zeros+1:),'(I0)') integ
