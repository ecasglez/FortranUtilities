num_digits = count_digits_integer (integ)
num_zeros = total_length - num_digits

!Automatic allocation is not allowed in the write statements.
ALLOCATE(character(len=total_length) :: str)
str(1:num_zeros) = REPEAT('0',num_zeros)
WRITE(str(num_zeros+1:),'(I0)') integ
