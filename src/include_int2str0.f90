!Automatic allocation is not allowed in the write statements.
ALLOCATE(character(len=count_digits_integer(integ)) :: str)
WRITE(str,'(I0)') integ
str = zfill(str, total_length)
