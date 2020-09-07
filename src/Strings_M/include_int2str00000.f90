!Automatic allocation is not allowed in the write statements.
ALLOCATE(character(len=total_length) :: str)
WRITE(str,'(I0.'//num2str(total_length)//')') integ
