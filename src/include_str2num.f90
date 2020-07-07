IF (.FALSE.) res = mold !To disable compilation warning about unused variable
READ(str,*,IOSTAT=IOERROR) res
IF (IOERROR /= 0) THEN
   WRITE(*,'(A)') 'ERROR in str2num: Inserted string is not an '//vartype//' number'
   STOP exit_error_code
END IF
