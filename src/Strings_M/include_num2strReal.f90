IF (startsWith(formato,'(') .OR. endsWith(formato,')')) THEN
   WRITE(*,*) 'ERROR in num2str. Format description does not have to include "(" at the begining nor&
      & ")" at the end'
   STOP exit_error_code
END IF
ALLOCATE(character(len=str2num(splitstr(formato(2:),'.',1),1_i16)) :: str)
WRITE(str,'('//formato//')') num
