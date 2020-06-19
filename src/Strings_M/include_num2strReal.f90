IF (startsWith(formato,'(') .OR. endsWith(formato,')')) THEN
   WRITE(*,*) 'ERROR in num2str. Format description does not have to include "(" at the begining nor&
      & ")" at the end'
   STOP exit_error_code
END IF
!adding the following statement in the allocate statement wihtout using
!variable length is causing memory leaks in valgrind. Using length to
!store the information fixes those memory leaks.
length = str2num(splitstr(formato(2:),1,'.'),1_i16)
ALLOCATE(character(len=length) :: str)
WRITE(str,'('//formato//')') num
