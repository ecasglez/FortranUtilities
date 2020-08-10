!adding the following statement in the allocate statement wihtout using
!variable length is causing memory leaks in valgrind. Using length to
!store the information fixes those memory leaks.
IF (startsWith(formato,'(')) THEN
   length = str2num(splitstr(formato(3:),1,'.'),1_i16)
ELSE
   length = str2num(splitstr(formato(2:),1,'.'),1_i16)
END IF
ALLOCATE(character(len=length) :: str)
WRITE(str,'('//formato//')') num
