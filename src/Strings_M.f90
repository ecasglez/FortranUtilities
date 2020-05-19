!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------
!
! MODULE: Strings_M
!
! DESCRIPTION:
!> @brief Useful tools to manipulate strings in Fortran programs.
!
! REVISION HISTORY:
! 07-05-2020 - Initial Version.
!
!> @author Emilio Castro.
!> @version 1.0.
!
!> @copyright See LICENSE file that comes with this distribution.
!--------------------------------------------------------------------

MODULE Strings_M
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: int2char, int2char00000, count_digits_integer, char2int
   PUBLIC :: startsWith, endsWith, splitstr, char2real

   !> Error code issued by all functions in module Strings_M
   INTEGER,PARAMETER :: exit_error_code = 10


CONTAINS


   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Splits a string and returns the portion selected by the user.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param str String that the user wants to split.
   !> @param delimiter String that the users wants to use as a delimiter for splitting.
   !> Optional parameter. Default is Space.
   !> @param fieldNumber Integer indicating which of the divisions to return.
   !> Optional parameter. Default is the first part obtained.
   !> @return A string with the selected part of str. If the fieldNumber does not exists
   !> or if the delimiter does not exists it returns an empty string.
   FUNCTION splitstr(str, delimiter, fieldNumber) RESULT(res)
      CHARACTER(LEN=*), INTENT(IN)            :: str
      INTEGER         , INTENT(IN), OPTIONAL  :: fieldNumber
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: delimiter
      CHARACTER(LEN=:), ALLOCATABLE           :: res
      !Local
      INTEGER                       :: f !field
      CHARACTER(LEN=:), ALLOCATABLE :: d !delimiter
      INTEGER                       :: i, pos

      IF (PRESENT(fieldNumber)) THEN
         f = fieldNumber
      ELSE
         f = 1
      END IF
      IF (PRESENT(delimiter)) THEN
         d = delimiter
      ELSE
         d = ' '
      END IF

      res = str

      ! If the delimiter is in the first positions of the string, remove it
      IF (INDEX(res,d) == 1) THEN
         res = res(LEN(d)+1:)
      END IF
      DO i = 1, f
         pos = INDEX(res,d)
         IF (pos == 0) THEN
            IF (i /= f) THEN
               res = ''
            END IF
            EXIT
         ELSE IF (i == f) THEN
            res = res(:pos-1)
         ELSE
            res = res(pos+LEN(d):)
         END IF
      END DO

   END FUNCTION splitstr






   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Checks if a string starts with a given substring.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param str String that the user wants to check how it starts.
   !> @param substr Substring to search to check if str starts with it.
   !> @return True if the string starts with the substring and False otherwise. If
   !> substr is empty it returns True.
   FUNCTION startsWith(str, substr) RESULT(res)
      CHARACTER(LEN=*), INTENT(IN) :: str
      CHARACTER(LEN=*), INTENT(IN) :: substr
      LOGICAL                      :: res
      res = INDEX(str,substr) == 1
   END FUNCTION startsWith





   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Checks if a string ends with a given substring.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param str String that the user wants to check how it ends.
   !> @param substr Substring to search to check if str ends with it.
   !> @return True if the string ends with the substring and False otherwise. If 
   !> substr is empty it returns True.
   FUNCTION endsWith(str, substr) RESULT(res)
      CHARACTER(LEN=*), INTENT(IN) :: str
      CHARACTER(LEN=*), INTENT(IN) :: substr
      LOGICAL                      :: res
      res = INDEX(str,substr,BACK = .TRUE.) == LEN(str) - LEN(substr) + 1
   END FUNCTION endsWith




   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Converts an integer variable into a character variable. Useful to open
   !> files named sequentially.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param integ Integer number to convert.
   !> @return Character containing the number.

   FUNCTION int2char(integ) RESULT(charac)
      IMPLICIT NONE
      INTEGER,INTENT(IN)            :: integ
      CHARACTER(LEN=:), ALLOCATABLE :: charac

      ALLOCATE(character(len=count_digits_integer(integ)) :: charac)
      WRITE(charac,'(I0)') integ

   END FUNCTION int2char






   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Converts an integer variable into a character variable, filling with leading
   !> zeros up to the limit imposed by the user. Useful to open files named 
   !> sequentially with leading zeros in the name.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param integ Integer number to convert. This number MUST be positive.
   !> @param total_length Number of digits to use, including zeros.
   !> @return Character containing the number.

   FUNCTION int2char00000(integ,total_length) RESULT(charac)
      IMPLICIT NONE
      INTEGER,INTENT(IN)            :: integ
      INTEGER,INTENT(IN)            :: total_length
      CHARACTER(LEN=:), ALLOCATABLE :: charac
      !local
      INTEGER                       :: num_digits, num_zeros

      IF (integ < 0) THEN
         WRITE(*,'(A)') 'ERROR in int2char00000: Number is negative'
         CALL EXIT(exit_error_code)
      END IF
      num_digits = count_digits_integer (integ)
      IF (num_digits > total_length) THEN
         WRITE(*,'(A)') 'ERROR in int2char00000: Total length is not enough'
         CALL EXIT(exit_error_code)
      END IF
      num_zeros = total_length - num_digits

      !Automatic allocation is not allowed in the write statements.
      ALLOCATE(character(len=total_length) :: charac)
      charac(1:num_zeros) = REPEAT('0',num_zeros)
      WRITE(charac(num_zeros+1:),'(I0)') integ
  

   END FUNCTION int2char00000









   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Counts the number of digits of an integer, including the - sign 
   !> in case it is a negative value.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param integ Integer number whose digits are to be counted.
   !> @returns The number of digits of the input number.

   FUNCTION count_digits_integer(integ) RESULT(num_digits)
      IMPLICIT NONE
      INTEGER, VALUE  :: integ
      INTEGER         :: num_digits

      IF (integ < 0) THEN
         num_digits = 2
      ELSE
         num_digits = 1
      END IF
      integ = ABS(integ)
      integ = integ / 10
      DO WHILE (integ /= 0)
         num_digits = num_digits + 1
         integ = integ / 10
      END DO

   END FUNCTION count_digits_integer





   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Converts a character string into an integer.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param charac String to convert to integer.
   !> @returns Integer containing the number of the input string.

   FUNCTION char2int(charac) RESULT(integ)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: charac
      INTEGER                      :: integ
      !local
      INTEGER                      :: IOERROR

      READ(charac,'(I'//int2char(LEN(TRIM(charac)))//')',IOSTAT=IOERROR) integ
      IF (IOERROR /= 0) THEN
         WRITE(*,'(A)') 'ERROR in char2int: Inserted character is not an integer number'
         CALL EXIT(exit_error_code)
      END IF
   END FUNCTION char2int





   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Converts a character string into a real with double precision.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param charac String to convert to double precision real.
   !> @returns Double preicision containing the number of the input string.

   FUNCTION char2real(charac) RESULT(realnum)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: charac
      REAL(KIND=8)                 :: realnum
      !local
      INTEGER                      :: IOERROR

      READ(charac,'(F'//int2char(LEN(TRIM(charac)))//'.0)',IOSTAT=IOERROR) realnum
      IF (IOERROR /= 0) THEN
         WRITE(*,'(A)') 'ERROR in char2real: Inserted character is not a real number'
         CALL EXIT(exit_error_code)
      END IF
   END FUNCTION char2real





END MODULE Strings_M
