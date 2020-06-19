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

   USE Prec_M

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: num2str, int2str00000, count_digits_integer, str2num
   PUBLIC :: startsWith, endsWith, splitstr

   !> Error code issued by all functions in module Strings_M
   INTEGER,PARAMETER :: exit_error_code = 10



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
   !> @param fieldNumber Integer indicating which of the divisions to return.
   !> @param delimiter String that the users wants to use as a delimiter for splitting.
   !> Optional parameter. Default is Space.
   !> @param rev. Logical variable. If true start spliting by the end of the string.
   !> Optional parameter. Default is False.
   !> @return A string with the selected part of str. If the fieldNumber does not exists
   !> or if the delimiter does not exists it returns an empty string.
   INTERFACE splitstr
      MODULE PROCEDURE splitstr_i8
      MODULE PROCEDURE splitstr_i16
      MODULE PROCEDURE splitstr_i32
      MODULE PROCEDURE splitstr_i64
   END INTERFACE splitstr



   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Converts an integer or real variable into a string variable.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param num Integer or real number to convert.
   !> @return String containing the number.
   INTERFACE num2str
      MODULE PROCEDURE num2str_i8
      MODULE PROCEDURE num2str_i16
      MODULE PROCEDURE num2str_i32
      MODULE PROCEDURE num2str_i64
      MODULE PROCEDURE num2str_sp
      MODULE PROCEDURE num2str_dp
      MODULE PROCEDURE num2str_qp
   END INTERFACE num2str


   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Converts an integer variable into a string variable, filling with leading
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
   !> @return string containing the number.
   INTERFACE int2str00000
      MODULE PROCEDURE int2str00000_i8
      MODULE PROCEDURE int2str00000_i16
      MODULE PROCEDURE int2str00000_i32
      MODULE PROCEDURE int2str00000_i64
   END INTERFACE int2str00000




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
   INTERFACE count_digits_integer
      MODULE PROCEDURE count_digits_integer_i8
      MODULE PROCEDURE count_digits_integer_i16
      MODULE PROCEDURE count_digits_integer_i32
      MODULE PROCEDURE count_digits_integer_i64
   END INTERFACE count_digits_integer





   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Converts a string into an integer or real number as specified by the type of variable mold.
   !
   ! REVISION HISTORY:
   ! 07-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param str String to convert to integer or real.
   !> @param mold Real or integer value to identify the type and kind of the output
   !> @returns Integer or real containing the number of the input string.
   INTERFACE str2num
      MODULE PROCEDURE str2num_i8
      MODULE PROCEDURE str2num_i16
      MODULE PROCEDURE str2num_i32
      MODULE PROCEDURE str2num_i64
      MODULE PROCEDURE str2num_sp
      MODULE PROCEDURE str2num_dp
      MODULE PROCEDURE str2num_qp
   END INTERFACE str2num




CONTAINS




   FUNCTION splitstr_i8(str, fieldNumber, delimiter, rev) RESULT(res)
      CHARACTER(LEN=*), INTENT(IN)            :: str
      INTEGER(KIND=i8), INTENT(IN)            :: fieldNumber
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: delimiter
      LOGICAL         , INTENT(IN), OPTIONAL  :: rev
      CHARACTER(LEN=:), ALLOCATABLE           :: res
      !Local
      CHARACTER(LEN=:), ALLOCATABLE :: d !delimiter
      INTEGER(KIND=i8)              :: i
      INTEGER                       :: pos

      INCLUDE 'Strings_M/include_splitstr.f90'

   END FUNCTION splitstr_i8

   FUNCTION splitstr_i16(str, fieldNumber, delimiter, rev) RESULT(res)
      CHARACTER(LEN=*) , INTENT(IN)            :: str
      INTEGER(KIND=i16), INTENT(IN)            :: fieldNumber
      CHARACTER(LEN=*) , INTENT(IN), OPTIONAL  :: delimiter
      LOGICAL          , INTENT(IN), OPTIONAL  :: rev
      CHARACTER(LEN=:) , ALLOCATABLE           :: res
      !Local
      CHARACTER(LEN=:), ALLOCATABLE :: d !delimiter
      INTEGER(KIND=i16)             :: i
      INTEGER                       :: pos

      INCLUDE 'Strings_M/include_splitstr.f90'

   END FUNCTION splitstr_i16

   FUNCTION splitstr_i32(str, fieldNumber, delimiter, rev) RESULT(res)
      CHARACTER(LEN=*) , INTENT(IN)            :: str
      INTEGER(KIND=i32), INTENT(IN)            :: fieldNumber
      CHARACTER(LEN=*) , INTENT(IN), OPTIONAL  :: delimiter
      LOGICAL          , INTENT(IN), OPTIONAL  :: rev
      CHARACTER(LEN=:) , ALLOCATABLE           :: res
      !Local
      CHARACTER(LEN=:), ALLOCATABLE :: d !delimiter
      INTEGER(KIND=i32)             :: i
      INTEGER                       :: pos

      INCLUDE 'Strings_M/include_splitstr.f90'

   END FUNCTION splitstr_i32

   FUNCTION splitstr_i64(str, fieldNumber, delimiter, rev) RESULT(res)
      CHARACTER(LEN=*) , INTENT(IN)            :: str
      INTEGER(KIND=i64), INTENT(IN)            :: fieldNumber
      CHARACTER(LEN=*) , INTENT(IN), OPTIONAL  :: delimiter
      LOGICAL          , INTENT(IN), OPTIONAL  :: rev
      CHARACTER(LEN=:) , ALLOCATABLE           :: res
      !Local
      CHARACTER(LEN=:), ALLOCATABLE :: d !delimiter
      INTEGER(KIND=i64)             :: i
      INTEGER                       :: pos

      INCLUDE 'Strings_M/include_splitstr.f90'

   END FUNCTION splitstr_i64






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



   FUNCTION num2str_i8(num) RESULT(str)
      IMPLICIT NONE
      INTEGER(KIND=i8), INTENT(IN)  :: num
      CHARACTER(LEN=:), ALLOCATABLE :: str
      CHARACTER(LEN=4), PARAMETER   :: formato = '(I0)'

      INCLUDE 'Strings_M/include_num2strInt.f90'

   END FUNCTION num2str_i8

   FUNCTION num2str_i16(num) RESULT(str)
      IMPLICIT NONE
      INTEGER(KIND=i16),INTENT(IN)  :: num
      CHARACTER(LEN=:), ALLOCATABLE :: str
      CHARACTER(LEN=4), PARAMETER   :: formato = '(I0)'

      INCLUDE 'Strings_M/include_num2strInt.f90'

   END FUNCTION num2str_i16

   FUNCTION num2str_i32(num) RESULT(str)
      IMPLICIT NONE
      INTEGER(KIND=i32),INTENT(IN)  :: num
      CHARACTER(LEN=:), ALLOCATABLE :: str
      CHARACTER(LEN=4), PARAMETER   :: formato = '(I0)'

      INCLUDE 'Strings_M/include_num2strInt.f90'

   END FUNCTION num2str_i32

   FUNCTION num2str_i64(num) RESULT(str)
      IMPLICIT NONE
      INTEGER(KIND=i64),INTENT(IN)  :: num
      CHARACTER(LEN=:), ALLOCATABLE :: str
      CHARACTER(LEN=4), PARAMETER   :: formato = '(I0)'

      INCLUDE 'Strings_M/include_num2strInt.f90'

   END FUNCTION num2str_i64

   FUNCTION num2str_sp(num,formato) RESULT(str)
      IMPLICIT NONE
      REAL(KIND=sp)   ,INTENT(IN)   :: num
      CHARACTER(LEN=*), INTENT(IN)  :: formato
      CHARACTER(LEN=:), ALLOCATABLE :: str
      INTEGER                       :: length

      INCLUDE 'Strings_M/include_num2strReal.f90'

   END FUNCTION num2str_sp

   FUNCTION num2str_dp(num,formato) RESULT(str)
      IMPLICIT NONE
      REAL(KIND=dp)   ,INTENT(IN)   :: num
      CHARACTER(LEN=*), INTENT(IN)  :: formato
      CHARACTER(LEN=:), ALLOCATABLE :: str
      INTEGER                       :: length

      INCLUDE 'Strings_M/include_num2strReal.f90'

   END FUNCTION num2str_dp

   FUNCTION num2str_qp(num,formato) RESULT(str)
      IMPLICIT NONE
      REAL(KIND=qp)   ,INTENT(IN)   :: num
      CHARACTER(LEN=*), INTENT(IN)  :: formato
      CHARACTER(LEN=:), ALLOCATABLE :: str
      INTEGER                       :: length

      INCLUDE 'Strings_M/include_num2strReal.f90'

   END FUNCTION num2str_qp








   FUNCTION int2str00000_i8(integ,total_length) RESULT(str)
      IMPLICIT NONE
      INTEGER(KIND=i8),INTENT(IN)  :: integ
      INTEGER(KIND=i8),INTENT(IN)  :: total_length
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !local
      INTEGER(KIND=i8)             :: num_digits, num_zeros

      INCLUDE 'Strings_M/include_int2str00000.f90' 

   END FUNCTION int2str00000_i8

   FUNCTION int2str00000_i16(integ,total_length) RESULT(str)
      IMPLICIT NONE
      INTEGER(KIND=i16),INTENT(IN) :: integ
      INTEGER(KIND=i16),INTENT(IN) :: total_length
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !local
      INTEGER(KIND=i16)            :: num_digits, num_zeros

      INCLUDE 'Strings_M/include_int2str00000.f90' 

   END FUNCTION int2str00000_i16

   FUNCTION int2str00000_i32(integ,total_length) RESULT(str)
      IMPLICIT NONE
      INTEGER(KIND=i32),INTENT(IN) :: integ
      INTEGER(KIND=i32),INTENT(IN) :: total_length
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !local
      INTEGER(KIND=i32)            :: num_digits, num_zeros

      INCLUDE 'Strings_M/include_int2str00000.f90' 

   END FUNCTION int2str00000_i32

   FUNCTION int2str00000_i64(integ,total_length) RESULT(str)
      IMPLICIT NONE
      INTEGER(KIND=i64),INTENT(IN) :: integ
      INTEGER(KIND=i64),INTENT(IN) :: total_length
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !local
      INTEGER(KIND=i64)            :: num_digits, num_zeros

      INCLUDE 'Strings_M/include_int2str00000.f90' 

   END FUNCTION int2str00000_i64










   PURE FUNCTION count_digits_integer_i8(i) RESULT(num_digits)
      IMPLICIT NONE
      INTEGER(KIND=i8), INTENT(IN):: i
      INTEGER(KIND=i8)            :: num_digits
      INTEGER(KIND=i8), PARAMETER :: ten = 10, one = 1, two = 2
      INTEGER(KIND=i8)            :: integ

      INCLUDE 'Strings_M/include_count_digits_integer.f90'

   END FUNCTION count_digits_integer_i8

   PURE FUNCTION count_digits_integer_i16(i) RESULT(num_digits)
      IMPLICIT NONE
      INTEGER(KIND=i16), INTENT(IN):: i
      INTEGER(KIND=i16)            :: num_digits
      INTEGER(KIND=i16), PARAMETER :: ten = 10, one = 1, two = 2
      INTEGER(KIND=i16)            :: integ

      INCLUDE 'Strings_M/include_count_digits_integer.f90'

   END FUNCTION count_digits_integer_i16

   PURE FUNCTION count_digits_integer_i32(i) RESULT(num_digits)
      IMPLICIT NONE
      INTEGER(KIND=i32), INTENT(IN):: i
      INTEGER(KIND=i32)            :: num_digits
      INTEGER(KIND=i32), PARAMETER :: ten = 10, one = 1, two = 2
      INTEGER(KIND=i32)            :: integ

      INCLUDE 'Strings_M/include_count_digits_integer.f90'

   END FUNCTION count_digits_integer_i32

   PURE FUNCTION count_digits_integer_i64(i) RESULT(num_digits)
      IMPLICIT NONE
      INTEGER(KIND=i64), INTENT(IN):: i
      INTEGER(KIND=i64)            :: num_digits
      INTEGER(KIND=i64), PARAMETER :: ten = 10, one = 1, two = 2
      INTEGER(KIND=i64)            :: integ

      INCLUDE 'Strings_M/include_count_digits_integer.f90'

   END FUNCTION count_digits_integer_i64






   FUNCTION str2num_i8(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: str
      INTEGER(KIND=i8), INTENT(IN) :: mold
      INTEGER(KIND=i8)             :: res
      INTEGER                      :: IOERROR
      CHARACTER(LEN=7), PARAMETER  :: vartype = 'integer'

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_i8

   FUNCTION str2num_i16(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      INTEGER(KIND=i16), INTENT(IN) :: mold
      INTEGER(KIND=i16)             :: res
      INTEGER                       :: IOERROR
      CHARACTER(LEN=7), PARAMETER   :: vartype = 'integer'

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_i16

   FUNCTION str2num_i32(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      INTEGER(KIND=i32), INTENT(IN) :: mold
      INTEGER(KIND=i32)             :: res
      INTEGER                       :: IOERROR
      CHARACTER(LEN=7), PARAMETER   :: vartype = 'integer'

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_i32

   FUNCTION str2num_i64(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      INTEGER(KIND=i64), INTENT(IN) :: mold
      INTEGER(KIND=i64)             :: res
      INTEGER                       :: IOERROR
      CHARACTER(LEN=7), PARAMETER   :: vartype = 'integer'

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_i64

   FUNCTION str2num_sp(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: str
      REAL(KIND=sp)   , INTENT(IN) :: mold
      REAL(KIND=sp)                :: res
      INTEGER                      :: IOERROR
      CHARACTER(LEN=4), PARAMETER  :: vartype = 'real'

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_sp

   FUNCTION str2num_dp(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: str
      REAL(KIND=dp)   , INTENT(IN) :: mold
      REAL(KIND=dp)                :: res
      INTEGER                      :: IOERROR
      CHARACTER(LEN=4), PARAMETER  :: vartype = 'real'

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_dp

   FUNCTION str2num_qp(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: str
      REAL(KIND=qp)   , INTENT(IN) :: mold
      REAL(KIND=qp)                :: res
      INTEGER                      :: IOERROR
      CHARACTER(LEN=4), PARAMETER  :: vartype = 'real'

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_qp


   FUNCTION strReverse(str) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      CHARACTER(LEN=:), ALLOCATABLE :: res
      INTEGER                       :: i
      res = str
      FORALL (i=1:len(res)) res(i:i) = res(len(res)-i+1:len(res)-i+1)
   END FUNCTION strReverse




END MODULE Strings_M
