!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------

MODULE FU_Strings
   !! author: Emilio Castro.
   !! date: 07/05/2020.
   !! version: 1.0.
   !! license: MIT.
   !! summary: Useful tools to manipulate strings in Fortran programs.
   !! Useful tools to manipulate strings in Fortran programs.

   USE FU_Prec

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: num2str, int2str00000, str2num
   PUBLIC :: startsWith, endsWith, splitstr, replace, mergeChars



   INTERFACE splitstr
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Splits a string.
      !! Splits a string and returns the portion selected by the user.
      MODULE PROCEDURE splitstr_i8
      MODULE PROCEDURE splitstr_i16
      MODULE PROCEDURE splitstr_i32
      MODULE PROCEDURE splitstr_i64
   END INTERFACE splitstr



   INTERFACE num2str
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Converts number into a string.
      !! Converts an integer or real variable into a string variable.
      !! Useful to open files named sequentially.
      MODULE PROCEDURE num2str_i8
      MODULE PROCEDURE num2str_i16
      MODULE PROCEDURE num2str_i32
      MODULE PROCEDURE num2str_i64
      MODULE PROCEDURE num2str_sp
      MODULE PROCEDURE num2str_dp
      MODULE PROCEDURE num2str_qp
   END INTERFACE num2str


   INTERFACE int2str00000
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Converts an integer into a string filling with leading zeros.
      !! Converts an integer variable into a string variable,
      !! filling with leading zeros up to the limit imposed by the user.
      !! Useful to open files named sequentially with leading zeros in the name.
      MODULE PROCEDURE int2str00000_i8
      MODULE PROCEDURE int2str00000_i16
      MODULE PROCEDURE int2str00000_i32
      MODULE PROCEDURE int2str00000_i64
   END INTERFACE int2str00000




   INTERFACE str2num
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Converts a string into an integer or real.
      !! Converts a string into an integer or real number as specified by the type of variable mold.
      MODULE PROCEDURE str2num_i8
      MODULE PROCEDURE str2num_i16
      MODULE PROCEDURE str2num_i32
      MODULE PROCEDURE str2num_i64
      MODULE PROCEDURE str2num_sp
      MODULE PROCEDURE str2num_dp
      MODULE PROCEDURE str2num_qp
   END INTERFACE str2num




CONTAINS




   PURE FUNCTION splitstr_i8(str, fieldNumber, delimiter, rev, mergedelim) RESULT(res)
      CHARACTER(LEN=*), INTENT(IN)            :: str
      !! String that the user wants to split.
      INTEGER(KIND=i8), INTENT(IN)            :: fieldNumber
      !! Integer indicating which of the divisions to return.
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: delimiter
      !! String that the users wants to use as a delimiter for splitting.
      !! Optional parameter. Default is Space.
      LOGICAL         , INTENT(IN), OPTIONAL  :: rev
      !! If true start spliting by the end of the string.
      !! Optional parameter. Default is False.
      LOGICAL          , INTENT(IN), OPTIONAL :: mergedelim
      !! If true, contiguous delimiters in the string are merged before splitting.
      !! Optional parameter. Default is False.
      CHARACTER(LEN=:), ALLOCATABLE           :: res
      !! A string with the selected part of str. If the fieldNumber does not exists
      !! or if the delimiter does not exists it returns an empty string.
      !Local
      CHARACTER(LEN=:), ALLOCATABLE :: d !delimiter
      INTEGER(KIND=i8)              :: i
      INTEGER                       :: pos

      INCLUDE 'Strings_M/include_splitstr.f90'

   END FUNCTION splitstr_i8

   PURE FUNCTION splitstr_i16(str, fieldNumber, delimiter, rev, mergedelim) RESULT(res)
      CHARACTER(LEN=*) , INTENT(IN)            :: str
      !! String that the user wants to split.
      INTEGER(KIND=i16), INTENT(IN)            :: fieldNumber
      !! Integer indicating which of the divisions to return.
      CHARACTER(LEN=*) , INTENT(IN), OPTIONAL  :: delimiter
      !! String that the users wants to use as a delimiter for splitting.
      !! Optional parameter. Default is Space.
      LOGICAL          , INTENT(IN), OPTIONAL  :: rev
      !! If true start spliting by the end of the string.
      !! Optional parameter. Default is False.
      LOGICAL          , INTENT(IN), OPTIONAL :: mergedelim
      !! If true, contiguous delimiters in the string are merged before splitting.
      !! Optional parameter. Default is False.
      CHARACTER(LEN=:) , ALLOCATABLE           :: res
      !! A string with the selected part of str. If the fieldNumber does not exists
      !! or if the delimiter does not exists it returns an empty string.
      !Local
      CHARACTER(LEN=:), ALLOCATABLE :: d !delimiter
      INTEGER(KIND=i16)             :: i
      INTEGER                       :: pos

      INCLUDE 'Strings_M/include_splitstr.f90'

   END FUNCTION splitstr_i16

   PURE FUNCTION splitstr_i32(str, fieldNumber, delimiter, rev, mergedelim) RESULT(res)
      CHARACTER(LEN=*) , INTENT(IN)            :: str
      !! String that the user wants to split.
      INTEGER(KIND=i32), INTENT(IN)            :: fieldNumber
      !! Integer indicating which of the divisions to return.
      CHARACTER(LEN=*) , INTENT(IN), OPTIONAL  :: delimiter
      !! String that the users wants to use as a delimiter for splitting.
      !! Optional parameter. Default is Space.
      LOGICAL          , INTENT(IN), OPTIONAL  :: rev
      !! If true start spliting by the end of the string.
      !! Optional parameter. Default is False.
      LOGICAL          , INTENT(IN), OPTIONAL :: mergedelim
      !! If true, contiguous delimiters in the string are merged before splitting.
      !! Optional parameter. Default is False.
      CHARACTER(LEN=:) , ALLOCATABLE           :: res
      !! A string with the selected part of str. If the fieldNumber does not exists
      !! or if the delimiter does not exists it returns an empty string.
      !Local
      CHARACTER(LEN=:), ALLOCATABLE :: d !delimiter
      INTEGER(KIND=i32)             :: i
      INTEGER                       :: pos

      INCLUDE 'Strings_M/include_splitstr.f90'

   END FUNCTION splitstr_i32

   PURE FUNCTION splitstr_i64(str, fieldNumber, delimiter, rev, mergedelim) RESULT(res)
      CHARACTER(LEN=*) , INTENT(IN)            :: str
      !! String that the user wants to split.
      INTEGER(KIND=i64), INTENT(IN)            :: fieldNumber
      !! Integer indicating which of the divisions to return.
      CHARACTER(LEN=*) , INTENT(IN), OPTIONAL  :: delimiter
      !! String that the users wants to use as a delimiter for splitting.
      !! Optional parameter. Default is Space.
      LOGICAL          , INTENT(IN), OPTIONAL  :: rev
      !! If true start spliting by the end of the string.
      !! Optional parameter. Default is False.
      LOGICAL          , INTENT(IN), OPTIONAL :: mergedelim
      !! If true, contiguous delimiters in the string are merged before splitting.
      !! Optional parameter. Default is False.
      CHARACTER(LEN=:) , ALLOCATABLE           :: res
      !! A string with the selected part of str. If the fieldNumber does not exists
      !! or if the delimiter does not exists it returns an empty string.
      !Local
      CHARACTER(LEN=:), ALLOCATABLE :: d !delimiter
      INTEGER(KIND=i64)             :: i
      INTEGER                       :: pos

      INCLUDE 'Strings_M/include_splitstr.f90'

   END FUNCTION splitstr_i64


   PURE FUNCTION mergeChars(str,c) RESULT(res)
      !! author: Emilio Castro.
      !! date: 14/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Merge characters in a string if they are contiguous.
      !! Merge characters in a string if they are contiguous.
      IMPLICIT NONE
      CHARACTER(LEN=*),INTENT(IN)  :: str
      !! String to search inside for contiguous duplicated characters.
      CHARACTER(LEN=*),INTENT(IN)  :: c
      !! Character to search for contiguous duplications.
      CHARACTER(LEN=:),ALLOCATABLE :: res
      !! String with the selected character contiguous duplications removed.
      CHARACTER(LEN=:),ALLOCATABLE :: string
      INTEGER :: pos
      string = str
      res = ''
      pos = INDEX(string,c)
      DO WHILE (pos /= 0)
         res = res//string(:pos)
         string = string(pos+1:)
         pos = INDEX(string,c)
         DO WHILE (pos == 1)
            string = string(2:)
            pos = INDEX(string,c)
         END DO
      END DO
      res = res//string
   END FUNCTION mergeChars





   ELEMENTAL FUNCTION startsWith(str, substr) RESULT(res)
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Checks if a string starts with a given substring.
      !! Checks if a string starts with a given substring. It can be an array of string.
      CHARACTER(LEN=*), INTENT(IN) :: str
      !! String that the user wants to check how it starts. It can be an array.
      CHARACTER(LEN=*), INTENT(IN) :: substr
      !! Substring to search to check if str starts with it.
      LOGICAL                      :: res
      !! True if the string starts with the substring and False otherwise. If
      !! substr is empty it returns True. If the input is an array, the returned
      !! values will also be in an array.
      res = INDEX(str,substr) == 1
   END FUNCTION startsWith





   ELEMENTAL FUNCTION endsWith(str, substr) RESULT(res)
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Checks if a string ends with a given substring.
      !! Checks if a string ends with a given substring. It can be an array of string.
      CHARACTER(LEN=*), INTENT(IN) :: str
      !! String that the user wants to check how it ends. It can be an array.
      CHARACTER(LEN=*), INTENT(IN) :: substr
      !! Substring to search to check if str ends with it.
      LOGICAL                      :: res
      !! True if the string ends with the substring and False otherwise. If 
      !! substr is empty it returns True. If the input is an array, the returned
      !! values will also be in an array.
      res = INDEX(str,substr,BACK = .TRUE.) == LEN(str) - LEN(substr) + 1
   END FUNCTION endsWith



   PURE FUNCTION num2str_i8(num) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i8), INTENT(IN)  :: num
      !! Number to convert to string.
      CHARACTER(LEN=:), ALLOCATABLE :: str
      !! String containing the number
      CHARACTER(LEN=4), PARAMETER   :: formato = '(I0)'

      INCLUDE 'Strings_M/include_num2strInt.f90'

   END FUNCTION num2str_i8

   PURE FUNCTION num2str_i16(num) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i16),INTENT(IN)  :: num
      !! Number to convert to string.
      CHARACTER(LEN=:), ALLOCATABLE :: str
      !! String containing the number
      CHARACTER(LEN=4), PARAMETER   :: formato = '(I0)'

      INCLUDE 'Strings_M/include_num2strInt.f90'

   END FUNCTION num2str_i16

   PURE FUNCTION num2str_i32(num) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i32),INTENT(IN)  :: num
      !! Number to convert to string.
      CHARACTER(LEN=:), ALLOCATABLE :: str
      !! String containing the number
      CHARACTER(LEN=4), PARAMETER   :: formato = '(I0)'

      INCLUDE 'Strings_M/include_num2strInt.f90'

   END FUNCTION num2str_i32

   PURE FUNCTION num2str_i64(num) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i64),INTENT(IN)  :: num
      !! Number to convert to string.
      CHARACTER(LEN=:), ALLOCATABLE :: str
      !! String containing the number
      CHARACTER(LEN=4), PARAMETER   :: formato = '(I0)'

      INCLUDE 'Strings_M/include_num2strInt.f90'

   END FUNCTION num2str_i64

   PURE FUNCTION num2str_sp(num,formato) RESULT(str)
      IMPLICIT NONE
      REAL(KIND=sp)   ,INTENT(IN)   :: num
      !! Number to convert to string.
      CHARACTER(LEN=*), INTENT(IN)  :: formato
      !! Format to use in the string variable. Only for real numbers.
      CHARACTER(LEN=:), ALLOCATABLE :: str
      !! String containing the number
      INTEGER                       :: length

      INCLUDE 'Strings_M/include_num2strReal.f90'

   END FUNCTION num2str_sp

   PURE FUNCTION num2str_dp(num,formato) RESULT(str)
      IMPLICIT NONE
      REAL(KIND=dp)   ,INTENT(IN)   :: num
      !! Number to convert to string.
      CHARACTER(LEN=*), INTENT(IN)  :: formato
      !! Format to use in the string variable. Only for real numbers.
      CHARACTER(LEN=:), ALLOCATABLE :: str
      !! String containing the number
      INTEGER                       :: length

      INCLUDE 'Strings_M/include_num2strReal.f90'

   END FUNCTION num2str_dp

   PURE FUNCTION num2str_qp(num,formato) RESULT(str)
      IMPLICIT NONE
      REAL(KIND=qp)   ,INTENT(IN)   :: num
      !! Number to convert to string.
      CHARACTER(LEN=*), INTENT(IN)  :: formato
      !! Format to use in the string variable. Only for real numbers.
      CHARACTER(LEN=:), ALLOCATABLE :: str
      !! String containing the number
      INTEGER                       :: length

      INCLUDE 'Strings_M/include_num2strReal.f90'

   END FUNCTION num2str_qp








   PURE FUNCTION int2str00000_i8(integ,total_length) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i8),INTENT(IN)  :: integ
      !! Integer number to convert. This number MUST be positive.
      INTEGER(KIND=i8),INTENT(IN)  :: total_length
      !! Number of digits to use, including zeros. This number MUST be positive.
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !! String containing the number.
      !local
      INTEGER(KIND=i8)             :: num_digits, num_zeros

      INCLUDE 'Strings_M/include_int2str00000.f90' 

   END FUNCTION int2str00000_i8

   PURE FUNCTION int2str00000_i16(integ,total_length) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i16),INTENT(IN) :: integ
      !! Integer number to convert. This number MUST be positive.
      INTEGER(KIND=i16),INTENT(IN) :: total_length
      !! Number of digits to use, including zeros. This number MUST be positive.
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !! String containing the number.
      !local
      INTEGER(KIND=i16)            :: num_digits, num_zeros

      INCLUDE 'Strings_M/include_int2str00000.f90' 

   END FUNCTION int2str00000_i16

   PURE FUNCTION int2str00000_i32(integ,total_length) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i32),INTENT(IN) :: integ
      !! Integer number to convert. This number MUST be positive.
      INTEGER(KIND=i32),INTENT(IN) :: total_length
      !! Number of digits to use, including zeros. This number MUST be positive.
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !! String containing the number.
      !local
      INTEGER(KIND=i32)            :: num_digits, num_zeros

      INCLUDE 'Strings_M/include_int2str00000.f90' 

   END FUNCTION int2str00000_i32

   PURE FUNCTION int2str00000_i64(integ,total_length) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i64),INTENT(IN) :: integ
      !! Integer number to convert. This number MUST be positive.
      INTEGER(KIND=i64),INTENT(IN) :: total_length
      !! Number of digits to use, including zeros. This number MUST be positive.
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !! String containing the number.
      !local
      INTEGER(KIND=i64)            :: num_digits, num_zeros

      INCLUDE 'Strings_M/include_int2str00000.f90' 

   END FUNCTION int2str00000_i64
















   PURE FUNCTION str2num_i8(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: str
      !! String to convert to number.
      INTEGER(KIND=i8), INTENT(IN) :: mold
      !! Real or integer value to identify the type and kind of the output.
      !! It is only used to set the type of the return value, so it can be any value.
      INTEGER(KIND=i8)             :: res
      !! The number of the input string.

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_i8

   PURE FUNCTION str2num_i16(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      !! String to convert to number.
      INTEGER(KIND=i16), INTENT(IN) :: mold
      !! Real or integer value to identify the type and kind of the output.
      !! It is only used to set the type of the return value, so it can be any value.
      INTEGER(KIND=i16)             :: res
      !! The number of the input string.

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_i16

   PURE FUNCTION str2num_i32(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      !! String to convert to number.
      INTEGER(KIND=i32), INTENT(IN) :: mold
      !! Real or integer value to identify the type and kind of the output.
      !! It is only used to set the type of the return value, so it can be any value.
      INTEGER(KIND=i32)             :: res
      !! The number of the input string.

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_i32

   PURE FUNCTION str2num_i64(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      !! String to convert to number.
      INTEGER(KIND=i64), INTENT(IN) :: mold
      !! Real or integer value to identify the type and kind of the output.
      !! It is only used to set the type of the return value, so it can be any value.
      INTEGER(KIND=i64)             :: res
      !! The number of the input string.

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_i64

   PURE FUNCTION str2num_sp(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: str
      !! String to convert to number.
      REAL(KIND=sp)   , INTENT(IN) :: mold
      !! Real or integer value to identify the type and kind of the output.
      !! It is only used to set the type of the return value, so it can be any value.
      REAL(KIND=sp)                :: res
      !! The number of the input string.

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_sp

   PURE FUNCTION str2num_dp(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: str
      !! String to convert to number.
      REAL(KIND=dp)   , INTENT(IN) :: mold
      !! Real or integer value to identify the type and kind of the output.
      !! It is only used to set the type of the return value, so it can be any value.
      REAL(KIND=dp)                :: res
      !! The number of the input string.

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_dp

   PURE FUNCTION str2num_qp(str,mold) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: str
      !! String to convert to number.
      REAL(KIND=qp)   , INTENT(IN) :: mold
      !! Real or integer value to identify the type and kind of the output.
      !! It is only used to set the type of the return value, so it can be any value.
      REAL(KIND=qp)                :: res
      !! The number of the input string.

      INCLUDE 'Strings_M/include_str2num.f90'

   END FUNCTION str2num_qp



   PURE FUNCTION replace(str,search,repla) RESULT(res)
      !! author: Emilio Castro.
      !! date: 10/07/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Searches and replaces a substring in a string
      !! Searches and replaces a substring in a string. It replaces 
      !! all occurences.
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      !! String to modify
      CHARACTER(LEN=*), INTENT(IN)  :: search
      !! String to search in str.
      CHARACTER(LEN=*), INTENT(IN)  :: repla
      !! String to replace in str.
      CHARACTER(LEN=:), ALLOCATABLE :: res
      !! Modified string.
      INTEGER :: pos
      INTEGER :: lensearch
      CHARACTER(LEN=:), ALLOCATABLE :: straux

      res = ''
      straux = str
      lensearch = LEN(search)
      pos = INDEX(straux,search)

      DO WHILE (pos /= 0)
         res = res // straux(:pos-1) // repla
         straux = straux(pos+lensearch:)
         pos = INDEX(straux,search)
      END DO
      res = res // straux

   END FUNCTION replace




   PURE FUNCTION strReverse(str) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      CHARACTER(LEN=:), ALLOCATABLE :: res
      INTEGER                       :: i
      res = str
      FORALL (i=1:len(res)) res(i:i) = res(len(res)-i+1:len(res)-i+1)
   END FUNCTION strReverse




END MODULE FU_Strings
