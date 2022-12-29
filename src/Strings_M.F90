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
   PUBLIC :: num2str, int2str0, str2num
   PUBLIC :: startsWith, endsWith, splitstr, replace, mergeChars
   PUBLIC :: upper, lower, cistrcmp, zfill

   CHARACTER(LEN=*), PARAMETER  :: lowercase = 'aáäàâbcdeéëèêfghiíïìîjklmnñoóöòôpqrstuúüùûvwxyz'
   CHARACTER(LEN=*), PARAMETER  :: uppercase = 'AÁÄÀÂBCDEÉËÈÊFGHIÍÏÌÎJKLMNÑOÓÖÒÔPQRSTUÚÜÙÛVWXYZ'

   INTERFACE zfill
      !! author: Emilio Castro.
      !! date: 16/12/2022.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Adds zeros at the beginning of a string.
      !! Adds zeros at the beginning of a string.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! newStr = zfill(str, l)
      !!```
      !!
      !! Where:
      !!
      !! * `str`: String that the user wants to fill with leading zeros..
      !! * `l`: Integer indicating the length of the resuling string.
      !! 
      !! If ```l``` is greater than the length of ```str```, it returns a string of length ```l```
      !! with ```str``` padded with zeros at the beginning. If ```l``` is lower or equal to the
      !! length of ```str```, it returns ```str```.
      !!
      !!### Example
      !!
      !! The following program fills a string with leading zeros:
      !!
      !!```Fortran
      !! PROGRAM zfillExample
      !!    USE FU_Strings, ONLY: zfill
      !!    IMPLICIT NONE
      !!    WRITE(*,*) zfill('myStr', 12)
      !! END PROGRAM zfillExample
      !!```
      MODULE PROCEDURE zfill_i8
      MODULE PROCEDURE zfill_i16
      MODULE PROCEDURE zfill_i32
      MODULE PROCEDURE zfill_i64
   END INTERFACE zfill


   INTERFACE splitstr
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Splits a string.
      !! Splits a string and returns the portion selected by the user.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! t = splitstr(str, fieldNumber, delimiter, rev, mergedelim)
      !!```
      !!
      !! Where:
      !!
      !! * `str`: String that the user whants to split.
      !! * `fieldNumber`: Integer indicating which of the divisions to return.
      !! * `delimiter`: String that the user wants to use as a delimiter for splitting.
      !! Optional parameter. Default is Space.
      !! * `rev`: If true start spliting by the end of the string.
      !! Optional parameter. Default is False.
      !! * `mergedelim`: If true, contiguous delimiters in the string are merged before splitting.
      !! Optional parameter. Default is False.
      !! 
      !! It returns a string with the selected part of str. If the fieldNumber does not exists
      !! or if the delimiter does not exists it returns an empty string.
      !!
      !!### Example
      !!
      !! The following program extracts some portions of a text:
      !!
      !!```Fortran
      !! PROGRAM splitstrExample
      !!    USE FU_Strings, ONLY: splitstr
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: text
      !!    CHARACTER(LEN=:), ALLOCATABLE :: portion
      !!    text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, &
      !!       &sed do eiusmod tempor incididunt ut labore et dolore magna al&
      !!       &iqua. Ut enim ad minim veniam, quis nostrud exercitation ulla&
      !!       &mco laboris nisi ut aliquip ex ea commodo consequat. Duis aut&
      !!       &e irure dolor in reprehenderit in voluptate velit esse cillum&
      !!       & dolore eu fugiat nulla pariatur. Excepteur sint occaecat cup&
      !!       &idatat non proident, sunt in culpa qui officia deserunt molli&
      !!       &t anim id est laborum."
      !!    portion = splitstr(text, 1)
      !!    WRITE(*,*) portion
      !!    portion = splitstr(text, 2, rev = .True.)
      !!    WRITE(*,*) portion
      !!    portion = splitstr(text, 6, delimiter = "l"
      !!    WRITE(*,*) portion
      !!    portion = splitstr(text, 7, delimiter = "l"
      !!    WRITE(*,*) portion
      !!    portion = splitstr(text, 7, delimiter = "l", mergedelim = .True.)  
      !!    WRITE(*,*) portion
      !! END PROGRAM splitstrExample
      !!```
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
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! t = num2str(num, format)
      !!```
      !!
      !! Where:
      !!
      !! * `num`: Real or Integer to convert to a string.
      !! * `format`: Format to use in the string variable. Only for real numbers.
      !! 
      !! It returns a string containing the number.
      !!
      !!### Example
      !!
      !! The following program a converts an integer and a real to a
      !! string, using them to create a filename:
      !!
      !!```Fortran
      !! PROGRAM num2strExample
      !!    USE FU_Strings, ONLY: num2str
      !!    IMPLICIT NONE
      !!    REAL :: temperature
      !!    INTEGER :: case_number
      !!    CHARACTER(LEN=:), ALLOCATABLE :: filename
      !!    temperature = 293.75
      !!    case_number = 17
      !!    filename = 'Case_'//num2str(case_number)// &
      !!       '_Temp_'//num2str(temperature, "F4.0")//'txt'
      !!    WRITE(*,*) filename
      !! END PROGRAM num2strExample
      !!```
      MODULE PROCEDURE num2str_i8
      MODULE PROCEDURE num2str_i16
      MODULE PROCEDURE num2str_i32
      MODULE PROCEDURE num2str_i64
      MODULE PROCEDURE num2str_sp
      MODULE PROCEDURE num2str_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE num2str_qp
#endif
   END INTERFACE num2str


   INTERFACE int2str0
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Converts an integer into a string filling with leading zeros.
      !! Converts an integer variable into a string variable,
      !! filling with leading zeros up to the limit imposed by the user.
      !! Useful to open files named sequentially with leading zeros in the name.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! t = int2str0(integ, total_length)
      !!```
      !!
      !! Where:
      !!
      !! * `integ`: Integer number to convert. This number MUST be positive.
      !! * `format`: Number of digits to use, including leading zeros. This number MUST be positive.
      !! 
      !! It returns a string containing the number.
      !!
      !!### Example
      !!
      !! The following program a converts an integer to a number with leading zeros
      !! to create sequential filenames
      !!
      !!```Fortran
      !! PROGRAM int2str0Example
      !!    USE FU_Strings, ONLY: int2str0
      !!    IMPLICIT NONE
      !!    INTEGER :: i
      !!    INTEGER :: total_length
      !!    CHARACTER(LEN=:), ALLOCATABLE :: filename
      !!    total_length = 5
      !!    DO i = 1, 25
      !!       filename = int2str0(i, total_length)//'.dat'
      !!       WRITE(*,*) filename
      !!    END DO
      !! END PROGRAM int2str0Example
      !!```
      MODULE PROCEDURE int2str0_i8
      MODULE PROCEDURE int2str0_i16
      MODULE PROCEDURE int2str0_i32
      MODULE PROCEDURE int2str0_i64
   END INTERFACE int2str0




   INTERFACE str2num
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Converts a string into an integer or real.
      !! Converts a string into an integer or real number as specified by the type of variable mold.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! t = str2num(str, mold)
      !!```
      !!
      !! Where:
      !!
      !! * `str`: String to convert to number
      !! * `mold`: Real or integer value to identify the type and kind of the output.
      !! It is only used to set the type of the return value, so it can be any value.
      !! 
      !! It returns an integer or real with the number contained in the string.
      !!
      !!### Example
      !!
      !! The following program converts a string to a real number
      !!
      !!```Fortran
      !! PROGRAM str2numExample
      !!    USE FU_Strings, ONLY: str2num
      !!    IMPLICIT NONE
      !!    REAL :: f
      !!    CHARACTER(LEN=:), ALLOCATABLE :: s
      !!    s = '293.75'
      !!    f = str2num(s, f)
      !!    WRITE(*,*) f
      !! END PROGRAM str2numExample
      !!```
      MODULE PROCEDURE str2num_i8
      MODULE PROCEDURE str2num_i16
      MODULE PROCEDURE str2num_i32
      MODULE PROCEDURE str2num_i64
      MODULE PROCEDURE str2num_sp
      MODULE PROCEDURE str2num_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE str2num_qp
#endif
   END INTERFACE str2num




CONTAINS


   PURE FUNCTION zfill_i8(str, l) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: str
      !! String that the user wants to fill with leading zeros.
      INTEGER(KIND=i8), INTENT(IN)    :: l
      !! Integer indicating the length of the resuling string.
      CHARACTER(LEN=MAX(l, LEN(str))) :: res
      !! If ```l``` is greater than the length of ```str```, it returns a string of length ```l```
      !! with ```str``` padded with zeros at the beginning. If ```l``` is lower or equal to the
      !! length of ```str```, it returns ```str```.

      INCLUDE 'Strings_M/include_zfill.f90'

   END FUNCTION zfill_i8

   PURE FUNCTION zfill_i16(str, l) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: str
      !! String that the user wants to fill with leading zeros.
      INTEGER(KIND=i16), INTENT(IN)   :: l
      !! Integer indicating the length of the resuling string.
      CHARACTER(LEN=MAX(l, LEN(str))) :: res
      !! If ```l``` is greater than the length of ```str```, it returns a string of length ```l```
      !! with ```str``` padded with zeros at the beginning. If ```l``` is lower or equal to the
      !! length of ```str```, it returns ```str```.

      INCLUDE 'Strings_M/include_zfill.f90'

   END FUNCTION zfill_i16

   PURE FUNCTION zfill_i32(str, l) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: str
      !! String that the user wants to fill with leading zeros.
      INTEGER(KIND=i32), INTENT(IN)   :: l
      !! Integer indicating the length of the resuling string.
      CHARACTER(LEN=MAX(l, LEN(str))) :: res
      !! If ```l``` is greater than the length of ```str```, it returns a string of length ```l```
      !! with ```str``` padded with zeros at the beginning. If ```l``` is lower or equal to the
      !! length of ```str```, it returns ```str```.

      INCLUDE 'Strings_M/include_zfill.f90'

   END FUNCTION zfill_i32

   PURE FUNCTION zfill_i64(str, l) RESULT(res)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: str
      !! String that the user wants to fill with leading zeros.
      INTEGER(KIND=i64), INTENT(IN)   :: l
      !! Integer indicating the length of the resuling string.
      CHARACTER(LEN=MAX(l, LEN(str))) :: res
      !! If ```l``` is greater than the length of ```str```, it returns a string of length ```l```
      !! with ```str``` padded with zeros at the beginning. If ```l``` is lower or equal to the
      !! length of ```str```, it returns ```str```.

      INCLUDE 'Strings_M/include_zfill.f90'

   END FUNCTION zfill_i64


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
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! s = mergeChars(str, c)
      !!```
      !!
      !! Where:
      !!
      !! * `str`: String to search inside for contiguous duplicated characters.
      !! * `substr`: Character to search for contiguous duplications.
      !! 
      !! String with the selected character contiguous duplications removed.
      !!
      !!### Example
      !!
      !! The following program removes contiguos characters:
      !!
      !!```Fortran
      !! PROGRAM mergeCharsExample
      !!    USE FU_Strings, ONLY: mergeChars
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: text
      !!    CHARACTER(LEN=:), ALLOCATABLE :: modified_text
      !!    text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, &
      !!       &sed do eiusmod tempor incididunt ut labore et dolore magna al&
      !!       &iqua. Ut enim ad minim veniam, quis nostrud exercitation ulla&
      !!       &mco laboris nisi ut aliquip ex ea commodo consequat. Duis aut&
      !!       &e irure dolor in reprehenderit in voluptate velit esse cillum&
      !!       & dolore eu fugiat nulla pariatur. Excepteur sint occaecat cup&
      !!       &idatat non proident, sunt in culpa qui officia deserunt molli&
      !!       &t anim id est                       laborum."
      !!     modified_text = mergeChars(text, ' ')
      !!     WRITE(*,*) modified_text
      !! END PROGRAM mergeCharsExample
      !!```
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
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! l = startsWith(str, substr)
      !!```
      !!
      !! Where:
      !!
      !! * `str`: String that the user wants to check how it starts. It can be an array.
      !! * `substr`: Substring to search for to check if str starts with it.
      !! 
      !! It returns True if the string starts with the substring and False otherwise. If
      !! substr is empty it returns True. If the input is an array, the returned
      !! values will also be in an array.
      !!
      !!### Example
      !!
      !! The following program checks if a string starts with a substring:
      !!
      !!```Fortran
      !! PROGRAM startsWithExample
      !!    USE FU_Strings, ONLY: startsWith
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: text1, text2
      !!    text1 = 'String1'
      !!    text2 = 'St'
      !!    IF (startsWith(text1, text2)) THEN
      !!       WRITE(*,*) 'String starts with St'
      !!    ELSE
      !!       WRITE(*,*) 'String does not start with St'
      !!    END IF
      !! END PROGRAM startsWithExample
      !!```
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
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! l = endsWith(str, substr)
      !!```
      !!
      !! Where:
      !!
      !! * `str`: String that the user wants to check how it ends. It can be an array.
      !! * `substr`: Substring to search for to check if str ends with it.
      !! 
      !! It returns True if the string ends with the substring and False otherwise. If
      !! substr is empty it returns True. If the input is an array, the returned
      !! values will also be in an array.
      !!
      !!### Example
      !!
      !! The following program checks if a string ends with a substring:
      !!
      !!```Fortran
      !! PROGRAM endsWithExample
      !!    USE FU_Strings, ONLY: endsWith
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: text1, text2
      !!    text1 = 'String1'
      !!    text2 = 'g1'
      !!    IF (endsWith(text1, text2)) THEN
      !!       WRITE(*,*) 'String ends with g1'
      !!    ELSE
      !!       WRITE(*,*) 'String does not end with g1'
      !!    END IF
      !! END PROGRAM endsWithExample
      !!```
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

#ifdef QPREC_FPP
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
#endif








   PURE FUNCTION int2str0_i8(integ,total_length) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i8),INTENT(IN)  :: integ
      !! Integer number to convert. This number MUST be positive.
      INTEGER(KIND=i8),INTENT(IN)  :: total_length
      !! Number of digits to use, including leading zeros. This number MUST be positive.
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !! String containing the number.

      INCLUDE 'Strings_M/include_int2str0.f90' 

   END FUNCTION int2str0_i8

   PURE FUNCTION int2str0_i16(integ,total_length) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i16),INTENT(IN) :: integ
      !! Integer number to convert. This number MUST be positive.
      INTEGER(KIND=i16),INTENT(IN) :: total_length
      !! Number of digits to use, including leading zeros. This number MUST be positive.
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !! String containing the number.

      INCLUDE 'Strings_M/include_int2str0.f90' 

   END FUNCTION int2str0_i16

   PURE FUNCTION int2str0_i32(integ,total_length) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i32),INTENT(IN) :: integ
      !! Integer number to convert. This number MUST be positive.
      INTEGER(KIND=i32),INTENT(IN) :: total_length
      !! Number of digits to use, including leading zeros. This number MUST be positive.
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !! String containing the number.

      INCLUDE 'Strings_M/include_int2str0.f90' 

   END FUNCTION int2str0_i32

   PURE FUNCTION int2str0_i64(integ,total_length) RESULT(str)
      USE FU_Numbers, ONLY: count_digits_integer
      IMPLICIT NONE
      INTEGER(KIND=i64),INTENT(IN) :: integ
      !! Integer number to convert. This number MUST be positive.
      INTEGER(KIND=i64),INTENT(IN) :: total_length
      !! Number of digits to use, including leading zeros. This number MUST be positive.
      CHARACTER(LEN=:),ALLOCATABLE :: str
      !! String containing the number.

      INCLUDE 'Strings_M/include_int2str0.f90' 

   END FUNCTION int2str0_i64



   ELEMENTAL FUNCTION str2num_i8(str,mold) RESULT(res)
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

   ELEMENTAL FUNCTION str2num_i16(str,mold) RESULT(res)
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

   ELEMENTAL FUNCTION str2num_i32(str,mold) RESULT(res)
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

   ELEMENTAL FUNCTION str2num_i64(str,mold) RESULT(res)
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

   ELEMENTAL FUNCTION str2num_sp(str,mold) RESULT(res)
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

   ELEMENTAL FUNCTION str2num_dp(str,mold) RESULT(res)
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

#ifdef QPREC_FPP
   ELEMENTAL FUNCTION str2num_qp(str,mold) RESULT(res)
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
#endif



   PURE FUNCTION replace(str,search,repla) RESULT(res)
      !! author: Emilio Castro.
      !! date: 10/07/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Searches and replaces a substring in a string
      !! Searches and replaces a substring in a string. It replaces 
      !! all occurences.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! t = replace(str, search, repla)
      !!```
      !!
      !! Where:
      !!
      !! * `str`: String to modify.
      !! * `search`: String to search for in str.
      !! * `repla`: String to replace in str..
      !! 
      !! It returns the modified modified string.
      !!
      !!### Example
      !!
      !! The following program searches and replaces in a string:
      !!
      !!```Fortran
      !! PROGRAM replaceExample
      !!    USE FU_Strings, ONLY: replace
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: text
      !!    CHARACTER(LEN=:), ALLOCATABLE :: modified_text
      !!    text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, &
      !!       &sed do eiusmod tempor incididunt ut labore et dolore magna al&
      !!       &iqua. Ut enim ad minim veniam, quis nostrud exercitation ulla&
      !!       &mco laboris nisi ut aliquip ex ea commodo consequat. Duis aut&
      !!       &e irure dolor in reprehenderit in voluptate velit esse cillum&
      !!       & dolore eu fugiat nulla pariatur. Excepteur sint occaecat cup&
      !!       &idatat non proident, sunt in culpa qui officia deserunt molli&
      !!       &t anim id est laborum."
      !!    modified_text = replace(text, 'm', 'X')
      !!    WRITE(*,*) modified_text
      !!    modified_text = replace(text, ' ad ', ' AD ')
      !!    WRITE(*,*) modified_text
      !! END PROGRAM replaceExample
      !!```
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      !! String to modify
      CHARACTER(LEN=*), INTENT(IN)  :: search
      !! String to search for in str.
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
      DO i = 1, LEN(str)
         res(i:i) = str(len(str)-i+1:len(str)-i+1)
      END DO
   END FUNCTION strReverse



   PURE FUNCTION upper(str) RESULT(res)
      !! author: Emilio Castro.
      !! date: 20/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Converts a string to uppercase characters.
      !! Converts a string to uppercase characters. It works with this dataset:
      !! 'aáäàâbcdeéëèêfghiíïìîjklmnñoóöòôpqrstuúüùûvwxyz'
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! t = upper(str)
      !!```
      !!
      !! Where:
      !!
      !! * `str`: String to convert to uppercase characters.
      !! 
      !! It returns the string converted to uppercase characters.
      !!
      !!### Example
      !!
      !! The following program converts a string to uppercase:
      !
      !!```Fortran
      !! PROGRAM upperExample
      !!    USE FU_Strings, ONLY: upper
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: text
      !!    CHARACTER(LEN=:), ALLOCATABLE :: modified_text
      !!    text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, &
      !!       &sed do eiusmod tempor incididunt ut labore et dolore magna al&
      !!       &iqua. Ut enim ad minim veniam, quis nostrud exercitation ulla&
      !!       &mco laboris nisi ut aliquip ex ea commodo consequat. Duis aut&
      !!       &e irure dolor in reprehenderit in voluptate velit esse cillum&
      !!       & dolore eu fugiat nulla pariatur. Excepteur sint occaecat cup&
      !!       &idatat non proident, sunt in culpa qui officia deserunt molli&
      !!       &t anim id est laborum."
      !!    modified_text = upper(text)
      !!    WRITE(*,*) modified_text
      !! END PROGRAM upperExample
      !!```
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      !! String to convert to uppercase characters.
      CHARACTER(LEN=:), ALLOCATABLE :: res
      !! String converted to uppercase characters.
      INTEGER :: i, pos
      res = str
      DO i = 1, LEN(res)
         pos = INDEX(lowercase,res(i:i))
         IF (pos /= 0) THEN
            res(i:i) = uppercase(pos:pos)
         END IF
      END DO
   END FUNCTION upper



   PURE FUNCTION lower(str) RESULT(res)
      !! author: Emilio Castro.
      !! date: 20/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Converts a string to lowercase characters.
      !! Converts a string to lowercase characters. It works with this dataset
      !! 'aáäàâbcdeéëèêfghiíïìîjklmnñoóöòôpqrstuúüùûvwxyz'
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! t = lower(str)
      !!```
      !!
      !! Where:
      !!
      !! * `str`: String to convert to lowercase characters.
      !! 
      !! It returns the string converted to lowercase characters.
      !!
      !!### Example
      !!
      !! The following program converts a string to lowercase:
      !
      !!```Fortran
      !! PROGRAM lowerExample
      !!    USE FU_Strings, ONLY: lower
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: text
      !!    CHARACTER(LEN=:), ALLOCATABLE :: modified_text
      !!    text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, &
      !!       &sed do eiusmod tempor incididunt ut labore et dolore magna al&
      !!       &iqua. Ut enim ad minim veniam, quis nostrud exercitation ulla&
      !!       &mco laboris nisi ut aliquip ex ea commodo consequat. Duis aut&
      !!       &e irure dolor in reprehenderit in voluptate velit esse cillum&
      !!       & dolore eu fugiat nulla pariatur. Excepteur sint occaecat cup&
      !!       &idatat non proident, sunt in culpa qui officia deserunt molli&
      !!       &t anim id est laborum."
      !!    modified_text = lower(text)
      !!    WRITE(*,*) modified_text
      !! END PROGRAM lowerExample
      !!```
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: str
      !! String to convert to lowercase characters.
      CHARACTER(LEN=:), ALLOCATABLE :: res
      !! String converted to lowercase characters.
      INTEGER :: i, pos
      res = str
      DO i = 1, LEN(res)
         pos = INDEX(uppercase,res(i:i))
         IF (pos /= 0) THEN
            res(i:i) = lowercase(pos:pos)
         END IF
      END DO
   END FUNCTION lower


   PURE FUNCTION cistrcmp(str1, str2) RESULT(res)
      !! author: Emilio Castro.
      !! date: 20/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Case-independent string comparison.
      !! Case-independent string comparison. It works with this dataset: 
      !! 'aáäàâbcdeéëèêfghiíïìîjklmnñoóöòôpqrstuúüùûvwxyz'
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! l = cistrcmp(str1, str2)
      !!```
      !!
      !! Where:
      !!
      !! * `str1`: First string to compare.
      !! * `str2`: Second string to compare.
      !! 
      !! It returns True if both strings are equal independently of the case and
      !! False otherwise.
      !!
      !!### Example
      !!
      !! The following program performs a case-independent string comparison:
      !!
      !!```Fortran
      !! PROGRAM cistrcmpExample
      !!    USE FU_Strings, ONLY: cistrcmp
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: text1, text2
      !!    text1 = 'String1'
      !!    text2 = 'string1'
      !!    IF (cistrcmp(text1, text2)) THEN
      !!       WRITE(*,*) 'Both strings are equal'
      !!    ELSE
      !!       WRITE(*,*) 'Both strings are not equal'
      !!    END IF
      !! END PROGRAM cistrcmpExample
      !!```
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: str1
      !! First string to compare.
      CHARACTER(LEN=*), INTENT(IN) :: str2
      !! Second string to compare
      LOGICAL                      :: res
      !! True if both strings are equal independently of the case. False otherwise.
      res = upper(str1) == upper(str2)
   END FUNCTION cistrcmp




END MODULE FU_Strings
