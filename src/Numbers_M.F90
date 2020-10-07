!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------

MODULE FU_Numbers
   !! author: Emilio Castro.
   !! date: 27/05/2020.
   !! version: 1.0.
   !! license: MIT.
   !! summary: Functions to analyze numbers in Fortran programs.
   !! Functions to analyze numbers in Fortran programs. Some of these functions (is_nan and is_inf)
   !! are now available in the intrinsics module IEEE_ARITHMETIC and
   !! are provided here only for compatibility with some old programs that use them. 

   USE FU_Prec
   USE, INTRINSIC :: IEEE_ARITHMETIC

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: is_nan, is_inf, count_digits_integer



   INTERFACE count_digits_integer
      !! author: Emilio Castro.
      !! date: 07/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Counts the number of digits of an integer.
      !! Counts the number of digits of an integer, including the - sign 
      !! in case it is a negative value.
      MODULE PROCEDURE count_digits_integer_i8
      MODULE PROCEDURE count_digits_integer_i16
      MODULE PROCEDURE count_digits_integer_i32
      MODULE PROCEDURE count_digits_integer_i64
   END INTERFACE count_digits_integer





   INTERFACE is_nan
      !! author: Emilio Castro.
      !! date: 27/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Determines if the value of the input variable is NaN.
      !! Determines if the value of the input variable is NaN.
      MODULE PROCEDURE is_nan_sp
      MODULE PROCEDURE is_nan_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE is_nan_qp
#endif
   END INTERFACE is_nan


   INTERFACE is_inf
      !! author: Emilio Castro.
      !! date: 27/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Determines if the value of the input variable is Infinity.
      !! Determines if the value of the input variable is NaN.
      MODULE PROCEDURE is_inf_sp
      MODULE PROCEDURE is_inf_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE is_inf_qp
#endif
   END INTERFACE is_inf



   CONTAINS



   PURE FUNCTION count_digits_integer_i8(i) RESULT(num_digits)
      IMPLICIT NONE
      INTEGER(KIND=i8), INTENT(IN):: i
      !! Integer number whose digits are to be counted.
      INTEGER(KIND=i8)            :: num_digits
      !! The number of digits of the input number.
      INTEGER(KIND=i8), PARAMETER :: ten = 10, one = 1, two = 2
      INTEGER(KIND=i8)            :: integ

      INCLUDE 'Numbers_M/include_count_digits_integer.f90'

   END FUNCTION count_digits_integer_i8

   PURE FUNCTION count_digits_integer_i16(i) RESULT(num_digits)
      IMPLICIT NONE
      INTEGER(KIND=i16), INTENT(IN):: i
      !! Integer number whose digits are to be counted.
      INTEGER(KIND=i16)            :: num_digits
      !! The number of digits of the input number.
      INTEGER(KIND=i16), PARAMETER :: ten = 10, one = 1, two = 2
      INTEGER(KIND=i16)            :: integ

      INCLUDE 'Numbers_M/include_count_digits_integer.f90'

   END FUNCTION count_digits_integer_i16

   PURE FUNCTION count_digits_integer_i32(i) RESULT(num_digits)
      IMPLICIT NONE
      INTEGER(KIND=i32), INTENT(IN):: i
      !! Integer number whose digits are to be counted.
      INTEGER(KIND=i32)            :: num_digits
      !! The number of digits of the input number.
      INTEGER(KIND=i32), PARAMETER :: ten = 10, one = 1, two = 2
      INTEGER(KIND=i32)            :: integ

      INCLUDE 'Numbers_M/include_count_digits_integer.f90'

   END FUNCTION count_digits_integer_i32

   PURE FUNCTION count_digits_integer_i64(i) RESULT(num_digits)
      IMPLICIT NONE
      INTEGER(KIND=i64), INTENT(IN):: i
      !! Integer number whose digits are to be counted.
      INTEGER(KIND=i64)            :: num_digits
      !! The number of digits of the input number.
      INTEGER(KIND=i64), PARAMETER :: ten = 10, one = 1, two = 2
      INTEGER(KIND=i64)            :: integ

      INCLUDE 'Numbers_M/include_count_digits_integer.f90'

   END FUNCTION count_digits_integer_i64






      ELEMENTAL FUNCTION is_nan_sp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp), INTENT(IN) :: val
         !! Value to analize. It can have any rank and dimension
         LOGICAL                   :: res
         !! True if the variable is NaN. False otherwise. It will
         !! have the same rank and dimension as the input value.

         res = ieee_is_nan(val)

      END FUNCTION is_nan_sp

      ELEMENTAL FUNCTION is_nan_dp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp), INTENT(IN) :: val
         !! Value to analize. It can have any rank and dimension
         LOGICAL                   :: res
         !! True if the variable is NaN. False otherwise. It will
         !! have the same rank and dimension as the input value.

         res = ieee_is_nan(val)

      END FUNCTION is_nan_dp

#ifdef QPREC_FPP
      ELEMENTAL FUNCTION is_nan_qp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp), INTENT(IN) :: val
         !! Value to analize. It can have any rank and dimension
         LOGICAL                   :: res
         !! True if the variable is NaN. False otherwise. It will
         !! have the same rank and dimension as the input value.

         res = ieee_is_nan(val)

      END FUNCTION is_nan_qp
#endif

      ELEMENTAL FUNCTION is_inf_sp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp), INTENT(IN) :: val
         !! Value to analize. It can have any rank and dimension
         LOGICAL                   :: res
         !! True if the variable is Inf. False otherwise. It will
         !! have the same rank and dimension as the input value.

         res = .NOT.ieee_is_finite(val)

      END FUNCTION is_inf_sp

      ELEMENTAL FUNCTION is_inf_dp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp), INTENT(IN) :: val
         !! Value to analize. It can have any rank and dimension
         LOGICAL                   :: res
         !! True if the variable is Inf. False otherwise. It will
         !! have the same rank and dimension as the input value.

         res = .NOT.ieee_is_finite(val)

      END FUNCTION is_inf_dp

#ifdef QPREC_FPP
      ELEMENTAL FUNCTION is_inf_qp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp), INTENT(IN) :: val
         !! Value to analize. It can have any rank and dimension
         LOGICAL                   :: res
         !! True if the variable is Inf. False otherwise. It will
         !! have the same rank and dimension as the input value.

         res = .NOT.ieee_is_finite(val)

      END FUNCTION is_inf_qp
#endif

END MODULE FU_Numbers
