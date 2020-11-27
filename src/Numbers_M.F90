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
   PUBLIC :: eq, ne



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
      !! Determines if the value of the input variable is Infinity.
      MODULE PROCEDURE is_inf_sp
      MODULE PROCEDURE is_inf_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE is_inf_qp
#endif
   END INTERFACE is_inf



   INTERFACE eq
      !! author: Emilio Castro.
      !! date: 14/10/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Tests two real numbers for equality.
      !! Tests two real numberes for equality using a tolerance if provided by the user,
      !! or selecting a tolerance automatically otherwise.
      MODULE PROCEDURE eq_sp
      MODULE PROCEDURE eq_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE eq_qp
#endif
   END INTERFACE eq


   INTERFACE ne
      !! author: Emilio Castro.
      !! date: 14/10/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Tests two real numbers for inequality.
      !! Tests two real numberes for inequality using a tolerance if provided by the user,
      !! or selecting a tolerance automatically otherwise.
      MODULE PROCEDURE ne_sp
      MODULE PROCEDURE ne_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE ne_qp
#endif
   END INTERFACE ne



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


      ELEMENTAL FUNCTION eq_sp(x1,x2,eps) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp), INTENT(IN)           :: x1
         !! First real value to compare for equality.
         REAL(KIND=sp), INTENT(IN)           :: x2
         !! Second real value to compare for equality.
         REAL(KIND=sp), OPTIONAL, INTENT(IN) :: eps
         !! User selected tolerance for the comparison. If not provided
         !! it will be selected automatically.
         LOGICAL                             :: res
         !! True if both numbers are equal according to the selected tolerance.
         !! False otherwise.
         REAL(KIND=sp) :: eps2

         INCLUDE 'Numbers_M/include_eq.f90'

      END FUNCTION eq_sp

      ELEMENTAL FUNCTION eq_dp(x1,x2,eps) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp), INTENT(IN)           :: x1
         !! First real value to compare for equality.
         REAL(KIND=dp), INTENT(IN)           :: x2
         !! Second real value to compare for equality.
         REAL(KIND=dp), OPTIONAL, INTENT(IN) :: eps
         !! User selected tolerance for the comparison. If not provided
         !! it will be selected automatically.
         LOGICAL                             :: res
         !! True if both numbers are equal according to the selected tolerance.
         !! False otherwise.
         REAL(KIND=dp) :: eps2

         INCLUDE 'Numbers_M/include_eq.f90'

      END FUNCTION eq_dp

#ifdef QPREC_FPP
      ELEMENTAL FUNCTION eq_qp(x1,x2,eps) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp), INTENT(IN)           :: x1
         !! First real value to compare for equality.
         REAL(KIND=qp), INTENT(IN)           :: x2
         !! Second real value to compare for equality.
         REAL(KIND=qp), OPTIONAL, INTENT(IN) :: eps
         !! User selected tolerance for the comparison. If not provided
         !! it will be selected automatically.
         LOGICAL                             :: res
         !! True if both numbers are equal according to the selected tolerance.
         !! False otherwise.
         REAL(KIND=qp) :: eps2

         INCLUDE 'Numbers_M/include_eq.f90'

      END FUNCTION eq_qp
#endif


      ELEMENTAL FUNCTION ne_sp(x1,x2,eps) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp), INTENT(IN)           :: x1
         !! First real value to compare for inequality.
         REAL(KIND=sp), INTENT(IN)           :: x2
         !! Second real value to compare for inequality.
         REAL(KIND=sp), OPTIONAL, INTENT(IN) :: eps
         !! User selected tolerance for the comparison. If not provided
         !! it will be selected automatically.
         LOGICAL                             :: res
         !! True if the numbers are not equal according to the selected tolerance.
         !! False otherwise.
         REAL(KIND=sp) :: eps2

         INCLUDE 'Numbers_M/include_ne.f90'

      END FUNCTION ne_sp

      ELEMENTAL FUNCTION ne_dp(x1,x2,eps) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp), INTENT(IN)           :: x1
         !! First real value to compare for inequality.
         REAL(KIND=dp), INTENT(IN)           :: x2
         !! Second real value to compare for inequality.
         REAL(KIND=dp), OPTIONAL, INTENT(IN) :: eps
         !! User selected tolerance for the comparison. If not provided
         !! it will be selected automatically.
         LOGICAL                             :: res
         !! True if the numbers are not equal according to the selected tolerance.
         !! False otherwise.
         REAL(KIND=dp) :: eps2

         INCLUDE 'Numbers_M/include_ne.f90'

      END FUNCTION ne_dp

#ifdef QPREC_FPP
      ELEMENTAL FUNCTION ne_qp(x1,x2,eps) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp), INTENT(IN)           :: x1
         !! First real value to compare for inequality.
         REAL(KIND=qp), INTENT(IN)           :: x2
         !! Second real value to compare for inequality.
         REAL(KIND=qp), OPTIONAL, INTENT(IN) :: eps
         !! User selected tolerance for the comparison. If not provided
         !! it will be selected automatically.
         LOGICAL                             :: res
         !! True if the numbers are not equal according to the selected tolerance.
         !! False otherwise.
         REAL(KIND=qp) :: eps2

         INCLUDE 'Numbers_M/include_ne.f90'

      END FUNCTION ne_qp
#endif






END MODULE FU_Numbers
