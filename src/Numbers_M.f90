!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------

MODULE FU_Numbers
   !! author: Emilio Castro.
   !! date: 27/05/2020.
   !! version: 1.0.
   !! license: MIT.
   !! summary: Functions to analyze numbers in Fortran programs.
   !! Functions to analyze numbers in Fortran programs. These functions
   !! are now available in the intrinsics module IEEE_ARITHMETIC and
   !! are provided here only for compatibility with some old programs that use them. 

   USE FU_Prec
   USE, INTRINSIC :: IEEE_ARITHMETIC

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: is_nan, is_inf

   INTERFACE is_nan
      !! author: Emilio Castro.
      !! date: 27/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Determines if the value of the input variable is NaN.
      !! Determines if the value of the input variable is NaN.
      MODULE PROCEDURE is_nan_sp
      MODULE PROCEDURE is_nan_dp
      MODULE PROCEDURE is_nan_qp
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
      MODULE PROCEDURE is_inf_qp
   END INTERFACE is_inf



   CONTAINS

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

      ELEMENTAL FUNCTION is_nan_qp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp), INTENT(IN) :: val
         !! Value to analize. It can have any rank and dimension
         LOGICAL                   :: res
         !! True if the variable is NaN. False otherwise. It will
         !! have the same rank and dimension as the input value.

         res = ieee_is_nan(val)

      END FUNCTION is_nan_qp

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

      ELEMENTAL FUNCTION is_inf_qp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp), INTENT(IN) :: val
         !! Value to analize. It can have any rank and dimension
         LOGICAL                   :: res
         !! True if the variable is Inf. False otherwise. It will
         !! have the same rank and dimension as the input value.

         res = .NOT.ieee_is_finite(val)

      END FUNCTION is_inf_qp

END MODULE FU_Numbers
