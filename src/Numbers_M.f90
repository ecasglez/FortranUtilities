!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------
!
! MODULE: Numbers_M
!
! DESCRIPTION:
!> @brief Tools to deal with numbers in Fortran programs.
!
! REVISION HISTORY:
! 27-05-2020 - Initial Version.
!
!> @author Emilio Castro.
!> @version 1.0.
!
!> @copyright See LICENSE file that comes with this distribution.
!--------------------------------------------------------------------

MODULE Numbers_M

   USE Prec_M
   USE, INTRINSIC :: IEEE_ARITHMETIC

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: is_nan, is_inf

   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Determines if the value of the input variable is NaN
   !
   ! REVISION HISTORY:
   ! 27-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param values Value to analize. It can have any rank and dimension
   !> @return Logical. True if the variable is NaN. False otherwise. It will
   !> have the same rank and dimension as the input value.
   INTERFACE is_nan
      MODULE PROCEDURE is_nan_sp
      MODULE PROCEDURE is_nan_dp
      MODULE PROCEDURE is_nan_qp
   END INTERFACE is_nan


   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Determines if the value of the input variable is Infinity
   !
   ! REVISION HISTORY:
   ! 27-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param values Value to analize. It can have any rank and dimension
   !> @return Logical. True if the variable is Inf. False otherwise. It will
   !> have the same rank and dimension as the input value.
   INTERFACE is_inf
      MODULE PROCEDURE is_inf_sp
      MODULE PROCEDURE is_inf_dp
      MODULE PROCEDURE is_inf_qp
   END INTERFACE is_inf



   CONTAINS

      ELEMENTAL FUNCTION is_nan_sp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp), INTENT(IN) :: val
         LOGICAL                   :: res

         res = ieee_is_nan(val)

      END FUNCTION is_nan_sp

      ELEMENTAL FUNCTION is_nan_dp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp), INTENT(IN) :: val
         LOGICAL                   :: res

         res = ieee_is_nan(val)

      END FUNCTION is_nan_dp

      ELEMENTAL FUNCTION is_nan_qp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp), INTENT(IN) :: val
         LOGICAL                   :: res

         res = ieee_is_nan(val)

      END FUNCTION is_nan_qp

      ELEMENTAL FUNCTION is_inf_sp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp), INTENT(IN) :: val
         LOGICAL                   :: res

         res = .NOT.ieee_is_finite(val)

      END FUNCTION is_inf_sp

      ELEMENTAL FUNCTION is_inf_dp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp), INTENT(IN) :: val
         LOGICAL                   :: res

         res = .NOT.ieee_is_finite(val)

      END FUNCTION is_inf_dp

      ELEMENTAL FUNCTION is_inf_qp(val) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp), INTENT(IN) :: val
         LOGICAL                   :: res

         res = .NOT.ieee_is_finite(val)

      END FUNCTION is_inf_qp

END MODULE Numbers_M
