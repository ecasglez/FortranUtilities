!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------
!
! MODULE: Statistics_M
!
! DESCRIPTION:
!> @brief Statistics tools for Fortran programs.
!
! REVISION HISTORY:
! 27-05-2020 - Initial Version.
!
!> @author Emilio Castro.
!> @version 1.0.
!
!> @copyright See LICENSE file that comes with this distribution.
!--------------------------------------------------------------------

MODULE Statistics_M

   USE Prec_M

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: mean

   !> Error code issued by all functions in module Statistics_M
   INTEGER,PARAMETER :: exit_error_code = 11



   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Calculates the mean value of a set of values given in a vector.
   !> of any size with one dimension
   !
   ! REVISION HISTORY:
   ! 27-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param values Vector of real numbers to calculate the mean value. It can 
   !> have any size and it must have one dimension.
   !> @return Real number with the average of values.
   INTERFACE mean
      MODULE PROCEDURE mean_sp
      MODULE PROCEDURE mean_dp
      MODULE PROCEDURE mean_qp
   END INTERFACE mean




   CONTAINS




      FUNCTION mean_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=sp)                         :: res
         INTEGER,PARAMETER                     :: prec = sp
         
         INCLUDE 'Statistics_M/include_mean.f90'

      END FUNCTION mean_sp

      FUNCTION mean_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=dp)                         :: res
         INTEGER,PARAMETER                     :: prec = dp
         
         INCLUDE 'Statistics_M/include_mean.f90'

      END FUNCTION mean_dp

      FUNCTION mean_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=qp)                         :: res
         INTEGER,PARAMETER                     :: prec = qp
         
         INCLUDE 'Statistics_M/include_mean.f90'

      END FUNCTION mean_qp


END MODULE Statistics_M
