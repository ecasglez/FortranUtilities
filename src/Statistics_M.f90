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
   PUBLIC :: mean, gmean, variance, stdev, pvariance, pstdev

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



   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Calculates the geometric mean value of a set of values given in a vector.
   !> of any size with one dimension
   !
   ! REVISION HISTORY:
   ! 27-05-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param values Vector of real numbers to calculate the geometric mean value. It can 
   !> have any size and it must have one dimension.
   !> @return Real number with the geometric mean of the values.
   INTERFACE gmean
      MODULE PROCEDURE gmean_sp
      MODULE PROCEDURE gmean_dp
      MODULE PROCEDURE gmean_qp
   END INTERFACE gmean


   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Calculates the sample variance of a set of values given in a vector.
   !> of any size with one dimension
   !
   ! REVISION HISTORY:
   ! 09-06-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param values Vector of real numbers to calculate the variance value. It can 
   !> have any size and it must have one dimension.
   !> @return Real number with the sample variance of values.
   INTERFACE variance
      MODULE PROCEDURE variance_sp
      MODULE PROCEDURE variance_dp
      MODULE PROCEDURE variance_qp
   END INTERFACE variance



   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Calculates the sample standard deviation of a set of values given in a vector.
   !> of any size with one dimension
   !
   ! REVISION HISTORY:
   ! 09-06-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param values Vector of real numbers to calculate the sample standard deviation value. 
   !> It can have any size and it must have one dimension.
   !> @return Real number with the sample standard deviation of values.
   INTERFACE stdev
      MODULE PROCEDURE stdev_sp
      MODULE PROCEDURE stdev_dp
      MODULE PROCEDURE stdev_qp
   END INTERFACE stdev


   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Calculates the population variance of a set of values given in a vector.
   !> of any size with one dimension
   !
   ! REVISION HISTORY:
   ! 09-06-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param values Vector of real numbers to calculate the population variance value.
   !> It can have any size and it must have one dimension.
   !> @return Real number with the variance of values.
   INTERFACE pvariance
      MODULE PROCEDURE pvariance_sp
      MODULE PROCEDURE pvariance_dp
      MODULE PROCEDURE pvariance_qp
   END INTERFACE pvariance



   !--------------------------------------------------------------------
   ! DESCRIPTION:
   !> @brief Calculates the population standard deviation of a set of values given in a vector.
   !> of any size with one dimension
   !
   ! REVISION HISTORY:
   ! 09-06-2020 - Initial Version.
   !
   !> @author Emilio Castro.
   !> @version 1.0.
   !> @param values Vector of real numbers to calculate the population standard deviation value. 
   !> It can have any size and it must have one dimension.
   !> @return Real number with the population standard deviation of values.
   INTERFACE pstdev
      MODULE PROCEDURE pstdev_sp
      MODULE PROCEDURE pstdev_dp
      MODULE PROCEDURE pstdev_qp
   END INTERFACE pstdev



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

      FUNCTION gmean_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=sp)                         :: res
         INTEGER,PARAMETER                     :: prec = sp

         INCLUDE 'Statistics_M/include_gmean.f90'

      END FUNCTION gmean_sp

      FUNCTION gmean_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=dp)                         :: res
         INTEGER,PARAMETER                     :: prec = dp

         INCLUDE 'Statistics_M/include_gmean.f90'

      END FUNCTION gmean_dp

      FUNCTION gmean_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=qp)                         :: res
         INTEGER,PARAMETER                     :: prec = qp

         INCLUDE 'Statistics_M/include_gmean.f90'

      END FUNCTION gmean_qp


      FUNCTION variance_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=sp)                         :: res
         INTEGER,PARAMETER                     :: prec = sp
         REAL(KIND=sp)                         :: avg
         INTEGER                               :: i

         INCLUDE 'Statistics_M/include_variance.f90'

      END FUNCTION variance_sp

      FUNCTION variance_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=dp)                         :: res
         INTEGER,PARAMETER                     :: prec = dp
         REAL(KIND=dp)                         :: avg
         INTEGER                               :: i

         INCLUDE 'Statistics_M/include_variance.f90'

      END FUNCTION variance_dp

      FUNCTION variance_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=qp)                         :: res
         INTEGER,PARAMETER                     :: prec = qp
         REAL(KIND=qp)                         :: avg
         INTEGER                               :: i

         INCLUDE 'Statistics_M/include_variance.f90'

      END FUNCTION variance_qp


      FUNCTION stdev_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=sp)                         :: res

         res = SQRT(variance(values))

      END FUNCTION stdev_sp

      FUNCTION stdev_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=dp)                         :: res

         res = SQRT(variance(values))

      END FUNCTION stdev_dp

      FUNCTION stdev_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=qp)                         :: res

         res = SQRT(variance(values))

      END FUNCTION stdev_qp



      FUNCTION pvariance_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=sp)                         :: res
         INTEGER,PARAMETER                     :: prec = sp

         res = variance(values) * REAL(SIZE(values) - 1, prec) / REAL(SIZE(values),prec)

      END FUNCTION pvariance_sp

      FUNCTION pvariance_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=dp)                         :: res
         INTEGER,PARAMETER                     :: prec = dp

         res = variance(values) * REAL(SIZE(values) - 1, prec) / REAL(SIZE(values),prec)

      END FUNCTION pvariance_dp

      FUNCTION pvariance_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=qp)                         :: res
         INTEGER,PARAMETER                     :: prec = qp

         res = variance(values) * REAL(SIZE(values) - 1, prec) / REAL(SIZE(values),prec)

      END FUNCTION pvariance_qp


      FUNCTION pstdev_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=sp)                         :: res

         res = SQRT(pvariance(values))

      END FUNCTION pstdev_sp

      FUNCTION pstdev_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=dp)                         :: res

         res = SQRT(pvariance(values))

      END FUNCTION pstdev_dp

      FUNCTION pstdev_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         REAL(KIND=qp)                         :: res

         res = SQRT(pvariance(values))

      END FUNCTION pstdev_qp

END MODULE Statistics_M
