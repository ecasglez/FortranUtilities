!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------

MODULE FU_Statistics
   !! author: Emilio Castro.
   !! date: 27/05/2020.
   !! version: 1.0.
   !! license: MIT.
   !! summary: Statistics tools for Fortran programs.
   !! Statistics tools for Fortran programs.

   USE FU_Prec

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: mean, gmean, variance, stdev, pvariance, pstdev



   INTERFACE mean
      !! author: Emilio Castro.
      !! date: 27/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the mean value.
      !! Calculates the mean value of a set of values given in a vector
      !! of any size with one dimension.
      MODULE PROCEDURE mean_sp
      MODULE PROCEDURE mean_dp
      MODULE PROCEDURE mean_qp
   END INTERFACE mean



   INTERFACE gmean
      !! author: Emilio Castro.
      !! date: 27/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the geometric mean.
      !! Calculates the geometric mean of a set of values given in a vector
      !! of any size with one dimension.
      MODULE PROCEDURE gmean_sp
      MODULE PROCEDURE gmean_dp
      MODULE PROCEDURE gmean_qp
   END INTERFACE gmean


   INTERFACE variance
      !! author: Emilio Castro.
      !! date: 09/06/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the sample variance.
      !! Calculates the sample variance of a set of values given in a vector
      !! of any size with one dimension.
      MODULE PROCEDURE variance_sp
      MODULE PROCEDURE variance_dp
      MODULE PROCEDURE variance_qp
   END INTERFACE variance



   INTERFACE stdev
      !! author: Emilio Castro.
      !! date: 09/06/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the sample standard deviation.
      !! Calculates the sample standard deviation of a set of values given in a vector
      !! of any size with one dimension.
      MODULE PROCEDURE stdev_sp
      MODULE PROCEDURE stdev_dp
      MODULE PROCEDURE stdev_qp
   END INTERFACE stdev


   INTERFACE pvariance
      !! author: Emilio Castro.
      !! date: 09/06/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the population variance.
      !! Calculates the population variance of a set of values given in a vector
      !! of any size with one dimension.
      MODULE PROCEDURE pvariance_sp
      MODULE PROCEDURE pvariance_dp
      MODULE PROCEDURE pvariance_qp
   END INTERFACE pvariance



   INTERFACE pstdev
      !! author: Emilio Castro.
      !! date: 09/06/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the population standard deviation.
      !! Calculates the population standard deviation of a set of values given in a vector
      !! of any size with one dimension.
      MODULE PROCEDURE pstdev_sp
      MODULE PROCEDURE pstdev_dp
      MODULE PROCEDURE pstdev_qp
   END INTERFACE pstdev



   CONTAINS



      PURE FUNCTION mean_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the mean value. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the average of values.
         INTEGER,PARAMETER                     :: prec = sp
         
         INCLUDE 'Statistics_M/include_mean.f90'

      END FUNCTION mean_sp

      PURE FUNCTION mean_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the mean value. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the average of values.
         INTEGER,PARAMETER                     :: prec = dp
         
         INCLUDE 'Statistics_M/include_mean.f90'

      END FUNCTION mean_dp

      PURE FUNCTION mean_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the mean value. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the average of values.
         INTEGER,PARAMETER                     :: prec = qp
         
         INCLUDE 'Statistics_M/include_mean.f90'

      END FUNCTION mean_qp

      PURE FUNCTION gmean_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the geometric mean. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the geometric mean of the values.
         INTEGER,PARAMETER                     :: prec = sp

         INCLUDE 'Statistics_M/include_gmean.f90'

      END FUNCTION gmean_sp

      PURE FUNCTION gmean_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the geometric mean. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the geometric mean of the values.
         INTEGER,PARAMETER                     :: prec = dp

         INCLUDE 'Statistics_M/include_gmean.f90'

      END FUNCTION gmean_dp

      PURE FUNCTION gmean_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the geometric mean. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the geometric mean of the values.
         INTEGER,PARAMETER                     :: prec = qp

         INCLUDE 'Statistics_M/include_gmean.f90'

      END FUNCTION gmean_qp


      PURE FUNCTION variance_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the sample variance. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the sample variance of values.
         INTEGER,PARAMETER                     :: prec = sp
         REAL(KIND=sp)                         :: avg

         INCLUDE 'Statistics_M/include_variance.f90'

      END FUNCTION variance_sp

      PURE FUNCTION variance_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the sample variance. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the sample variance of values.
         INTEGER,PARAMETER                     :: prec = dp
         REAL(KIND=dp)                         :: avg

         INCLUDE 'Statistics_M/include_variance.f90'

      END FUNCTION variance_dp

      PURE FUNCTION variance_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the sample variance. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the sample variance of values.
         INTEGER,PARAMETER                     :: prec = qp
         REAL(KIND=qp)                         :: avg

         INCLUDE 'Statistics_M/include_variance.f90'

      END FUNCTION variance_qp


      PURE FUNCTION stdev_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the sample standard deviation.
         !! It can have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the sample standard deviation of values.

         res = SQRT(variance(values))

      END FUNCTION stdev_sp

      PURE FUNCTION stdev_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the sample standard deviation.
         !! It can have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the sample standard deviation of values.

         res = SQRT(variance(values))

      END FUNCTION stdev_dp

      PURE FUNCTION stdev_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the sample standard deviation.
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the sample standard deviation of values.

         res = SQRT(variance(values))

      END FUNCTION stdev_qp



      PURE FUNCTION pvariance_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the population variance.
         !! It can have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the variance of values.
         INTEGER,PARAMETER                     :: prec = sp

         res = variance(values) * REAL(SIZE(values) - 1, prec) / REAL(SIZE(values),prec)

      END FUNCTION pvariance_sp

      PURE FUNCTION pvariance_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the population variance.
         !! It can have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the variance of values.
         INTEGER,PARAMETER                     :: prec = dp

         res = variance(values) * REAL(SIZE(values) - 1, prec) / REAL(SIZE(values),prec)

      END FUNCTION pvariance_dp

      PURE FUNCTION pvariance_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the population variance.
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the variance of values.
         INTEGER,PARAMETER                     :: prec = qp

         res = variance(values) * REAL(SIZE(values) - 1, prec) / REAL(SIZE(values),prec)

      END FUNCTION pvariance_qp


      PURE FUNCTION pstdev_sp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the population standard deviation. 
         !! It can have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the population standard deviation of values.

         res = SQRT(pvariance(values))

      END FUNCTION pstdev_sp

      PURE FUNCTION pstdev_dp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the population standard deviation. 
         !! It can have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the population standard deviation of values.

         res = SQRT(pvariance(values))

      END FUNCTION pstdev_dp

      PURE FUNCTION pstdev_qp(values) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: values
         !! Vector of real numbers to calculate the population standard deviation. 
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the population standard deviation of values.

         res = SQRT(pvariance(values))

      END FUNCTION pstdev_qp

END MODULE FU_Statistics
