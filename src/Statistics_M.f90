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
   PUBLIC :: mean, gmean, variance, stdev, pvariance, pstdev, &
      covariance, pcovariance, correlation, lin_error_propagation, median, &
      skewness, pskewness

   INTERFACE c_sort
      !To sort the array of values using c++ functions in order
      !to calculate median and quantiles.
      SUBROUTINE c_sort_float(x, n) BIND(c,name='c_sort_float')
         USE iso_c_binding
         INTEGER(C_INT)    ,        VALUE         :: n
         REAL(C_FLOAT),DIMENSION(n),INTENT(INOUT) :: x
      END SUBROUTINE c_sort_float
      SUBROUTINE c_sort_double(x, n) BIND(c,name='c_sort_double')
         USE iso_c_binding
         INTEGER(C_INT)    ,         VALUE         :: n
         REAL(C_DOUBLE),DIMENSION(n),INTENT(INOUT) :: x
      END SUBROUTINE c_sort_double
      SUBROUTINE c_sort_long_double(x, n) BIND(c,name='c_sort_long_double')
         USE iso_c_binding
         INTEGER(C_INT)    ,              VALUE         :: n
         REAL(C_LONG_DOUBLE),DIMENSION(n),INTENT(INOUT) :: x
      END SUBROUTINE c_sort_long_double
   END INTERFACE c_sort


   INTERFACE mean
      !! author: Emilio Castro.
      !! date: 27/05/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the mean value.
      !! Calculates the mean value of a set of values given in a vector
      !! of any size with one dimension applying the following equation:
      !!
      !! $$\overline{x} = \frac{\sum\limits_{i=1}^n x_{i}}{n}$$
      !!
      !! where:
      !!
      !! * x is a vector with real numbers.
      !! * n is how many numbers are included in x.
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
      !! of any size with one dimension applying the following equation:
      !!
      !! $$\overline{x} = \left(\prod\limits_{i=1}^{n}x_{i}\right)^\frac{1}{n}
      !! = \sqrt[n]{x_{1} \times x_{2} \times \dots \times x_{n}}$$
      !!
      !! where:
      !!
      !! * x is a vector with real numbers.
      !! * n is how many numbers are included in x.
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
      !! of any size with one dimension applying the following equation:
      !!
      !! $$\sigma^{2} = \frac{\sum\limits_{i=1}^{n}\left(x_{i} -
      !!              \overline{x}\right)^2}{n-1}$$
      !!
      !! where:
      !!
      !! * x is a vector with real numbers.
      !! * n is how many numbers are included in x.
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
      !! of any size with one dimension applying the following equation:
      !!
      !! $$\sigma = \sqrt{\frac{\sum\limits_{i=1}^{n}\left(x_{i} -
      !!              \overline{x}\right)^2}{n-1}}$$
      !!
      !! where:
      !!
      !! * x is a vector with real numbers.
      !! * n is how many numbers are included in x.
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
      !! of any size with one dimension applying the following equation:
      !!
      !! $$\sigma^{2} = \frac{\sum\limits_{i=1}^{n}\left(x_{i} -
      !!              \overline{x}\right)^2}{n}$$
      !!
      !! where:
      !!
      !! * x is a vector with real numbers.
      !! * n is how many numbers are included in x.
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
      !! of any size with one dimension applying the following equation:
      !!
      !! $$\sigma = \sqrt{\frac{\sum\limits_{i=1}^{n}\left(x_{i} -
      !!              \overline{x}\right)^2}{n}}$$
      !!
      !! where:
      !!
      !! * x is a vector with real numbers.
      !! * n is how many numbers are included in x.
      MODULE PROCEDURE pstdev_sp
      MODULE PROCEDURE pstdev_dp
      MODULE PROCEDURE pstdev_qp
   END INTERFACE pstdev



   INTERFACE covariance
      !! author: Emilio Castro.
      !! date: 10/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the sample covariance between two variables.
      !! Calculates the sample covariance between two variables given in two vectors
      !! of any size with one dimension applying the following equation:
      !!
      !! $$\sigma_{xy} = \frac{\sum\limits_{i=1}^{n}\left(x_{i} -
      !!              \overline{x}\right)\left(y_{i} -
      !!              \overline{y}\right)}{n-1}$$
      !!
      !! where:
      !!
      !! * x and y are vectors with real numbers.
      !! * n is how many numbers are included in x and y.
      MODULE PROCEDURE covariance_sp
      MODULE PROCEDURE covariance_dp
      MODULE PROCEDURE covariance_qp
   END INTERFACE covariance



   INTERFACE pcovariance
      !! author: Emilio Castro.
      !! date: 10/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the population covariance between two variables.
      !! Calculates the population covariance between two variables given in two vectors
      !! of any size with one dimension applying the following equation:
      !!
      !! $$\sigma_{xy} = \frac{\sum\limits_{i=1}^{n}\left(x_{i} -
      !!              \overline{x}\right)\left(y_{i} -
      !!              \overline{y}\right)}{n}$$
      !!
      !! where:
      !!
      !! * x and y are vectors with real numbers.
      !! * n is how many numbers are included in x and y.
      MODULE PROCEDURE pcovariance_sp
      MODULE PROCEDURE pcovariance_dp
      MODULE PROCEDURE pcovariance_qp
   END INTERFACE pcovariance



   INTERFACE correlation
      !! author: Emilio Castro.
      !! date: 10/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the correlation coefficient between two variables.
      !! Calculates the correlation coefficient between two variables given in two vectors
      !! of any size with one dimension applying the following equation:
      !!
      !! $$\rho_{xy} = \frac{\sigma_{xy}
      !!                 }{\sigma_{x} \sigma_{y}}$$
      !!
      !! where:
      !!
      !! * x and y are vectors with real numbers.
      !! * n is how many numbers are included in x and y.
      MODULE PROCEDURE correlation_sp
      MODULE PROCEDURE correlation_dp
      MODULE PROCEDURE correlation_qp
   END INTERFACE correlation



   INTERFACE lin_error_propagation
      !! author: Emilio Castro.
      !! date: 10/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Performs linear error (or uncertainties) propagation.
      !! Performs linear error (or uncertainties) propagation given the
      !! sensitivity coefficients and a covariance matrix. The following
      !! formula is applied:
      !!
      !! $$\sigma^2_{y} = S \Sigma^{X} S^\intercal$$
      !! where:
      !!
      !! * y is the response whose uncertainty is to be calculated.
      !! * X is a set of input parameters to propagate their uncertainty to y.
      !! * S is the vector of sensitivity coefficients of y with respect to the
      !!   different parameters in X.
      !! * \(\Sigma^{x}\) is the covariance matrix of the parameters in X.
      MODULE PROCEDURE lin_error_propagation_sp
      MODULE PROCEDURE lin_error_propagation_dp
      MODULE PROCEDURE lin_error_propagation_qp
   END INTERFACE lin_error_propagation


   INTERFACE median
      !! author: Emilio Castro.
      !! date: 12/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the median value.
      !! Calculates the median value.
      !! This function does not work with quadruple precision numbers
      !! because of the ordering subroutine written in C++.
      MODULE PROCEDURE median_sp
      MODULE PROCEDURE median_dp
   END INTERFACE median




   INTERFACE skewness
      !! author: Emilio Castro.
      !! date: 19/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the sample skewness of a set of values.
      !! Calculates the sample skewness of a set of values given in a vector
      !! of any size with one dimension applying the following equation:
      !!
      !! $$S_{x} = \frac{n}{\left( n-1 \right)\left( n-2 \right)
      !!               }\sum\limits_{i=1}^{n}\left( \frac{x_{i}-\overline{x}
      !!               }{\sigma_{x}} \right)^3$$
      !!
      !! where:
      !!
      !! * x is a vector with real numbers.
      !! * n is how many numbers are included in x.
      MODULE PROCEDURE skewness_sp
      MODULE PROCEDURE skewness_dp
      MODULE PROCEDURE skewness_qp
   END INTERFACE skewness



   INTERFACE pskewness
      !! author: Emilio Castro.
      !! date: 19/08/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Calculates the population skewness of a set of values.
      !! Calculates the population skewness of a set of values given in a vector
      !! of any size with one dimension applying the following equation:
      !!
      !! $$S_{x} = \frac{1}{n
      !!               }\sum\limits_{i=1}^{n}\left( \frac{x_{i}-\overline{x}
      !!               }{\sigma_{x}} \right)^3$$
      !!
      !! where:
      !!
      !! * x is a vector with real numbers.
      !! * n is how many numbers are included in x.
      MODULE PROCEDURE pskewness_sp
      MODULE PROCEDURE pskewness_dp
      MODULE PROCEDURE pskewness_qp
   END INTERFACE pskewness




   CONTAINS


      PURE FUNCTION mean_sp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the mean value. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the average of x.
         INTEGER,PARAMETER                     :: prec = sp
         
         INCLUDE 'Statistics_M/include_mean.f90'

      END FUNCTION mean_sp

      PURE FUNCTION mean_dp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the mean value. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the average of x.
         INTEGER,PARAMETER                     :: prec = dp
         
         INCLUDE 'Statistics_M/include_mean.f90'

      END FUNCTION mean_dp

      PURE FUNCTION mean_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the mean value. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the average of x.
         INTEGER,PARAMETER                     :: prec = qp
         
         INCLUDE 'Statistics_M/include_mean.f90'

      END FUNCTION mean_qp

      PURE FUNCTION gmean_sp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the geometric mean. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the geometric mean of the x.
         INTEGER,PARAMETER                     :: prec = sp

         INCLUDE 'Statistics_M/include_gmean.f90'

      END FUNCTION gmean_sp

      PURE FUNCTION gmean_dp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the geometric mean. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the geometric mean of the x.
         INTEGER,PARAMETER                     :: prec = dp

         INCLUDE 'Statistics_M/include_gmean.f90'

      END FUNCTION gmean_dp

      PURE FUNCTION gmean_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the geometric mean. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the geometric mean of the x.
         INTEGER,PARAMETER                     :: prec = qp

         INCLUDE 'Statistics_M/include_gmean.f90'

      END FUNCTION gmean_qp


      PURE FUNCTION variance_sp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample variance. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the sample variance of x.
         INTEGER,PARAMETER                     :: prec = sp
         REAL(KIND=sp)                         :: avg

         INCLUDE 'Statistics_M/include_variance.f90'

      END FUNCTION variance_sp

      PURE FUNCTION variance_dp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample variance. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the sample variance of x.
         INTEGER,PARAMETER                     :: prec = dp
         REAL(KIND=dp)                         :: avg

         INCLUDE 'Statistics_M/include_variance.f90'

      END FUNCTION variance_dp

      PURE FUNCTION variance_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample variance. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the sample variance of x.
         INTEGER,PARAMETER                     :: prec = qp
         REAL(KIND=qp)                         :: avg

         INCLUDE 'Statistics_M/include_variance.f90'

      END FUNCTION variance_qp


      PURE FUNCTION stdev_sp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample standard deviation.
         !! It can have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the sample standard deviation of x.

         res = SQRT(variance(x))

      END FUNCTION stdev_sp

      PURE FUNCTION stdev_dp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample standard deviation.
         !! It can have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the sample standard deviation of x.

         res = SQRT(variance(x))

      END FUNCTION stdev_dp

      PURE FUNCTION stdev_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample standard deviation.
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the sample standard deviation of x.

         res = SQRT(variance(x))

      END FUNCTION stdev_qp



      PURE FUNCTION pvariance_sp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population variance.
         !! It can have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the variance of x.
         INTEGER,PARAMETER                     :: prec = sp

         res = variance(x) * REAL(SIZE(x) - 1, prec) / REAL(SIZE(x),prec)

      END FUNCTION pvariance_sp

      PURE FUNCTION pvariance_dp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population variance.
         !! It can have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the variance of x.
         INTEGER,PARAMETER                     :: prec = dp

         res = variance(x) * REAL(SIZE(x) - 1, prec) / REAL(SIZE(x),prec)

      END FUNCTION pvariance_dp

      PURE FUNCTION pvariance_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population variance.
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the variance of x.
         INTEGER,PARAMETER                     :: prec = qp

         res = variance(x) * REAL(SIZE(x) - 1, prec) / REAL(SIZE(x),prec)

      END FUNCTION pvariance_qp


      PURE FUNCTION pstdev_sp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population standard deviation. 
         !! It can have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the population standard deviation of x.

         res = SQRT(pvariance(x))

      END FUNCTION pstdev_sp

      PURE FUNCTION pstdev_dp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population standard deviation. 
         !! It can have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the population standard deviation of x.

         res = SQRT(pvariance(x))

      END FUNCTION pstdev_dp

      PURE FUNCTION pstdev_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population standard deviation. 
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the population standard deviation of x.

         res = SQRT(pvariance(x))

      END FUNCTION pstdev_qp


      PURE FUNCTION covariance_sp(x,y) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the sample covariance between both variables.
         INTEGER,PARAMETER                     :: prec = sp
         REAL(KIND=sp)                         :: avg1, avg2

         INCLUDE 'Statistics_M/include_covariance.f90'

      END FUNCTION covariance_sp

      PURE FUNCTION covariance_dp(x,y) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the sample covariance between both variables.
         INTEGER,PARAMETER                     :: prec = dp
         REAL(KIND=dp)                         :: avg1, avg2

         INCLUDE 'Statistics_M/include_covariance.f90'

      END FUNCTION covariance_dp

      PURE FUNCTION covariance_qp(x,y) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the sample covariance between both variables.
         INTEGER,PARAMETER                     :: prec = qp
         REAL(KIND=qp)                         :: avg1, avg2

         INCLUDE 'Statistics_M/include_covariance.f90'

      END FUNCTION covariance_qp



      PURE FUNCTION pcovariance_sp(x,y) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the population covariance between both variables.
         INTEGER,PARAMETER                     :: prec = sp

         res = covariance(x,y) &
            * REAL(SIZE(x) - 1, prec) / REAL(SIZE(x),prec)

      END FUNCTION pcovariance_sp

      PURE FUNCTION pcovariance_dp(x,y) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the population covariance between both variables.
         INTEGER,PARAMETER                     :: prec = dp

         res = covariance(x,y) &
            * REAL(SIZE(x) - 1, prec) / REAL(SIZE(x),prec)

      END FUNCTION pcovariance_dp

      PURE FUNCTION pcovariance_qp(x,y) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the population covariance between both variables.
         INTEGER,PARAMETER                     :: prec = qp

         res = covariance(x,y) &
            * REAL(SIZE(x) - 1, prec) / REAL(SIZE(x),prec)

      END FUNCTION pcovariance_qp



      PURE FUNCTION correlation_sp(x,y) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the population covariance between both variables.
         INTEGER,PARAMETER                     :: prec = sp

         INCLUDE 'Statistics_M/include_correlation.f90'

      END FUNCTION correlation_sp

      PURE FUNCTION correlation_dp(x,y) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the population covariance between both variables.
         INTEGER,PARAMETER                     :: prec = dp

         INCLUDE 'Statistics_M/include_correlation.f90'

      END FUNCTION correlation_dp

      PURE FUNCTION correlation_qp(x,y) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the population covariance between both variables.
         INTEGER,PARAMETER                     :: prec = qp

         INCLUDE 'Statistics_M/include_correlation.f90'

      END FUNCTION correlation_qp



      PURE FUNCTION lin_error_propagation_sp(sensitivities,matcovar) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: sensitivities
         !! Vector of sensitivity coefficients of the new variable with the respect the prior variable.
         !! It can have any size and it must have one dimension.
         REAL(KIND=sp),DIMENSION(:,:),INTENT(IN) :: matcovar
         !! Covariance matrix with the error or uncertainty of the prior variable.
         !! Dimensions of sensitivities and matcovar must be in agreement.
         REAL(KIND=sp)                         :: res
         !! Real number with the error or uncertainty (variance) propagated to the new variable.

         INCLUDE 'Statistics_M/include_lin_error_propagation.f90'

      END FUNCTION lin_error_propagation_sp

      PURE FUNCTION lin_error_propagation_dp(sensitivities,matcovar) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: sensitivities
         !! Vector of sensitivity coefficients of the new variable with the respect the prior variable.
         !! It can have any size and it must have one dimension.
         REAL(KIND=dp),DIMENSION(:,:),INTENT(IN) :: matcovar
         !! Covariance matrix with the error or uncertainty of the prior variable.
         !! Dimensions of sensitivities and matcovar must be in agreement.
         REAL(KIND=dp)                         :: res
         !! Real number with the error or uncertainty (variance) propagated to the new variable.

         INCLUDE 'Statistics_M/include_lin_error_propagation.f90'

      END FUNCTION lin_error_propagation_dp

      PURE FUNCTION lin_error_propagation_qp(sensitivities,matcovar) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: sensitivities
         !! Vector of sensitivity coefficients of the new variable with the respect the prior variable.
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp),DIMENSION(:,:),INTENT(IN) :: matcovar
         !! Covariance matrix with the error or uncertainty of the prior variable.
         !! Dimensions of sensitivities and matcovar must be in agreement.
         REAL(KIND=qp)                         :: res
         !! Real number with the error or uncertainty (variance) propagated to the new variable.

         INCLUDE 'Statistics_M/include_lin_error_propagation.f90'

      END FUNCTION lin_error_propagation_qp


      FUNCTION median_sp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the median. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp) :: res
         !! Real number with the median
         REAL(KIND=sp),DIMENSION(SIZE(x)) :: x_cp
         ! x_cp is a copy of x to avoid modifying it when ordering
         INTEGER :: size_x
         INTEGER,PARAMETER                     :: prec = sp

         INCLUDE 'Statistics_M/include_median.f90'

      END FUNCTION median_sp

      FUNCTION median_dp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the median. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp) :: res
         !! Real number with the median
         REAL(KIND=dp),DIMENSION(SIZE(x)) :: x_cp
         ! x_cp is a copy of x to avoid modifying it when ordering
         INTEGER :: size_x
         INTEGER,PARAMETER                     :: prec = dp

         INCLUDE 'Statistics_M/include_median.f90'

      END FUNCTION median_dp


      PURE FUNCTION skewness_sp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample skewness.
         !! It can have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the sample skewness of the x.
         REAL(KIND=sp)                         :: avg
         ! mean value
         REAL(KIND=sp)                         :: sd
         ! standard deviation
         INTEGER                               :: n

         INCLUDE 'Statistics_M/include_skewness.f90'

      END FUNCTION skewness_sp

      PURE FUNCTION skewness_dp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample skewness.
         !! It can have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the sample skewness of the x.
         REAL(KIND=dp)                         :: avg
         ! mean value
         REAL(KIND=dp)                         :: sd
         ! standard deviation
         INTEGER                               :: n

         INCLUDE 'Statistics_M/include_skewness.f90'

      END FUNCTION skewness_dp

      PURE FUNCTION skewness_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample skewness.
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the sample skewness of the x.
         REAL(KIND=qp)                         :: avg
         ! mean value
         REAL(KIND=qp)                         :: sd
         ! standard deviation
         INTEGER                               :: n

         INCLUDE 'Statistics_M/include_skewness.f90'

      END FUNCTION skewness_qp



      PURE FUNCTION pskewness_sp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population skewness.
         !! It can have any size and it must have one dimension.
         REAL(KIND=sp)                         :: res
         !! Real number with the population skewness of the x.
         REAL(KIND=sp)                         :: avg
         ! mean value
         REAL(KIND=sp)                         :: sd
         ! standard deviation
         INTEGER                               :: n

         INCLUDE 'Statistics_M/include_pskewness.f90'

      END FUNCTION pskewness_sp

      PURE FUNCTION pskewness_dp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population skewness.
         !! It can have any size and it must have one dimension.
         REAL(KIND=dp)                         :: res
         !! Real number with the population skewness of the x.
         REAL(KIND=dp)                         :: avg
         ! mean value
         REAL(KIND=dp)                         :: sd
         ! standard deviation
         INTEGER                               :: n

         INCLUDE 'Statistics_M/include_pskewness.f90'

      END FUNCTION pskewness_dp

      PURE FUNCTION pskewness_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population skewness.
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the population skewness of the x.
         REAL(KIND=qp)                         :: avg
         ! mean value
         REAL(KIND=qp)                         :: sd
         ! standard deviation
         INTEGER                               :: n

         INCLUDE 'Statistics_M/include_pskewness.f90'

      END FUNCTION pskewness_qp


END MODULE FU_Statistics
