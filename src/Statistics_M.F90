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
      skewness, pskewness, linreg, logreg, expreg, potreg

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
      !! * \(x\) is a vector with real numbers.
      !! * \(n\) is how many numbers are included in \(x\).
      !!
      !! Usage:
      !!
      !! ```
      !! y = mean(x)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` = vector of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```y``` = real number of the same kind as ```x``` with the mean value of ```x```.
      MODULE PROCEDURE mean_sp
      MODULE PROCEDURE mean_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE mean_qp
#endif
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
      !! * \(x\) is a vector with real numbers.
      !! * \(n\) is how many numbers are included in \(x\).
      !!
      !! Usage:
      !!
      !! ```
      !! y = gmean(x)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` = vector of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```y``` = real number of the same kind as ```x``` with the geometric mean of ```x```.
      MODULE PROCEDURE gmean_sp
      MODULE PROCEDURE gmean_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE gmean_qp
#endif
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
      !! * \(x\) is a vector with real numbers.
      !! * \(n\) is how many numbers are included in \(x\).
      !!
      !! Usage:
      !!
      !! ```
      !! y = variance(x)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` = vector of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```y``` = real number of the same kind as ```x``` with the sample variance of ```x```.
      MODULE PROCEDURE variance_sp
      MODULE PROCEDURE variance_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE variance_qp
#endif
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
      !! * \(x\) is a vector with real numbers.
      !! * \(n\) is how many numbers are included in \(x\).
      !!
      !! Usage:
      !!
      !! ```
      !! y = stdev(x)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` = vector of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```y``` = real number of the same kind as ```x``` with the sample standard deviation of ```x```.
      MODULE PROCEDURE stdev_sp
      MODULE PROCEDURE stdev_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE stdev_qp
#endif
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
      !! * \(x\) is a vector with real numbers.
      !! * \(n\) is how many numbers are included in \(x\).
      !!
      !! Usage:
      !!
      !! ```
      !! y = pvariance(x)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` = vector of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```y``` = real number of the same kind as ```x``` with the population variance of ```x```.
      MODULE PROCEDURE pvariance_sp
      MODULE PROCEDURE pvariance_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE pvariance_qp
#endif
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
      !! * \(x\) is a vector with real numbers.
      !! * \(n\) is how many numbers are included in \(x\).
      !!
      !! Usage:
      !!
      !! ```
      !! y = pstdev(x)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` = vector of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```y``` = real number of the same kind as ```x``` with the population standard deviation of ```x```.
      MODULE PROCEDURE pstdev_sp
      MODULE PROCEDURE pstdev_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE pstdev_qp
#endif
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
      !! * \(x\) and \(y\) are vectors with real numbers.
      !! * \(n\) is how many numbers are included in \(x\) and \(y\).
      !!
      !! Usage:
      !!
      !! ```
      !! z = covariance(x,y)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` and ```y``` = vectors of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```z``` = real number of the same kind as ```x``` and ```y``` with the sample covariance of ```x``` and ```y```.
      MODULE PROCEDURE covariance_sp
      MODULE PROCEDURE covariance_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE covariance_qp
#endif
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
      !! * \(x\) and \(y\) are vectors with real numbers.
      !! * \(n\) is how many numbers are included in \(x\) and \(y\).
      !!
      !! Usage:
      !!
      !! ```
      !! z = pcovariance(x,y)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` and ```y``` = vectors of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```z``` = real number of the same kind as ```x``` and ```y``` with the population covariance of ```x``` and ```y```.
      MODULE PROCEDURE pcovariance_sp
      MODULE PROCEDURE pcovariance_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE pcovariance_qp
#endif
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
      !! * \(x\) and \(y\) are vectors with real numbers.
      !! * \(n\) is how many numbers are included in \(x\) and \(y\).
      !!
      !! Usage:
      !!
      !! ```
      !! z = correlation(x,y)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` and ```y``` = vectors of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```z``` = real number of the same kind as ```x``` and ```y``` with the correlation coefficient of ```x``` and ```y```.
      MODULE PROCEDURE correlation_sp
      MODULE PROCEDURE correlation_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE correlation_qp
#endif
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
      !! * \(y\) is the response whose uncertainty is to be calculated.
      !! * \(X\) is a set of input parameters to propagate their uncertainty to \(y\).
      !! * \(S\) is the vector of sensitivity coefficients of \(y\) with respect to the
      !!   different parameters in \(X\).
      !! * \(\Sigma^{x}\) is the covariance matrix of the parameters in \(X\).
      !!
      !! Usage:
      !!
      !! ```
      !! y = lin_error_propagation(s,m)
      !! ```
      !!
      !! where:
      !!
      !! * ```s``` = vector of rank 1 with real numbers containing the sensitivity coefficients.
      !! * ```m``` = array of rank 2 containing the covariance matrix.
      !! * ```y``` = real number of the same kind as ```s``` and ```m``` with the
      !! error or uncertainty propagated to this new variable.
      MODULE PROCEDURE lin_error_propagation_sp
      MODULE PROCEDURE lin_error_propagation_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE lin_error_propagation_qp
#endif
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
      !!
      !! Usage:
      !!
      !! ```
      !! y = median(x)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` = vector of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```y``` = real number of the same kind as ```x``` with the median of ```x```.
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
      !! * \(x\) is a vector with real numbers.
      !! * \(n\) is how many numbers are included in \(x\).
      !!
      !! Usage:
      !!
      !! ```
      !! y = skewness(x)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` = vector of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```y``` = real number of the same kind as ```x``` with the sample skewness of ```x```.
      MODULE PROCEDURE skewness_sp
      MODULE PROCEDURE skewness_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE skewness_qp
#endif
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
      !! * \(x\) is a vector with real numbers.
      !! * \(n\) is how many numbers are included in \(x\).
      !!
      !! Usage:
      !!
      !! ```
      !! y = pskewness(x)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` = vector of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```y``` = real number of the same kind as ```x``` with the population skewness of ```x```.
      MODULE PROCEDURE pskewness_sp
      MODULE PROCEDURE pskewness_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE pskewness_qp
#endif
   END INTERFACE pskewness





   INTERFACE regression
      !! author: Emilio Castro.
      !! date: 23/09/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Performs different types of regression between two sets of values.
      !! Performs different types of regression between two sets of values. This is a
      !! private subroutine accesible by using one of [[linreg]], [[logreg]], [[expreg]] or [[potreg]].
      MODULE PROCEDURE regression_sp
      MODULE PROCEDURE regression_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE regression_qp
#endif
   END INTERFACE regression

   INTEGER, PARAMETER :: linreg_id = 1
   !! Selector flag for linear regression in function [[regression]].
   INTEGER, PARAMETER :: logreg_id = 2
   !! Selector flag for logarithmic regression in function [[regression]].
   INTEGER, PARAMETER :: expreg_id = 3
   !! Selector flag for exponential regression in function [[regression]].
   INTEGER, PARAMETER :: potreg_id = 4
   !! Selector flag for potential regression in function [[regression]].


   INTERFACE linreg
      !! author: Emilio Castro.
      !! date: 23/09/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Performs linear regression between two sets of values.
      !! Performs linear regression between two sets of values,
      !! obtaining parameters \(a\) and \(b\) of the following equation.
      !!
      !! $$y = a \cdot x+b$$
      !!
      !! where:
      !!
      !! * \(x\) and \(y\) are vectors with real numbers.
      !! * \(a\) and \(b\) are the regression coefficients.
      !!
      !! Parameter \(R^2\) is also calculated to measure the goodness of fit.
      !!
      !! Usage:
      !!
      !! ```
      !! CALL linreg(x,y,a,b,R2)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` and ```y``` = vectors of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```a```, ```b``` = regression coefficients calculated by the subroutine.
      !! * ```R2``` = the determination coefficient to measure the goodness of fit, calculated by the subroutine.
      MODULE PROCEDURE linreg_sp
      MODULE PROCEDURE linreg_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE linreg_qp
#endif
   END INTERFACE linreg


   INTERFACE logreg
      !! author: Emilio Castro.
      !! date: 23/09/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Performs logarithmic regression between two sets of values.
      !! Performs logarithmic regression between two sets of values,
      !! obtaining parameters \(a\) and \(b\) of the following equation.
      !!
      !! $$y = a \cdot ln(x)+b$$
      !!
      !! where:
      !!
      !! * \(x\) and \(y\) are vectors with real numbers.
      !! * \(a\) and \(b\) are the regression coefficients.
      !!
      !! Parameter \(R^2\) is also calculated to determine the goodness of fit.
      !!
      !! Usage:
      !!
      !! ```
      !! CALL logreg(x,y,a,b,R2)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` and ```y``` = vectors of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```a```, ```b``` = regression coefficients calculated by the subroutine.
      !! * ```R2``` = the determination coefficient to measure the goodness of fit, calculated by the subroutine.
      MODULE PROCEDURE logreg_sp
      MODULE PROCEDURE logreg_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE logreg_qp
#endif
   END INTERFACE logreg


   INTERFACE expreg
      !! author: Emilio Castro.
      !! date: 23/09/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Performs exponential regression between two sets of values.
      !! Performs exponential regression between two sets of values,
      !! obtaining parameters \(a\) and \(b\) of the following equation.
      !!
      !! $$y = b \cdot e^{(a \cdot x)}$$
      !!
      !! where:
      !!
      !! * \(x\) and \(y\) are vectors with real numbers.
      !! * \(a\) and \(b\) are the regression coefficients.
      !!
      !! Parameter \(R^2\) is also calculated to determine the goodness of fit.
      !!
      !! Usage:
      !!
      !! ```
      !! CALL expreg(x,y,a,b,R2)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` and ```y``` = vectors of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```a```, ```b``` = regression coefficients calculated by the subroutine.
      !! * ```R2``` = the determination coefficient to measure the goodness of fit, calculated by the subroutine.
      MODULE PROCEDURE expreg_sp
      MODULE PROCEDURE expreg_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE expreg_qp
#endif
   END INTERFACE expreg


   INTERFACE potreg
      !! author: Emilio Castro.
      !! date: 23/09/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Performs potential regression between two sets of values.
      !! Performs potential regression between two sets of values,
      !! obtaining parameters \(a\) and \(b\) of the following equation.
      !!
      !! $$y = b \cdot x^a$$
      !!
      !! where:
      !!
      !! * \(x\) and \(y\) are vectors with real numbers.
      !! * \(a\) and \(b\) are the regression coefficients.
      !!
      !! Parameter \(R^2\) is also calculated to determine the goodness of fit.
      !!
      !! Usage:
      !!
      !! ```
      !! CALL potreg(x,y,a,b,R2)
      !! ```
      !!
      !! where:
      !!
      !! * ```x``` and ```y``` = vectors of rank 1 with real numbers. See examples to use an array of
      !! rank larger than 1.
      !! * ```a```, ```b``` = regression coefficients calculated by the subroutine.
      !! * ```R2``` = the determination coefficient to measure the goodness of fit, calculated by the subroutine.
      MODULE PROCEDURE potreg_sp
      MODULE PROCEDURE potreg_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE potreg_qp
#endif
   END INTERFACE potreg

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

#ifdef QPREC_FPP
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
#endif

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

#ifdef QPREC_FPP
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
#endif


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

#ifdef QPREC_FPP
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
#endif


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

#ifdef QPREC_FPP
      PURE FUNCTION stdev_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the sample standard deviation.
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the sample standard deviation of x.

         res = SQRT(variance(x))

      END FUNCTION stdev_qp
#endif



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

#ifdef QPREC_FPP
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
#endif


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

#ifdef QPREC_FPP
      PURE FUNCTION pstdev_qp(x) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=qp),DIMENSION(:),INTENT(IN) :: x
         !! Vector of real numbers to calculate the population standard deviation. 
         !! It can have any size and it must have one dimension.
         REAL(KIND=qp)                         :: res
         !! Real number with the population standard deviation of x.

         res = SQRT(pvariance(x))

      END FUNCTION pstdev_qp
#endif


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

#ifdef QPREC_FPP
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
#endif



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

#ifdef QPREC_FPP
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
#endif



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

#ifdef QPREC_FPP
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
#endif



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

#ifdef QPREC_FPP
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
#endif


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

#ifdef QPREC_FPP
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
#endif



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

#ifdef QPREC_FPP
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
#endif




      PURE SUBROUTINE linreg_sp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(linreg_id,x,y,a,b,R2)

      END SUBROUTINE linreg_sp

      PURE SUBROUTINE linreg_dp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(linreg_id,x,y,a,b,R2)

      END SUBROUTINE linreg_dp

#ifdef QPREC_FPP
      PURE SUBROUTINE linreg_qp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(linreg_id,x,y,a,b,R2)

      END SUBROUTINE linreg_qp
#endif


      PURE SUBROUTINE logreg_sp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(logreg_id,x,y,a,b,R2)

      END SUBROUTINE logreg_sp

      PURE SUBROUTINE logreg_dp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(logreg_id,x,y,a,b,R2)

      END SUBROUTINE logreg_dp

#ifdef QPREC_FPP
      PURE SUBROUTINE logreg_qp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(logreg_id,x,y,a,b,R2)

      END SUBROUTINE logreg_qp
#endif


      PURE SUBROUTINE expreg_sp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(expreg_id,x,y,a,b,R2)

      END SUBROUTINE expreg_sp

      PURE SUBROUTINE expreg_dp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(expreg_id,x,y,a,b,R2)

      END SUBROUTINE expreg_dp

#ifdef QPREC_FPP
      PURE SUBROUTINE expreg_qp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(expreg_id,x,y,a,b,R2)

      END SUBROUTINE expreg_qp
#endif


      PURE SUBROUTINE potreg_sp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(potreg_id,x,y,a,b,R2)

      END SUBROUTINE potreg_sp

      PURE SUBROUTINE potreg_dp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(potreg_id,x,y,a,b,R2)

      END SUBROUTINE potreg_dp

#ifdef QPREC_FPP
      PURE SUBROUTINE potreg_qp(x,y,a,b,R2)
         IMPLICIT NONE
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: R2
         !! Determination coefficient.

         CALL regression(potreg_id,x,y,a,b,R2)

      END SUBROUTINE potreg_qp
#endif



      PURE SUBROUTINE regression_sp(typeRegression,x,y,a,b,R2)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: typeRegression
         !! Flag to select the type of regression.
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=sp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=sp), INTENT(OUT) :: R2
         !! Determination coefficient.
         REAL(KIND=sp), DIMENSION(SIZE(x)) :: x_cp
         REAL(KIND=sp), DIMENSION(SIZE(y)) :: y_cp
         REAL(KIND=sp) :: var_x, var_y, covar_xy

         INCLUDE 'Statistics_M/include_regression.f90'

      END SUBROUTINE regression_sp


      PURE SUBROUTINE regression_dp(typeRegression,x,y,a,b,R2)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: typeRegression
         !! Flag to select the type of regression.
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=dp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=dp), INTENT(OUT) :: R2
         !! Determination coefficient.
         REAL(KIND=dp), DIMENSION(SIZE(x)) :: x_cp
         REAL(KIND=dp), DIMENSION(SIZE(y)) :: y_cp
         REAL(KIND=dp) :: var_x, var_y, covar_xy

         INCLUDE 'Statistics_M/include_regression.f90'

      END SUBROUTINE regression_dp


#ifdef QPREC_FPP
      PURE SUBROUTINE regression_qp(typeRegression,x,y,a,b,R2)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: typeRegression
         !! Flag to select the type of regression.
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: x
         !! Vector of real numbers with the values of the first variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: y
         !! Vector of real numbers with the values of the second variable. It can
         !! have any size and it must have one dimension.
         REAL(KIND=qp), INTENT(OUT) :: a
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: b
         !! Regression coefficient.
         REAL(KIND=qp), INTENT(OUT) :: R2
         !! Determination coefficient.
         REAL(KIND=qp), DIMENSION(SIZE(x)) :: x_cp
         REAL(KIND=qp), DIMENSION(SIZE(y)) :: y_cp
         REAL(KIND=qp) :: var_x, var_y, covar_xy

         INCLUDE 'Statistics_M/include_regression.f90'

      END SUBROUTINE regression_qp
#endif

END MODULE FU_Statistics
