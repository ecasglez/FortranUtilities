PROGRAM FU_example1
   ! Example program for FU_Prec, FU_Statistics and FU_Timing modules of ecasglez's FortranUtilities.
   ! It repeats a lot of mean, variance and median calculations using random numbers.
   ! Uses OpenMP for parallel execution.
   ! compile using: gfortran example1.f90 -o example1 -fopenmp -I/path/to/include/ -lFortranUtilities -L/path/to/lib/ -O2
   ! before running: export LD_LIBRARY_PATH=/path/to/lib:${LD_LIBRARY_PATH}
   ! run using: ./example1
   ! license: MIT.

   !$   USE omp_lib
   USE FU_Prec      , ONLY: dp
   USE FU_statistics, ONLY: mean, variance, median
   USE FU_Timing    , ONLY: resetTotalTime, IntervalTIme, TotalTime

   IMPLICIT NONE

   INTEGER, PARAMETER :: max_num_threads = 7
   INTEGER, PARAMETER :: n = 1000
   REAL(KIND=dp),DIMENSION(:,:),ALLOCATABLE :: matrix
   REAL(KIND=dp),DIMENSION(n) :: media, varianza, mediana
   INTEGER :: i, k

   ALLOCATE(matrix(300000,n))

   DO i=1,n
      CALL random_number(matrix(:,i))
   END DO

   CALL resetTotalTime()
   DO k = 1, max_num_threads
!$    CALL omp_set_num_threads(k)
!$OMP PARALLEL DO
      DO i=1,n
         media(i) = mean(matrix(:,i))
         varianza(i) = variance(matrix(:,i))
         mediana(i) = median(matrix(:,i))
      END DO
!$OMP END PARALLEL DO
      WRITE(*,'(A,I0,A,F7.3,A)') 'Number of threads: ', k, '. Time spent: ', IntervalTime(), ' s.'
   END DO
   WRITE(*,'(A,F7.3,A)') 'Total time spent: ', TotalTime(), ' s.'

   DEALLOCATE(matrix)

END PROGRAM FU_example1
