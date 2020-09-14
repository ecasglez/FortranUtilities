PROGRAM FU_example2
   ! Example program for FU_Prec and FU_Statistics modules of ecasglez's FortranUtilities,
   ! showing how to apply FU_Statistics functions to an array with rank larger than 1.
   ! It shows 3 examples on how to calculate the mean value of a rank 3 array.
   ! compile using: gfortran example2.f90 -o example2 -I/path/to/include/ -lFortranUtilities -L/path/to/lib/ -O2
   ! before running: export LD_LIBRARY_PATH=/path/to/lib:${LD_LIBRARY_PATH}
   ! run using: ./example2
   ! license: MIT.

   USE FU_Prec      , ONLY: dp
   USE FU_statistics, ONLY: mean

   IMPLICIT NONE

   INTEGER, PARAMETER :: n = 100
   REAL(KIND=8),DIMENSION(:,:,:),ALLOCATABLE :: matrix
   REAL(KIND=8) :: media

   ALLOCATE(matrix(n,n,n))
   CALL random_number(matrix(:,:,:))

   !First method. Use RESHAPE.
   media = mean(RESHAPE(matrix,([SIZE(matrix)])))
   WRITE(*,'(A,F9.5,A)') 'Mean value: ', media, '.'

   !Second method. Use an array constructor.
   media = mean([matrix])
   WRITE(*,'(A,F9.5,A)') 'Mean value: ', media, '.'

   !Third method. Use sequence association. Use auxiliary function mean3D (see below)
   media = mean3D(matrix,SIZE(matrix))
   WRITE(*,'(A,F9.5,A)') 'Mean value: ', media, '.'

   DEALLOCATE(matrix)

   CONTAINS

      FUNCTION mean3D(a,n) RESULT(res)
         USE FU_statistics, ONLY: mean
         IMPLICIT NONE
         REAL(KIND=dp),DIMENSION(n), INTENT(IN) :: a
         INTEGER, INTENT(IN) :: n
         REAL(KIND=dp) :: res
         res = mean(a)
      END FUNCTION mean3D

END PROGRAM FU_example2
