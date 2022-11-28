PROGRAM FU_example4
   ! Example program for FU_Prec, FU_Files and FU_Interpolation modules of ecasglez's FortranUtilities,
   ! showing how to load data from a file and perform a linear interpolation.
   ! The dataset can be downloaded from: https://ecasglez.github.io/FortranUtilities/page/Examples/Example04/example4.dat
   ! compile using: gfortran example4.f90 -o example4 -I/path/to/include/ -lFortranUtilities -L/path/to/lib/ -O2
   ! before running: export LD_LIBRARY_PATH=/path/to/lib:${LD_LIBRARY_PATH}
   ! run using: ./example4
   ! license: MIT.

   USE FU_Prec         , ONLY: dp
   USE FU_Files        , ONLY: readMatrix
   USE FU_Interpolation, ONLY: lin_interp

   IMPLICIT NONE

   LOGICAL :: exists
   REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE :: matrix

   !First check if the dataset exists.
   INQUIRE(FILE='example4.dat', EXIST=exists)
   IF (.NOT.exists) THEN
      WRITE(*,*) 'ERROR: Dataset named "example04.dat" not found.'
      STOP
   END IF

   !Load the data in the file.
   CALL readMatrix('example4.dat',matrix)

   PRINT *, 'Value at x=0.364 is ', lin_interp(0.364_dp, matrix(:,1), matrix(:,2))
   PRINT *, 'Value at x=4.111 is ', lin_interp(4.111_dp, matrix(:,1), matrix(:,2))


   DEALLOCATE(matrix)

END PROGRAM FU_example4
