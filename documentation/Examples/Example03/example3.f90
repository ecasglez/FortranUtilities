PROGRAM FU_example3
   ! Example program for FU_Prec, FU_Files and FU_Statistics modules of ecasglez's FortranUtilities,
   ! showing how to load data from a file and how to perform different types of regressions.
   ! The dataset can be downloaded from: https://ecasglez.github.io/FortranUtilities/page/Examples/Example03/example3.dat
   ! compile using: gfortran example3.f90 -o example3 -I/path/to/include/ -lFortranUtilities -L/path/to/lib/ -O2
   ! before running: export LD_LIBRARY_PATH=/path/to/lib:${LD_LIBRARY_PATH}
   ! run using: ./example3
   ! license: MIT.

   USE FU_Prec      , ONLY: dp
   USE FU_Files     , ONLY: readMatrix
   USE FU_statistics, ONLY: linreg, logreg, expreg, potreg

   IMPLICIT NONE

   LOGICAL :: exists
   REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE :: matrix
   REAL(KIND=dp) :: a, b, r2

   !First check if the dataset exists.
   INQUIRE(FILE='example3.dat', EXIST=exists)
   IF (.NOT.exists) THEN
      WRITE(*,*) 'ERROR: Dataset named "example03.dat" not found.'
      STOP
   END IF

   !Load the data in the file.
   CALL readMatrix('example3.dat',matrix)

   !Calculate regression parameters and print the results.
   CALL linreg(matrix(:,1),matrix(:,2),a,b,r2)
   WRITE(*,'(A,F6.4,A,F6.4,A,F6.4)') 'Linear regression:      f(x) = ',a,' x + ',b,'.      Determination coefficient R2 = ',r2
   CALL logreg(matrix(:,1),matrix(:,3),a,b,r2)
   WRITE(*,'(A,F6.4,A,F6.4,A,F6.4)') 'Logarithmic regression: f(x) = ',a,' ln(x) + ',b,'.  Determination coefficient R2 = ',r2
   CALL expreg(matrix(:,1),matrix(:,4),a,b,r2)
   WRITE(*,'(A,F6.4,A,F6.4,A,F6.4)') 'Exponential regression: f(x) = ',b,' exp(',a,'x).    Determination coefficient R2 = ',r2
   CALL potreg(matrix(:,1),matrix(:,5),a,b,r2)
   WRITE(*,'(A,F6.4,A,F6.4,A,F6.4)') 'Potential regression:   f(x) = ',b,' x^',a,'.        Determination coefficient R2 = ',r2

   DEALLOCATE(matrix)

END PROGRAM FU_example3
