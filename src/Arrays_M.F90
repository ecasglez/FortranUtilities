!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------

MODULE FU_Arrays
   !! author: Emilio Castro.
   !! date: 24/11/2022.
   !! version: 1.0.
   !! license: MIT.
   !! summary: Functions to manipulate 1D arrays.
   !! Functions to manipulate 1D arrays.

   USE FU_Prec

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: is_ordered

   INTERFACE is_ordered
      !! author: Emilio Castro.
      !! date: 24/11/2022.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Checks if an array is ascending ordered.
      !! Checks if an array is ascending ordered.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! y = is_ordered(arr)
      !!```
      !!
      !! Where:
      !!
      !! * `arr`: Array to test if it is ascending ordered.
      !! 
      !! It returns .TRUE. if the array is ascending ordered and .FALSE. otherwise.
      !!
      !!### Example
      !!
      !! The following program checks if an array is ascending ordered:
      !!
      !!```Fortran
      !! PROGRAM is_orderedExample
      !!    USE FU_Arrays, ONLY: is_ordered
      !!    IMPLICIT NONE
      !!    REAL,DIMENSION(6) :: X = (/1., 2., 3., 4., 5., 6./)
      !!    WRITE(*,*) is_ordered(X)
      !! END PROGRAM is_orderedExample
      !!```
      MODULE PROCEDURE is_ordered_sp
      MODULE PROCEDURE is_ordered_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE is_ordered_qp
#endif
   END INTERFACE is_ordered

   CONTAINS

   PURE FUNCTION is_ordered_sp(arr) RESULT(res)
      IMPLICIT NONE
      !! Vector to test if it is ascending ordered
      REAL(KIND=sp), DIMENSION(:), INTENT(IN) :: arr
      !! .TRUE. if it is ascending ordered, .FALSE. otherwise
      LOGICAL :: res
      INTEGER :: s

      INCLUDE 'Arrays_M/include_is_ordered.f90'

   END FUNCTION is_ordered_sp

   PURE FUNCTION is_ordered_dp(arr) RESULT(res)
      IMPLICIT NONE
      !! Vector to test if it is ascending ordered
      REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: arr
      !! .TRUE. if it is ascending ordered, .FALSE. otherwise
      LOGICAL :: res
      INTEGER :: s

      INCLUDE 'Arrays_M/include_is_ordered.f90'

   END FUNCTION is_ordered_dp

#ifdef QPREC_FPP
   PURE FUNCTION is_ordered_qp(arr) RESULT(res)
      IMPLICIT NONE
      !! Vector to test if it is ascending ordered
      REAL(KIND=qp), DIMENSION(:), INTENT(IN) :: arr
      !! .TRUE. if it is ascending ordered, .FALSE. otherwise
      LOGICAL :: res
      INTEGER :: s

      INCLUDE 'Arrays_M/include_is_ordered.f90'

   END FUNCTION is_ordered_qp
#endif

END MODULE FU_Arrays
