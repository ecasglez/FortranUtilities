!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------
!
! MODULE: Prec_M
!
! DESCRIPTION:
!> @brief Precision parameters to use in Fortran programs.
!
! REVISION HISTORY:
! 13-05-2020 - Initial Version.
!
!> @author Emilio Castro.
!> @version 1.0.
!
!> @copyright See LICENSE file that comes with this distribution.
!--------------------------------------------------------------------

MODULE Prec_M
   USE ISO_FORTRAN_ENV
   IMPLICIT NONE
   !> Kind parameter to specify a real type with a storage size of 32 bits.
   INTEGER, PARAMETER :: sp  = REAL32 
   !> Kind parameter to specify a real type with a storage size of 64 bits.
   INTEGER, PARAMETER :: dp  = REAL64
   !> Kind parameter to specify a real type with a storage size of 128 bits.
   INTEGER, PARAMETER :: qp  = REAL128
   !> Kind parameter to specify an integer type with a storage size of 8 bits.
   INTEGER, PARAMETER :: i8  = INT8
   !> Kind parameter to specify an integer type with a storage size of 16 bits.
   INTEGER, PARAMETER :: i16 = INT16
   !> Kind parameter to specify an integer type with a storage size of 32 bits.
   INTEGER, PARAMETER :: i32 = INT32
   !> Kind parameter to specify an integer type with a storage size of 64 bits.
   INTEGER, PARAMETER :: i64 = INT64
END MODULE Prec_M
