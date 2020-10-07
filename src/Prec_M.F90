!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------

MODULE FU_Prec
   !! author: Emilio Castro.
   !! date: 13/05/2020
   !! version: 1.0.
   !! license: MIT.
   !! summary: Precision parameters to use in Fortran programs.
   !! Precision parameters to use in Fortran programs with real or integer variable types.
   USE ISO_FORTRAN_ENV
   IMPLICIT NONE
   INTEGER, PARAMETER :: sp  = REAL32 
   !! Kind parameter to specify a real type with a storage size of 32 bits.
   INTEGER, PARAMETER :: dp  = REAL64
   !! Kind parameter to specify a real type with a storage size of 64 bits.
#ifdef QPREC_FPP
   INTEGER, PARAMETER :: qp  = REAL128
   !! Kind parameter to specify a real type with a storage size of 128 bits.
#endif
   INTEGER, PARAMETER :: i8  = INT8
   !! Kind parameter to specify an integer type with a storage size of 8 bits.
   INTEGER, PARAMETER :: i16 = INT16
   !! Kind parameter to specify an integer type with a storage size of 16 bits.
   INTEGER, PARAMETER :: i32 = INT32
   !! Kind parameter to specify an integer type with a storage size of 32 bits.
   INTEGER, PARAMETER :: i64 = INT64
   !! Kind parameter to specify an integer type with a storage size of 64 bits.
END MODULE FU_Prec
