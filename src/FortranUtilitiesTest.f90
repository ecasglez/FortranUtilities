PROGRAM FortranUtilitiesTest

   USE Strings_M
   USE Prec_M
   USE Statistics_M

   IMPLICIT NONE

   REAL(KIND=sp),DIMENSION(6) :: vecSp = (/1., 2., 3., 4., 5., 6./)
   REAL(KIND=dp),DIMENSION(6) :: vecDp = (/1., 2., 3., 4., 5., 6./)
   REAL(KIND=qp),DIMENSION(6) :: vecQp = (/1., 2., 3., 4., 5., 6./)

   CALL test(splitstr('campo1 campo2 campo3 campo4',fieldNumber=1_i8) == 'campo1')
   CALL test(splitstr('campo1 campo2 campo3 campo4',fieldNumber=2_i16) == 'campo2')
   CALL test(splitstr('campo1 campo2 campo3 campo4',fieldNumber=3_i32) == 'campo3')
   CALL test(splitstr('campo1 campo2 campo3 campo4',fieldNumber=4_i64) == 'campo4')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=1_i64) == 'c')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=2_i32) == 'mpo1 c')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=3_i16) == 'mpo2 c')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=4_i8) == 'mpo3 c')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=5_i32) == 'mpo4 ')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=1_i32) == 'ampo1 ')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=2_i16) == 'ampo2 ')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=3_i8) == 'ampo3 ')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=4_i64) == 'ampo4')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=1_i32) == 'mpo1 ')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=2_i16) == 'mpo2 ')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=3_i8) == 'mpo3 ')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=4_i8) == 'mpo4')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = '4',fieldNumber=1_i64) == 'campo1 campo2 campo3 campo')
   CALL test(splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=8_i32) == '')
   CALL test(startsWith('frase','fra') .EQV. .TRUE.)
   CALL test(startsWith('frase','f') .EQV. .TRUE.)
   CALL test(startsWith('frase','afra') .EQV. .FALSE.)
   CALL test(endsWith('frase','ase') .EQV. .TRUE.)
   CALL test(endsWith('frase','e') .EQV. .TRUE.)
   CALL test(endsWith('frase','asa') .EQV. .FALSE.)
   CALL test(num2str(5) == '5')
   CALL test(num2str(5_i8) == '5')
   CALL test(num2str(5_i16) == '5')
   CALL test(num2str(5_i32) == '5')
   CALL test(num2str(5_i64) == '5')
   CALL test(num2str(0) == '0')
   CALL test(num2str(-125) == '-125')
   CALL test(num2str(-125_i8) == '-125')
   CALL test(num2str(-125_i16) == '-125')
   CALL test(num2str(-125_i32) == '-125')
   CALL test(num2str(-125_i64) == '-125')
   CALL test(num2str(-65.889_sp,'F7.3') == '-65.889')
   CALL test(num2str(-65.889_sp,'E10.3') == '-0.659E+02')
   CALL test(int2str00000(5_i8,9_i8) == '000000005')
   CALL test(int2str00000(5_i16,9_i16) == '000000005')
   CALL test(int2str00000(5_i32,9_i32) == '000000005')
   CALL test(int2str00000(5_i64,9_i64) == '000000005')
   CALL test(int2str00000(5_i8,1_i8) == '5')
   CALL test(int2str00000(5_i16,1_i16) == '5')
   CALL test(int2str00000(5_i32,1_i32) == '5')
   CALL test(int2str00000(5_i64,1_i64) == '5')
   CALL test(int2str00000(0,3) == '000')
   CALL test(count_digits_integer(824) == 3)
   CALL test(count_digits_integer(-4421) == 5)
   CALL test(str2num('4321',1_i32) == 4321_i32)
   CALL test(str2num('-4134',1_i64) == -4134_i64)
   CALL test(ABS(str2num('-4134.8786',1._sp) - (-4134.8786_sp)) < 1E-10)
   CALL test(ABS(str2num('0.55',1._dp) - (0.55_dp)) < 1E-10)
   CALL test(ABS(str2num('55E10',1._dp) - (55E10_dp)) < 1E-10)
   CALL test(ABS(mean((/4.2_sp/)) - 4.2_sp) < 1E-10)
   CALL test(ABS(mean((/4.2_dp/)) - 4.2_dp) < 1E-10)
   CALL test(ABS(mean((/4.2_qp/)) - 4.2_qp) < 1E-10)
   CALL test(ABS(mean(vecSp) - 3.5_sp) < 1E-10)
   CALL test(ABS(mean(vecDp) - 3.5_dp) < 1E-10)
   CALL test(ABS(mean(vecQp) - 3.5_qp) < 1E-10)

   CONTAINS
      SUBROUTINE test(testRes)
         IMPLICIT NONE
         LOGICAL, INTENT(IN) :: testRes
         INTEGER             :: i = 0
         i = i + 1
         WRITE(*,*) 'Test' , i, testRes
      END SUBROUTINE test

END PROGRAM FortranUtilitiesTest
