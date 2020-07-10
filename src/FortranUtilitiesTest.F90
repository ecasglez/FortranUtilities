PROGRAM FortranUtilitiesTest

   USE FU_Strings
   USE FU_Prec
   USE FU_Statistics
   USE FU_Numbers
   USE FU_Files

   IMPLICIT NONE

   REAL(KIND=sp),DIMENSION(6) :: vecSp = (/1., 2., 3., 4., 5., 6./)
   REAL(KIND=dp),DIMENSION(6) :: vecDp = (/1., 2., 3., 4., 5., 6./)
   REAL(KIND=qp),DIMENSION(6) :: vecQp = (/1., 2., 3., 4., 5., 6./)
   REAL(KIND=sp) :: zero_sp = 0.0
   REAL(KIND=dp) :: zero_dp = 0.0
   REAL(KIND=qp) :: zero_qp = 0.0
   INTEGER :: u


   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',fieldNumber=1_i8) == 'campo1')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',fieldNumber=2_i16) == 'campo2')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',fieldNumber=3_i32) == 'campo3')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',fieldNumber=4_i64) == 'campo4')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=1_i64) == 'c')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=2_i32) == 'mpo1 c')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=3_i16) == 'mpo2 c')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=4_i8) == 'mpo3 c')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=5_i32) == 'mpo4 ')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=1_i32) == 'ampo1 ')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=2_i16) == 'ampo2 ')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=3_i8) == 'ampo3 ')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=4_i64) == 'ampo4')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=1_i32) == 'mpo1 ')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=2_i16) == 'mpo2 ')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=3_i8) == 'mpo3 ')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=4_i8) == 'mpo4')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = '4',fieldNumber=1_i64) == 'campo1 campo2 campo3 campo')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=8_i32) == '')
   CALL test('splitstr',splitstr('campo1 campo2 campo3 campo4',fieldNumber=3_i32, rev = .TRUE.) == 'campo2')
   CALL test('startsWith',startsWith('frase','fra') .EQV. .TRUE.)
   CALL test('startsWith',startsWith('frase','f') .EQV. .TRUE.)
   CALL test('startsWith',startsWith('frase','afra') .EQV. .FALSE.)
   CALL test('endsWith',endsWith('frase','ase') .EQV. .TRUE.)
   CALL test('endsWith',endsWith('frase','e') .EQV. .TRUE.)
   CALL test('endsWith',endsWith('frase','asa') .EQV. .FALSE.)
   CALL test('num2str',num2str(5) == '5')
   CALL test('num2str',num2str(5_i8) == '5')
   CALL test('num2str',num2str(5_i16) == '5')
   CALL test('num2str',num2str(5_i32) == '5')
   CALL test('num2str',num2str(5_i64) == '5')
   CALL test('num2str',num2str(0) == '0')
   CALL test('num2str',num2str(-125) == '-125')
   CALL test('num2str',num2str(-125_i8) == '-125')
   CALL test('num2str',num2str(-125_i16) == '-125')
   CALL test('num2str',num2str(-125_i32) == '-125')
   CALL test('num2str',num2str(-125_i64) == '-125')
   CALL test('num2str',num2str(-65.889_sp,'F7.3') == '-65.889')
   CALL test('num2str',num2str(-65.889_sp,'E10.3') == '-0.659E+02')
   CALL test('int2str00000',int2str00000(5_i8,9_i8) == '000000005')
   CALL test('int2str00000',int2str00000(5_i16,9_i16) == '000000005')
   CALL test('int2str00000',int2str00000(5_i32,9_i32) == '000000005')
   CALL test('int2str00000',int2str00000(5_i64,9_i64) == '000000005')
   CALL test('int2str00000',int2str00000(5_i8,1_i8) == '5')
   CALL test('int2str00000',int2str00000(5_i16,1_i16) == '5')
   CALL test('int2str00000',int2str00000(5_i32,1_i32) == '5')
   CALL test('int2str00000',int2str00000(5_i64,1_i64) == '5')
   CALL test('int2str00000',int2str00000(0,3) == '000')
   CALL test('replace',replace('fggasdfggre23fgg','fgg','X') == 'XasdXre23X')
   CALL test('replace',replace('fggasdfggre23fgg','Y','X') == 'fggasdfggre23fgg')
   CALL test('count_digits_integer',count_digits_integer(824) == 3)
   CALL test('count_digits_integer',count_digits_integer(-4421) == 5)
   CALL test('str2num',str2num('4321',1_i32) == 4321_i32)
   CALL test('str2num',str2num('-4134',1_i64) == -4134_i64)
   CALL test('str2num',ABS(str2num('-4134.8786',1._sp) - (-4134.8786_sp)) < 1E-10)
   CALL test('str2num',ABS(str2num('0.55',1._dp) - (0.55_dp)) < 1E-10)
   CALL test('str2num',ABS(str2num('55E10',1._dp) - (55E10_dp)) < 1E-10)
   CALL test('mean',ABS(mean((/4.2_sp/)) - 4.2_sp) < 1E-10)
   CALL test('mean',ABS(mean((/4.2_dp/)) - 4.2_dp) < 1E-10)
   CALL test('mean',ABS(mean((/4.2_qp/)) - 4.2_qp) < 1E-10)
   CALL test('mean',ABS(mean(vecSp) - 3.5_sp) < 1E-10)
   CALL test('mean',ABS(mean(vecDp) - 3.5_dp) < 1E-10)
   CALL test('mean',ABS(mean(vecQp) - 3.5_qp) < 1E-10)
   CALL test('gmean',ABS(gmean((/4.2_sp/)) - 4.2_sp) < 1E-10)
   CALL test('gmean',ABS(gmean((/4.2_dp/)) - 4.2_dp) < 1E-10)
   CALL test('gmean',ABS(gmean((/4.2_qp/)) - 4.2_qp) < 1E-10)
   CALL test('gmean',ABS(gmean(vecSp) - 2.99379516_sp) < 1E-10)
   CALL test('gmean',ABS(gmean(vecDp) - 2.9937951655239088_dp) < 1E-10)
   CALL test('gmean',ABS(gmean(vecQp) - 2.99379516552390895491016056788943720_qp) < 1E-10)
   CALL test('variance',ABS(variance(vecSp) - 3.5_sp) < 1E-10)
   CALL test('variance',ABS(variance(vecDp) - 3.5_dp) < 1E-10)
   CALL test('variance',ABS(variance(vecQp) - 3.5_qp) < 1E-10)
   CALL test('stdev',ABS(stdev(vecSp) - 1.87082875_sp) < 1E-10)
   CALL test('stdev',ABS(stdev(vecDp) - 1.8708286933869707_dp) < 1E-10)
   CALL test('stdev',ABS(stdev(vecQp) - 1.87082869338697069279187436615827459_qp) < 1E-10)
   CALL test('pvariance',ABS(pvariance(vecSp) - 2.91666675_sp) < 1E-10)
   CALL test('pvariance',ABS(pvariance(vecDp) - 2.9166666666666665_dp) < 1E-10)
   CALL test('pvariance',ABS(pvariance(vecQp) - 2.91666666666666666666666666666666654_qp) < 1E-10)
   CALL test('pstdev',ABS(pstdev(vecSp) - 1.70782518_sp) < 1E-10)
   CALL test('pstdev',ABS(pstdev(vecDp) - 1.7078251276599330_dp) < 1E-10)
   CALL test('pstdev',ABS(pstdev(vecQp) - 1.70782512765993306387017311342017542_qp) < 1E-10)
   CALL test('is_nan',.NOT.is_nan(5._sp))
   CALL test('is_nan',.NOT.is_nan(5._dp))
   CALL test('is_nan',.NOT.is_nan(5._qp))
   CALL test('is_nan',is_nan(zero_sp/zero_sp))
   CALL test('is_nan',is_nan(zero_dp/zero_dp))
   CALL test('is_nan',is_nan(zero_qp/zero_qp))
   CALL test('is_nan',ALL(.NOT.is_nan(vecSp)))
   CALL test('is_nan',ALL(.NOT.is_nan(vecDp)))
   CALL test('is_nan',ALL(.NOT.is_nan(vecQp)))
   !Falta comprobar is_nan a true con un vector de zeros.
   CALL test('is_inf',.NOT.is_inf(5._sp))
   CALL test('is_inf',.NOT.is_inf(5._dp))
   CALL test('is_inf',.NOT.is_inf(5._qp))
   CALL test('is_inf',is_inf(5._sp/zero_sp))
   CALL test('is_inf',is_inf(5._dp/zero_dp))
   CALL test('is_inf',is_inf(5._qp/zero_qp))
   CALL test('is_inf',is_inf(-5._sp/zero_sp))
   CALL test('is_inf',is_inf(-5._dp/zero_dp))
   CALL test('is_inf',is_inf(-5._qp/zero_qp))
   CALL test('is_inf',ALL(.NOT.is_inf(vecSp)))
   CALL test('is_inf',ALL(.NOT.is_inf(vecDp)))
   CALL test('is_inf',ALL(.NOT.is_inf(vecQp)))
   CALL test('is_inf',ALL(is_inf(vecSp/zero_sp)))
   CALL test('is_inf',ALL(is_inf(vecDp/zero_dp)))
   CALL test('is_inf',ALL(is_inf(vecQp/zero_qp)))
   CALL test('mkdir',mkdir("testdir32"))
   CALL test('is_empty',is_empty("testdir32"))
   OPEN(NEWUNIT=u,FILE="testfile32",STATUS="REPLACE")
   WRITE(u,*) 'test content'
   CLOSE(u)
   OPEN(NEWUNIT=u,FILE="testdir32"//filesep//"testemptyfile",STATUS="REPLACE")
   CLOSE(u)
   CALL test('is_empty and filesep',is_empty("testdir32"//filesep//"testemptyfile"))
   CALL test('is_empty',.NOT.is_empty("testdir32"))
   CALL test('is_empty',.NOT.is_empty("testfile32"))
   CALL test('cp',cp("testdir32","testdir33"))
   CALL test('cp',cp("testfile32","testfile33"))
   CALL test('mv',mv("testdir32","testdir34"))
   CALL test('mv',mv("testfile32","testfile34"))
   CALL test('mv',.NOT.mv("testdir34","testdir33",.TRUE.))
   CALL test('mv',mv("testfile34","testfile33"))
   CALL test('exists',exists("testdir33"))
   CALL test('exists',exists("testfile33"))
#ifdef LIN_CPP
   CALL test('create_symlink',create_symlink("testdir33","linkedtestdir33"))
   CALL test('create_symlink',create_symlink("testfile33","linkedtestfile33"))
#elif WIN_CPP
   CALL test('create_symlink. N/A on Windows',.TRUE.)
   CALL test('create_symlink. N/A on Windows',.TRUE.)
#endif
   CALL test('is_directory',is_directory("testdir33"))
#ifdef LIN_CPP
   CALL test('is_directory',is_directory("linkedtestdir33"))
#elif WIN_CPP
   CALL test('is_directory. N/A on Windows',.TRUE.)
#endif
   CALL test('is_directory',.NOT.is_directory("testfile33"))
   CALL test('is_regular_file',.NOT.is_regular_file("testdir33"))
   CALL test('is_regular_file',is_regular_file("testfile33"))
#ifdef LIN_CPP
   CALL test('is_regular_file',is_regular_file("linkedtestfile33"))
#elif WIN_CPP
   CALL test('is_regular_file. N/A on Windows',.TRUE.)
#endif
#ifdef LIN_CPP
   CALL test('is_symlink',.NOT.is_symlink("testdir33"))
   CALL test('is_symlink',.NOT.is_symlink("testfile33"))
   CALL test('is_symlink',is_symlink("linkedtestdir33"))
   CALL test('is_symlink',is_symlink("linkedtestfile33"))
#elif WIN_CPP
   CALL test('is_symlink. N/A on Windows',.TRUE.)
   CALL test('is_symlink. N/A on Windows',.TRUE.)
   CALL test('is_symlink. N/A on Windows',.TRUE.)
   CALL test('is_symlink. N/A on Windows',.TRUE.)
#endif
   CALL test('rm',rm("testdir33"))
   CALL test('rm',rm("testdir34"))
   CALL test('rm',rm("testfile33"))
   CALL test('rm',rm("testfile34"))
   CALL test('rm',rm("linkedtestfile33"))
   CALL test('rm',rm("linkedtestdir33"))

   CONTAINS
      SUBROUTINE test(testname, testRes)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: testName
         LOGICAL         , INTENT(IN) :: testRes
         INTEGER                      :: i = 0
         i = i + 1
         WRITE(*,'(A,1X,I3,1X,L,1X,A)') 'Test' , i, testRes, testName
      END SUBROUTINE test

END PROGRAM FortranUtilitiesTest
