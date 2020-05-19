PROGRAM FortranUtilitiesTest

   USE Strings_M

   IMPLICIT NONE

   WRITE(*,*) 'Test  1', splitstr('campo1 campo2 campo3 campo4') == 'campo1'
   WRITE(*,*) 'Test  2', splitstr('campo1 campo2 campo3 campo4',fieldNumber=1) == 'campo1'
   WRITE(*,*) 'Test  3', splitstr('campo1 campo2 campo3 campo4',fieldNumber=2) == 'campo2'
   WRITE(*,*) 'Test  4', splitstr('campo1 campo2 campo3 campo4',fieldNumber=3) == 'campo3'
   WRITE(*,*) 'Test  5', splitstr('campo1 campo2 campo3 campo4',fieldNumber=4) == 'campo4'
   WRITE(*,*) 'Test  6', splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=1) == 'c'
   WRITE(*,*) 'Test  7', splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=2) == 'mpo1 c'
   WRITE(*,*) 'Test  8', splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=3) == 'mpo2 c'
   WRITE(*,*) 'Test  9', splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=4) == 'mpo3 c'
   WRITE(*,*) 'Test 10', splitstr('campo1 campo2 campo3 campo4',delimiter = 'a',fieldNumber=5) == 'mpo4 '
   WRITE(*,*) 'Test 11', splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=1) == 'ampo1 '
   WRITE(*,*) 'Test 12', splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=2) == 'ampo2 '
   WRITE(*,*) 'Test 13', splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=3) == 'ampo3 '
   WRITE(*,*) 'Test 14', splitstr('campo1 campo2 campo3 campo4',delimiter = 'c',fieldNumber=4) == 'ampo4'
   WRITE(*,*) 'Test 15', splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=1) == 'mpo1 '
   WRITE(*,*) 'Test 16', splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=2) == 'mpo2 '
   WRITE(*,*) 'Test 17', splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=3) == 'mpo3 '
   WRITE(*,*) 'Test 18', splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=4) == 'mpo4'
   WRITE(*,*) 'Test 19', splitstr('campo1 campo2 campo3 campo4',delimiter = '4',fieldNumber=1) == 'campo1 campo2 campo3 campo'
   WRITE(*,*) 'Test 20', splitstr('campo1 campo2 campo3 campo4',delimiter = 'ca',fieldNumber=8) == ''
   WRITE(*,*) 'Test 21', splitstr('campo1 campo2 campo3 campo4',delimiter = 'j') == 'campo1 campo2 campo3 campo4'
   WRITE(*,*) 'Test 22', startsWith('frase','fra') .EQV. .TRUE.
   WRITE(*,*) 'Test 23', startsWith('frase','f') .EQV. .TRUE.
   WRITE(*,*) 'Test 24', startsWith('frase','afra') .EQV. .FALSE.
   WRITE(*,*) 'Test 25', endsWith('frase','ase') .EQV. .TRUE.
   WRITE(*,*) 'Test 26', endsWith('frase','e') .EQV. .TRUE.
   WRITE(*,*) 'Test 27', endsWith('frase','asa') .EQV. .FALSE.
   WRITE(*,*) 'Test 28', int2char(5) == '5'
   WRITE(*,*) 'Test 29', int2char(0) == '0'
   WRITE(*,*) 'Test 20', int2char(-125) == '-125'
   WRITE(*,*) 'Test 31', int2char00000(5,9) == '000000005'
   WRITE(*,*) 'Test 32', int2char00000(5,1) == '5'
   WRITE(*,*) 'Test 33', int2char00000(0,3) == '000'
   WRITE(*,*) 'Test 34', count_digits_integer(824) == 3
   WRITE(*,*) 'Test 35', count_digits_integer(-4421) == 5
   WRITE(*,*) 'Test 35', char2int('4321') == 4321
   WRITE(*,*) 'Test 36', char2int('-4134') == -4134
   WRITE(*,*) 'Test 37', ABS(char2real('-4134.8786') - (-4134.8786_8)) < 1E-10
   WRITE(*,*) 'Test 38', ABS(char2real('0.55') - (0.55_8)) < 1E-10

END PROGRAM FortranUtilitiesTest
