PROGRAM FortranUtilitiesTest

   USE FU_Strings
   USE FU_Prec
   USE FU_Statistics
   USE FU_Numbers
   USE FU_Files
   USE FU_Timing
   USE FU_Arrays
   USE FU_Interpolation

   IMPLICIT NONE

   REAL(KIND=sp),DIMENSION(6) :: vecSp = (/1., 2., 3., 4., 5., 6./)
   REAL(KIND=dp),DIMENSION(6) :: vecDp = (/1., 2., 3., 4., 5., 6./)
#ifdef QPREC_FPP
   REAL(KIND=qp),DIMENSION(6) :: vecQp = (/1., 2., 3., 4., 5., 6./)
#endif
   REAL(KIND=sp),DIMENSION(6) :: vec1Sp = (/0.9, 2.6, 4.3, 8.2, 10.0, 11.1/)
   REAL(KIND=dp),DIMENSION(6) :: vec1Dp = (/0.9, 2.6, 4.3, 8.2, 10.0, 11.1/)
#ifdef QPREC_FPP
   REAL(KIND=qp),DIMENSION(6) :: vec1Qp = (/0.9, 2.6, 4.3, 8.2, 10.0, 11.1/)
#endif
   REAL(KIND=sp),DIMENSION(6) :: vec2Sp = (/10.0, 0.9, 8.2, 4.3, 2.6, 11.1/)
   REAL(KIND=dp),DIMENSION(6) :: vec2Dp = (/10.0, 0.9, 8.2, 4.3, 2.6, 11.1/)
#ifdef QPREC_FPP
   REAL(KIND=qp),DIMENSION(6) :: vec2Qp = (/10.0, 0.9, 8.2, 4.3, 2.6, 11.1/)
#endif
   REAL(KIND=sp),DIMENSION(3,3) :: matSp = RESHAPE((/0.5, 2., 3., 2., 5.4, 6., 3., 6., 3.3/),SHAPE(matSp))
   REAL(KIND=dp),DIMENSION(3,3) :: matDp = RESHAPE((/0.5, 2., 3., 2., 5.4, 6., 3., 6., 3.3/),SHAPE(matDp))
#ifdef QPREC_FPP
   REAL(KIND=qp),DIMENSION(3,3) :: matQp = RESHAPE((/0.5, 2., 3., 2., 5.4, 6., 3., 6., 3.3/),SHAPE(matQp))
#endif
   REAL(KIND=sp) :: zero_sp = 0.0
   REAL(KIND=dp) :: zero_dp = 0.0
#ifdef QPREC_FPP
   REAL(KIND=qp) :: zero_qp = 0.0
#endif
   INTEGER :: u


   CALL sleep(1) !For later testing timing functions

   !
   ! FU_Strings tests
   !

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
   CALL test('splitstr',splitstr('campo1     campo2     campo3    campo4',&
      fieldNumber=3_i32, rev = .TRUE.,mergedelim=.TRUE.) == 'campo2')
   CALL test('splitstr',splitstr('campo1     campo2     campo3    campo4',&
      fieldNumber=5_i32, rev = .TRUE.,mergedelim=.FALSE.) == 'campo3')
   CALL test('splitstr',splitstr('campo1     campo2     campo3    campo4',&
      fieldNumber=5_i32, rev = .TRUE.) == 'campo3')
   CALL test('mergeChars',mergeChars("asdf ghjj     qwer",' ') == "asdf ghjj qwer")
   CALL test('mergeChars',mergeChars("asdf ghjj qwer",' ') == "asdf ghjj qwer")
   CALL test('mergeChars',mergeChars("asdfghjjqwer",' ') == "asdfghjjqwer")
   CALL test('mergeChars',mergeChars("asdfghjjqwer ",' ') == "asdfghjjqwer ")
   CALL test('mergeChars',mergeChars(" asdfghjjqwer ",' ') == " asdfghjjqwer ")
   CALL test('mergeChars',mergeChars("asdf ghjj     qwer ",' ') == 'asdf ghjj qwer ')
   CALL test('mergeChars',mergeChars("      ",' ') == " ")
   CALL test('mergeChars',mergeChars("",' ') == "")
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
   CALL test('num2str',num2str(-65.889_sp,'(E10.3)') == '-0.659E+02')
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
   CALL test('str2num',str2num('4321',1_i32) == 4321_i32)
   CALL test('str2num',str2num('-4134',1_i64) == -4134_i64)
   CALL test('str2num',eq(str2num('-4134.8786',1._sp) ,-4134.8786_sp))
   CALL test('str2num',eq(str2num('0.55',1._dp) , 0.55_dp))
   CALL test('str2num',eq(str2num('55E10',1._dp), 55E10_dp))
   CALL test('str2num',ALL(str2num(['4321','3333'],1_i32) == [4321_i32,3333_i32]))
   CALL test('str2num',ALL(str2num(['-4134','3333 '],1_i64) == [-4134_i64,3333_i64]))
   CALL test('str2num',ALL(str2num(['-4134',' 3333'],1_i64) == [-4134_i64,3333_i64]))
   CALL test('upper',upper('abcdefghijklmnopqrstuvwxyz') == 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
   CALL test('upper',upper('áéíóúäëïöüàèìòùâêîôûñ') == 'ÁÉÍÓÚÄËÏÖÜÀÈÌÒÙÂÊÎÔÛÑ')
   CALL test('lower',lower('ABCDEFGHIJKLMNOPQRSTUVWXYZ') == 'abcdefghijklmnopqrstuvwxyz')
   CALL test('lower',lower('ÁÉÍÓÚÄËÏÖÜÀÈÌÒÙÂÊÎÔÛÑ') == 'áéíóúäëïöüàèìòùâêîôûñ')
   CALL test('cistrcmp',cistrcmp('Fortran Utilities','fortran uTILITIES'))
   CALL test('cistrcmp',.NOT.cistrcmp('FortranoUtilities','fortran uTILITIES'))

   !
   ! FU_Statistics tests
   !

   CALL test('mean',eq(mean((/4.2_sp/)), 4.2_sp))
   CALL test('mean',eq(mean((/4.2_dp/)), 4.2_dp))
#ifdef QPREC_FPP
   CALL test('mean',eq(mean((/4.2_qp/)), 4.2_qp))
#endif
   CALL test('mean',eq(mean(vecSp), 3.5_sp))
   CALL test('mean',eq(mean(vecDp), 3.5_dp))
#ifdef QPREC_FPP
   CALL test('mean',eq(mean(vecQp), 3.5_qp))
#endif
   CALL test('gmean',eq(gmean((/4.2_sp/)), 4.2_sp))
   CALL test('gmean',eq(gmean((/4.2_dp/)), 4.2_dp))
#ifdef QPREC_FPP
   CALL test('gmean',eq(gmean((/4.2_qp/)), 4.2_qp))
#endif
   CALL test('gmean',eq(gmean(vecSp), 2.99379516_sp))
   CALL test('gmean',eq(gmean(vecDp), 2.9937951655239088_dp))
#ifdef QPREC_FPP
   CALL test('gmean',eq(gmean(vecQp), 2.99379516552390895491016056788943720_qp))
#endif
   CALL test('variance',eq(variance(vecSp), 3.5_sp))
   CALL test('variance',eq(variance(vecDp), 3.5_dp))
#ifdef QPREC_FPP
   CALL test('variance',eq(variance(vecQp), 3.5_qp))
#endif
   CALL test('stdev',eq(stdev(vecSp), 1.87082875_sp))
   CALL test('stdev',eq(stdev(vecDp), 1.8708286933869707_dp))
#ifdef QPREC_FPP
   CALL test('stdev',eq(stdev(vecQp), 1.87082869338697069279187436615827459_qp))
#endif
   CALL test('pvariance',eq(pvariance(vecSp), 2.91666675_sp))
   CALL test('pvariance',eq(pvariance(vecDp), 2.9166666666666665_dp))
#ifdef QPREC_FPP
   CALL test('pvariance',eq(pvariance(vecQp), 2.91666666666666666666666666666666654_qp))
#endif
   CALL test('pstdev',eq(pstdev(vecSp), 1.70782518_sp))
   CALL test('pstdev',eq(pstdev(vecDp), 1.7078251276599330_dp))
#ifdef QPREC_FPP
   CALL test('pstdev',eq(pstdev(vecQp), 1.70782512765993306387017311342017542_qp))
#endif
   CALL test('covariance',eq(covariance(vecSp, vec1Sp), 7.71000004_sp))
   CALL test('covariance',eq(covariance(vecDp, vec1Dp), 7.7100001931190487_dp))
#ifdef QPREC_FPP
   CALL test('covariance',eq(covariance(vecQp, vec1Qp), 7.71000019311904907226562499999999969_qp))
#endif
   CALL test('pcovariance',eq(pcovariance(vecSp, vec1Sp), 6.42499971_sp))
   CALL test('pcovariance',eq(pcovariance(vecDp, vec1Dp), 6.4250001609325409_dp))
#ifdef QPREC_FPP
   CALL test('pcovariance',eq(pcovariance(vecQp, vec1Qp), 6.42500016093254089355468750000000000_qp))
#endif
   CALL test('correlation',eq(correlation(vecSp, vec1Sp), 0.987359941_sp))
   CALL test('correlation',eq(correlation(vecDp, vec1Dp), 0.98735995385629849_dp))
#ifdef QPREC_FPP
   CALL test('correlation',eq(correlation(vecQp, vec1Qp), 0.987359953856298526574298348025205074_qp))
#endif
   CALL test('lin_error_propagation',eq(lin_error_propagation(vecSp(1:3),matSp), 149.799988_sp))
   CALL test('lin_error_propagation',eq(lin_error_propagation(vecDp(1:3),matDp), 149.79999995231628_dp))
#ifdef QPREC_FPP
   CALL test('lin_error_propagation',eq(lin_error_propagation(vecQp(1:3),matQp),  &
                                 149.799999952316284179687500000000000_qp))
#endif
   CALL test('median',eq(median(vec2Sp), 6.25000000_sp))
   CALL test('median',eq(median(vec2Dp), 6.2500000000000000_dp))
   CALL test('median',eq(median(vec2Sp(:SIZE(vec2Sp) - 1)), 4.30000019_sp))
   CALL test('median',eq(median(vec2Dp(:SIZE(vec2Sp) - 1)), 4.3000001907348633_dp))
   CALL test('skewness',eq(skewness(vec1Sp), -7.22614899E-02_sp))
   CALL test('skewness',eq(skewness(vec1Dp), -7.2262060797200045E-002_dp))
#ifdef QPREC_FPP
   CALL test('skewness',eq(skewness(vec1Qp), -7.22620607971998014386482122745445625E-0002_qp))
#endif
   CALL test('pskewness',eq(pskewness(vec1Sp), -5.27723245E-02_sp))
   CALL test('pskewness',eq(pskewness(vec1Dp), -5.2772747667248286E-002_dp))
#ifdef QPREC_FPP
   CALL test('pskewness',eq(pskewness(vec1Qp), -5.27727476672481041162418142064811042E-0002_qp))
#endif
   BLOCK
      REAL(KIND=sp) :: a, b, R2
      CALL linreg(vec1Sp,vec2Sp,a,b,R2)
      CALL test('linreg', eq(a,1.91524588E-02_sp) .AND. &
         eq(b,6.06490707_sp) .AND. eq(R2,3.66816646E-04_sp))
      CALL logreg(vec1Sp,vec2Sp,a,b,R2)
      CALL test('logreg', eq(a,-0.613968790_sp) .AND. &
         eq(b,7.11681414_sp) .AND. eq(R2,2.03586817E-02_sp))
      CALL expreg(vec1Sp,vec2Sp,a,b,R2)
      CALL test('expreg', eq(a, 3.06743123E-02_sp) .AND. &
         eq(b,3.78382611_sp) .AND. eq(R2,1.74217839E-02_sp))
      CALL potreg(vec1Sp,vec2Sp,a,b,R2)
      CALL test('potreg', eq(a, -1.72789115E-02_sp) .AND. &
         eq(b, 4.69583559_sp) .AND. eq(R2, 2.98560743E-04_sp))
   END BLOCK
   BLOCK
      REAL(KIND=dp) :: a, b, R2
      CALL linreg(vec1Dp,vec2Dp,a,b,R2)
      CALL test('linreg', eq(a, 1.9152457421550508E-002_dp) .AND. &
         eq(b, 6.0649073478163302_dp) .AND. eq(R2, 3.6681662528430503E-004_dp))
      CALL logreg(vec1Dp,vec2Dp,a,b,R2)
      CALL test('logreg', eq(a, -0.61396915025911369_dp) .AND. &
         eq(b, 7.1168151736358585_dp) .AND. eq(R2, 2.0358702706984266E-002_dp))
      CALL expreg(vec1Dp,vec2Dp,a,b,R2)
      CALL test('expreg', eq(a, 3.0674307466084936E-002_dp) .AND. &
         eq(b, 3.7838256737292624_dp) .AND. eq(R2, 1.7421780282640000E-002_dp))
      CALL potreg(vec1Dp,vec2Dp,a,b,R2)
      CALL test('potreg', eq(a, -1.7278895501656680E-002_dp) .AND. &
         eq(b, 4.6958351143312553_dp) .AND. eq(R2, 2.9856022975717142E-004_dp))
   END BLOCK
#ifdef QPREC_FPP
   BLOCK
      REAL(KIND=qp) :: a, b, R2
      CALL linreg(vec1Qp,vec2Qp,a,b,R2)
      CALL test('linreg', eq(a, 1.91524574215504391728990839762617023E-0002_qp) .AND. &
         eq(b, 6.06490734781633064419667328817424442_qp) .AND. eq(R2, 3.66816625284302496884903309253573062E-0004_qp))
      CALL logreg(vec1Qp,vec2Qp,a,b,R2)
      CALL test('logreg', eq(a, -0.613969150259113476418879831643327107_qp) .AND. &
         eq(b, 7.11681517363585798700524653428860835_qp) .AND. eq(R2, 2.03587027069842497190441285013442309E-0002_qp))
      CALL expreg(vec1Qp,vec2Qp,a,b,R2)
      CALL test('expreg', eq(a, 3.06743074660849355769293512171716035E-0002_qp) .AND. &
         eq(b, 3.78382567372926279146815783938640435_qp) .AND. eq(R2, 1.74217802826400032248230923507079901E-0002_qp))
      CALL potreg(vec1Qp,vec2Qp,a,b,R2)
      CALL test('potreg', eq(a, -1.72788955016566840961205556717785084E-0002_qp) .AND. &
         eq(b, 4.69583511433125501102239312634619302_qp) .AND. eq(R2, 2.98560229757171592749502675903525136E-0004_qp))
   END BLOCK
#endif

   !
   ! FU_Numbers tests
   !

   CALL test('count_digits_integer',count_digits_integer(824) == 3)
   CALL test('count_digits_integer',count_digits_integer(-4421) == 5)
   CALL test('is_nan',.NOT.is_nan(5._sp))
   CALL test('is_nan',.NOT.is_nan(5._dp))
#ifdef QPREC_FPP
   CALL test('is_nan',.NOT.is_nan(5._qp))
#endif
   CALL test('is_nan',is_nan(zero_sp/zero_sp))
   CALL test('is_nan',is_nan(zero_dp/zero_dp))
#ifdef QPREC_FPP
   CALL test('is_nan',is_nan(zero_qp/zero_qp))
#endif
   CALL test('is_nan',ALL(.NOT.is_nan(vecSp)))
   CALL test('is_nan',ALL(.NOT.is_nan(vecDp)))
#ifdef QPREC_FPP
   CALL test('is_nan',ALL(.NOT.is_nan(vecQp)))
#endif
   !Falta comprobar is_nan a true con un vector de zeros.
   CALL test('is_inf',.NOT.is_inf(5._sp))
   CALL test('is_inf',.NOT.is_inf(5._dp))
#ifdef QPREC_FPP
   CALL test('is_inf',.NOT.is_inf(5._qp))
#endif
   CALL test('is_inf',is_inf(5._sp/zero_sp))
   CALL test('is_inf',is_inf(5._dp/zero_dp))
#ifdef QPREC_FPP
   CALL test('is_inf',is_inf(5._qp/zero_qp))
#endif
   CALL test('is_inf',is_inf(-5._sp/zero_sp))
   CALL test('is_inf',is_inf(-5._dp/zero_dp))
#ifdef QPREC_FPP
   CALL test('is_inf',is_inf(-5._qp/zero_qp))
#endif
   CALL test('is_inf',ALL(.NOT.is_inf(vecSp)))
   CALL test('is_inf',ALL(.NOT.is_inf(vecDp)))
#ifdef QPREC_FPP
   CALL test('is_inf',ALL(.NOT.is_inf(vecQp)))
#endif
   CALL test('is_inf',ALL(is_inf(vecSp/zero_sp)))
   CALL test('is_inf',ALL(is_inf(vecDp/zero_dp)))
#ifdef QPREC_FPP
   CALL test('is_inf',ALL(is_inf(vecQp/zero_qp)))
#endif
   CALL test('eq',eq(1._sp,1._sp))
   CALL test('eq',eq(1._dp,1._dp))
   CALL test('eq',.NOT.eq(1._sp,2._sp))
   CALL test('eq',.NOT.eq(1._dp,2._dp))
   CALL test('eq',eq(1._sp,2._sp,10._sp))
   CALL test('eq',eq(1._dp,2._dp,10._dp))
   CALL test('ne',.NOT.ne(1._sp,1._sp))
   CALL test('ne',.NOT.ne(1._dp,1._dp))
   CALL test('ne',ne(1._sp,2._sp))
   CALL test('ne',ne(1._dp,2._dp))
   CALL test('ne',.NOT.ne(1._sp,2._sp,10._sp))
   CALL test('ne',.NOT.ne(1._dp,2._dp,10._dp))
#ifdef QPREC_FPP
   CALL test('eq',eq(1._qp,1._qp))
   CALL test('eq',.NOT.eq(1._qp,2._qp))
   CALL test('eq',eq(1._qp,2._qp,10._qp))
   CALL test('ne',.NOT.ne(1._qp,1._qp))
   CALL test('ne',ne(1._qp,2._qp))
   CALL test('ne',.NOT.ne(1._qp,2._qp,10._qp))
#endif

   !
   ! FU_Arrays tests
   !

   CALL test('is_ordered', is_ordered(vecSp))
   CALL test('is_ordered', .NOT.is_ordered([1._sp, 4._sp, 9._sp, 1._sp, 8._sp]))
   CALL test('is_ordered', is_ordered([-4._sp, -1._sp, 0._sp, 1._sp, 8._sp]))
   CALL test('is_ordered', is_ordered(vecDp))
   CALL test('is_ordered', .NOT.is_ordered([1._dp, 4._dp, 9._dp, 1._dp, 8._dp]))
   CALL test('is_ordered', is_ordered([-4._dp, -1._dp, 0._dp, 1._dp, 8._dp]))
#ifdef QPREC_FPP
   CALL test('is_ordered', is_ordered(vecQp))
   CALL test('is_ordered', .NOT.is_ordered([1._qp, 4._qp, 9._qp, 1._qp, 8._qp]))
   CALL test('is_ordered', is_ordered([-4._qp, -1._qp, 0._qp, 1._qp, 8._qp]))
#endif

   !
   ! FU_Arrays tests
   !
  CALL test('lin_interp', eq(lin_interp(1._sp, vecSp, vec1Sp), 0.9_sp, 0.01_sp))
  CALL test('lin_interp', eq(lin_interp(6._sp, vecSp, vec1Sp), 11.1_sp, 0.01_sp))
  CALL test('lin_interp', eq(lin_interp(4.4_sp, vecSp, vec1Sp), 8.92_sp, 0.01_sp))
  CALL test('lin_interp', eq(lin_interp(4.4_sp, [1._sp], [3._sp]), 3._sp, 0.01_sp))
  CALL test('lin_interp', is_nan(lin_interp(4.4_sp, vecSp, vec1Sp(1:3))))
  CALL test('lin_interp', is_nan(lin_interp(4.4_sp, vec2Sp, vecSp)))
  CALL test('lin_interp', eq(lin_interp(1._dp, vecDp, vec1Dp), 0.9_dp, 0.01_dp))
  CALL test('lin_interp', eq(lin_interp(6._dp, vecDp, vec1Dp), 11.1_dp, 0.01_dp))
  CALL test('lin_interp', eq(lin_interp(4.4_dp, vecDp, vec1Dp), 8.92_dp, 0.01_dp))
  CALL test('lin_interp', eq(lin_interp(4.4_dp, [1._dp], [3._dp]), 3._dp, 0.01_dp))
  CALL test('lin_interp', is_nan(lin_interp(4.4_dp, vecDp, vec1Dp(1:3))))
  CALL test('lin_interp', is_nan(lin_interp(4.4_dp, vec2Dp, vecDp)))
#ifdef QPREC_FPP
  CALL test('lin_interp', eq(lin_interp(1._qp, vecQp, vec1Qp), 0.9_qp, 0.01_qp))
  CALL test('lin_interp', eq(lin_interp(6._qp, vecQp, vec1Qp), 11.1_qp, 0.01_qp))
  CALL test('lin_interp', eq(lin_interp(4.4_qp, vecQp, vec1Qp), 8.92_qp, 0.01_qp))
  CALL test('lin_interp', eq(lin_interp(4.4_qp, [1._qp], [3._qp]), 3._qp, 0.01_qp))
  CALL test('lin_interp', is_nan(lin_interp(4.4_qp, vecQp, vec1Qp(1:3))))
  CALL test('lin_interp', is_nan(lin_interp(4.4_qp, vec2Qp, vecQp)))
#endif

   !
   ! FU_Files tests (except writeMatrix and readMatrix, see below)
   !

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
#ifdef WIN_CPP
   CALL test('is_path_absolute',is_path_absolute("C:\users\file.dat"))
   CALL test('is_path_absolute',.NOT.is_path_absolute("file.dat"))
   CALL test('is_path_relative',.NOT.is_path_relative("C:\users\file.dat"))
   CALL test('is_path_relative',is_path_relative("file.dat"))
   CALL test('extension',extension("file.dat") == '.dat')
   CALL test('extension',extension("fi.le.dat") == '.dat')
   CALL test('extension',extension("file") == '')
   CALL test('extension',extension("C:\use.rs\file.dat") == '.dat')
   CALL test('replace_extension',replace_extension('file.dat','new') == 'file.new')
   CALL test('replace_extension',replace_extension('fi.le.dat','new') == 'fi.le.new')
   CALL test('replace_extension',replace_extension('file','new') == 'file.new')
   CALL test('replace_extension',replace_extension('C:\use.rs\file.dat"','new') == 'C:\use.rs\file.dat"')
   CALL test('stem',stem("file.dat") == 'file')
   CALL test('stem',stem("fi.le.dat") == 'fi.le')
   CALL test('stem',stem("file") == 'file')
   CALL test('stem',stem(".\.dat") == '.dat')
   CALL test('stem',stem("C:\use.rs\file.dat") == 'file')
   CALL test('filename',filename("file.dat") == 'file.dat')
   CALL test('filename',filename("fi.le.dat") == 'fi.le.dat')
   CALL test('filename',filename("file") == 'file')
   CALL test('filename',filename(".\.dat") == '.dat')
   CALL test('filename',filename("C:\use.rs\file.dat") == 'file.dat')
   CALL test('replace_filename',replace_filename('file.dat','new') == 'new')
   CALL test('replace_filename',replace_filename('C:\use.rs\file.dat','new') == 'C:\use.rs\file.dat')
   CALL test('remove_filename',remove_filename('file.dat') == '')
   CALL test('remove_filename',remove_filename('C:\use.rs\file.dat') == 'C:\use.rs\')
   CALL test('parent_path',parent_path("file.dat") == '.')
   CALL test('parent_path',parent_path("C:\use.rs\file.dat") == 'C:\use.rs')
   CALL test('parent_path',parent_path("C:\use.rs\.") == 'C:\use.rs')
#else
   CALL test('is_path_absolute',is_path_absolute("/home/user/file.dat"))
   CALL test('is_path_absolute',.NOT.is_path_absolute("./file.dat"))
   CALL test('is_path_relative',.NOT.is_path_relative("/home/user/file.dat"))
   CALL test('is_path_relative',is_path_relative("./file.dat"))
   CALL test('extension',extension("./file.dat") == '.dat')
   CALL test('extension',extension("./fi.le.dat") == '.dat')
   CALL test('extension',extension("./file") == '')
   CALL test('extension',extension("/ho.me/user/file.dat") == '.dat')
   CALL test('replace_extension',replace_extension('file.dat','new') == 'file.new')
   CALL test('replace_extension',replace_extension('./file.dat','new') == './file.new')
   CALL test('replace_extension',replace_extension('fi.le.dat','new') == 'fi.le.new')
   CALL test('replace_extension',replace_extension('file','new') == 'file.new')
   CALL test('replace_extension',replace_extension('/ho.me/user/file.dat','new') == '/ho.me/user/file.new')
   CALL test('replace_extension',replace_extension('','new') == '.new')
   CALL test('stem',stem("./file.dat") == 'file')
   CALL test('stem',stem("./fi.le.dat") == 'fi.le')
   CALL test('stem',stem("./file") == 'file')
   ! The next test fails with gfortran 7.5. It returns an empty string.
   CALL test('stem',stem("./.dat") == '.dat')
   CALL test('stem',stem("/ho.me/user/file.dat") == 'file')
   CALL test('filename',filename("./file.dat") == 'file.dat')
   CALL test('filename',filename("./fi.le.dat") == 'fi.le.dat')
   CALL test('filename',filename("./file") == 'file')
   CALL test('filename',filename("./.dat") == '.dat')
   CALL test('filename',filename("/ho.me/user/file.dat") == 'file.dat')
   CALL test('replace_filename',replace_filename('file.dat','new') == 'new')
   CALL test('replace_filename',replace_filename('./file.dat','new') == './new')
   CALL test('replace_filename',replace_filename('/ho.me/user/file.dat','new') == '/ho.me/user/new')
   CALL test('replace_filename',replace_filename('','new') == 'new')
   CALL test('remove_filename',remove_filename('file.dat') == '')
   CALL test('remove_filename',remove_filename('./file.dat') == './')
   CALL test('remove_filename',remove_filename('/ho.me/user/file.dat') == '/ho.me/user/')
   CALL test('remove_filename',remove_filename('') == '')
   ! The next test fails with gfortran 7.5. It returns an empty string
   CALL test('remove_filename',remove_filename('/') == '/')
   CALL test('remove_filename',remove_filename('/file.dat') == '/')
   CALL test('remove_filename',remove_filename('/folder/file.dat') == '/folder/')
   CALL test('remove_filename',remove_filename('abc/file.dat') == 'abc/')
   CALL test('parent_path',parent_path("./file.dat") == '.')
   CALL test('parent_path',parent_path("/ho.me/user/file.dat") == '/ho.me/user')
   CALL test('parent_path',parent_path("/ho.me/user/.") == '/ho.me/user')
#endif

   !
   ! FU_Timing tests
   !

   CALL sleep(3)
   CALL test('TotalTime',INT(TotalTime()) == 4)
   CALL test('IntervalTime',INT(IntervalTime()) == 4)
   CALL sleep(1)
   CALL test('TotalTime',INT(TotalTime(1._sp)) == 5)
   CALL test('IntervalTime',INT(IntervalTime(1._sp)) == 1)
   CALL ResetTotalTime()
   CALL sleep(1)
   CALL test('TotalTime',INT(TotalTime(1._dp)) == 1)
   CALL test('IntervalTime',INT(IntervalTime(1._dp)) == 1)


   !
   ! FU_Files writeMatrix and readMatrix tests
   !

   BLOCK
      INTEGER(KIND=i8) , DIMENSION(:,:), ALLOCATABLE :: matrix_i8 , matrixR_i8 
      INTEGER(KIND=i16), DIMENSION(:,:), ALLOCATABLE :: matrix_i16, matrixR_i16
      INTEGER(KIND=i32), DIMENSION(:,:), ALLOCATABLE :: matrix_i32, matrixR_i32
      INTEGER(KIND=i64), DIMENSION(:,:), ALLOCATABLE :: matrix_i64, matrixR_i64
      REAL(KIND=sp) , DIMENSION(:,:), ALLOCATABLE :: matrix_sp , matrixR_sp 
      REAL(KIND=dp) , DIMENSION(:,:), ALLOCATABLE :: matrix_dp , matrixR_dp 
#ifdef QPREC_FPP
      REAL(KIND=qp) , DIMENSION(:,:), ALLOCATABLE :: matrix_qp , matrixR_qp 
#endif
      INTEGER :: i, j
      LOGICAL :: l

      ALLOCATE(matrix_i8 (10,10))
      ALLOCATE(matrix_i16(10,10))
      ALLOCATE(matrix_i32(10,10))
      ALLOCATE(matrix_i64(10,10))
      ALLOCATE(matrix_sp (10,10))
      ALLOCATE(matrix_dp (10,10))
#ifdef QPREC_FPP
      ALLOCATE(matrix_qp (10,10))
#endif

      DO i = 1, 10
         DO j = 1, 10
            matrix_i8(i,j)  = INT(i*j,i8)
            matrix_i16(i,j) = INT(i*j,i16)
            matrix_i32(i,j) = INT(i*j,i32)
            matrix_i64(i,j) = INT(i*j,i64)
            matrix_sp(i,j)  = REAL(i*j,sp) / 3._sp
            matrix_dp(i,j)  = REAL(i*j,dp) / 3._dp
#ifdef QPREC_FPP
            matrix_qp(i,j)  = REAL(i*j,qp) / 3._qp
#endif
         END DO
      END DO

      CALL writeMatrix('testi8',matrix_i8)
      CALL readMatrix('testi8',matrixR_i8)
      CALL test('readMatrix - writeMatrix',ALL(matrix_i8 == matrixR_i8))
      CALL writeMatrix('testi16',matrix_i16,'my Header', 'I0')
      CALL readMatrix('testi16',matrixR_i16)
      CALL test('readMatrix - writeMatrix',ALL(matrix_i16 == matrixR_i16))
      CALL writeMatrix('testi32',matrix_i32, formato='I3')
      CALL readMatrix('testi32',matrixR_i32)
      CALL test('readMatrix - writeMatrix',ALL(matrix_i32 == matrixR_i32))
      CALL writeMatrix('testi64',matrix_i64)
      CALL readMatrix('testi64',matrixR_i64)
      CALL test('readMatrix - writeMatrix',ALL(matrix_i64 == matrixR_i64))
      CALL writeMatrix('testsp',matrix_sp)
      CALL readMatrix('testsp',matrixR_sp)
      CALL test('readMatrix - writeMatrix',ALL(matrix_sp == matrixR_sp))
      CALL writeMatrix('testdp',matrix_dp)
      CALL readMatrix('testdp',matrixR_dp)
      CALL test('readMatrix - writeMatrix',ALL(matrix_dp == matrixR_dp))
#ifdef QPREC_FPP
      CALL writeMatrix('testqp',matrix_qp)
      CALL readMatrix('testqp',matrixR_qp)
      CALL test('readMatrix - writeMatrix',ALL(matrix_qp == matrixR_qp))
#endif

      DEALLOCATE(matrix_i8)
      DEALLOCATE(matrixR_i8)
      DEALLOCATE(matrix_i16)
      DEALLOCATE(matrixR_i16)
      DEALLOCATE(matrix_i32)
      DEALLOCATE(matrixR_i32)
      DEALLOCATE(matrix_i64)
      DEALLOCATE(matrixR_i64)
      DEALLOCATE(matrix_sp)
      DEALLOCATE(matrixR_sp)
      DEALLOCATE(matrix_dp)
      DEALLOCATE(matrixR_dp)
#ifdef QPREC_FPP
      DEALLOCATE(matrix_qp)
      DEALLOCATE(matrixR_qp)
#endif
      l = rm('testi8')
      l = rm('testi16')
      l = rm('testi32')
      l = rm('testi64')
      l = rm('testsp')
#ifdef QPREC_FPP
      l = rm('testdp')
      l = rm('testqp')
#endif
   END BLOCK


   CALL test('summary',.TRUE.)

   CONTAINS
      SUBROUTINE test(testname, testRes)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: testName
         LOGICAL         , INTENT(IN) :: testRes
         INTEGER                      :: i = 0
         INTEGER                      :: tests_failed = 0, tests_ok = 0
         IF (testname == 'summary') THEN
            WRITE(*,'(A,2(1X,I0,1X,A))') 'Total:', tests_ok, 'tests OK.', tests_failed, 'tests failed.'
         ELSE
            i = i + 1
            IF (testRes) THEN
               WRITE(*,'(A,1X,I3,1X,A,1X,A)') 'Test' , i, CHAR(27)//'[32mPASS'//CHAR(27)//'[0m', testName
            ELSE
               WRITE(*,'(A,1X,I3,1X,A,1X,A)') 'Test' , i, CHAR(27)//'[31mFAIL'//CHAR(27)//'[0m', testName
            END IF
            IF (testRes) THEN
               tests_ok = tests_ok + 1
            ELSE
               tests_failed = tests_failed + 1
            END IF
         END IF
      END SUBROUTINE test


END PROGRAM FortranUtilitiesTest
