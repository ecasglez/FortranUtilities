         nrows = SIZE(matrix,1)
         ncols = SIZE(matrix,2)
         OPEN(NEWUNIT=u,FILE=TRIM(filename),STATUS='REPLACE',ACTION='WRITE')
         IF (PRESENT(header)) THEN
            WRITE(u,*) nrows, ncols, .TRUE.
            WRITE(u,'(A)') TRIM(header)
         ELSE
            WRITE(u,*) nrows, ncols, .FALSE.
         END IF
         IF (PRESENT(formato)) THEN
            DO r= 1, nrows
               WRITE(u,"(*("//TRIM(formato)//",1X))") (matrix(r,c),c = 1, ncols)
            END DO
         ELSE
            DO r= 1, nrows
               WRITE(u,*) (matrix(r,c),c = 1, ncols)
            END DO
         END IF
         CLOSE(u)
