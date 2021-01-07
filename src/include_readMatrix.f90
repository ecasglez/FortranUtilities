         OPEN(NEWUNIT=u,FILE=TRIM(filename),STATUS='OLD',ACTION='READ')
         READ(u,*) nrows, ncols, header
         ALLOCATE(res(nrows,ncols))
         IF (header) THEN
            READ(u,*)
         END IF
         DO r= 1, nrows
            READ(u,*) (res(r,c),c = 1, ncols)
         END DO
         CLOSE(u)
