      s = SIZE(arr)
      IF (s == 1) THEN
         res = .TRUE.
      ELSE
         res = .NOT. ANY((arr(2:s) - arr(1:s-1)) < 0.)
      END IF
