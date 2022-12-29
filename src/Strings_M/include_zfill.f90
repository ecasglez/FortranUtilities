      IF (LEN(str) >= l) THEN
         res = str
      ELSE
         res = REPEAT('0', l - LEN(str)) // str
      END IF
