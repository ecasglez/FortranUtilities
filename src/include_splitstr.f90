IF (PRESENT(delimiter)) THEN
   d = delimiter
ELSE
   d = ' '
END IF

IF (PRESENT(mergedelim)) THEN
   IF (mergedelim) THEN
      res = mergeChars(str,d)
   ELSE
      res = str
   END IF
ELSE
   res = str
END IF

IF (PRESENT(rev)) THEN
   IF (rev) THEN
      res = strReverse(res)
   END IF
END IF

! If the delimiter is in the first positions of the string, remove it
IF (INDEX(res,d) == 1) THEN
   res = res(LEN(d)+1:)
END IF
DO i = 1, fieldNumber
   pos = INDEX(res,d)
   IF (pos == 0) THEN
      IF (i /= fieldNumber) THEN
         res = ''
      END IF
      EXIT
   ELSE IF (i == fieldNumber) THEN
      res = res(:pos-1)
   ELSE
      res = res(pos+LEN(d):)
   END IF
END DO

IF (PRESENT(rev)) THEN
   IF (rev) THEN
      res = strReverse(res)
   END IF
END IF
