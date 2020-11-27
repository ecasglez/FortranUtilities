         IF (PRESENT(eps)) THEN
            eps2 = eps
         ELSE
            eps2 = EPSILON(x1)
         END IF
         res = ABS(x1 - x2) < eps2
