      zero = 0.

      !Check if x is lower or larger than the limits of known_xs
      !or find the nearest pints. No extrapolation is performed.
      siz = SIZE(known_xs)
      IF (siz /= SIZE(known_ys)) THEN
         y = zero / zero
      ELSE IF (.NOT.is_ordered(known_xs)) THEN
         y = zero / zero
      ELSE IF (siz == 1) THEN
         y = known_ys(1)
      ELSE IF (x <= known_xs(1)) THEN
         y = known_ys(1)
      ELSE IF (x >= known_xs(siz)) THEN
         y = known_ys(siz)
      ELSE
         !Find nearest points
         i_1 = COUNT(x >= known_xs)
         i_2 = i_1 + 1
         CALL linreg(known_xs(i_1:i_2), known_ys(i_1:i_2), a, b, R2)
         y = a * x + b
      END IF
