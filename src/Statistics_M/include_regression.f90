         SELECT CASE(typeRegression)
         CASE(linreg_id)
            x_cp = x
            y_cp = y
         CASE(logreg_id)
            x_cp = LOG(x)
            y_cp = y
         CASE(expreg_id)
            x_cp = x
            y_cp = LOG(y)
         CASE(potreg_id)
            x_cp = LOG10(x)
            y_cp = LOG10(y)
         END SELECT

         var_x = variance(x_cp)
         var_y = variance(y_cp)
         covar_xy = covariance(x_cp,y_cp)

         a = covar_xy / var_x

         SELECT CASE(typeRegression)
         CASE(expreg_id)
            b = EXP(mean(y_cp) - a*mean(x_cp))
         CASE(potreg_id)
            b = 10**(mean(y_cp) - a*mean(x_cp))
         CASE DEFAULT
            b = mean(y_cp) - a*mean(x_cp)
         END SELECT

         R2 = covar_xy**2/var_x/var_y

