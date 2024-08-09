       # generate test data
       t <- seq(from = 1, to = 10, length.out= 100)
       x <- generate_periodic_data(t, period=5.25,amplitude = 7.4,phase=0,noise=0.8)
       unc <- rep(0.8,length(t))
       df <- data.frame(t,x)
       ## Invalid inputs testing
       test_that("The package gives helpful errors for invalid inputs", {
          expect_error(LS(df,unc="x",maxt=10,100), "Incorrect uncertainty")
          expect_error(LS(df,unc=0,maxt=10,100), "Incorrect uncertainty")
       })
