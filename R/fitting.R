# Helper function 1: Generata Data
generate_periodic_data <- function(t,period, amplitude, phase,noise=0)
{
  y <- amplitude*sin( (2*pi*t/period)+phase)
  if (noise != 0)
  {
    y <- y+  stats::rnorm(length(t),mean= 0, sd= sqrt(noise))
  }
  return(y)
}

phase_plotB <- function(t,x, period=5)
{
  phases <- t %% period
  plot_order <- sort(phases,index.return=TRUE)$ix
  plot(phases[plot_order],x[plot_order], xlab="x", ylab="t modulo period")
}

# Tutorial Creating a new package (Chi Square optimizer)
# This function will return the parameters that optimize chisquare with respect to amplitude and phase
min_chi2 <- function(theta,t,x){
  # array for variables to be optimized
  par  <- c(theta[2],theta[3])
  # array for variables to keepfixed
  known.x  <- c(theta[1],theta[4])
  known.t  <- c(t)
  known.y <- c(x)
  # call the optimizer function from the stats package to optimize the user defined chisquare function we will define later
  res <- stats::optim(par=par, fn=chi2, method = c("Nelder-Mead"),known.t=t, known.x=known.x, known.y=known.y)
  return(res$par)
}


# Tutorial Creating a new package (Chi Square)
chi2 <- function(par,known.x,known.t, known.y){
  period <- known.x[1]
  unc <- known.x[2]
  amplitude <- par[1]
  phase <- par[2]
  t <- known.t
  model <- amplitude*sin(2*pi*t/period+phase)
  chi2 <-   sum(((known.y-model)/unc)**2)
  return(chi2)
}


# Tutorial Creating a new package (Lomb-Scargle)
ls_periodogram <- function(t,x, grid, unc){
  # create empty array
  psd <-  rep(0, length(grid))
  # # create chisquare to which we compared all other chisquares
  chi2_0 <- sum((x-mean(x)/unc)**2)
  # loop over all possible periods in grid
  for (i in 1:length(grid))
  {
    # create any array with (period, amplitude, phase and uncertainty)
    theta <- c(grid[i],0,0,unc)
    # obtain the values for the amplitude and phase that optimize the chisquare
    # while we keep the period and the uncertainty fixed.
    par <- min_chi2(theta,t,x)
    # calculate the chisquare using the amplitude and phase we just found
    chi <- chi2(par, c(grid[i],unc),t,x)
    # return the difference between the chisquares
    psd[i] <- 0.5*(chi2_0  - chi)
  }
  return(psd)
}
