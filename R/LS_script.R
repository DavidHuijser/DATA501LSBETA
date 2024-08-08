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

# Helper function 2: Create phase plot
phase_plot <- function(t,x, period=5)
{
  phases <- t %% period
  plot_order <- sort(phases,index.return=TRUE)$ix
  if (any(!is.finite(t))){
    print("not finite t")
  }
  if (any(!is.finite(period))){
    print("not finite t")
  }
  if (any(!is.finite(plot_order))){
    print("not finite order")
  }
  plot(phases[plot_order],x[plot_order], xlab="x", ylab="t modulo period")
}

phase_plot <- function(t,x, period=5)
{
  phases <- t %% period
  plot_order <- sort(phases,index.return=TRUE)$ix
  plot(phases[plot_order],x[plot_order], xlab="x", ylab="t modulo period")
}


# Tutorial Creating a new package (Main Function)
LS <- function(df, unc,maxt=10, N=100) {
  # split data into two parts
  x <- df$x
  t <- df$t
  # create the period-grid
  grid <- seq(from = 1/(max(t)-min(t)), to = maxt, length.out= N)
  # create the psd (array with periods as a function of (the difference in chi2)
  psd <- ls_periodogram (t,x, grid,unc)
  # create periodogram
  plot(grid, psd, xlab="amplitude", ylab="chisquare")
  # find the period associated with the maximum chisquare difference
  max_period <- grid[which.max(psd)]
  # create a phase plot with period estimate associated with the maximum chisquare difference
  #phase_plot(t,x,period=grid[which.max(psd)])
  return(max_period)
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
    par <- min_chi2(theta,t)
    # calculate the chisquare using the amplitude and phase we just found
    chi <- chi2(par, c(grid[i],unc),t)
    # return the difference between the chisquares
    psd[i] <- 0.5*(chi2_0  - chi)
  }
  return(psd)
}

# Tutorial Creating a new package (Chi Square optimizer)
# This function will return the parameters that optimize chisquare with respect to amplitude and phase
min_chi2 <- function(theta,t){
  # array for variables to be optimized
  par  <- c(theta[2],theta[3])
  # array for variables to keepfixed
  known.x  <- c(theta[1],theta[4])
  known.t  <- c(t)
  # call the optimizer function from the stats package to optimize the user defined chisquare function we will define later
  res <- stats::optim(par=par, fn=chi2, method = c("Nelder-Mead"),known.t=t, known.x=known.x)
  return(res$par)
}


# Tutorial Creating a new package (Chi Square)
chi2 <- function(par,known.x,known.t){
  period <- known.x[1]
  unc <- known.x[2]
  amplitude <- par[1]
  phase <- par[2]
  t <- known.t
  model <- amplitude*sin(2*pi*t/period+phase)
  chi2 <-   sum(((x-model)/unc)**2)
  return(chi2)
}

t <- seq(from = 1, to = 10, length.out= 100)
x <- generate_periodic_data(t, period=5.25,amplitude = 7.4,phase=0,noise=0.8)
plot(t,x, xlab="t", ylab="x")


phase_plot(t,x,period=5)


# Tutorial - Running the code
t <- seq(from = 1, to = 10, length.out= 1000)
x <- 2*sin(2*pi*t/5+0.5)+ 1 + rnorm(length(t),0, 0.25)
df <- data.frame(t,x)
period_estimate <- LS(df,unc=0.25,maxt=10,1000)
print(period_estimate)

phase_plot(t,x,period=period_estimate)
