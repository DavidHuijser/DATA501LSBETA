
# Helper function 2: Create phase plot
#' Phase plot
#'
#' This is the description
#'
#' @param t independent
#' @param x input
#' @param period  single value
#'
#' @return None, output is plot
#' @export
#'
#' @examples
#' t <- seq(from = 1, to = 10, length.out= 100)
#' x <- 2*sin(2*pi*t/5+0.5)+ 1 + rnorm(length(t),0, 0.25)
#' phase_plot(t,x, period=5)
#'
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




# Tutorial Creating a new package (Main Function)
#' LS periodogram
#'
#' @param df dataframe
#' @param unc uncertain
#' @param maxt  maximum T
#' @param N number on grid
#'
#' @return period estimate
#' @export
#'
#' @examples
#' t <- seq(from = 1, to = 10, length.out= 1000)
#' x <- 2*sin(2*pi*t/5+0.5)+ 1 + rnorm(length(t),0, 0.25)
#' df <- data.frame(t,x)
#' LS(df,unc=0.25,maxt=10,1000)

LS <- function(df, unc,maxt=10, N=100) {

  if (!(typeof(unc) == "double")) stop(paste("Incorrect uncertainty"))
  if (unc <= 0) stop(paste("Incorrect uncertainty"))

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


