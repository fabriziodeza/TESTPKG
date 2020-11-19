#' Normal curve with highlighted area
#'
#' Uses the parameters to generate a normal curve, and then uses the parameter xt to shade the region below the curve from 0 to xt.
#'
#' @param mu , mean
#' @param sigma , standard deviation
#' @param xt , x axis of the graph
#'
#' @return , normally distributed curve with the region between the curve and xt being shaded along with the area of the region.
#' @export
#'
#' @examples
#' myncurve(mu=10, sigma=5, xt =5)
#'
#'
myncurve = function(mu, sigma, xt){

  #Creates a normal curve with the parameters mu and sigma
  curve(dnorm(x, mu , sigma, log = FALSE), xlim = c((mu-3*sigma),(mu+3*sigma)));

  #Calculates the area for the shaded region below the curve
  b2 = round((pnorm(xt, mu, sigma)), 4);

  #Creates the x and y values for the shaded area
  bx2 = seq(-9,xt,length=1000);
  by2 = dnorm(bx2, mu, sigma);

  #Uses the x and y values above to generate the shaded region below the curve
  polygon(c(-9,bx2, xt),c(0,by2,0),col="Red");

  #Adds the label of the area under the shaded region
  text(x = 6, y = .1,paste("Area=",b2));

}

