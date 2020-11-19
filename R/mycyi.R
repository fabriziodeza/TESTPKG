#' Confidence Intervals for sample distributions
#'
#' This function takes in a sample distribution and return a 95 percent confidence interval
#'
#' @param x Sample distribution
#'
#' @return 95 percent confidence interval  for the sample in the parameters
#' @export
#'
#' @examples
#' set.seed(23);
#' x = rnorm(30,mean=10,sd=12);
#' mycyi(x)
#'
#'
mycyi = function(x){
  #Utilized a one sample t.test to calculate a 95% confidence interval
  t.test(x, conf.level=.95)$conf.int
}
