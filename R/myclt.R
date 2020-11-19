#'Histogram of the sum of columns
#'
#'Takes in the number of columns and number of samples. Using these parameters it creates a matrix of random samples. The columns of
#'this matrix is summed and then displayed in a histogram to display the central limit theorm.
#'
#' @param n , number of samples
#' @param iter , number of columns
#'
#' @return , Histograph of the sum of columns of the calculated sample
#' @export
#'
#' @examples
#' myclt(10, 5)
#'
myclt=function(n,iter){

  #Creates n*iter random deviates of the uniform distribution that have a lower limit 0 and upper limit of 5
  y=runif(n*iter,0,5);

  #Creates a matrix with the sample y
  data=matrix(y,nr=n,nc=iter,byrow=TRUE);

  #Applies the sum function to the columns in the data object and inputs data to sm object
  sm=apply(data,2,sum);

  #creates a histogram displaying the sum of columns in the x axis
  hist(sm)
}
