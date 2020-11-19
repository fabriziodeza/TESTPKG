#' Binomial Plot Simulation
#'
#' Takes the number of iterations (iter), the sample size (n), probability (p), and a boolean (ret).
#' The boolean, ret, is used to signify whether you would prefer you would like extra information output
#'
#' @param iter, the number of iterations in the function
#' @param n , the sample size
#' @param p , the probability of the sample
#' @param ret , boolean value in regards to data output
#'
#' @return , a simulated binomial experiment
#' @export
#'
#' @examples
#' mybin(iter=5, n=10, p=.7, ret=TRUE)
#'
mybin=function(iter,n, p, ret){
  #generate a matrix in order to hold each sample
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE);

  #creates a vector populated by number of successes in each trial
  succ=c();
  for( i in 1:iter){
    #populate each column with each new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p));
    #calculate the sum from the sample
    succ[i]=sum(sam.mat[,i]);
  };

  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n));

  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes");
  if(ret == TRUE){
    succ.tab/iter
  }
}
