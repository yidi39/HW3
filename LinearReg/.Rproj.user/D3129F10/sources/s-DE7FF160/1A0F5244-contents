# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

LinearReg <- function(x,y) {
  beta = (solve(t(x)%*%x))%*%t(x)%*%y
  cat("beta: ", beta,"\n")
  se = sqrt(sum((y-x%*%beta)**2)/(dim(x)[1]-dim(x)[2]))
  cov = sqrt(se*se*solve(t(x)%*%x))
  cat("se: ", diag(cov),"\n")
  t = beta/diag(cov)
  cat("t-value: ",t,"\n")
  p =2*pt(abs(t), dim(x)[1]-1,lower=FALSE)
  cat("p-value: ", p,"\n")
}
