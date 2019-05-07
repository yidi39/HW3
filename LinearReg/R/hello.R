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
  cat("beta: ", beta)
  se = sqrt(sum((y-x%*%beta)**2)/(dim(x)[1]-dim(x)[2]))
  cat("se: ", se)
  t = beta/se*sqrt(dim(x)[1])
  cat("t-value: ",t)
  p =2*pt(t, dim(x)[1]-2)
  cat("p-value: ", p)
  return(beta)
}


