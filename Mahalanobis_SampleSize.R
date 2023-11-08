#################################################################################################################################
###################### Sample size calculation and optimal design for multivariate regression-based norming. ###################
###                        	R code to compute the Mahalanobis distance as a function of Z1, Z2, and rho,                     ###
###                            and to compute the required sample size for the Mahalanobis distance                          ###
#################################################################################################################################
rm(list=ls(all=T))
#########################################################################################################################################
################################################ FUNCTIONS: ############################################################################
#########################################################################################################################################

### To compute percentiles of the chi distribution 
install.packages("chi") # to install the package "chi", see https://cran.r-project.org/web/packages/chi/chi.pdf
library(chi)
P<-2 # number of outcomes = degrees of freedom of the chi distribution
PR<-c(0.90,0.95,0.99) # percentile rank score
#Example: compute the 90th, 95th, and 99th percentile of the chi distribution with 2 degrees of freedom
qchi(p=PR, df=P, ncp = 0, lower.tail = TRUE, log.p = FALSE)

####### Function to compute the Mahalanobis distance from the Z-scores and the correlation between them
# Z1 = Z-score for test score 1
# Z2 = Z-score for test score 2
# rho = correlation between Z1 and Z2

Mahala_dist_rho<-function(Z1,Z2,rho){
  delta_0<-sqrt(((Z1^2)+(Z2^2)-2*rho*Z1*Z2)/(1-rho^2))
  return(delta_0)
}

# Examples
round(Mahala_dist_rho(Z1=-2,Z2=-2,rho=0),2) # rho=0
round(Mahala_dist_rho(Z1=-2,Z2=-2,rho=0.6),2) # rho=0.6
round(Mahala_dist_rho(Z1=-2,Z2=-2,rho=-0.6),2) # rho=-0.6


#######  Function to compute the required sample size for the Mahalanobis distance-based approach (i.e. equation 17)
# alpha = Type I error rate
# gamma = Type II error rate
# k = number of predictors in the multivariate regression model
# Mahala_c = cut-off point for decision making (e.g. a percentile of the chi distribution)
# delta_Mahala = effect size = tested person's Mahalanobis distance - Mahala_c

N_star_Mahala<-function(alpha,gamma,k,Mahala_c,delta_Mahala){
  
  N<-((qnorm(1-alpha)*sqrt(k+1+((Mahala_c^2)/2))+qnorm(1-gamma)*sqrt(k+1+(((Mahala_c+delta_Mahala)^2)/2)))/delta_Mahala)^2
  return(N)
}


# Examples:
N_star_Mahala(alpha=0.05,gamma=0.2,k=2,Mahala_c=2.15,delta_Mahala=0.25)
N_star_Mahala(alpha=0.01,gamma=0.2,k=3,Mahala_c=2.45,delta_Mahala=0.35)
N_star_Mahala(alpha=0.05,gamma=0.1,k=5,Mahala_c=2.45,delta_Mahala=0.35)

# To obtain the size of the normative sample such that half the confidence interval width for Mahala_c 
# equals the desired margin of estimation error, that is, equation (18)

# Example 
N_star_Mahala(alpha=0.05/2,gamma=0.5,k=5,Mahala_c=2.45,delta_Mahala=0.15) 
# note that alpha should be halved and power (i.e. 1-gamma) should be 0.5


