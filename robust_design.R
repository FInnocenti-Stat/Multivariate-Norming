#################################################################################################################################
###################### Sample size calculation and optimal design for multivariate regression-based norming. ###################
###                                  	Robustness of the optimal design: Maximin Design                                      ###
#################################################################################################################################
rm(list=ls(all=T))
N<-16*6*5*13

################################# Generate the design matrix for each design under each model ##########################################

########### Optimal designs in Table 2
######## true model = model 1 = E(Y)= b0+b1X1+b2X2
p<-3 # number of parameters in the model
# d1= (1,0), (1,1), (-1,0), (-1,1), with equal weight=1/4
X_d1_M1<-matrix(cbind(rep(1,times=N),rep(c(1,-1),each=2),rep(c(0,1),times=2)),N,3)
X_d1_M1
# d2= (1,0), (0,0), (-1,0), (1,1), (0,1), (-1,1) with equal weight=1/6
X_d2_M1<-matrix(cbind(rep(1,times=N),rep(c(1,0,-1),each=2),rep(c(0,1),times=2)),N,3)
X_d2_M1
# d3= (-1,0), (1,0), (-1,1), (1,1) with equal weight=3/16 and (0,1), (0,0) with equal weight=2/16
X_d3_M1<-matrix(cbind(rep(1,times=N),c(rep(c(1,-1), times=36),rep(0,times=24)),c(rep(c(0,1),each=36),rep(c(0,1),each=12))),N,3)
X_d3_M1

l=0;g=0;d=0;e=0
for(i in 1:N){
  
  ifelse(all(X_d1_M1[i,]==rbind(c(1,1,0))),l<-l+1,l<-l)
  ifelse(all(X_d2_M1[i,]==rbind(c(1,1,0))),g<-g+1,g<-g)
  ifelse(all(X_d3_M1[i,]==rbind(c(1,1,0))),d<-d+1,d<-d)
  ifelse(all(X_d3_M1[i,]==rbind(c(1,0,0))),e<-e+1,e<-e)
  
}
l==N/4 # N times weigth
g==N/6
d==N*(3/16)
e==N*(2/16)



######## true model = model 2 = E(Y)= b0+b1X1+b2X2+b3x1x1
p<-4 # number of parameters in the model
# d1= (1,0), (1,1), (-1,0), (-1,1), with equal weight=1/4
X_d1_M2<-matrix(cbind(rep(1,times=N),rep(c(1,-1),each=2),rep(c(0,1),times=2),rep(c(1,-1),each=2)^2),N,p)
X_d1_M2
# d2= (1,0), (0,0), (-1,0), (1,1), (0,1), (-1,1) with equal weight=1/6
X_d2_M2<-matrix(cbind(rep(1,times=N),rep(c(1,0,-1),each=2),rep(c(0,1),times=2),X_d2_M1[,2]^2),N,p)
X_d2_M2  
# d3= (-1,0), (1,0), (-1,1), (1,1) with equal weight=3/16 and (0,1), (0,0) with equal weight=2/16
X_d3_M2<-matrix(cbind(rep(1,times=N),c(rep(c(1,-1), times=36),rep(0,times=24)),c(rep(c(0,1),each=36),rep(c(0,1),each=12)),
                      c(rep(c(1,-1), times=36),rep(0,times=24))^2),N,p)
X_d3_M2

l=0;g=0;d=0;e=0
for(i in 1:N){
  
  ifelse(all(X_d2_M2[i,]==rbind(c(1,1,0,1))),l<-l+1,l<-l)
  ifelse(all(X_d2_M2[i,]==rbind(c(1,1,1,1))),g<-g+1,g<-g)
  ifelse(all(X_d3_M2[i,]==rbind(c(1,1,0,1))),d<-d+1,d<-d)
  ifelse(all(X_d3_M2[i,]==rbind(c(1,0,0,0))),e<-e+1,e<-e)
  
}

l==N/6  # N times weigth
g==N/6
d==N*(3/16)
e==N*(2/16)


######## true model = model 3 = E(Y)= b0+b1X1+b2X2+b4x1x2
p<-4 # number of parameters in the model
# d1= (1,0), (1,1), (-1,0), (-1,1), with equal weight=1/4
X_d1_M3<-matrix(cbind(rep(1,times=N),rep(c(1,-1),each=2),rep(c(0,1),times=2),rep(c(1,-1),each=2)*rep(c(0,1),times=2)),N,p)
X_d1_M3
# d2= (1,0), (0,0), (-1,0), (1,1), (0,1), (-1,1) with equal weight=1/6
X_d2_M3<-matrix(cbind(rep(1,times=N),rep(c(1,0,-1),each=2),rep(c(0,1),times=2),X_d2_M1[,2]*X_d2_M1[,3]),N,p)
X_d2_M3

# d3= (-1,0), (1,0), (-1,1), (1,1) with equal weight=3/16 and (0,1), (0,0) with equal weight=2/16
X_d3_M3<-matrix(cbind(rep(1,times=N),c(rep(c(1,-1), times=36),rep(0,times=24)),c(rep(c(0,1),each=36),rep(c(0,1),each=12)),
                      c(rep(c(1,-1), times=36),rep(0,times=24))*c(rep(c(0,1),each=36),rep(c(0,1),each=12))),N,p)
X_d3_M3

l=0;g=0;d=0;e=0
for(i in 1:N){
  
  ifelse(all(X_d1_M3[i,1:3]==rbind(c(1,1,0))),l<-l+1,l<-l)
  ifelse(all(X_d2_M3[i,1:3]==rbind(c(1,1,1))),g<-g+1,g<-g)
  ifelse(all(X_d3_M3[i,1:3]==rbind(c(1,1,0))),d<-d+1,d<-d)
  ifelse(all(X_d3_M3[i,1:3]==rbind(c(1,0,0))),e<-e+1,e<-e)
  
}

l==N/4  # N times weigth
g==N/6
d==N*(3/16)
e==N*(2/16)


######## true model = model 4 = E(Y)= b0+b1X1+b2X2+b3x1x1+b4x1x2
p<-5 # number of parameters in the model
# d1= (1,0), (1,1), (-1,0), (-1,1), with equal weight=1/4
X_d1_M4<-matrix(cbind(rep(1,times=N),rep(c(1,-1),each=2),rep(c(0,1),times=2),rep(c(1,-1),each=2)^2,X_d1_M2[,2]*X_d1_M2[,3]),N,p)
X_d1_M4
# d2= (1,0), (0,0), (-1,0), (1,1), (0,1), (-1,1) with equal weight=1/6
X_d2_M4<-matrix(cbind(rep(1,times=N),rep(c(1,0,-1),each=2),rep(c(0,1),times=2),X_d2_M1[,2]^2,X_d2_M2[,2]*X_d2_M2[,3]),N,p)
X_d2_M4

# d3= (-1,0), (1,0), (-1,1), (1,1) with equal weight=3/16 and (0,1), (0,0) with equal weight=2/16
X_d3_M4<-matrix(cbind(rep(1,times=N),c(rep(c(1,-1), times=36),rep(0,times=24)),c(rep(c(0,1),each=36),rep(c(0,1),each=12)),
                      c(rep(c(1,-1), times=36),rep(0,times=24))^2,X_d3_M2[,2]*X_d3_M2[,3]),N,p)
X_d3_M4

l=0;g=0;d=0;e=0
for(i in 1:N){
  
  ifelse(all(X_d2_M4[i,1:3]==rbind(c(1,1,1))),g<-g+1,g<-g)
  ifelse(all(X_d3_M4[i,1:3]==rbind(c(1,1,0))),d<-d+1,d<-d)
  ifelse(all(X_d3_M4[i,1:3]==rbind(c(1,0,0))),e<-e+1,e<-e)
  
}

# N times weigth
g==N/6
d==N*(3/16)
e==N*(2/16)



######## true model = model 5 = E(Y)= b0+b1X1+b2X2+b3x1x1+b4x1x2+b5x1x1x2
p<-6 # number of parameters in the model
# d1= (1,0), (1,1), (-1,0), (-1,1), with equal weight=1/4
X_d1_M5<-matrix(cbind(rep(1,times=N),rep(c(1,-1),each=2),rep(c(0,1),times=2),rep(c(1,-1),each=2)^2,X_d1_M2[,2]*X_d1_M2[,3],X_d1_M2[,4]*X_d1_M2[,3]),N,p)
X_d1_M5
# d2= (1,0), (0,0), (-1,0), (1,1), (0,1), (-1,1) with equal weight=1/6
X_d2_M5<-matrix(cbind(rep(1,times=N),rep(c(1,0,-1),each=2),rep(c(0,1),times=2),X_d2_M1[,2]^2,X_d2_M2[,2]*X_d2_M2[,3],X_d2_M2[,4]*X_d2_M2[,3]),N,p)
X_d2_M5

# d3= (-1,0), (1,0), (-1,1), (1,1) with equal weight=3/16 and (0,1), (0,0) with equal weight=2/16
X_d3_M5<-matrix(cbind(rep(1,times=N),c(rep(c(1,-1), times=36),rep(0,times=24)),c(rep(c(0,1),each=36),rep(c(0,1),each=12)),
                      c(rep(c(1,-1), times=36),rep(0,times=24))^2,X_d3_M2[,2]*X_d3_M2[,3],X_d3_M2[,4]*X_d3_M2[,3]),N,p)
X_d3_M5

l=0;g=0;d=0;e=0
for(i in 1:N){
  
  ifelse(all(X_d2_M5[i,1:3]==rbind(c(1,1,1))),g<-g+1,g<-g)
  ifelse(all(X_d3_M5[i,1:3]==rbind(c(1,1,0))),d<-d+1,d<-d)
  ifelse(all(X_d3_M5[i,1:3]==rbind(c(1,0,0))),e<-e+1,e<-e)
  
}

#l==N/4  # N times weigth
g==N/6
d==N*(3/16)
e==N*(2/16)



########### Equidistant age levels designs (with more than 3 age levels) 
AGE<-seq(from=20,to=80,by=1)
d_tilde<-min(AGE)+(max(AGE)-min(AGE))/2
Age_scaled<-(AGE-d_tilde)/(max(AGE)-d_tilde)
cbind(AGE,Age_scaled)


# 13 age levels (equal weight 1/26 per sex's level)
seq(20,80,by=5)
ThirteenEquiDistpts<-Age_scaled[c(which(AGE==seq(20,80,by=5)[1]),which(AGE==seq(20,80,by=5)[2]),which(AGE==seq(20,80,by=5)[3]),which(AGE==seq(20,80,by=5)[4]),which(AGE==seq(20,80,by=5)[5]),which(AGE==seq(20,80,by=5)[6]),
                                  which(AGE==seq(20,80,by=5)[7]),which(AGE==seq(20,80,by=5)[8]),which(AGE==seq(20,80,by=5)[9]),which(AGE==seq(20,80,by=5)[10]),which(AGE==seq(20,80,by=5)[11]),which(AGE==seq(20,80,by=5)[12]),
                                  which(AGE==seq(20,80,by=5)[13]))]
ThirteenEquiDistpts
X_13equip<-matrix(cbind(rep(1,times=N),rep(ThirteenEquiDistpts,times=(N/13)),rep(c(0,1),each=13)),N,3)
X_13equip

X_d13_M1<-X_13equip # model 1 =true model
X_d13_M1
X_d13_M2<-cbind(X_13equip,X_13equip[,2]^2) # model 2 =true model
X_d13_M2
X_d13_M3<-cbind(X_13equip,X_13equip[,2]*X_13equip[,3]) # model 3 =true model
X_d13_M3
X_d13_M4<-cbind(X_13equip,X_13equip[,2]^2,X_13equip[,2]*X_13equip[,3]) # model 4 =true model
X_d13_M4
X_d13_M5<-cbind(X_13equip,X_13equip[,2]^2,X_13equip[,2]*X_13equip[,3],(X_13equip[,2]^2)*X_13equip[,3]) # model 5 =true model
X_d13_M5

l=0;g=0;d=0;e=0
for(i in 1:N){
  ifelse(all(X_d13_M5[i,1:3]==rbind(c(1,1,0))),l<-l+1,l<-l)
  ifelse(all(X_d13_M1[i,1:3]==rbind(c(1,1,1))),g<-g+1,g<-g)
  ifelse(all(X_d13_M2[i,1:3]==rbind(c(1,1,0))),d<-d+1,d<-d)
  ifelse(all(X_d13_M3[i,1:3]==rbind(c(1,1,0))),e<-e+1,e<-e)
  
}
l==N/26 # 1/26 is the weight
g==N/26
d==N/26
e==N/26


########################################## Compute (X'X)^-1 for each design under each model ###########################################################

# under model (1)
V_d1_M1<-solve(t(X_d1_M1)%*%X_d1_M1)   # design with 2 age levels
V_d2_M1<-solve(t(X_d2_M1)%*%X_d2_M1)   # balanced design with 3 age levels
V_d3_M1<-solve(t(X_d3_M1)%*%X_d3_M1)   # unbalanced design with 3 age levels
V_d7_M1<-solve(t(X_d13_M1)%*%X_d13_M1) # design with 13 age levels

# under model (2)
#V_d1_M2<-solve(t(X_d1_M2)%*%X_d1_M2)  # the effect of Age^2 is not identifiable for a design with only 2 age levels
V_d2_M2<-solve(t(X_d2_M2)%*%X_d2_M2)
V_d3_M2<-solve(t(X_d3_M2)%*%X_d3_M2)
V_d7_M2<-solve(t(X_d13_M2)%*%X_d13_M2)

# under model (3)
V_d1_M3<-solve(t(X_d1_M3)%*%X_d1_M3)
V_d2_M3<-solve(t(X_d2_M3)%*%X_d2_M3)
V_d3_M3<-solve(t(X_d3_M3)%*%X_d3_M3)
V_d7_M3<-solve(t(X_d13_M3)%*%X_d13_M3)

# under model (4)
#V_d1_M4<-solve(t(X_d1_M4)%*%X_d1_M4) # the effect of Age^2 is not identifiable for a design with only 2 age levels
V_d2_M4<-solve(t(X_d2_M4)%*%X_d2_M4)
V_d3_M4<-solve(t(X_d3_M4)%*%X_d3_M4)
V_d7_M4<-solve(t(X_d13_M4)%*%X_d13_M4)

# under model (5)
#V_d1_M5<-solve(t(X_d1_M5)%*%X_d1_M5) # the effect of Age^2 is not identifiable for a design with only 2 age levels
V_d2_M5<-solve(t(X_d2_M5)%*%X_d2_M5)
V_d3_M5<-solve(t(X_d3_M5)%*%X_d3_M5)
V_d7_M5<-solve(t(X_d13_M5)%*%X_d13_M5)


############################################ Maximin Design based on the Efficiency criterion #######################################################################
# Compute the maximum standardized prediction variance over x0
# generate x0
AGE<-seq(from=20,to=80,by=1)
d_tilde<-min(AGE)+(max(AGE)-min(AGE))/2
Age_scaled<-(AGE-d_tilde)/(max(AGE)-d_tilde)
cbind(AGE,Age_scaled)

Sex<-c(0,1)

X_comb<-expand.grid(Age_scaled,Sex)
colnames(X_comb)<-c("Age","Sex")
X_comb

x0<-data.frame(1,X_comb$Age,X_comb$Sex,X_comb$Age^2,X_comb$Age*X_comb$Sex,(X_comb$Age^2)*X_comb$Sex)
colnames(x0)<-c("Int", "Age","Sex", "Age2", "AgeSex", "Age2Sex")
x0

# note: d1 = 2 age levels design, d2 = 3 age levels balanced design, d3 = 3 age levels unbalanced design
# d7 = 13 age levels design

# under model (1)
D_d1_M1<-max(N*diag(data.matrix(x0[,1:3])%*%V_d1_M1%*%t(data.matrix(x0[,1:3]))))
D_d2_M1<-max(N*diag(data.matrix(x0[,1:3])%*%V_d2_M1%*%t(data.matrix(x0[,1:3]))))
D_d3_M1<-max(N*diag(data.matrix(x0[,1:3])%*%V_d3_M1%*%t(data.matrix(x0[,1:3]))))
D_d7_M1<-max(N*diag(data.matrix(x0[,1:3])%*%V_d7_M1%*%t(data.matrix(x0[,1:3]))))

# under model (2): recall that the effect of Age^2 is not identifiable for d1 (i.e. design with 2 age levels)
D_d2_M2<-max(N*diag(data.matrix(x0[,1:4])%*%V_d2_M2%*%t(data.matrix(x0[,1:4]))))
D_d3_M2<-max(N*diag(data.matrix(x0[,1:4])%*%V_d3_M2%*%t(data.matrix(x0[,1:4]))))
D_d7_M2<-max(N*diag(data.matrix(x0[,1:4])%*%V_d7_M2%*%t(data.matrix(x0[,1:4]))))

# under model (3)
D_d1_M3<-max(N*diag(data.matrix(x0[,c(1:3,5)])%*%V_d1_M3%*%t(data.matrix(x0[,c(1:3,5)]))))
D_d2_M3<-max(N*diag(data.matrix(x0[,c(1:3,5)])%*%V_d2_M3%*%t(data.matrix(x0[,c(1:3,5)]))))
D_d3_M3<-max(N*diag(data.matrix(x0[,c(1:3,5)])%*%V_d3_M3%*%t(data.matrix(x0[,c(1:3,5)]))))
D_d7_M3<-max(N*diag(data.matrix(x0[,c(1:3,5)])%*%V_d7_M3%*%t(data.matrix(x0[,c(1:3,5)]))))

# under model (4): recall that the effect of Age^2 is not identifiable for d1 (i.e. design with 2 age levels)
D_d2_M4<-max(N*diag(data.matrix(x0[,-6])%*%V_d2_M4%*%t(data.matrix(x0[,-6]))))
D_d3_M4<-max(N*diag(data.matrix(x0[,-6])%*%V_d3_M4%*%t(data.matrix(x0[,-6]))))
D_d7_M4<-max(N*diag(data.matrix(x0[,-6])%*%V_d7_M4%*%t(data.matrix(x0[,-6]))))

# under model (5): recall that the effect of Age^2 is not identifiable for d1 (i.e. design with 2 age levels)
D_d2_M5<-max(N*diag(data.matrix(x0)%*%V_d2_M5%*%t(data.matrix(x0))))
D_d3_M5<-max(N*diag(data.matrix(x0)%*%V_d3_M5%*%t(data.matrix(x0))))
D_d7_M5<-max(N*diag(data.matrix(x0)%*%V_d7_M5%*%t(data.matrix(x0))))

library(htmlTable)
# Table S.A.1, online supplement A
write.table(
round(rbind(cbind(D_d1_M1,0,D_d1_M3,0,0),
cbind(D_d2_M1,D_d2_M2,D_d2_M3,D_d2_M4,D_d2_M5),
cbind(D_d3_M1,D_d3_M2,D_d3_M3,D_d3_M4,D_d3_M5),
cbind(D_d7_M1,D_d7_M2,D_d7_M3,D_d7_M4,D_d7_M5)),2)
, file = "Maximum_SPV.txt", sep = ",", quote = FALSE, row.names = F)
############################################ Maximin Design based on the RE criterion #######################################################################

# generate x0
AGE<-seq(from=20,to=80,by=1)
d_tilde<-min(AGE)+(max(AGE)-min(AGE))/2
Age_scaled<-(AGE-d_tilde)/(max(AGE)-d_tilde)
cbind(AGE,Age_scaled)

Sex<-c(0,1)

X_comb<-expand.grid(Age_scaled,Sex)
colnames(X_comb)<-c("Age","Sex")
X_comb

x0<-data.frame(1,X_comb$Age,X_comb$Sex,X_comb$Age^2,X_comb$Age*X_comb$Sex,(X_comb$Age^2)*X_comb$Sex)
colnames(x0)<-c("Int", "Age","Sex", "Age2", "AgeSex", "Age2Sex")
x0

### Find the minimum RE  across x0 values --> results in Table S.A.2, online supplement A

# note: d1 = 2 age levels design, d2 = 3 age levels balanced design, d3 = 3 age levels unbalanced design
#  d7 = 13 age levels design

#### RE based on the trace of V(z_hat)
Z1<-seq(from=-3,to=3,by=0.5)
Z2<-cbind(Z1,c(Z1[1:7]+0.5,Z1[-c(1:7)]-0.5),c(Z1[1:7]+2,Z1[-c(1:7)]-2))
Z1
Z2

RE_trace<-function(x0,V_ODE,V_NON_ODE,Z1,Z2,P){ # RE based on the trace 
  
NUM<-(P*(N*diag(data.matrix(x0)%*%V_ODE%*%t(data.matrix(x0)))))+(((Z1^2)+(Z2^2))/2)
DEN<-(P*(N*diag(data.matrix(x0)%*%V_NON_ODE%*%t(data.matrix(x0)))))+(((Z1^2)+(Z2^2))/2)

RE<-sqrt(NUM/DEN)  
  
  return(RE)
}
# steps 1-3: to find the RE maximin design, step 4: to check whether it depends on Z1 and Z2
# step 1): find the minimum RE over x0, for each design under each model, given Z1, Z2  
# step 2): find the smallest minimum RE over x0, for each design across all models, given Z1, Z2  
# step 3): find the design with the highest smallest minimum RE over x0, given Z1, Z2 
# step 4): check that the design in step 3 is the same for all values of given Z1, Z2
RE_d1_mod1<-RE_d2_mod1<-RE_d3_mod1<-RE_d7_mod1<-matrix(0,length(Z1),(dim(Z2)[2]))
RE_d2_mod2<-RE_d3_mod2<-RE_d7_mod2<-matrix(0,length(Z1),(dim(Z2)[2]))
RE_d1_mod3<-RE_d2_mod3<-RE_d3_mod3<-RE_d7_mod3<-matrix(0,length(Z1),(dim(Z2)[2]))
RE_d2_mod4<-RE_d3_mod4<-RE_d7_mod5<-matrix(0,length(Z1),(dim(Z2)[2]))
RE_d2_mod5<-RE_d3_mod5<-RE_d7_mod4<-matrix(0,length(Z1),(dim(Z2)[2]))
Min_RE_d2<-Min_RE_d3<-Min_RE_d7<-matrix(0,length(Z1),(dim(Z2)[2]))
Maximin_RE<-matrix(0,length(Z1),(dim(Z2)[2]))


for(j in 1:(dim(Z2)[2])){
for(i in 1:length(Z1)){
  
####### step 1): find the minimum RE over x0, for each design under each model, given Z1, Z2, and rho  
  
# Model (1)
RE_d1_mod1[i,j]<-min(RE_trace(x0=x0[,1:3],V_ODE=V_d1_M1,V_NON_ODE=V_d1_M1,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d2_mod1[i,j]<-min(RE_trace(x0=x0[,1:3],V_ODE=V_d1_M1,V_NON_ODE=V_d2_M1,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d3_mod1[i,j]<-min(RE_trace(x0=x0[,1:3],V_ODE=V_d1_M1,V_NON_ODE=V_d3_M1,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d7_mod1[i,j]<-min(RE_trace(x0=x0[,1:3],V_ODE=V_d1_M1,V_NON_ODE=V_d7_M1,Z1=Z1[i],Z2=Z2[i,j],P=2))

# Model (2)
RE_d2_mod2[i,j]<-min(RE_trace(x0=x0[,1:4],V_ODE=V_d2_M2,V_NON_ODE=V_d2_M2,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d3_mod2[i,j]<-min(RE_trace(x0=x0[,1:4],V_ODE=V_d2_M2,V_NON_ODE=V_d3_M2,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d7_mod2[i,j]<-min(RE_trace(x0=x0[,1:4],V_ODE=V_d2_M2,V_NON_ODE=V_d7_M2,Z1=Z1[i],Z2=Z2[i,j],P=2))

# Model (3)
RE_d1_mod3[i,j]<-min(RE_trace(x0=x0[,c(1:3,5)],V_ODE=V_d1_M3,V_NON_ODE=V_d1_M3,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d2_mod3[i,j]<-min(RE_trace(x0=x0[,c(1:3,5)],V_ODE=V_d1_M3,V_NON_ODE=V_d2_M3,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d3_mod3[i,j]<-min(RE_trace(x0=x0[,c(1:3,5)],V_ODE=V_d1_M3,V_NON_ODE=V_d3_M3,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d7_mod3[i,j]<-min(RE_trace(x0=x0[,c(1:3,5)],V_ODE=V_d1_M3,V_NON_ODE=V_d7_M3,Z1=Z1[i],Z2=Z2[i,j],P=2))

# Model (4)
RE_d2_mod4[i,j]<-min(RE_trace(x0=x0[,-6],V_ODE=V_d3_M4,V_NON_ODE=V_d2_M4,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d3_mod4[i,j]<-min(RE_trace(x0=x0[,-6],V_ODE=V_d3_M4,V_NON_ODE=V_d3_M4,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d7_mod4[i,j]<-min(RE_trace(x0=x0[,-6],V_ODE=V_d3_M4,V_NON_ODE=V_d7_M4,Z1=Z1[i],Z2=Z2[i,j],P=2))

# Model (5)
RE_d2_mod5[i,j]<-min(RE_trace(x0=x0,V_ODE=V_d2_M5,V_NON_ODE=V_d2_M5,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d3_mod5[i,j]<-min(RE_trace(x0=x0,V_ODE=V_d2_M5,V_NON_ODE=V_d3_M5,Z1=Z1[i],Z2=Z2[i,j],P=2))
RE_d7_mod5[i,j]<-min(RE_trace(x0=x0,V_ODE=V_d2_M5,V_NON_ODE=V_d7_M5,Z1=Z1[i],Z2=Z2[i,j],P=2))

####### step 2): find the smallest minimum RE over x0, for each design across all models, given Z1, Z2, and rho
# for each design, find the minimum RE across models
Min_RE_d2[i,j]<-min(c(RE_d2_mod1[i,j],RE_d2_mod2[i,j],RE_d2_mod3[i,j],RE_d2_mod4[i,j],RE_d2_mod5[i,j]))
Min_RE_d3[i,j]<-min(c(RE_d3_mod1[i,j],RE_d3_mod2[i,j],RE_d3_mod3[i,j],RE_d3_mod4[i,j],RE_d3_mod5[i,j]))
Min_RE_d7[i,j]<-min(c(RE_d7_mod1[i,j],RE_d7_mod2[i,j],RE_d7_mod3[i,j],RE_d7_mod4[i,j],RE_d7_mod5[i,j]))

####### step 3): find the design with the highest smallest minimum RE over x0, given Z1, Z2, and rho 
# find the design with the highest minimum RE
Maximin_RE[i,j]<-max(c(Min_RE_d2[i,j],Min_RE_d3[i,j],Min_RE_d7[i,j]))

}}

####### step 4): check that the design in step 3 is the same for all values of given Z1, Z2, and rho
all(Maximin_RE==Min_RE_d2) # the optimal design for models (2) and (5) is the RE maximin design, because it yields the highest minimum RE
                           # furthermore, the RE maximin design does not depend on Z1, Z2
all(Maximin_RE>Min_RE_d3)
all(Maximin_RE>Min_RE_d7)

# row =  Z1, col = Z2
# Z1=Z2=-2: conjunctive  rule

Table_trace<-matrix(0,4,5)


Table_trace[1,1]<-RE_d1_mod1[which(Z1==-2),1]
Table_trace[2,1]<-RE_d2_mod1[which(Z1==-2),1]
Table_trace[3,1]<-RE_d3_mod1[which(Z1==-2),1]
Table_trace[4,1]<-RE_d7_mod1[which(Z1==-2),1]

Table_trace[2,2]<-RE_d2_mod2[which(Z1==-2),1]
Table_trace[3,2]<-RE_d3_mod2[which(Z1==-2),1]
Table_trace[4,2]<-RE_d7_mod2[which(Z1==-2),1]

Table_trace[1,3]<-RE_d1_mod3[which(Z1==-2),1]
Table_trace[2,3]<-RE_d2_mod3[which(Z1==-2),1]
Table_trace[3,3]<-RE_d3_mod3[which(Z1==-2),1]
Table_trace[4,3]<-RE_d7_mod3[which(Z1==-2),1]

Table_trace[2,4]<-RE_d2_mod4[which(Z1==-2),1]
Table_trace[3,4]<-RE_d3_mod4[which(Z1==-2),1]
Table_trace[4,4]<-RE_d7_mod4[which(Z1==-2),1]

Table_trace[2,5]<-RE_d2_mod5[which(Z1==-2),1]
Table_trace[3,5]<-RE_d3_mod5[which(Z1==-2),1]
Table_trace[4,5]<-RE_d7_mod5[which(Z1==-2),1]

# Table S.A.2 lower half, online supplement A
library(htmlTable)
write.table(round(Table_trace,3), file = "RE_trace_conjunctive.txt", sep = ",", quote = FALSE, row.names = F)

# Z1=-2 and Z2=0: disjunctive rule

Table_trace[1,1]<-RE_d1_mod1[which(Z1==-2),3]
Table_trace[2,1]<-RE_d2_mod1[which(Z1==-2),3]
Table_trace[3,1]<-RE_d3_mod1[which(Z1==-2),3]
Table_trace[4,1]<-RE_d7_mod1[which(Z1==-2),3]

Table_trace[2,2]<-RE_d2_mod2[which(Z1==-2),3]
Table_trace[3,2]<-RE_d3_mod2[which(Z1==-2),3]
Table_trace[4,2]<-RE_d7_mod2[which(Z1==-2),3]

Table_trace[1,3]<-RE_d1_mod3[which(Z1==-2),3]
Table_trace[2,3]<-RE_d2_mod3[which(Z1==-2),3]
Table_trace[3,3]<-RE_d3_mod3[which(Z1==-2),3]
Table_trace[4,3]<-RE_d7_mod3[which(Z1==-2),3]

Table_trace[2,4]<-RE_d2_mod4[which(Z1==-2),3]
Table_trace[3,4]<-RE_d3_mod4[which(Z1==-2),3]
Table_trace[4,4]<-RE_d7_mod4[which(Z1==-2),3]

Table_trace[2,5]<-RE_d2_mod5[which(Z1==-2),3]
Table_trace[3,5]<-RE_d3_mod5[which(Z1==-2),3]
Table_trace[4,5]<-RE_d7_mod5[which(Z1==-2),3]

# Table S.A.2 lower half, online supplement A
write.table(round(Table_trace,3), file = "RE_trace_disjunctive.txt", sep = ",", quote = FALSE, row.names = F)


#### RE based on the V(delta0_hat)

library(chi)
Mahalanobis<-seq(0.15, 3.05, by=0.05) 
round(pchi(q=c(0.15,3.05), df=2, ncp = 0, lower.tail = TRUE, log.p = FALSE),3)*100

RE_delta0<-function(x0,V_ODE,V_NON_ODE,Mahalanobis){ # RE based on delta_0 
  
  NUM<-N*diag(data.matrix(x0)%*%V_ODE%*%t(data.matrix(x0)))+((Mahalanobis^2)/2)
  DEN<-N*diag(data.matrix(x0)%*%V_NON_ODE%*%t(data.matrix(x0)))+((Mahalanobis^2)/2)
  
  RE<-sqrt(NUM/DEN)  
  
  return(RE)
}
# steps 1-3: to find the RE maximin design, step 4: to check whether it depends on delta_0
# step 1): find the minimum RE over x0, for each design under each model, given delta_0  
# step 2): find the smallest minimum RE over x0, for each design across all models, given delta_0  
# step 3): find the design with the highest smallest minimum RE over x0, given delta_0 
# step 4): check that the design in step 3 is the same for all values of given delta_0
RE_d1_mod1<-RE_d2_mod1<-RE_d3_mod1<-RE_d7_mod1<-matrix(0,length(Mahalanobis))
RE_d2_mod2<-RE_d3_mod2<-RE_d7_mod2<-matrix(0,length(Mahalanobis))
RE_d1_mod3<-RE_d2_mod3<-RE_d3_mod3<-RE_d7_mod3<-matrix(0,length(Mahalanobis))
RE_d2_mod4<-RE_d3_mod4<-RE_d7_mod5<-matrix(0,length(Mahalanobis))
RE_d2_mod5<-RE_d3_mod5<-RE_d7_mod4<-matrix(0,length(Mahalanobis))
Min_RE_d2<-Min_RE_d3<-Min_RE_d7<-matrix(0,length(Mahalanobis))
Maximin_RE<-matrix(0,length(Mahalanobis))


for(i in 1:length(Mahalanobis)){
    
####### step 1): find the minimum RE over x0, for each design under each model, given delta_0  
    
# Model (1)
RE_d1_mod1[i]<-min(RE_delta0(x0=x0[,1:3],V_ODE=V_d1_M1,V_NON_ODE=V_d1_M1,Mahalanobis=Mahalanobis[i]))
RE_d2_mod1[i]<-min(RE_delta0(x0=x0[,1:3],V_ODE=V_d1_M1,V_NON_ODE=V_d2_M1,Mahalanobis=Mahalanobis[i]))
RE_d3_mod1[i]<-min(RE_delta0(x0=x0[,1:3],V_ODE=V_d1_M1,V_NON_ODE=V_d3_M1,Mahalanobis=Mahalanobis[i]))
RE_d7_mod1[i]<-min(RE_delta0(x0=x0[,1:3],V_ODE=V_d1_M1,V_NON_ODE=V_d7_M1,Mahalanobis=Mahalanobis[i]))
    
# Model (2)
RE_d2_mod2[i]<-min(RE_delta0(x0=x0[,1:4],V_ODE=V_d2_M2,V_NON_ODE=V_d2_M2,Mahalanobis=Mahalanobis[i]))
RE_d3_mod2[i]<-min(RE_delta0(x0=x0[,1:4],V_ODE=V_d2_M2,V_NON_ODE=V_d3_M2,Mahalanobis=Mahalanobis[i]))
RE_d7_mod2[i]<-min(RE_delta0(x0=x0[,1:4],V_ODE=V_d2_M2,V_NON_ODE=V_d7_M2,Mahalanobis=Mahalanobis[i]))
    
# Model (3)
RE_d1_mod3[i]<-min(RE_delta0(x0=x0[,c(1:3,5)],V_ODE=V_d1_M3,V_NON_ODE=V_d1_M3,Mahalanobis=Mahalanobis[i]))
RE_d2_mod3[i]<-min(RE_delta0(x0=x0[,c(1:3,5)],V_ODE=V_d1_M3,V_NON_ODE=V_d2_M3,Mahalanobis=Mahalanobis[i]))
RE_d3_mod3[i]<-min(RE_delta0(x0=x0[,c(1:3,5)],V_ODE=V_d1_M3,V_NON_ODE=V_d3_M3,Mahalanobis=Mahalanobis[i]))
RE_d7_mod3[i]<-min(RE_delta0(x0=x0[,c(1:3,5)],V_ODE=V_d1_M3,V_NON_ODE=V_d7_M3,Mahalanobis=Mahalanobis[i]))
    
# Model (4)
RE_d2_mod4[i]<-min(RE_delta0(x0=x0[,-6],V_ODE=V_d3_M4,V_NON_ODE=V_d2_M4,Mahalanobis=Mahalanobis[i]))
RE_d3_mod4[i]<-min(RE_delta0(x0=x0[,-6],V_ODE=V_d3_M4,V_NON_ODE=V_d3_M4,Mahalanobis=Mahalanobis[i]))
RE_d7_mod4[i]<-min(RE_delta0(x0=x0[,-6],V_ODE=V_d3_M4,V_NON_ODE=V_d7_M4,Mahalanobis=Mahalanobis[i]))
    
# Model (5)
RE_d2_mod5[i]<-min(RE_delta0(x0=x0,V_ODE=V_d2_M5,V_NON_ODE=V_d2_M5,Mahalanobis=Mahalanobis[i]))
RE_d3_mod5[i]<-min(RE_delta0(x0=x0,V_ODE=V_d2_M5,V_NON_ODE=V_d3_M5,Mahalanobis=Mahalanobis[i]))
RE_d7_mod5[i]<-min(RE_delta0(x0=x0,V_ODE=V_d2_M5,V_NON_ODE=V_d7_M5,Mahalanobis=Mahalanobis[i]))
    
####### step 2): find the smallest minimum RE over x0, for each design across all models, given delta_0
# for each design, find the minimum RE across models
Min_RE_d2[i]<-min(c(RE_d2_mod1[i],RE_d2_mod2[i],RE_d2_mod3[i],RE_d2_mod4[i],RE_d2_mod5[i]))
Min_RE_d3[i]<-min(c(RE_d3_mod1[i],RE_d3_mod2[i],RE_d3_mod3[i],RE_d3_mod4[i],RE_d3_mod5[i]))
Min_RE_d7[i]<-min(c(RE_d7_mod1[i],RE_d7_mod2[i],RE_d7_mod3[i],RE_d7_mod4[i],RE_d7_mod5[i]))
    
####### step 3): find the design with the highest smallest minimum RE over x0, given delta_0
# find the design with the highest minimum RE
Maximin_RE[i]<-max(c(Min_RE_d2[i],Min_RE_d3[i],Min_RE_d7[i]))
    
}

####### step 4): check that the design in step 3 is the same for all values of given delta_0
all(Maximin_RE==Min_RE_d2) # the optimal design for models (2) and (5) is the RE maximin design, because it yields the highest minimum RE
# furthermore, the RE maximin design does not depend on delta_0
all(Maximin_RE>Min_RE_d3)
all(Maximin_RE>Min_RE_d7)

Table_delta0<-matrix(0,4,5)

Table_delta0[1,1]<-RE_d1_mod1[which(Mahalanobis==2.45)]
Table_delta0[2,1]<-RE_d2_mod1[which(Mahalanobis==2.45)]
Table_delta0[3,1]<-RE_d3_mod1[which(Mahalanobis==2.45)]
Table_delta0[4,1]<-RE_d7_mod1[which(Mahalanobis==2.45)]

Table_delta0[2,2]<-RE_d2_mod2[which(Mahalanobis==2.45)]
Table_delta0[3,2]<-RE_d3_mod2[which(Mahalanobis==2.45)]
Table_delta0[4,2]<-RE_d7_mod2[which(Mahalanobis==2.45)]

Table_delta0[1,3]<-RE_d1_mod3[which(Mahalanobis==2.45)]
Table_delta0[2,3]<-RE_d2_mod3[which(Mahalanobis==2.45)]
Table_delta0[3,3]<-RE_d3_mod3[which(Mahalanobis==2.45)]
Table_delta0[4,3]<-RE_d7_mod3[which(Mahalanobis==2.45)]

Table_delta0[2,4]<-RE_d2_mod4[which(Mahalanobis==2.45)]
Table_delta0[3,4]<-RE_d3_mod4[which(Mahalanobis==2.45)]
Table_delta0[4,4]<-RE_d7_mod4[which(Mahalanobis==2.45)]

Table_delta0[2,5]<-RE_d2_mod5[which(Mahalanobis==2.45)]
Table_delta0[3,5]<-RE_d3_mod5[which(Mahalanobis==2.45)]
Table_delta0[4,5]<-RE_d7_mod5[which(Mahalanobis==2.45)]

# Table S.A.2 upper half, online supplement A
library(htmlTable)
write.table(round(Table_delta0,3), file = "RE_delta.txt", sep = ",", quote = FALSE, row.names = F)

