setwd("C:/Users/yyuan11/OneDrive - William & Mary/Documents/DATA440/Lecture Intro to Mathematical Modeling")


#######################################################################
############# Logistic growth model ####################################
################################################################
##### dN/dt=r*N*(1-N/K) ######################################
##### N2-N1=r*N1*(1-N1/K)*(t2-t1), let t2-t1=1
r=.05
K=100

N1=1
Pop=N1
for(tt in 2:200){ # 200 days
  N2=N1+r*N1*(1-N1/K)
  Pop=append(Pop,N2)
  N1=N2
}
plot(Pop)

uscensus=data.frame(Year=seq(1790,1990,by=10),Pop=1:21)
uscensus$Pop=c(3.929,5.308,7.240,9.638,12.866,17.069,23.192,31.443,38.558,
               50.156,62.948,75.996,91.972,105.711,122.775,131.669,150.697,
               179.323,203.185,226.546,248.710)
plot(uscensus$Year,uscensus$Pop)

## Find r,K from US census data
dPop=diff(uscensus$Pop) # first data point is gone, so 20 data points remain
rPop=uscensus[1:20,'Pop']
plot(rPop,dPop/rPop)

dat=data.frame(N=rPop,growth_rate=dPop/rPop)
lm(growth_rate~N,dat)

r=0.33
K=r/0.001243

N1=3.929
Pop=N1
for(tt in 2:21){ 
  N2=N1+r*N1*(1-N1/K)
  Pop=append(Pop,N2)
  N1=N2
}
plot(Pop) # model prediction
lines(uscensus$Pop,col='red') # true population

###########################################################
#### What's you evaluation of this model? ################
### 1.How would lots of data help us?
### 2.Fit well?
### 3.Criticism? 



#### Reference: https://www.maa.org/book/export/html/115630 ############