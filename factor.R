## Step 0 - Read in Data

data <- read.csv("food.csv")
data

rownames(data)=data[,1]
data=data[,-1]

## Step 1 , Explore the data
summary(data)

var1=var(data$Bread)
var2=var(data$Hamburger)
var3=var(data$Butter)
var4=var(data$Apples)
var5=var(data$Tomato)



rho=cor(data)
rho


## Step 2 - Compute the eigenvalues and eigenvactors of the correlation matrix
eigenvalues=eigen(rho)$values
eigenvalues
(eigenvalues)>1
m=2  ## can change this to include more factors

eigenvectors=eigen(rho)$vectors

## Step 3 - Compute Estimated Factor Loadings
L=matrix(nrow=5,ncol=m)
for (j in 1:m){
L[,j]=sqrt(eigenvalues[j])*eigenvectors[,j]  
  }

L  # first column is Factor 1, 2nd column is factor 2


## Step 4 - Compute common variance and unique variance

common=rowSums(L^2)
unique=1-common  ## this diagonal of error matrix

common
unique


## Step 5 - Check the model to reproduce correlation

phi=diag(5)*unique

recreate=L%*%t(L)+phi
recreate

rho

## Step 6 - Create Residual Matrix

residual=rho-recreate
residual  ## check to see if off-diagonal elements are "small"

sum(residual[lower.tri(residual)]^2)  ## sum of square of off-diagonal elements

sum(sqrt(residual[lower.tri(residual)]^2))/(10-2)  ## sum of square of off-diagonal elements


sum(eigenvalues[3:5]^2)  ## sum of square of non-used eigenvalues



## Step 7  - Plot pairs of loadings to interpret factor loadings
## if we can't tell, we may need to do a varimax rotation

plot(L[,1],L[,2],col=1:5,xlab="Loading 1",ylab="Loading 2")
text(L[,1],L[,2],names(data))


## Step 8

install.packages('psych')
library(psych)

## should reproduce our results
fit2 <- principal(data, nfactors=2, rotate="none")

fit2

fit <- principal(data, nfactors=2, rotate="varimax")
fit
plot(fit$loadings[,1],fit$loadings[,2],col=1:5)
text(fit$loadings[,1],fit$loadings[,2],names(data))





### ALTERNATIVE PACKAGES  ###



 
# Determine Number of Factors to Extract
install.packages('nFactors')
library(nFactors)
ev <- eigen(cor(data)) # get eigenvalues
ap <- parallel(subject=nrow(data),var=ncol(data),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


