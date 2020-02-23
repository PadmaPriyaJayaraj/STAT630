## Read in Data

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


## Step 2 - Define the problem in terms of principal components
sigma=var(data)
sigma

vars=diag(sigma)
percentvars=vars/sum(vars)

ident=diag(5)
ident

## Step 3 - Compute all the eigenvalues and eigenvectors in R

eigenvalues=eigen(sigma)$values
eigenvectors=eigen(sigma)$vectors

eigenvalues
eigenvectors

# define principal componenets
y1=as.matrix(data)%*%(eigenvectors[,1])
y2=as.matrix(data)%*%(eigenvectors[,2])
y3=as.matrix(data)%*%(eigenvectors[,3])
y4=as.matrix(data)%*%(eigenvectors[,4])
y5=as.matrix(data)%*%(eigenvectors[,5])


y=as.matrix(data)%*%eigenvectors



## Step 4 - Check variance estimates of the pcs and all other properties

var1+var2+var3+var4+var5
percentvars

percentvars_pc=eigenvalues/sum(eigenvalues)
percentvars_pc





## C: sum of variances  
var1+var2+var3+var4+var5
var(y1)+var(y2)+var(y3)+var(y4)+var(y5)

## D: Magnitude of eigenvectors are importance of kth variable in the ith PC
eigenvectors

## E:  correlation between Yi and Xk
eigenvectors[,1]
cor(y1,data)
eigenvectors[,1]*sqrt(eigenvalues[1])/sqrt(diag(vars))


# E: are they uncorrelated?
sigma_pc=var(y)
rho_pc=cor(y)
rho_pc

## Step 5 - Nice plots and Interpret PCs

ts.plot(cbind(percentvars,percentvars_pc),col=c("blue","red"),xlab="ith vector",ylab="percent variance")

plot(y1,y2)
text(y1,y2, labels = rownames(data), pos = 4)

#prcomp(data)
#autoplot(prcomp(data))

## Step 6 - regression, use as an input
set.seed(1002)
dv=rowSums(data)+rnorm(24,mean=0,sd=10)
summary(lm(dv~as.matrix(data)))

summary(lm(dv~as.matrix(y)))

## let's pick the best two
cor(dv,data)
summary(lm(dv~data$Hamburger+data$Tomato))
summary(lm(dv~y1+y2))

## Step 7 - standardize variables, see how PC changes.  "each is equally important to diet"
data=scale(data)
data=as.data.frame(data)


### ALTERNATIVE PACKAGES  ###
## princomp()  prcomp() 
install.packages('factoextra')
library(factoextra)
pp=prcomp((data), scale = TRUE)  ## can change here
fviz_eig(pp)

fviz_pca_var(pp,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)    

fviz_pca_biplot(pp, repel = TRUE,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), col.ind = "#696969")


## https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
p=princomp(data)  ## be careful centering and scaling - this ones seems not to scale the data
names(p)
biplot(p)
             
## Biplot - A vector points in the direction which is most like the variable represented by the vector. 
##This is the direction which has the highest squared multiple correlation with the principal 
##components. The length of the vector is proportional to the squared multiple correlation 
## between the fitted values for the variable and the variable itself.

## The plot is showing:

##the score of each case  on the first two principal components
##the loading of each variable  on the first two principal components.
