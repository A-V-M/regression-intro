
n_observations <- 500
n_variables <- 2
variable_means = c(0,0)
variable_stds = c(10,20)


train_prop <- 0.8
test_prop <- 0.2

n_train <- ceiling(n_observations * train_prop)
n_test <- n_observations - n_train

x <- matrix(0,n_observations,n_variables)

for (n in 1:n_variables) 
  
{
  
  x[,n] <- rnorm(n_observations,mean=variable_means[n],sd=variable_stds[n])
  
}
weights = runif(n_variables,-5,5)
y = colSums((weights * t(x)))

noise<-matrix(runif(n_observations * n_variables,-20,20),n_observations,n_variables)
x <- x + noise

x_full=data.frame(x1=x[,1],x2=x[,2])

i = sample(seq(0,n_observations),n_train)

y_train = y[i]
x_train = x_full[i,]

y_test = y[-i]
x_test = x_full[-i,]

lmodel.one <- lm(y_train ~ x1,data=x_train)
summary(lmodel.one)
preds1<-predict(lmodel.one,newdata=x_test,interval="predict")

lmodel.two <- lm(y_train ~ x1 + x2,data=x_train)
summary(lmodel.two)
preds2<-predict(lmodel.two,newdata=x_test,interval="predict")
perf = data.frame(R2metric1 = cor(preds1[,1],y_test)^2, R2metric2 = cor(preds2[,1],y_test)^2)

perf

plot(unlist(x_train["x1"]),y_train,cex=.50)
cints = confint(lmodel.one, 'x1', level=0.95)
a = lmodel.one$coefficients[1]
b = lmodel.one$coefficients[2]
abline(a,b,col=2)
abline(a,cints[1],col=4,lty='dashed')
abline(a,cints[2],col=4,lty='dashed')

y_max = round(max(y_test[1:10]))

errbar(y_test[1:10], preds1[1:10,1], preds1[1:10,3], preds1[1:10,2],ylim=c(-y_max,y_max),xlim=c(-y_max,y_max),ylab="Y'",xlab="Y")
abline(a = 0, b = 1, col = 2)
term1 = toString(round(lmodel.one$coefficients[1],digits=2))
term2 = toString(round(lmodel.one$coefficients[2],digits=2))
tlabel=paste("y' = ",term2,"x1"," + ",term1,sep="")
title(tlabel)

errbar(y_test[1:10], preds2[1:10,1], preds2[1:10,3], preds2[1:10,2],ylim=c(-y_max,y_max),xlim=c(-y_max,y_max),ylab="Y'",xlab="Y")
abline(a = 0, b = 1, col = 2)

term1 = toString(round(lmodel.two$coefficients[1],digits=2))
term2 = toString(round(lmodel.two$coefficients[2],digits=2))
term3 = toString(round(lmodel.two$coefficients[3],digits=2))

tlabel=paste("y' = ",term2,"x1"," + ",term3,"x2"," + ", term1,sep="")
title(tlabel)

#what happens when we add a highly correlated variable?

x3 = x_full$x1 + x_full$x2 + runif(500,-0.1,0.1)

x_full3 = data.frame(x1=x[,1],x2=x[,2],x3=x3)
x_train3 = x_full3[i,]
x_test3 = x_full3[-i,]

lmodel.three <- lm(y_train ~ x1 + x2 + x3,data=x_train3)
preds3<-predict(lmodel.three,newdata=x_test3,interval="predict")
cor(preds3[,1],y_test)^2
