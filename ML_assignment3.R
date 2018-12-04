xc<-read.csv('C:\\Users\\SASI\\Downloads\\house_prediction.csv')
xc<-data.frame(xc$SalePrice,xc$GrLivArea)
colnames(xc)<-c('price','Area')


#DATA:
set.seed(0)
rand = sample(1:nrow(xc),100)
train = xc[-rand, ]
rand1=sample(1:nrow(train),100)
train1=train[rand1,]
test = xc[rand, ]
 
test_RMSE=c()
colnames(xc)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================

m1 <- lm(price ~ Area, train1)
m1

png('order_11.png')
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Area,train1$price, pch=19, cex=0.5,xlab='Area',ylab='Price')
lines(sort(train1$Area), fitted(m1)[order(train1$Area)], col='red', type='l',lwd=2) 
title('Regression line for 1st order complexity(sample_size=100)')
dev.off()

#TRAIN AND TEST ACCURACY
sqrt(sum(m1$residuals^2))
pred = predict(m1, newdata=test)
sqrt(sum((pred-test$price)^2))
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

m2 <- lm(price ~ Area + I(Area^2), train1)
m2

png('order_21.png')
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Area,train1$price, pch=19, cex=0.5,xlab='Area',ylab='Price')
lines(sort(train1$Area), fitted(m2)[order(train1$Area)], col='blue', type='l',lwd=2) 
title('Regression line for 2nd order complexity(sample_size=100)')
dev.off()

#TRAIN AND TEST ACCURACY
sqrt(sum(m2$residuals^2))
pred = predict(m2, newdata=test)
sqrt(sum((pred-test$price)^2))
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================

m7 <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7), train1)
m7

png('order_71.png')
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Area,train1$price, pch=19, cex=0.5,xlab='Area',ylab='Price')
lines(sort(train1$Area), fitted(m7)[order(train1$Area)], col='magenta', type='l',lwd=2) 
title('Regression line for 7th order complexity(sample_size=100)')
dev.off()

#TRAIN AND TEST ACCURACY
sqrt(sum(m7$residuals^2))
pred = predict(m7, newdata=test)
sqrt(sum((pred-test$price)^2))
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================

m8 <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7)+I(Area^8), train1)
m8

png('order_81.png')
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Area,train1$price, pch=19, cex=0.5,xlab='Area',ylab='Price')
lines(sort(train1$Area), fitted(m8)[order(train1$Area)], col='Green', type='l',lwd=2) 
title('Regression line for 8th order complexity(sample_size=100)')
dev.off()

#TRAIN AND TEST ACCURACY
sqrt(sum(m8$residuals^2))
pred = predict(m8, newdata=test)
sqrt(sum((pred-test$price)^2))
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================

m9 <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7)+I(Area^8)+I(Area^9), train1)
m9

png('order_91.png')
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Area,train1$price, pch=19, cex=0.5,xlab='Area',ylab='Price')
lines(sort(train1$Area), fitted(m9)[order(train1$Area)], col='Brown', type='l',lwd=2) 
title('Regression line for 9th order complexity(sample_size=100)')
dev.off()

#TRAIN AND TEST ACCURACY
sqrt(sum(m9$residuals^2))
pred = predict(m9, newdata=test)
sqrt(sum((pred-test$price)^2))
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================

m10 <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7)+I(Area^8)+I(Area^9)+I(Area^10), train1)
m10

png('order_101.png')
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Area,train1$price, pch=19, cex=0.5,xlab='Area',ylab='Price')
lines(sort(train1$Area), fitted(m10)[order(train1$Area)], col='orange', type='l',lwd=2) 
title('Regression line for 10th order complexity(sample_size=100)')
dev.off()

#TRAIN AND TEST ACCURACY
sqrt(sum(m10$residuals^2))
pred = predict(m10, newdata=test)
sqrt(sum((pred-test$price)^2))
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))

plot(train1$Area,train1$price, pch=19, cex=0.5,xlab='Area',ylab='Price')
lines(sort(train1$Area), fitted(m1)[order(train1$Area)], col='red', type='l') 
lines(sort(train1$Area), fitted(m2)[order(train1$Area)], col='blue', type='l')
lines(sort(train1$Area), fitted(m7)[order(train1$Area)], col='magenta', type='l')
lines(sort(train1$Area), fitted(m8)[order(train1$Area)], col='Green', type='l') 
lines(sort(train1$Area), fitted(m9)[order(train1$Area)], col='Brown', type='l') 
lines(sort(train1$Area), fitted(m10)[order(train1$Area)], col='orange', type='l')
legend("topleft",legend=c('degree-1','degree-2','degree-7','degree-8','degree-9','degree-10')
       ,col=c("red","blue","magenta","Green","Brown","orange"),lty=5,title = 'Different degrees',cex=0.7,text.font = 2)
title('Plots for Different Complexity(sample_size=100)')

