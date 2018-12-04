xc<-read.csv('C:\\Users\\SASI\\Downloads\\house_prediction.csv')
xc<-data.frame(xc$SalePrice,xc$GrLivArea)
colnames(xc)<-c('price','Area')

#DATA:
set.seed(0)
rand = sample(1:nrow(xc),100)
train = xc[-rand, ]
rand1=sample(1:nrow(train),20)
train1=train[rand1,]
test = xc[rand, ]

test_RMSE=c()
colnames(xc)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================

m1 <- lm(price ~ Area, train1)
m1

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

#TRAIN AND TEST ACCURACY
sqrt(sum(m10$residuals^2))
pred = predict(m10, newdata=test)
sqrt(sum((pred-test$price)^2))
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))


complexity=c(1,2,7,8,9,10)
train_RMSE=c(sqrt(sum(m1$residuals^2)),sqrt(sum(m2$residuals^2)),sqrt(sum(m7$residuals^2)),sqrt(sum(m8$residuals^2)),sqrt(sum(m9$residuals^2)),sqrt(sum(m10$residuals^2)))
df<-data.frame(complexity,train_RMSE,test_RMSE)



library(ggplot2)
g <- ggplot(df, aes(complexity))+geom_line(aes(y=train_RMSE,colour="train_error"))+ 
  geom_line(aes(y=test_RMSE,colour="test_error"))+
  labs(title='Train and Test Error Vs Model Complexity',x='complexity',y='Error(RMSE)')+
  scale_colour_manual(name="Type of Error",values=c(test_error="blue", train_error="red"))
g



plot(complexity,train_RMSE,type='line')
