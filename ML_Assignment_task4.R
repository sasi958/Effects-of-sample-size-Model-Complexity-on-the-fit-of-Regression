xc<-read.csv('C:\\Users\\SASI\\Downloads\\house_prediction.csv')
xc<-data.frame(xc$SalePrice,xc$GrLivArea)
colnames(xc)<-c('price','Area')

#DATA:
set.seed(0)

rand = sample(1:nrow(xc),100)
test = xc[rand, ]
train = xc[-rand, ]

rand1=sample(1:nrow(train),100)
train1=train[rand1,]
train_new=train[-rand1,]

rand2=sample(1:nrow(train_new),100)
train2=train_new[rand2,]
train_new1=train_new[-rand2,]

rand3=sample(1:nrow(train_new1),100)
train3=train_new1[rand3,]
train_new2=train_new1[-rand3,]

rand4=sample(1:nrow(train_new2),100)
train4=train_new2[rand4,]

diff_samp<-c(train1,train2,train3,train4)

complexity=c(1,2,7,8,9,10)

  
test_RMSE=c()

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================

m1 <- lm(price ~ Area, train1)
m1

#TRAIN AND TEST ACCURACY
pred = predict(m1, newdata=test)
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

m2 <- lm(price ~ Area + I(Area^2), train1)
m2


#TRAIN AND TEST ACCURACY

pred = predict(m2, newdata=test)
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================

m7 <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7), train1)
m7


#TRAIN AND TEST ACCURACY


pred = predict(m7, newdata=test)
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================

m8 <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7)+I(Area^8), train1)
m8


#TRAIN AND TEST ACCURACY

pred = predict(m8, newdata=test)
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================

m9 <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7)+I(Area^8)+I(Area^9), train1)
m9


#TRAIN AND TEST ACCURACY

pred = predict(m9, newdata=test)
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================

m10 <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7)+I(Area^8)+I(Area^9)+I(Area^10), train1)
m10

#TRAIN AND TEST ACCURACY

pred = predict(m10, newdata=test)
test_RMSE=c(test_RMSE,sqrt(sum((pred-test$price)^2)))


png('testVscomplexity1_100.png')
plot(complexity,test_RMSE,type='line',ylab ='Test Error(RMSE)',colour='blue')
title('Test Error Vs Complexity(sample_size=100,sample1)')
dev.off()



