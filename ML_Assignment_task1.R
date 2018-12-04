xc<-read.csv('C:\\Users\\SASI\\Downloads\\house_prediction.csv')
xc<-data.frame(xc$SalePrice,xc$GrLivArea)
colnames(xc)<-c('price','Area')


sample_size=c(10,20,30,50,75,100,200,500,750,1000,1360)
train_rmse=c()
test_rmse=c()

for (i in sample_size){
  set.seed(0)
  rand = sample(1:nrow(xc),100)
  train = xc[-rand, ]
  rand1=sample(1:nrow(train),i)
  train1=train[rand1,]
  test = xc[rand, ]
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 7
  #=============================================================================================
  
  m <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7), train1)
  m
  
  
  #TRAIN AND TEST ACCURACY
  train_rmse=c(train_rmse,sqrt(sum(m$residuals^2)))
  pred = predict(m, newdata=test)
  test_rmse=c(test_rmse,sqrt(sum((pred-test$price)^2)))
}

png('test_error_order_7.png')
plot(sample_size,test_rmse,type='line',col='blue',ylab= 'Test Error(RMSE)')
title('Test Error Vs Sample size(order 7)',col='blue')
dev.off()

sample_size=c(10,20,30,50,75,100,200,500,750,1000,1360)
train_rmse=c()
test_rmse=c()


for (i in sample_size){
  set.seed(0)
  rand = sample(1:nrow(xc),100)
  train = xc[-rand, ]
  rand1=sample(1:nrow(train),i)
  train1=train[rand1,]
  test = xc[rand, ]
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 10
  #=============================================================================================
  
  m <- lm(price ~ Area + I(Area^2)+I(Area^3)+I(Area^4)+I(Area^5)+I(Area^6)+I(Area^7)+I(Area^8)+I(Area^9)+I(Area^10), train1)
  m
  
  
  #TRAIN AND TEST ACCURACY
  train_rmse=c(train_rmse,sqrt(sum(m$residuals^2)))
  pred = predict(m, newdata=test)
  test_rmse=c(test_rmse,sqrt(sum((pred-test$price)^2)))
}
png('test_error_order_10.png')
plot(sample_size,test_rmse,type='line',col='blue',ylab= 'Test Error(RMSE)')
title('Test Error Vs Sample size(order 10)',col='blue')
dev.off()