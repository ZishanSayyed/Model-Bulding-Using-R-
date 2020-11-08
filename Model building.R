  #Crideit Card Analysis 
c_card=read.csv("C:/Users/ADMIN/OneDrive/Desktop/Data analytics workshop/Dataset/Credit_Card_Expenses.csv",header = TRUE)
View(c_card)
summary(c_card)
sd(c_card$CC_Expenses)
library(psych )
describe((c_card$CC_Expenses))
hist(c_card$CC_Expenses)
boxplot(c_card$CC_Expenses)


#supervise learing
#creating an modle of bank data whom gona likely to be taking life insurance polyci

#importing the data 
data=read.csv('C:/Users/ADMIN/OneDrive/Desktop/Data analytics workshop/Dataset/bank-data.csv',head=T)
View(data)
head(data)
summary(data)

#as we dont req id data so we remove it from data set (data cleaing)
data=data[,-1]
head(data)

#creating model 
#we divide data into two group test and training data set 

set.seed(0)
samples=sample(2,600,replace = T,prob = c(0.85,0.15))
data1=cbind(data,samples)
head(data1)


train_data=data[samples==1,]
head(train_data)


test_data=data[samples==2,]
head(test_data)

library(rpart)
library(rpart.plot)

model=rpart(pep~.,data = train_data , method = 'class' , control = rpart.control(minsplit = 2))
plotcp(model)
#plot cp will show how many tree can be in model 
#as we see at 13 cp is min therefore decision tree it is good to add 13 branches in decision tree 
model=prune(model,cp=0.013)
rpart.plot(model)


#prediction the model
pred=predict(model,type = "class")

table=table(train_data$pep,pred)
table


#model accuricy 
258+204
462/511
#modle is 90% accurate 
round(prop.table(table)*100 , 2)

#see for test data 
test_pred=predict(model,type="class",newdata = test_data)
test_table=table(test_data$pep,test_pred)
test_table

#test data accuricy 
44+36
80/89
#test modle accuricy is 89.88%


#see the prob also 
pred_prob = predict(model , newdata = test_data , type = 'prob')
output_test=cbind(test_data,test_pred,pred_prob)
View(output_test)

#exporting file 
write.csv(output_test,"C:/Users/ADMIN/OneDrive/Desktop/Data analytics workshop/test_model.csv")
saveRDS(model ,'C:/Users/ADMIN/OneDrive/Desktop/Data analytics workshop/model.RDS')
