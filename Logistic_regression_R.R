#Data
#Goal is to classify whether student is admited or rejected(vars=GRE,GPA,RANK)
my_data <- read.csv("binary.csv")
head(my_data)
#admit:0=no;1=yes
str(my_data)
#admit and rank are int var so we need to convert it into factor variable
my_data$admit <- as.factor(my_data$admit)
my_data$rank <- as.factor(my_data$rank)
str(my_data)
#Lets make a two way table of factor variable 
xtabs(~admit + rank,data=my_data)
#Clear view of adimissions with the basis of ranks
#data partition
set.seed(1234)
ind <- sample(2,nrow(my_data),replace = T,prob = c(0.8,0.2))
train <- my_data[ind==1,]
test <- my_data[ind==2,]
#Logistic Regression
my_model <- glm(admit~.,data=my_data,family="binomial")
summary(my_model)
#the p value of gre is not statistically significant so we test with a model without gre
model_2 <- glm(admit~gpa+rank,data=my_data,family="binomial")
summary(model_2)
#take model_2 for final as it is fast and significant
p1 <- predict(model_2,train,type="response")
head(p1)
head(train)
#log((p)/(1-p))=intercept + a*v1 + b*v2 +c*v3 .......
#p is the probability and (p)/(1-p) is the odds
#to calculate probability p =(e^y)/(1+e^y)

#Misclassfication error(train) 
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(predicted=pred1,actual=train$admit)
tab1
#we got 27% misclassification
miss_class_per <- (1-sum(diag(tab1))/sum(tab1))*100
miss_class_per

#Misclassification error on test
p2 <- predict(model_2,test,type="response")
p2 <- ifelse(p2>0.5,1,0)
tab2 <- table(predicted=p2,actual=test$admit)
tab2
miss_class_per <- (1-sum(diag(tab2))/sum(tab2))*100
miss_class_per

#Goodness of fit
total_p <-with(model_2,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))
confidence <- (1-total_p)*100
confidence
