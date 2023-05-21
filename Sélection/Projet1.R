
#title: "heart_attack"
#author: "Oumar Kane"
#date: "15/03/2022"
#target: pdf_document

#https://towardsdatascience.com/implementing-binary-logistic-regression-in-r-7d802a9d98fe

# Variables selection with heart attack dataset

## We have recuperated a dataset named heart_attack which is very interesting for a debut


## Let's import #  needed libraries fir# r

library(MASS)
library(randomForest)
library(VSURF)
library(corrplot)
library(VGAM)

# #  glm function will be use because we have binary categorical target


## We can recuperate #  dataset by indicated #  path


heart_data = read.csv("C:/Users/USER/Documents/Master1IA/Analyse de données/archive (1)/heart.csv")



#Let's attach #  dataset to manipulate fastly #  columns

attach(heart_data)



#We have to see #  five first lines of #  dataset and verify #  types

head(heart_data)


##  data is already cleaned. We have some categorical variables but those variables are encoded.
#Let's go deeper to analyzing

## Let's see if #  different variables are correlated toge# r with a pairplot
## and a corrplot

pairs(heart_data)
corrplot(cor(heart_data))

#We didn't very see well #  shapes of #  scatter maded beetween variables if we trace #  plot
#with #  full dataset. But we can do it again with only some variables.

# choose only four first variables
pairs(heart_data[, 1:4])

#We see that with #  categorical variables we did not obtain good distributions. We cannot #interprate #  plots with #  categorical variables but only with #  quantitative variables. 
#Let's recuperate only #  quantitative variables in a new dataframe.

quanti = heart_data[, c(1, 4, 5, 8, 10)]



# Let's trace a new pairplot without categorical variables

pairs(quanti)

# We see that some variables like thalach and age or chol and age are correlated 

# Let's trace #  corrplot to see more clearly #  variables interactions
corrplot(cor(quanti))


## Preprocessing

### Let's change #  type of #  categorical variables to factor


for (i in c(2, 3, 6, 7, 9, 11, 12, 13, 14)){
  heart_data[, i] = as.factor(heart_data[, i])
}



### Let's see we if obtain a good fit in a logistic regression model
# We use a logistic regression model because #  target `target` is a binary categorical variable

summary(glm(target~., data = heart_data, family = binomial))


## We can begin #  selection using #  likelihood ratio

# create a function calculating #  likelihood ratio
LRT_dev = function(mod1, mod2){
  p_value =  1 - pchisq(deviance(mod1) - deviance(mod2), df.residual(mod1) - df.residual(mod2)) 
  print(paste("p_value is : ", p_value))
  print(paste("p_value < 0.05 : ", as.logical(p_value < 0.05)))
}



## Forward selection with likelihood ratio
### age

model0 = glm(target~1, data = heart_data, family = binomial)
model1 = glm(target~age, data = heart_data, family = binomial)

LRT_dev(model0, model1)

#  age variable is relevant, we can maintain it

### sex

model2 = glm(target~age+sex, data = heart_data, family = binomial)

LRT_dev(model1, model2)

#  sex variable is relevant, we can maintain it

### cp

model3 = glm(target~age+sex+cp, data = heart_data, family = binomial)

LRT_dev(model2, model3)

#  cp variable is relevant, we can maintain it

### trestbps

model4 = glm(target~age+sex+cp+trestbps, data = heart_data, family = binomial)

LRT_dev(model3, model4)

#  trestbps variable is relevant, we can maintain it


### chol

model5 = glm(target~age+sex+cp+trestbps+chol, data = heart_data, family = binomial)

LRT_dev(model4, model5)

#  chol variable is relevant, we can maintain it

### fbs

model6 = glm(target~age+sex+cp+trestbps+chol+fbs, data = heart_data, family = binomial)

LRT_dev(model5, model6)

#  fbs variable is not relevant, we can exclude it

### restecg

model7 = glm(target~age+sex+cp+trestbps+chol+restecg, data = heart_data, family = binomial)

LRT_dev(model5, model7)

#  restecg variable is  relevant,we can maintain it

### thalach

model8 = glm(target~age+sex+cp+trestbps+chol+restecg+thalach, data = heart_data, family = binomial)

LRT_dev(model7, model8)

#  thalachh variable is relevant, we can maintain it

### exang

model9 = glm(target~age+sex+cp+trestbps+chol+restecg+thalach+exang, data = heart_data, family = binomial)

LRT_dev(model8, model9)

#  exang variable is relevant, we can maintain it

### oldpeak

model10 = glm(target~age+sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak, data = heart_data, family = binomial)

LRT_dev(model9, model10)

#  oldpeak variable is relevant, we can maintain it

### slope

model11 = glm(target~age+sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope, data = heart_data, family = binomial)

LRT_dev(model10, model11)

#  slope variable is relevant, we can maintain it

### ca


model12 = glm(target~age+sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope+ca, data = heart_data, family = binomial)

LRT_dev(model11, model12)

#  ca variable is relevant, we can maintain it

### thal

model13 = glm(target~age+sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope+ca+thal, data = heart_data, family = binomial)

LRT_dev(model12, model13)

#  thal variable is, we can maintain it

### Conclusion : 
# #  final model contains only #  variables : age, sex,cp,trestbps,chol,restecg,thalach,exang,oldpeak,slope,ca+thal #  not relevant variables is  fbs.


final_model_forw = model13
# summary of #  model
summary(final_model_forw)



## Backward selection with likelihood ratio
### age

model0 = glm(target~., data = heart_data, family = binomial)
model1 = glm(target~.-age, data = heart_data, family = binomial)

LRT_dev(model1, model0)

#  age variable is not relevant, we can exclude it

### sex

model2 = glm(target~.-age-sex, data = heart_data, family = binomial)

LRT_dev(model2, model1)

#  sex variable is relevant, we can maintain it

### cp

model3 = glm(target~.-age-cp, data = heart_data, family = binomial)

LRT_dev(model3, model1)

#  cp variable is relevant, we can maintain it

### trestbps

model4 = glm(target~.-age-trestbps, data = heart_data, family = binomial)

LRT_dev(model4, model1)

#  trestbps variable is relevant, we can maintain it

### chol

model5 = glm(target~.-age-chol, data = heart_data, family = binomial)

LRT_dev(model5, model1)

#  chol variable is  relevant, we can maintain it

### fbs

model6 = glm(target~.-age-fbs, data = heart_data, family = binomial)

LRT_dev(model6, model1)

#  fbs variable is not relevant, we can exclude it

### restecg

model7 = glm(target~.-age-fbs-restecg, data = heart_data, family = binomial)

LRT_dev(model7, model6)

#  restecg variable is not relevant, we can exclude it

### thalach

model8 = glm(target~.-age-fbs-restecg-thalach, data = heart_data, family = binomial)

LRT_dev(model8, model7)

#  thalach variable is  relevant, we can maintain it

### exang

model9 = glm(target~.-age-fbs-restecg-exang, data = heart_data, family = binomial)

LRT_dev(model9, model7)

#  exang variable is  relevant, we can maintain it

### oldpeak

model10 = glm(target~.-age-fbs-restecg-oldpeak, data = heart_data, family = binomial)

LRT_dev(model10, model7)

#  oldpeak variable is relevant, we can maintain it

### slope

model11 = glm(target~.-age-fbs-restecg-slope, data = heart_data, family = binomial)

LRT_dev(model11, model7)

#  slope variable is relevant, we can maintain it

### ca

model12 = glm(target~.-age-fbs-restecg-ca, data = heart_data, family = binomial)

LRT_dev(model12, model7)

#  ca variable is relevant, we can maintain it

### thal

model13 = glm(target~.-age-fbs-restecg-thal, data = heart_data, family = binomial)

LRT_dev(model13, model7)

#  thal variable is relevant, we can maintain it


### conclusion : 

final_model_back = model7
summary(final_model_back)

###2. STEPWISE

logit_1 <- glm(target~., family = binomial,data = heart_data)
summary(logit_1)

logit_2 <- stepAIC(logit_1)
summary(logit_2)
















































































