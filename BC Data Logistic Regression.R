setwd("G:/Imarticus/DSP_Online_2/Logistic Reg Practice Datasets")


bc = read.csv('bc_data.csv')

# Understanding the data
head(bc)
str(bc)
summary(bc)

# Checking for any NA values
library(Amelia)
missmap(bc)


                        # Data Cleaning


# Naming data in class variable as 'Malignant' or 'Benign'
bc$class = as.character(bc$class)

bc$class[bc$class == '2'] <- 'Benign'
bc$class[bc$class == '4'] <- 'Malignant'

# Since class is categorical, we convert it to factor

bc$class = as.factor(bc$class)

# Lets remove the variables that wouldn't contribute to the analysis

bc = subset(bc, select = -(id))



                    # Exploratory Data Analysis


myplot = function(x)
    {
        boxplot(bc[,x]~bc$class, horizontal = T,
            xlab = x,
            ylab = "Class")
    }

myplot("clump_thickness")

print("We notice from the plot that cases with clump thickness less than
      4 come under Benign, while clump thickness greater than 5 are Malignant")

myplot("size_uniformity")

print("It appears as though for Benign cases, the size uniformity is almost nill.
      While for Malignant cases, the size uniformity is above 4.")

myplot("shape_uniformity")

print("Similar observation with shape_uniformity as with size_uniformity.
      The tumor size for Benign class is almost nill.")

myplot("marginal_adhesion")

print("Similar as Shape uniformity.")

myplot("epithelial_size")

print("The average epithelial size for Benign is 2,
      wheareas for Malignant cases, it ranges from 3 to 6")

myplot("bare_nucleoli")

myplot("bland_chromatin")

myplot("normal_nucleoli")

myplot("mitoses")


                            # Logistic Regression

# Splitting the data

library(caTools)
set.seed(1234)
sample = sample.split(bc, 0.8)
train = subset(bc, sample==T)
test = subset(bc, sample==F)

# Training the model using logistic regression

log_model = glm(class~., family=binomial(),train)
summary(log_model)

opt_model = step(log_model, direction = 'both')
summary(opt_model)

log_preds = predict(opt_model,test, type = "response")

log_preds = ifelse(log_preds>=0.2,'Malignant','Benign')
log_preds = as.factor(log_preds)

# Creating the confusion matrix to check the accuracy
library(caret)
confusionMatrix(test$class,log_preds)
    



                            # Decision Tree

# Training the model using Decision Tree
library(rpart)    
library(rpart.plot)
dt_model = rpart(class~.,test,method = 'class')

test$dt_preds = predict(dt_model, test, type = 'class')
confusionMatrix(test$class,test$dt_preds)
rpart.plot(dt_model)


                        # Naive Bayes Model
library(e1071)
NB_model = naiveBayes(class~.,data=train)
NB_preds = predict(NB_model, test, type = 'class')
confusionMatrix(test$class, NB_preds)



                    # Linear Discriminatory Analysis
library(MASS)
lda_model = lda(class~.,train)
summary(lda_model)
preds = predict(lda_model, test)
lda_preds = preds$class
confusionMatrix(test$class, lda_preds)
