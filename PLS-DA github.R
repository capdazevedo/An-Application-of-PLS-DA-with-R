#Packages
install.packages("mdatools")
library(mdatools)

#data set
library(readxl)
data = read_excel("C:/Users/claud/Desktop/TESE/dadosplsda.xlsx")
str(data) #This shows that our data is a tibble

# To procede it is needed to tranform the tibble into a data frame
data = as.data.frame(data)
str(data)

# As we have 3 classes in this problem we need to separate them in order to storage 
# in new datasets
dataqa = data[1:16,]
dataqg = data[17:28,]
dataqs = data[29:44,]

#Understand our data 
summary(dataqa)
summary(dataqg)
summary(dataqs)

#Now there's a need to transform a local variable into a categorical one
data$Local = factor(data$Local, levels = c("QA", "QS", "QG"));data$Local

#Check how many observations we have in each class
table(data$Local) 

# Has we are going to randomly select observations from each class
# we should set a seed to be able to reach the same results if needed
set.seed(123)

# Random selection of 12 observations from each class
# 12 is the lowest number of observations in one of the classes
# and we want to have balanced classes, i.e, the same number of obs in each 
# class otherwise we may defined a wrong threeshold
qa_index = sample(which(data$Local == "QA"), 12)
qg_index = sample(which(data$Local == "QG"), 12)
qs_index = sample(which(data$Local == "QS"), 12)

# Now we will split the data set into calibration and validation sets
# in this case we will try 70% for cal and 30% val, again the selection 
# will be random 

# Starting with calibration set
qa_index_cal = sample(qa_index, 8)
qg_index_cal = sample(qg_index, 8)
qs_index_cal = sample(qs_index, 8)

# Combine the indexes of the 24 obs that will belong to the calibration set
calibration = c(qa_index_cal, qg_index_cal, qs_index_cal)

# Determine the obs index of the obs that aren't in the calibration set
qa_index_val = setdiff(qa_index, qa_index_cal)
qg_index_val = setdiff(qg_index, qg_index_cal)
qs_index_val = setdiff(qs_index, qs_index_cal)

# Randomly select for obs from the remain obs that are not in the calibration set
qa_index_val = sample(qa_index_val, 4)
qg_index_val = sample(qg_index_val, 4)
qs_index_val = sample(qs_index_val, 4)

# Combine the indexes of the 12 obs that will belong to the validation set
validation = c(qa_index_val, qg_index_val, qs_index_val);validation

# Create the calibration and validation sets using the selected indexes
cal.ind = data[calibration, ];cal.ind
val.ind = data[validation, ];val.ind

# Separation of the X matrix and Y matrix for the calibration and validation 
# sets
Xc1 = cal.ind[,2:21];Xc1
Xv1 = val.ind[,2:21];Xv1

Yc = cal.ind[,1];Yc
Yv = val.ind[,1];Yv

# Boxplot of the X matrix for calibration and validation sets to provide
# compact visual representation of the dataset's distribution, highlighting 
# the central location, spread, and overall shape
boxplot(Xc1)
boxplot(Xv1)

# Center and scale the X matrix of both datasets in order to preprocess
# data, Y matrix for both datasets are already centered and scaled 
# due to the fact that we have the same number of obs in each class
Xc = prep.autoscale(Xc1, center = TRUE, scale = TRUE)
Xv = prep.autoscale(Xv1, center = TRUE, scale = TRUE)

# Get the rank of X matrix (calibration) because this will be the maximum number of
# components in the model 
matriz_X = as.matrix(Xc)
rankx = qr(matriz_X);rankx #20

# PLS-DA model using leave-one-out cross-validation
# The partitions proposed in the literature are leave-one-out, 7-fold and 10-fold
model = plsda(Xc,Yc,20,center = FALSE, cv=1)
model

# This function will provide the number of selected components
# and for each class the cumulative explained variance for both X and Y matrices
# the values of TP, FP, TN, FN which are the values from the confusion matrix
# and three model performance metrics such as Specificity, sensibility and Accuracy
summary(model)

# By this step different cross-validation folds were tried but the best results 
# were accomplished by using leave-one-out and this makes sense because our data set
# is small. Trainning with smaller groups will provide better results. 
# Also I may state that cumulative explained variance values are not showned to the cv
# models. Distances are calculated within individual local models, making their 
# combination technically possible but not accurate due to each model having its
# own center. Combining them would be incorrect. As variance relies on the sum of 
# squared distances, it's not computed for cross-validation results.

# The values of the coefficient matrix B can be access with this command 
# With this values you can now predict the values of an unknown matrix Y 
# providing a matrix X, because Y = XB
model$coeffs$values 

# With this command you can acess the loadings of the X matrix. From this values
# you can study the relationship between the real variables and components
model$xloadings

model$yloadings

# Last but not least you can also access the values of the matrix T (weights)
# that ensure the required orthogonality from the loadings vectors and also 
# provide information about the relationship between the observations
model$weights


# Model interpretation 
# Using VIP values in the case where two or more components are selected
vipscores(model)
# Or using SELRATIO values in the case where only one component is selected
selratio(model)

# Not so important (because this info is provided in summary(model)) but you 
# can access the confusion matrix with this command
getConfusionMatrix(model$calres)

# Classification plot
plotMisclassified(model, type = "h", show.labels = T)
plotMisclassified(model)

# Predictions for new data using the validation X matrix 
# using this matriz we can compare if the results achived by the model 
# are correct because we have the real results in the validation Y matrix
# if you have another obs from the same interest population you can 
# actually make predictions

res = predict(model, Xv)
res$y #Predicted values
res$y.pred

# Prediction plot
plotPredictions(res)

# If you want to use the validation data sets (which you should) to compare
# how the model will perform 
res = predict(model, Xv, Yv)
summary(res)

