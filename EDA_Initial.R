#Load Libraries needed for analysis (will need to trim this down)
library(dplyr)
library(Rtools)
library(tidyverse)
library(visdat)
library(GGally)
library(usmap)
library(mice)
library(VIM)
library(plotly)
library(ggpubr)
library(caret)
library(e1071)
library(class)
library(maps)
library(mapproj)
library(stringr)
library(ggplot2)
library(ggthemes)
library(table1)
library(ggcorrplot)
library(mlbench)
library(Boruta)
library(e1071)
library(MASS)
library(naivebayes)
library(mboost)
library(kernlab)
library(randomForest)
library(doParallel)


#Set themes for GGPlot Graphs
theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text()) #the default for fivethirtyeight is to not show axis labels, this removes that default so we can choose to specify and display axis titles
theme_update(plot.title = element_text(hjust = 0.5)) # changing default to center all titles

#Read in all the needed files from my Github Repo
talent_raw = read.csv("https://raw.githubusercontent.com/jherbaugh/CaseStudy2DDS/main/CaseStudy2-data.csv", header = TRUE)
talent_attrition_test = read.csv("https://raw.githubusercontent.com/jherbaugh/CaseStudy2DDS/main/CaseStudy2CompSet%20No%20Attrition.csv", header = TRUE)
talent_salary_test = read.csv("https://raw.githubusercontent.com/jherbaugh/CaseStudy2DDS/main/CaseStudy2CompSet%20No%20Salary.csv", header = TRUE)



####Exploratory Data Analysis #######

#Read in Dimension of the Dataset
dim(talent_raw) #870 observations x 36 columns


#Summary Statistics and Glimpse into DataTypes and first observations
glimpse(talent_raw)
summary(talent_raw)


#Convert character to factors, drop those columns with 0 variance
talent_factor <- talent_raw %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(Attrition, everything(), -Over18, -EmployeeCount, -StandardHours)

#Verify no missing observations
md.pattern(talent_factor)

#Summary Statistics and Glimpse into DataTypes and first observations for Training
glimpse(talent_factor)
summary(talent_factor)

#Summary Statistics and Glimpse into DataTypes and first observations for Test sets
summary(talent_attrition_test)

#About 84% of our training data was among employees who did not quit and 16% was with those who quit.
prop.table(table(talent_factor$Attrition))



talent_continuous <- talent_factor[, c("YearsWithCurrManager", "YearsSinceLastPromotion", "YearsInCurrentRole", "YearsAtCompany", "PerformanceRating",
                                       "TotalWorkingYears", "PercentSalaryHike", "MonthlyIncome", "MonthlyRate", "HourlyRate", "DailyRate", "WorkLifeBalance", "OverTime",
                                       "DistanceFromHome", "Age", "Attrition", "JobSatisfaction", "JobLevel", "RelationshipSatisfaction", "EnvironmentSatisfaction")]


talent_continuous$Attrition <- as.numeric(talent_continuous$Attrition) - 1
talent_continuous$OverTime <- as.numeric(talent_continuous$OverTime) - 1

quit <- talent_continuous %>% filter(talent_continuous$Attrition == 1)
stayed <- talent_continuous %>% filter(talent_continuous$Attrition == 0)
mean_quit <- lapply(quitters, mean)
meann_stayed <- lapply(nonquitters, mean)

mean_quit
meann_stayed


# Comparision Averages between Training data for certain factors
#
# quitters <- talent_factor %>% filter(talent_factor$Attrition == 1)
# nonquitters <- talent_factor %>% filter(talent_factor$Attrition == 0)
# meanquit <- lapply(quitters, mean)
# meannonquit <- lapply(nonquitters, mean)
#
# Age
# - Yes = 33.8
# - No = 37.4
#
# Monthly Income
# - Yes = $4,762
# - No = $6,702
#
# Job Satisfaction
# - Yes = 2.435714
# - No = 2.761644
#
# Years with Current Manager
# - Yes = 2.942857
# - No = 4.369863
#
# Job Level
# - Yes = 1.635714
# - No = 2.116438



correlations <- round(cor(talent_continuous), 1)

# method = "circle"
ggcorrplot(correlations, hc.order = TRUE, type = "lower",
           lab = TRUE)


train$agegroup <- with(train, ifelse(Age < 25, "18-24", ifelse(Age < 34, "25-34", ifelse(Age < 44, "35-44", ifelse(Age < 52, "45 - 52", "Over53")))))
testattrition$agegroup <- with(testattrition, ifelse(Age < 25, "18-24", ifelse(Age < 34, "25-34", ifelse(Age < 44, "35-44", ifelse(Age < 52, "45 - 52", "Over53")))))
testsalary$agegroup <- with(testsalary, ifelse(Age < 25, "18-24", ifelse(Age < 34, "25-34", ifelse(Age < 44, "35-44", ifelse(Age < 52, "45 - 52", "Over53")))))
counts3 <- table(train$agegroup)
barplot(counts3, main = "Age Count")


### Mean Salary for Job Title/ Education and Salary

Meansbyjobtitle <- train %>% group_by(JobRole) %>% dplyr::summarise(MedSalary = median(MonthlyIncome, na.rm=TRUE),Meansatis= mean(JobSatisfaction, na.rm=TRUE))
#Subsetting the job satisfaction data into top/bottoms
meanjs <- Meansbyjobtitle %>% arrange(desc(Meansatis))
#Subsetting the salary data into top/bottoms
medsal <- Meansbyjobtitle %>% arrange(desc(MedSalary))
#Mean Job Satisfaction by Job Role
ggplot(meanjs, aes(x=reorder(JobRole,-Meansatis),y=Meansatis,fill=JobRole)) + geom_bar(stat = "identity") + theme_minimal() + theme(plot.title = element_text(hjust=0.1), axis.text.x = element_text(hjust = 0.9, angle = 65)) + labs(x="Job Role", y="Mean Job Satisfaction", title = "Job Satisfaction by Title")
#Median Income by Job Role
ggplot(medsal, aes(x=reorder(JobRole,-MedSalary),y=MedSalary,fill=JobRole)) + geom_bar(stat = "identity") + theme_minimal() + theme(plot.title = element_text(hjust=0.1), axis.text.x = element_text(hjust = 0.9, angle = 65)) + labs(x="Job Role", y="Median Salary", title = "Median Salary by Title")





Attritionplot <- ggplot(train, aes(x=DailyRate, y=MonthlyIncome), title ='Attrition') + geom_point()
Attritionplot + facet_wrap(~Attrition)
Satisfactionplot <- ggplot(train, aes(x=DailyRate, y=MonthlyIncome), title ='Attrition') + geom_point()
Attritionplot + facet_wrap(~JobSatisfaction)
Attritionsat <- ggplot(train, aes(x=JobSatisfaction, y=MonthlyIncome), title ='Attrition') + geom_point()
Attritionsat + facet_wrap(~Attrition)
ggplot(train, aes(x=JobRole, fill=Attrition)) + geom_bar() + theme(plot.title = element_text(hjust = 0.9), axis.text.x = element_text(hjust=0.9, angle = 65)) + labs(x = "Job Role", y = "Number of Employees", title = "Attrition Job Role")


# distance grouping
train$DistanceGrouping <- with(train, ifelse(DistanceFromHome > 25, "25+ Miles", ifelse(DistanceFromHome > 18, "19 - 25 Miles", ifelse(DistanceFromHome > 10, "11 - 18 Miles", ifelse(DistanceFromHome >5, "6 - 10 Miles", "Less than 6  Miles")))))
testattrition$DistanceGrouping <- with(testattrition, ifelse(DistanceFromHome > 25, "25+ Miles", ifelse(DistanceFromHome > 18, "19 - 25 Miles", ifelse(DistanceFromHome > 10, "11 - 18 Miles", ifelse(DistanceFromHome >5, "6 - 10 Miles", "Less than 6  Miles")))))
testsalary$DistanceGrouping <- with(testsalary, ifelse(DistanceFromHome > 25, "25+ Miles", ifelse(DistanceFromHome > 18, "19 - 25 Miles", ifelse(DistanceFromHome > 10, "11 - 18 Miles", ifelse(DistanceFromHome >5, "6 - 10 Miles", "Less than 6  Miles")))))
ggplot(train, aes(x=reorder(DistanceGrouping, DistanceFromHome), fill=Attrition)) + geom_bar() + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Distance From Home Group", y = "Number of Employees", title = "Attrition and Distance from Home")




# Reviewing our correlation matrix, we can see instances of high correlation int eh following variables, which would be caused to remove from our models with future analysis.
#
# - Performance Rating and Percent Salary Hike
# - Monthly Income and Job Level
# - Age and Total Working years
# - Years at Current Role and Years at Company
# - Years at Company and Years with Current Manager
#
# So with this in mind, we'll drop the following variables:
#   - Percent Salary Hike
# - Job Level
# - Total Working Years
# - Years at Company

##GGPairs Correlation matrix broken down into different themed groups
##Observations based upon the output

#Employee measurements
#People that left were younger
#People that left were less educated
talent_employee <- talent_factor %>% select(Attrition, Age, Education, Gender, MaritalStatus)
ggpairs(talent_employee, aes(colour = Attrition))


#Employer based measurements
#Pople who left had less time in role , at company, job level, enviornment satisfication
talent_company <- talent_factor %>% select(Attrition, EnvironmentSatisfaction, JobInvolvement, YearsInCurrentRole, YearsAtCompany , JobLevel)
ggpairs(talent_company, aes(colour = Attrition))


#EmployeeWorkLifeBalance
#Maybe distance from home
talent_wellbeing <- talent_factor %>% select(Attrition, BusinessTravel, DistanceFromHome, WorkLifeBalance)
ggpairs(talent_wellbeing, aes(colour = Attrition))

#Income Based Grouping
#Not Seeing strong correlation with attrition in relation to salary. Drop OverTime, StockOptionLevel no real info. Add SalaryHike
talent_income <- talent_factor %>% select(Attrition, DailyRate, HourlyRate, MonthlyRate, MonthlyIncome, PercentSalaryHike )
ggpairs(talent_income, aes(colour = Attrition))


# People who left salary left made less over time
ggscatter(
  talent_factor, x = "TotalWorkingYears", y = "MonthlyIncome",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()

# People who left made less to start but slalary increased same
ggscatter(
  talent_factor, x = "YearsAtCompany", y = "MonthlyIncome",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()


# People who left had fewer total working year and years at company
ggscatter(
  talent_factor, x = "TotalWorkingYears", y = "YearsAtCompany",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()

# People who left had fewer years with current manager / current role (to start with)
ggscatter(
  talent_factor, x = "YearsWithCurrManager", y = "TotalWorkingYears",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()


# People who left had fewer years in current role (to start with)
ggscatter(
  talent_factor, x = "YearsInCurrentRole", y = "TotalWorkingYears",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()


# Attrition -> People who left at 1/2 as much time at current company (to start)
ggscatter(
  talent_factor, x = "YearsWithCurrManager", y = "YearsAtCompany",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()


# Attrition -> People who left at 1/2 as much time at current company (to start)
ggscatter(
  talent_factor, x = "YearsInCurrentRole", y = "YearsAtCompany",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()


# Attrition -> People with left had 1/2 the time with current manager since last promotion (same as below, can remove one)
ggscatter(
  talent_factor, x = "YearsSinceLastPromotion", y = "YearsInCurrentRole",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()


# Attrition -> People with left had 1/2 the time with current manager since last promotion
ggscatter(
  talent_factor, x = "YearsSinceLastPromotion", y = "YearsWithCurrManager",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()


# Attrition -> High correlation with same manager and number of years in current role
ggscatter(
  talent_factor, x = "YearsInCurrentRole", y = "YearsWithCurrManager",
  color = "Attrition", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~Attrition) +
  stat_cor(label.y = 5.4) +
  stat_regline_equation()









# Modeling
#This will be the dataset we use to model our Attrition and Salary Prediction
set.seed(755)

### Removed high correlation variables from set
splitPerc = .70


talent_factor_model <-  talent_factor %>% select(everything(), -YearsAtCompany, -TotalWorkingYears, -JobLevel, -PercentSalaryHike )
trainInd <- sample(1:dim(talent_factor_model)[1],round(splitPerc * dim(talent_factor_model)[1]))

talent_factor_model_Train = talent_factor_model[trainInd,]
talent_factor_model_Test = talent_factor_model[-trainInd,]



#Linear Model = need more on this
lm(Attrition ~., data=talent_factor_model_Train)

#Naive Bayes
NB_model = naiveBayes(talent_factor_model_Train[,-1],as.factor(talent_factor_model_Train$Attrition),laplace = 1)
table(predict(NB_model,talent_factor_model_Test[,-1]),as.factor(talent_factor_model_Test$Attrition))
CM = confusionMatrix(table(predict(NB_model,talent_factor_model_Test[,-1]),as.factor(talent_factor_model_Test$Attrition)))

CM


#Random Forest Model
RF_talent_factor_model_train <- train(Attrition ~., talent_factor_model_Train, method = 'rf', trControl = trainControl(method='repeatedcv'), importance = T)

predTrain <- predict(RF_talent_factor_model_train, talent_factor_model_Test)
confusionMatrix(table(talent_factor_model_Test$Attrition, predTrain))
cMatrixRF <- table(predTrain, talent_factor_model_Test$Attrition)
plot(cMatrixRF, col="blue", ylab="Actual", xlab="Predicted", main='Random Forest Confusion Matrix')


### Let's try a Support Vector Machine model

svm_talent_factor_model_train <- train(Attrition ~., talent_factor_model_Train, method = 'svmRadial', trControl = trainControl(method='repeatedcv'), importance=T)
predTrainsvm <- predict(svm_talent_factor_model_train, talent_factor_model_Test)
confusionMatrix(table(talent_factor_model_Test$Attrition, predTrainsvm))
cMatrixsvm <- table(predTrainsvm, talent_factor_model_Test$Attrition)
plot(cMatrixsvm, col="blue", ylab="Actual", xlab="Predicted", main='Support Vector Machine Confusion Matrix')




### Feature Importance for Predictive ability
# ensure results are repeatable
# load the library


# prepare training scheme
trcontrol <- trainControl(method="repeatedcv", number=10, repeats=3)

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# train the model
model <- train(Attrition~., data=talent_factor, method="lvq", preProcess="scale", trControl=trcontrol)
results <- rfe(talent_factor[,2:33], talent_factor[,1], sizes=c(2:33), rfeControl=control)

# estimate variable importance
importance <- varImp(model, scale=FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)

# summarize the results
print(results)

# list the chosen features
predictors(results)

# plot the results
plot(results, type=c("g", "o"))


#Boruta Feature Engineering
boruta.talent_train <- Boruta(Attrition~., data = talent_factor, doTrace = 2)
print(boruta.talent_train)

#take a call on tentative features
boruta.talent <- TentativeRoughFix(boruta.talent_train)
print(boruta.talent)

getSelectedAttributes(boruta.talent_train, withTentative = F)

plot(boruta.talent, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.talent$ImpHistory),function(i)
  boruta.talent$ImpHistory[is.finite(boruta.talent$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.talent$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.talent$ImpHistory), cex.axis = 0.7)



## Conclusion
# From the implications to the research question standpoint, if Frito lay wants to reduce attrition of quality employees, they would want to re-evaluate their Overtime policy, income level, and stock options level to ensure their good employees are happy within those areas. You can also use this as an ongoing predictive model so that if an employee starts to meet the criteria on these variables that are indicative of attrition, employees could be flagged for a raise or increase in stock options level.




# Salary Prediction

## Check for Collinnearity Within our Salary data
library(HH)
salaryfitincome <- lm(MonthlyIncome ~ ., data = talent_factor_model_Train)
summary(salaryfitincome)
vif(salaryfitincome)


talent_factor_model_salary <-  talent_factor %>% dplyr::select(everything(), -Age, -DistanceFromHome, -Department )
trainInd <- sample(1:dim(talent_factor_model_salary)[1],round(splitPerc * dim(talent_factor_model_salary)[1]))

talent_factor_model_salary_Train = talent_factor_model_salary[trainInd,]
talent_factor_model_salary_Test = talent_factor_model_salary[-trainInd,]

##Looking at the interaction rate of all the variables, we can see that the following variables show to have high collinearity per our model, which we can remove. For the variables where we are seeing high collinearity within categorical variables, we will leave in. This is all validated through our significant p-value of <0.0001.

#Below are the variables with high collinearity

# Age/Age Group
# Distance Grouping
# Department





## Regression Procedure

lm(MonthlyIncome ~., data= talent_factor_model_salary_Train)

#Looking at the linear model, we can see that job role coefficients are going to be the biggest predictors of monthly income. followed by business travel and performance rating.

# K Nearest Neighbor

traind <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
knn_fit <- train(MonthlyIncome ~ ., data = talent_factor_model_salary_Train, method = 'knn', trControl = traind, preProcess = c('center', 'scale'), tuneLength = 10)
knn_fit
plot(knn_fit)
predregtrainknn <- predict(knn_fit, talent_factor_model_salary_Test)

#Our Lowest RMSE came with a K nearest neighbors of 7. This prediction model gives us an RMSE of $2,300 on monthly income, which falls below our $3,000 evaluation criteria.


#Random Forest
rf_fit <- train(MonthlyIncome ~ ., data = talent_factor_model_salary_Train, method = 'rf', trControl = traind, preProcess = c('center', 'scale'), tuneLength = 10, importance=T)
rf_fit
plot(rf_fit)
predregtrainrf <- predict(rf_fit, talent_factor_model_salary_Test)

# To compare our KNN procedure, we are going to employ a random forest procedure with our data to predict the Monthly Income of employees.
#
# Like KNN, Random Forest is an ensemble technique capable of performing regression tasks which leverage multiple decision trees, bootstrapped into an aggregate score. Like cross-validation, we are training the training set to validate and make itself smarter.

# For this particular case, we were able to use 19 frees to minimize our RMSE score down to $1951, which is significantly better than our KNN model. Our R2 score is also 0.80 at this level, which is predictive while also avoiding overfitting. In layman's terms, this means it's both predictive to its own data and we can expect it to do the same with new data.

### Let's try with the standard variable method

svm_fit <- train(MonthlyIncome ~ ., data = talent_factor_model_salary_Train, method = 'svmRadial', trControl = traind, preProcess = c('center', 'scale'), tuneLength = 10, importance=T)
svm_fit
plot(svm_fit)
predregtrainsvm <- predict(svm_fit, talent_factor_model_salary_Test)

#In hopes to improve on our random forest score, Support Vector Machines are a subclass of supervised regressors that attempt to regress a feature space into a set of regressive linear predictors.

#In this case, our minimum RMSE score dropped down to $2035, which is higher than our Random forest score. It should be noted that this score is less computationally expensive than Random Forest at 2 C rather than 19 trees. So while that's not a factor with $1000 data points, it will be a concern if we go up to the millions.

#So with the 3 models in question, we will use the random forest predictor as our final model.

### Variable importance of our regressor
rfimportreg <- varImp(rf_fit, scale = FALSE)
#svmimportreg <- varImp(svm_fit, scale = FALSE)
plot(rfimportreg)
#plot(svmimportreg)

# Breaking out the top features that are diving Monthly Income at Frito Lay, we can say that Job Role is the most important predictor, followed by Age and then Years with Current Manager. This would make sense from a prediction standpoint as a Job title is typically a heavier predictor in salary, and the older you are, the more senior you typically are, which affects your Salary. We also have Years with Current Manager closely followed by Years in Current Role, which would make sense as it can tie back to a users job performance and raises once they have plenty of experience working the same job with the same boss.

## Conclusion
#
# From the implications to the research question standpoint, Frito Lay can reduce attrition of quality of employees if they can predict which one will leave and provide them an offer related to Stock Option Value, Overtime, and Monthly Income. This could come in the form of a raise, or a bump in an employee's vested interest and potentially offering overtime to help keep people happy and working for the company.
#
# For monthly Salary, if we need to predict a salary growth for an employee, we can use the linear model and the significant predictions gained through job role, age and their years with a manager. Since monthly income was a factor in attrition, we could potentially use this salary regression model to help show current employees how their salary will likely change over time as they work for a company.
#
# This could be powerful as they can plot out their futures and optimize their growth over time.


## Output data
# #run prediction on rf classification model using our test set
# dftestpreds <- predict(RF_train2, testattrition)
# dftestpreds1 <- cbind(testattrition$ID, dftestpreds)
# dfclasspred <- as.data.frame(dftestpreds1)
# dfclasspred$Attrition <- with(dfclasspred, ifelse(dftestpreds == 1, "No", "Yes"))
# classtestfinal <- dfclasspred[,!(names(dfclasspred) %in% c("dftestpreds"))]
# #outputting data
# write.csv(classtestfinal,"/Users/danielclark/Desktop/SMU/DoingDataScience/Project2/DanielClark_DDSCaseStudy2_PredictAttrition.csv",row.names = FALSE)
# ```
#
#
# ```{r, regression}
# #run prediction on rf classification model using our test set
# testsalary$Attrition <- with(testsalary, ifelse(Attrition == 1, "Yes", "No"))
# dftestregress <- predict(rf_fit, testsalary)
# dftestpreds2 <- cbind(testsalary$ID, dftestregress)
# dfregpred <- as.data.frame(dftestpreds2)
# names(dfregpred)<- c("ID", "MonthlySalary")
# #outputting data
# write.csv(classtestfinal,"/Users/danielclark/Desktop/SMU/DoingDataScience/Project2/DanielClark_DDSCaseStudy2_PredictSalary.csv",row.names = FALSE)
# ```
#













iterations = 500
numks = 60
splitPerc = .70


masterAcc = matrix(nrow = iterations, ncol = numks)
masterSens = matrix(nrow = iterations, ncol = numks)
masterSpec = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  trainInd <- sample(1:dim(talent_model_knn)[1],round(splitPerc * dim(talent_model_knn)[1]))
  #trainInd = sample(seq(1,dim(talent_model_knn)[1],1),round(.7*dim(talent_model_knn)[1]))
  talent_model_knn_Train = talent_model_knn[trainInd,]
  talent_model_knn_Test = talent_model_knn[-trainInd,]

  for(i in 1:numks)
  {
    classifications = knn(talent_model_knn_Train[,-1],talent_model_knn_Test[,-1],talent_model_knn_Train$Attrition, k = i, prob = TRUE )
    table(as.factor(talent_model_knn_Test$Attrition),classifications)
    CM = confusionMatrix(table(as.factor(talent_model_knn_Test$Attrition),classifications))
    masterAcc[j,i] = CM$overall[1]
    masterSens[j,i] = mean(CM$byClass['Sensitivity'])
    masterSpec[j,i] = mean(CM$byClass['Specificity'] )
  }

}

MeanAcc = colMeans(masterAcc)
MeanSens = colMeans(masterSens)
MeanSpec = colMeans(masterSpec)

plot(seq(1,numks,1),MeanAcc, type = "l",xlab="k",ylab="Mean Accuracy",main="Optimal k for max Accuracy",sub="NA values omitted")
plot(seq(1,numks,1),MeanSens, type = "l")
plot(seq(1,numks,1),MeanSpec, type = "l")


rm(MeanAcc)
rm(MeanSens)
rm(MeanSpec)

#optimal k = 7

classifications = knn(talent_model_knn_Train[,-1],talent_model_knn_Test[,-1],talent_model_knn_Train$Attrition, k = 7, prob = TRUE )
table(as.factor(talent_model_knn_Test$Attrition),classifications)
CM = confusionMatrix(table(as.factor(talent_model_knn_Test$Attrition),classifications))
CM


# Naive Bayes




