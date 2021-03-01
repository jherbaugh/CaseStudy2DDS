# CaseStudy2DDS
Case Study 2: DDSAnalytics 

# Talent Analysis 

created by [Jason Herbaugh](https://github.com/jherbaugh) 

## Purpose

The Client, "DDSAnalytics ," has hired me to conduct specific analysis to identify factors that lead to attrition, provide any interesting trends in the data and build a model to predict attrition and salary (seperately).

## Project Overview

My EDA will consists of running summary statistics against the data provided. Inspecting the correlation matrix I will see what variables are multicollinear. In addition to the Boruta Feature selection,I will remove some unimportant variables from the dataset. Additionally, I will also plot interesting relationships between the variables. Finally, i will run through a multitude of models for both attrition classification and salary regression and output the results to the validation set. 

## Data
Source Data ['CaseStudy2-data'](https://github.com/jherbaugh/CaseStudy2DDS/blob/main/CaseStudy2-data.csv) - talent management dataset.

## Analysis

1. Top three factors in the data that lead to attrition.

2. Interesting trends in the data (open ended).  

3.  Build and Deploy a Model to Predict Salary. Must used k-nn and/or Naive Bayes as well as others.  Sensitivity and Specificity must be greater than > 60%.

4.  Build and Deploy a Model to Classify Attrition Rates. Must used k-nn and/or Naive Bayes as well as others. Sensitivity and Specificity must be greater than > 60%.

### Details 
The primary focus of this project is to display skill in each step of the **Data Science Process**; 
1. Define the Goal 
2. Get the Data 
3. Clean the Data
4. Enrich the Data
5. Find insights and visualize
6. Deploy Machine Learning
7. Iterate ∞

in order to **Interperate and Communicate** Findings with stake holders. 

## YouTube Video
* [Jason's Youtube Video](https://www.youtube.com/watch?v=pkeAmR0H_j8) 

## Presentations

* [DDS Analysis Final Presentation](https://github.com/jherbaugh/CaseStudy2DDS/blob/main/DDS%20Analytics%20Presentation.pptx) - Power Point Presentation covering the questions of Interest from our EDA as well as additional insights.

## Knit File in rendered html page

* [Knitted presentation](https://github.com/jherbaugh/CaseStudy2DDS/blob/main/DDSAnalytics_EDA.html)


### Conclusion
Based on my analysis, the three trend that contribute to employee attributions are Stock Options, Monthly Income, and Overtime. 
Random Forest Model is the best for both model predictor and classifiers mainluy becaused it is based on trees so scaling variables doesn’t matter,
deals with missing data (not needed here), and also has automated feature selection built in. Other than that Age, Total Working Years and Marital Status strong factors for attrition.

## Contributing

Don't. Project End of Life is Feburary 28, 2021

