# Titanic-Survival-Model-Using-Logistics-Regression

# Logistics Regression Class Exercise

#### Kexin Wang

#### Oct 02, 2019

> We will do small exercise in Logistic Regression today. This will also
> become your submission for the day. We will do this work today in groups of
> two. This will later also become your submission for the next homework. For
> the class submission, only one submission per team is needed - mention in
> comments (and in author field above) who all worked on it together.

## Titanic Survival Model

We want to find out what factors affected passengers’ survival when Titanic
sank. As survival is a binary measure, this task is well suited for a Logistic
Regression analysis.

### Task A: Load and Explore the Data

Training Data (data for building our model) is saved in the file
TitanicTrain.csv

You should import TitanicTrain.csv data. Check the structure of the data using
str() function.

  * PClass: Passenger Class, Sibsp: Number of Siblings/Spouse aboard
  * Parch: Number of Parents/Children aboard
  * Are there variables that are not in the right class? If yes then convert those variables to the correct class

    
    
    #Enter your code for loading and exploring the data here.
    Titanic<-read.csv("/Users/JakeBecker3/Desktop/TitanicTrain.csv")
    str(Titanic)
    
    
    ## 'data.frame':    891 obs. of  12 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : Factor w/ 891 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 417 581 ...
    ##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : Factor w/ 681 levels "110152","110413",..: 524 597 670 50 473 276 86 396 345 133 ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : Factor w/ 148 levels "","A10","A14",..: 1 83 1 57 1 1 131 1 1 1 ...
    ##  $ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...
    
    
    Titanic$PassengerId<-NULL
    Titanic$name<-NULL
    Titanic$Ticket<-NULL
    Titanic$Cabin<-NULL
    
    Titanic$Survived<-as.factor(Titanic$Survived)
    Titanic$Pclass<-as.factor(Titanic$Pclass)
    Titanic$Sex<-as.factor(Titanic$Sex)
    Titanic$Embarked<-as.factor(Titanic$Embarked)
    
    avgage<-tapply(Titanic$Age, Titanic$Sex, mean, na.rm=T)
    avgage
    
    
    ##   female     male 
    ## 27.91571 30.72664
    
    
    Titanic$Age<-ifelse (is.na(Titanic$Age)&Titanic$Sex=="male",
    avgage[2], ifelse(is.na(Titanic$Age)&Titanic$Sex=="female",
    avgage[1], Titanic$Age))
    summary(Titanic$Age)
    
    
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.42   22.00   30.00   29.74   35.00   80.00
    
    
    ### Task B: Build Our Model
    
    #Build a logistic regression model with Survival as the response variable. In this section, let's first build a main effects model. What variables do you need to include as the predictor variables?
    
    #Enter your code for your logistic regression model here.
    logit1 <- glm(Survived ~ Sex + Age + Pclass + Parch + Fare + SibSp + Embarked, data=Titanic, family="binomial")
    summary(logit1)
    
    
    ## 
    ## Call:
    ## glm(formula = Survived ~ Sex + Age + Pclass + Parch + Fare + 
    ##     SibSp + Embarked, family = "binomial", data = Titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6276  -0.6107  -0.4192   0.6151   2.4521  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  16.448912 610.213699   0.027  0.97849    
    ## Sexmale      -2.691038   0.201133 -13.379  < 2e-16 ***
    ## Age          -0.039927   0.007910  -5.048 4.47e-07 ***
    ## Pclass2      -0.931593   0.297933  -3.127  0.00177 ** 
    ## Pclass3      -2.160520   0.298287  -7.243 4.39e-13 ***
    ## Parch        -0.093693   0.119118  -0.787  0.43154    
    ## Fare          0.002236   0.002460   0.909  0.36353    
    ## SibSp        -0.326134   0.109547  -2.977  0.00291 ** 
    ## EmbarkedC   -12.331526 610.213584  -0.020  0.98388    
    ## EmbarkedQ   -12.376249 610.213636  -0.020  0.98382    
    ## EmbarkedS   -12.774300 610.213571  -0.021  0.98330    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  783.08  on 880  degrees of freedom
    ## AIC: 805.08
    ## 
    ## Number of Fisher Scoring iterations: 13
    
    
    logit2 <- glm(Survived ~ Sex + Age + Pclass + Parch + SibSp + Fare, data=Titanic, family="binomial")
    summary(logit2)
    
    
    ## 
    ## Call:
    ## glm(formula = Survived ~ Sex + Age + Pclass + Parch + SibSp + 
    ##     Fare, family = "binomial", data = Titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7125  -0.6050  -0.4263   0.6159   2.4385  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.862013   0.446357   8.652  < 2e-16 ***
    ## Sexmale     -2.741177   0.198900 -13.782  < 2e-16 ***
    ## Age         -0.040234   0.007873  -5.110 3.21e-07 ***
    ## Pclass2     -1.031817   0.293924  -3.510 0.000447 ***
    ## Pclass3     -2.166318   0.290618  -7.454 9.04e-14 ***
    ## Parch       -0.111150   0.117721  -0.944 0.345074    
    ## SibSp       -0.353821   0.109573  -3.229 0.001242 ** 
    ## Fare         0.002955   0.002444   1.209 0.226724    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  787.86  on 883  degrees of freedom
    ## AIC: 803.86
    ## 
    ## Number of Fisher Scoring iterations: 5

Builging a model is always only the first step. The more important task is
**interpreting** the model - what insights does your model give you?

    
    
    #$Enter your text interpretation of your model here.
    #Being a male in the third class has the lowest survival rate.

### Task C: Improve Our Model

> We will likely not be able to do this during the class exercise. This task
> is for you to do as your homework.

How can we improve our model. There are several things you can try:

  * Do we need any interaction effects?
  * Do any of the variables have non-linear effects - should we include them in the model as a square term?
  * Can we clean the data better? Can we **infer** the missing values rather than losing all that information?

Pay specific attention to how will you compare whether any particular model is
**better** than other models. Potential choices are looking at the AIC value
and ANOVA test for nested models.

    
    
    #Enter your code for improving your model here.
    logit3 <- glm(Survived ~ Sex + Pclass + Age + Parch + SibSp + Age*Pclass, data=Titanic, family="binomial")
    summary(logit3)
    
    
    ## 
    ## Call:
    ## glm(formula = Survived ~ Sex + Pclass + Age + Parch + SibSp + 
    ##     Age * Pclass, family = "binomial", data = Titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4713  -0.5882  -0.4110   0.5857   2.4678  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.57411    0.53314   6.704 2.03e-11 ***
    ## Sexmale     -2.78007    0.20136 -13.806  < 2e-16 ***
    ## Pclass2     -0.12409    0.68022  -0.182  0.85524    
    ## Pclass3     -1.78323    0.61612  -2.894  0.00380 ** 
    ## Age         -0.02647    0.01228  -2.156  0.03108 *  
    ## Parch       -0.08320    0.11558  -0.720  0.47157    
    ## SibSp       -0.34939    0.11257  -3.104  0.00191 ** 
    ## Pclass2:Age -0.03266    0.01930  -1.692  0.09058 .  
    ## Pclass3:Age -0.01588    0.01812  -0.877  0.38062    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  786.57  on 882  degrees of freedom
    ## AIC: 804.57
    ## 
    ## Number of Fisher Scoring iterations: 5
    
    
    logit4 <- glm(Survived ~ Sex + Pclass + Age + Parch + SibSp + Sex*Pclass, data=Titanic, family="binomial")
    summary(logit4)
    
    
    ## 
    ## Call:
    ## glm(formula = Survived ~ Sex + Pclass + Age + Parch + SibSp + 
    ##     Sex * Pclass, family = "binomial", data = Titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1281  -0.6343  -0.4687   0.3781   2.5218  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      5.392928   0.703077   7.670 1.71e-14 ***
    ## Sexmale         -4.038748   0.626021  -6.451 1.11e-10 ***
    ## Pclass2         -1.267825   0.735019  -1.725  0.08455 .  
    ## Pclass3         -3.981520   0.634764  -6.272 3.55e-10 ***
    ## Age             -0.046569   0.008529  -5.460 4.77e-08 ***
    ## Parch           -0.036384   0.121030  -0.301  0.76371    
    ## SibSp           -0.342002   0.110905  -3.084  0.00204 ** 
    ## Sexmale:Pclass2 -0.336316   0.805675  -0.417  0.67636    
    ## Sexmale:Pclass3  2.150641   0.669401   3.213  0.00131 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  758.77  on 882  degrees of freedom
    ## AIC: 776.77
    ## 
    ## Number of Fisher Scoring iterations: 6
    
    
    Titanic$AgeCat<-as.factor(ifelse(Titanic$Age<16,"child","adult"))
    logit5 <- glm(Survived ~ Sex + Pclass + Age + Parch + SibSp + AgeCat, data=Titanic, family="binomial")
    summary(logit5)  
    
    
    ## 
    ## Call:
    ## glm(formula = Survived ~ Sex + Pclass + Age + Parch + SibSp + 
    ##     AgeCat, family = "binomial", data = Titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8630  -0.5855  -0.4218   0.5785   2.5316  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.555561   0.430768   8.254  < 2e-16 ***
    ## Sexmale     -2.794801   0.201509 -13.869  < 2e-16 ***
    ## Pclass2     -1.154329   0.261244  -4.419 9.93e-06 ***
    ## Pclass3     -2.287323   0.243096  -9.409  < 2e-16 ***
    ## Age         -0.024944   0.009111  -2.738 0.006185 ** 
    ## Parch       -0.162016   0.120526  -1.344 0.178872    
    ## SibSp       -0.435033   0.120278  -3.617 0.000298 ***
    ## AgeCatchild  1.334911   0.427425   3.123 0.001789 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  779.24  on 883  degrees of freedom
    ## AIC: 795.24
    ## 
    ## Number of Fisher Scoring iterations: 5
    
    
    logit6 <- glm(Survived ~ Sex + Pclass + Age + Parch + SibSp + Sex*SibSp, data=Titanic, family="binomial")
    summary(logit6)
    
    
    ## 
    ## Call:
    ## glm(formula = Survived ~ Sex + Pclass + Age + Parch + SibSp + 
    ##     Sex * SibSp, family = "binomial", data = Titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6383  -0.6063  -0.4260   0.5983   2.4424  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    4.229222   0.414829  10.195  < 2e-16 ***
    ## Sexmale       -2.979858   0.227366 -13.106  < 2e-16 ***
    ## Pclass2       -1.212427   0.263046  -4.609 4.04e-06 ***
    ## Pclass3       -2.362536   0.244829  -9.650  < 2e-16 ***
    ## Age           -0.040390   0.007868  -5.133 2.84e-07 ***
    ## Parch         -0.084396   0.114571  -0.737 0.461353    
    ## SibSp         -0.530553   0.146230  -3.628 0.000285 ***
    ## Sexmale:SibSp  0.440110   0.191048   2.304 0.021242 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  784.42  on 883  degrees of freedom
    ## AIC: 800.42
    ## 
    ## Number of Fisher Scoring iterations: 5
    
    
    logit7 <- glm(Survived ~ Sex + Pclass + Age + Parch + SibSp + Sex*Parch, data=Titanic, family="binomial")
    summary(logit7)
    
    
    ## 
    ## Call:
    ## glm(formula = Survived ~ Sex + Pclass + Age + Parch + SibSp + 
    ##     Sex * Parch, family = "binomial", data = Titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5062  -0.6051  -0.4187   0.5901   2.4956  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    4.123359   0.405483  10.169  < 2e-16 ***
    ## Sexmale       -3.020015   0.220761 -13.680  < 2e-16 ***
    ## Pclass2       -1.162646   0.264474  -4.396 1.10e-05 ***
    ## Pclass3       -2.287530   0.245885  -9.303  < 2e-16 ***
    ## Age           -0.037863   0.007836  -4.832 1.35e-06 ***
    ## Parch         -0.295367   0.140689  -2.099 0.035779 *  
    ## SibSp         -0.360501   0.108705  -3.316 0.000912 ***
    ## Sexmale:Parch  0.699464   0.221275   3.161 0.001572 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  780.07  on 883  degrees of freedom
    ## AIC: 796.07
    ## 
    ## Number of Fisher Scoring iterations: 5

What is your best model? Explain your best model - what are you seeing as
significant? What is the interpretation? What does it mean?

    
    
    Enter your text interpretation here.
    #Logit4 is the best model becuase it has the lowest AIC. It tells us that being a male at class 3 has higher survival rate compared to the one in class 2.Logit5 shows that being a child has higher survival rate. Loigt6 shows that in siblings, brothers have higehr survival rate than sisters. According to logit7, a dad has higher survival rate than mom do when traveling with children on Titanic.

### Task D: Predict Outcomes in Testing Data

> We will likely not be able to do this during the class exercise. This task
> is for you to do as your homework.

We have saved a small part of data to test our model. This is the **Testing
Data**. We will use this data to see how good of a prediction is made by the
model we created in Task D above.

You should do the following:

  * Import the testing data in TitanicTest.csv
  * Check that you have all the predictor variables. It will natually not have the response variable
  * Do any variable need recoding to match the training dataset? If yes then do the necessary variable recoding
  * Predict the Survival Probability for the Testing Dataset using the model developed on the training dataset in Step C above
  * Convert probabilities to a a binary outcome (1 for Prob > 0.5, otherwise 0 - or choose your own threshold)

    
    
    #Enter your code for predicting outcomes in testing data here.
    #setwd("C:/TO414 data")
    TitanicTest<-read.csv("/Users/JakeBecker3/Desktop/TitanicTest.csv")
    summary(TitanicTest)
    
    
    ##   PassengerId         Pclass     
    ##  Min.   : 892.0   Min.   :1.000  
    ##  1st Qu.: 996.2   1st Qu.:1.000  
    ##  Median :1100.5   Median :3.000  
    ##  Mean   :1100.5   Mean   :2.266  
    ##  3rd Qu.:1204.8   3rd Qu.:3.000  
    ##  Max.   :1309.0   Max.   :3.000  
    ##                                  
    ##                                         Name         Sex     
    ##  Abbott, Master. Eugene Joseph            :  1   female:152  
    ##  Abelseth, Miss. Karen Marie              :  1   male  :266  
    ##  Abelseth, Mr. Olaus Jorgensen            :  1               
    ##  Abrahamsson, Mr. Abraham August Johannes :  1               
    ##  Abrahim, Mrs. Joseph (Sophie Halaut Easu):  1               
    ##  Aks, Master. Philip Frank                :  1               
    ##  (Other)                                  :412               
    ##       Age            SibSp            Parch             Ticket   
    ##  Min.   : 0.17   Min.   :0.0000   Min.   :0.0000   PC 17608:  5  
    ##  1st Qu.:21.00   1st Qu.:0.0000   1st Qu.:0.0000   113503  :  4  
    ##  Median :27.00   Median :0.0000   Median :0.0000   CA. 2343:  4  
    ##  Mean   :30.27   Mean   :0.4474   Mean   :0.3923   16966   :  3  
    ##  3rd Qu.:39.00   3rd Qu.:1.0000   3rd Qu.:0.0000   220845  :  3  
    ##  Max.   :76.00   Max.   :8.0000   Max.   :9.0000   347077  :  3  
    ##  NA's   :86                                        (Other) :396  
    ##       Fare                     Cabin     Embarked
    ##  Min.   :  0.000                  :327   C:102   
    ##  1st Qu.:  7.896   B57 B59 B63 B66:  3   Q: 46   
    ##  Median : 14.454   A34            :  2   S:270   
    ##  Mean   : 35.627   B45            :  2           
    ##  3rd Qu.: 31.500   C101           :  2           
    ##  Max.   :512.329   C116           :  2           
    ##  NA's   :1         (Other)        : 80
    
    
    TitanicTest$Pclass<-as.factor(TitanicTest$Pclass)
    TitanicTest$Sex<-as.factor(TitanicTest$Sex)
    TitanicTest$Embarked<-as.factor(TitanicTest$Embarked)
    
    avgage<-tapply(TitanicTest$Age, TitanicTest$Sex, mean, na.rm=T)
    avgage
    
    
    ##   female     male 
    ## 30.27236 30.27273
    
    
    TitanicTest$Age<-ifelse (is.na(TitanicTest$Age)&TitanicTest$Sex=="male",
    avgage[2], ifelse(is.na(TitanicTest$Age)&TitanicTest$Sex=="female",
    avgage[1], TitanicTest$Age))
    summary(TitanicTest$Age)
    
    
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.17   23.00   30.27   30.27   35.75   76.00
    
    
    summary(TitanicTest)
    
    
    ##   PassengerId     Pclass                                         Name    
    ##  Min.   : 892.0   1:107   Abbott, Master. Eugene Joseph            :  1  
    ##  1st Qu.: 996.2   2: 93   Abelseth, Miss. Karen Marie              :  1  
    ##  Median :1100.5   3:218   Abelseth, Mr. Olaus Jorgensen            :  1  
    ##  Mean   :1100.5           Abrahamsson, Mr. Abraham August Johannes :  1  
    ##  3rd Qu.:1204.8           Abrahim, Mrs. Joseph (Sophie Halaut Easu):  1  
    ##  Max.   :1309.0           Aks, Master. Philip Frank                :  1  
    ##                           (Other)                                  :412  
    ##      Sex           Age            SibSp            Parch       
    ##  female:152   Min.   : 0.17   Min.   :0.0000   Min.   :0.0000  
    ##  male  :266   1st Qu.:23.00   1st Qu.:0.0000   1st Qu.:0.0000  
    ##               Median :30.27   Median :0.0000   Median :0.0000  
    ##               Mean   :30.27   Mean   :0.4474   Mean   :0.3923  
    ##               3rd Qu.:35.75   3rd Qu.:1.0000   3rd Qu.:0.0000  
    ##               Max.   :76.00   Max.   :8.0000   Max.   :9.0000  
    ##                                                                
    ##       Ticket         Fare                     Cabin     Embarked
    ##  PC 17608:  5   Min.   :  0.000                  :327   C:102   
    ##  113503  :  4   1st Qu.:  7.896   B57 B59 B63 B66:  3   Q: 46   
    ##  CA. 2343:  4   Median : 14.454   A34            :  2   S:270   
    ##  16966   :  3   Mean   : 35.627   B45            :  2           
    ##  220845  :  3   3rd Qu.: 31.500   C101           :  2           
    ##  347077  :  3   Max.   :512.329   C116           :  2           
    ##  (Other) :396   NA's   :1         (Other)        : 80
    
    
    TitanicTest$Survived <- ifelse(predict(logit4, TitanicTest, type = "response")>0.5,1,0)
    
    library(tidyverse)
    
    
    ## ── Attaching packages ─────────────────────────────────────────────────────── tidyverse 1.2.1 ──
    
    
    ## ✔ ggplot2 3.1.0       ✔ purrr   0.3.2  
    ## ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
    ## ✔ readr   1.3.1       ✔ forcats 0.4.0
    
    
    ## ── Conflicts ────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    
    
    answer <- TitanicTest %>% select(PassengerId, Survived)
    answer
    
    
    ##     PassengerId Survived
    ## 1           892        0
    ## 2           893        0
    ## 3           894        0
    ## 4           895        0
    ## 5           896        1
    ## 6           897        0
    ## 7           898        1
    ## 8           899        0
    ## 9           900        1
    ## 10          901        0
    ## 11          902        0
    ## 12          903        0
    ## 13          904        1
    ## 14          905        0
    ## 15          906        1
    ## 16          907        1
    ## 17          908        0
    ## 18          909        0
    ## 19          910        0
    ## 20          911        0
    ## 21          912        0
    ## 22          913        0
    ## 23          914        1
    ## 24          915        1
    ## 25          916        1
    ## 26          917        0
    ## 27          918        1
    ## 28          919        0
    ## 29          920        0
    ## 30          921        0
    ## 31          922        0
    ## 32          923        0
    ## 33          924        0
    ## 34          925        0
    ## 35          926        0
    ## 36          927        0
    ## 37          928        1
    ## 38          929        1
    ## 39          930        0
    ## 40          931        0
    ## 41          932        0
    ## 42          933        0
    ## 43          934        0
    ## 44          935        1
    ## 45          936        1
    ## 46          937        0
    ## 47          938        0
    ## 48          939        0
    ## 49          940        1
    ## 50          941        0
    ## 51          942        0
    ## 52          943        0
    ## 53          944        1
    ## 54          945        1
    ## 55          946        0
    ## 56          947        0
    ## 57          948        0
    ## 58          949        0
    ## 59          950        0
    ## 60          951        1
    ## 61          952        0
    ## 62          953        0
    ## 63          954        0
    ## 64          955        1
    ## 65          956        0
    ## 66          957        1
    ## 67          958        1
    ## 68          959        0
    ## 69          960        0
    ## 70          961        1
    ## 71          962        1
    ## 72          963        0
    ## 73          964        1
    ## 74          965        1
    ## 75          966        1
    ## 76          967        0
    ## 77          968        0
    ## 78          969        1
    ## 79          970        0
    ## 80          971        1
    ## 81          972        0
    ## 82          973        0
    ## 83          974        0
    ## 84          975        0
    ## 85          976        0
    ## 86          977        0
    ## 87          978        1
    ## 88          979        1
    ## 89          980        1
    ## 90          981        0
    ## 91          982        1
    ## 92          983        0
    ## 93          984        1
    ## 94          985        0
    ## 95          986        1
    ## 96          987        0
    ## 97          988        1
    ## 98          989        0
    ## 99          990        1
    ## 100         991        0
    ## 101         992        1
    ## 102         993        0
    ## 103         994        0
    ## 104         995        0
    ## 105         996        1
    ## 106         997        0
    ## 107         998        0
    ## 108         999        0
    ## 109        1000        0
    ## 110        1001        0
    ## 111        1002        0
    ## 112        1003        1
    ## 113        1004        1
    ## 114        1005        1
    ## 115        1006        1
    ## 116        1007        0
    ## 117        1008        0
    ## 118        1009        1
    ## 119        1010        0
    ## 120        1011        1
    ## 121        1012        1
    ## 122        1013        0
    ## 123        1014        1
    ## 124        1015        0
    ## 125        1016        0
    ## 126        1017        1
    ## 127        1018        0
    ## 128        1019        0
    ## 129        1020        0
    ## 130        1021        0
    ## 131        1022        0
    ## 132        1023        0
    ## 133        1024        0
    ## 134        1025        0
    ## 135        1026        0
    ## 136        1027        0
    ## 137        1028        0
    ## 138        1029        0
    ## 139        1030        1
    ## 140        1031        0
    ## 141        1032        0
    ## 142        1033        1
    ## 143        1034        0
    ## 144        1035        0
    ## 145        1036        0
    ## 146        1037        0
    ## 147        1038        0
    ## 148        1039        0
    ## 149        1040        0
    ## 150        1041        0
    ## 151        1042        1
    ## 152        1043        0
    ## 153        1044        0
    ## 154        1045        0
    ## 155        1046        0
    ## 156        1047        0
    ## 157        1048        1
    ## 158        1049        1
    ## 159        1050        0
    ## 160        1051        1
    ## 161        1052        1
    ## 162        1053        0
    ## 163        1054        1
    ## 164        1055        0
    ## 165        1056        0
    ## 166        1057        0
    ## 167        1058        0
    ## 168        1059        0
    ## 169        1060        1
    ## 170        1061        1
    ## 171        1062        0
    ## 172        1063        0
    ## 173        1064        0
    ## 174        1065        0
    ## 175        1066        0
    ## 176        1067        1
    ## 177        1068        1
    ## 178        1069        0
    ## 179        1070        1
    ## 180        1071        1
    ## 181        1072        0
    ## 182        1073        0
    ## 183        1074        1
    ## 184        1075        0
    ## 185        1076        1
    ## 186        1077        0
    ## 187        1078        1
    ## 188        1079        0
    ## 189        1080        0
    ## 190        1081        0
    ## 191        1082        0
    ## 192        1083        0
    ## 193        1084        0
    ## 194        1085        0
    ## 195        1086        0
    ## 196        1087        0
    ## 197        1088        1
    ## 198        1089        1
    ## 199        1090        0
    ## 200        1091        1
    ## 201        1092        1
    ## 202        1093        0
    ## 203        1094        0
    ## 204        1095        1
    ## 205        1096        0
    ## 206        1097        0
    ## 207        1098        0
    ## 208        1099        0
    ## 209        1100        1
    ## 210        1101        0
    ## 211        1102        0
    ## 212        1103        0
    ## 213        1104        0
    ## 214        1105        1
    ## 215        1106        0
    ## 216        1107        0
    ## 217        1108        1
    ## 218        1109        0
    ## 219        1110        1
    ## 220        1111        0
    ## 221        1112        1
    ## 222        1113        0
    ## 223        1114        1
    ## 224        1115        0
    ## 225        1116        1
    ## 226        1117        0
    ## 227        1118        0
    ## 228        1119        1
    ## 229        1120        0
    ## 230        1121        0
    ## 231        1122        0
    ## 232        1123        1
    ## 233        1124        0
    ## 234        1125        0
    ## 235        1126        0
    ## 236        1127        0
    ## 237        1128        0
    ## 238        1129        0
    ## 239        1130        1
    ## 240        1131        1
    ## 241        1132        1
    ## 242        1133        1
    ## 243        1134        0
    ## 244        1135        0
    ## 245        1136        0
    ## 246        1137        0
    ## 247        1138        1
    ## 248        1139        0
    ## 249        1140        1
    ## 250        1141        0
    ## 251        1142        1
    ## 252        1143        0
    ## 253        1144        0
    ## 254        1145        0
    ## 255        1146        0
    ## 256        1147        0
    ## 257        1148        0
    ## 258        1149        0
    ## 259        1150        1
    ## 260        1151        0
    ## 261        1152        0
    ## 262        1153        0
    ## 263        1154        1
    ## 264        1155        1
    ## 265        1156        0
    ## 266        1157        0
    ## 267        1158        0
    ## 268        1159        0
    ## 269        1160        1
    ## 270        1161        0
    ## 271        1162        0
    ## 272        1163        0
    ## 273        1164        1
    ## 274        1165        0
    ## 275        1166        0
    ## 276        1167        1
    ## 277        1168        0
    ## 278        1169        0
    ## 279        1170        0
    ## 280        1171        0
    ## 281        1172        1
    ## 282        1173        0
    ## 283        1174        1
    ## 284        1175        1
    ## 285        1176        1
    ## 286        1177        0
    ## 287        1178        0
    ## 288        1179        0
    ## 289        1180        0
    ## 290        1181        0
    ## 291        1182        0
    ## 292        1183        1
    ## 293        1184        0
    ## 294        1185        0
    ## 295        1186        0
    ## 296        1187        0
    ## 297        1188        1
    ## 298        1189        0
    ## 299        1190        0
    ## 300        1191        0
    ## 301        1192        0
    ## 302        1193        0
    ## 303        1194        0
    ## 304        1195        0
    ## 305        1196        1
    ## 306        1197        1
    ## 307        1198        0
    ## 308        1199        0
    ## 309        1200        0
    ## 310        1201        0
    ## 311        1202        0
    ## 312        1203        0
    ## 313        1204        0
    ## 314        1205        0
    ## 315        1206        1
    ## 316        1207        1
    ## 317        1208        0
    ## 318        1209        0
    ## 319        1210        0
    ## 320        1211        0
    ## 321        1212        0
    ## 322        1213        0
    ## 323        1214        0
    ## 324        1215        0
    ## 325        1216        1
    ## 326        1217        0
    ## 327        1218        1
    ## 328        1219        0
    ## 329        1220        0
    ## 330        1221        0
    ## 331        1222        1
    ## 332        1223        0
    ## 333        1224        0
    ## 334        1225        1
    ## 335        1226        0
    ## 336        1227        0
    ## 337        1228        0
    ## 338        1229        0
    ## 339        1230        0
    ## 340        1231        0
    ## 341        1232        0
    ## 342        1233        0
    ## 343        1234        0
    ## 344        1235        1
    ## 345        1236        0
    ## 346        1237        1
    ## 347        1238        0
    ## 348        1239        0
    ## 349        1240        0
    ## 350        1241        1
    ## 351        1242        1
    ## 352        1243        0
    ## 353        1244        0
    ## 354        1245        0
    ## 355        1246        1
    ## 356        1247        0
    ## 357        1248        1
    ## 358        1249        0
    ## 359        1250        0
    ## 360        1251        0
    ## 361        1252        0
    ## 362        1253        1
    ## 363        1254        1
    ## 364        1255        0
    ## 365        1256        1
    ## 366        1257        0
    ## 367        1258        0
    ## 368        1259        1
    ## 369        1260        1
    ## 370        1261        0
    ## 371        1262        0
    ## 372        1263        1
    ## 373        1264        0
    ## 374        1265        0
    ## 375        1266        1
    ## 376        1267        1
    ## 377        1268        0
    ## 378        1269        0
    ## 379        1270        0
    ## 380        1271        0
    ## 381        1272        0
    ## 382        1273        0
    ## 383        1274        1
    ## 384        1275        1
    ## 385        1276        0
    ## 386        1277        1
    ## 387        1278        0
    ## 388        1279        0
    ## 389        1280        0
    ## 390        1281        0
    ## 391        1282        1
    ## 392        1283        1
    ## 393        1284        0
    ## 394        1285        0
    ## 395        1286        0
    ## 396        1287        1
    ## 397        1288        0
    ## 398        1289        1
    ## 399        1290        0
    ## 400        1291        0
    ## 401        1292        1
    ## 402        1293        0
    ## 403        1294        1
    ## 404        1295        1
    ## 405        1296        0
    ## 406        1297        0
    ## 407        1298        0
    ## 408        1299        0
    ## 409        1300        1
    ## 410        1301        1
    ## 411        1302        1
    ## 412        1303        1
    ## 413        1304        1
    ## 414        1305        0
    ## 415        1306        1
    ## 416        1307        0
    ## 417        1308        0
    ## 418        1309        0
    
    
    #write.csv(answer, file = "wkx.csv")

### Task E: Compete with Rest of the World

What you just did happens to be one of the ongoing competitions at kaggle.com.
<https://www.kaggle.com/c/titanic> has more details. Once you have predicted
outcomes on the test data, you need to create a submission file that has two
columns: “PasserngerID” and “Survived”. The second column should have 1 or 0
for survived or not. NA or missing values in this field is not permitted.
Create an account at kaggle and upload your solution at
<https://www.kaggle.com/c/titanic/submit>

Kaggle will evaluate your submission and let you know

  1. What is your score (how good is your prediction)
  2. What is your rank among all submissions

    
    
    Enter here the following:
    1. Your Kaggle Username:wakexin
    2. Your Kaggle Rank:0.77511
    3. What did you learn in this exercise? Summarize your experience. What can you do better given more time?
    I learned how to build a model, interpret its outcome and use the model to predict outcomes from different data sets. I can explore the relationship between different variables and improve the accuracy of my model if given more time.

> When you are done, submit you files (RMD and HTML) to Canvas for our weekly
> homework. Also **submit you Kaggle rank and username**. Lets see who in
> class gets the best rank on Kaggle. **There will be a surprise prize for the
> best submission!**

