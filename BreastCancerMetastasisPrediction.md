Predicting Breast Cancer Metastasis Using Clinicopathological Data and Machine Learning Technologies
================
Yi-Ju Tseng, Chuang-En Huang, Po-Yin Lai, Yu-Chen Sun, Chiao-Ni Wen, Hsin-Yao Wang, and Jang-Jih Lu

Set environment
---------------

### Load all librarys

``` r
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(ROCR)
library(caret)
library(tableone)
```

### Load all functions

``` r
# The ModelFunction.R can be download from GitHub
# https://github.com/DHLab-CGU/Breast-Cancer-Metastasis-Prediction/blob/master/ModelFunction.R
source('ModelFunction.R')
```

Data load and pre-process
-------------------------

### Load breast cancer data for model development

``` r
TimeIndepData<-readRDS('TimeIndepData.rds')
str(select(TimeIndepData,-ID))
```

    ## Classes 'data.table' and 'data.frame':   205 obs. of  10 variables:
    ##  $ BRNDAT     : Date, format: "1957-01-04" "1964-02-08" ...
    ##  $ OPAge      : num  43.5 45.8 69.4 69.2 38 ...
    ##  $ pTi        : chr  "3" "1c" "2" "2" ...
    ##  $ pNi        : chr  "3" "1" "0" "0" ...
    ##  $ pMi        : chr  "0" "0" "0" "0" ...
    ##  $ Tissue.ER  : num  0 1 0 3 0 2 0 0 0 0 ...
    ##  $ Tissue.PR  : num  0 1 0 2 0 1 2 0 0 2 ...
    ##  $ Tissue.HER2: num  3 2 3 3 3 3 3 3 3 3 ...
    ##  $ IsRe       : Factor w/ 2 levels "Non.Recurrence",..: 2 1 2 1 1 1 2 1 1 1 ...
    ##  $ IndexDAT   : Date, format: "2003-02-21" "2016-09-14" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
TimeVariedData<-readRDS('TimeVariedData.rds')
str(select(TimeVariedData,-ID))
```

    ## Classes 'data.table' and 'data.frame':   4321 obs. of  4 variables:
    ##  $ LabName       : chr  "CA153" "CEA" "CA153" "CA153" ...
    ##  $ LabValue      : num  6.7 1.49 18.8 25.2 25.5 14.4 16.3 16.6 13.3 15.4 ...
    ##  $ RCVDAT        : Date, format: "2003-01-16" "2003-01-16" ...
    ##  $ Index_duration: num  36 36 2495 2273 2217 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

### T stage code conversion

``` r
TimeIndepData[pTi %in% c('4a','4b','4c','4d'),pTi:='8']
TimeIndepData[pTi=='3',pTi:='7']
TimeIndepData[pTi=='2',pTi:='6']
TimeIndepData[pTi=='1c',pTi:='5']
TimeIndepData[pTi=='1b',pTi:='4']
TimeIndepData[pTi=='1a',pTi:='3']
TimeIndepData[pTi=='1micro',pTi:='2']
TimeIndepData[pTi=='is',pTi:='1']
TimeIndepData[pTi=='x',pTi:=NA]
```

### Process categorical variables

``` r
cols <- c("pTi", "pNi", "pMi", "Tissue.ER", "Tissue.PR", "Tissue.HER2")
TimeIndepData[, c(cols) := lapply(.SD, as.factor), .SDcols= cols]
```

Patient characteristics
-----------------------

``` r
LastData <- TimeVariedData[order(ID,LabName,-RCVDAT)][,.SD[c(1)],by=.(ID,LabName)]
LabWide <- spread(LastData[,.(ID,LabName,LabValue)],key=LabName,value=LabValue)
IndexDateGap<-LastData[,.(ID,Index_duration)][order(ID,Index_duration)][,.SD[c(1)],by=.(ID)]

LastRecordAfterOP <- inner_join(TimeIndepData,LabWide, by = "ID")%>%
  inner_join(., IndexDateGap, by='ID') 

vars <- c("OPAge","pTi","pNi","pMi","Tissue.ER","Tissue.PR","Tissue.HER2","CA153","CEA","HER2","Index_duration") 
cols <- c("pTi", "pNi", "pMi", "Tissue.ER", "Tissue.PR", "Tissue.HER2")
ALL_tableOne <- CreateTableOne(vars = vars, strata = c("IsRe"),factorVars = cols, data = LastRecordAfterOP) 
```

``` r
ALL_tableOneDF<-data.table(as.matrix(print(ALL_tableOne,nonnormal= "Index_duration")),keep.rownames = T)
```

``` r
knitr::kable(ALL_tableOneDF)
```

| rn                               | Non.Recurrence            | Recurrence            | p         | test    |
|:---------------------------------|:--------------------------|:----------------------|:----------|:--------|
| n                                | 136                       | 69                    |           |         |
| OPAge (mean (sd))                | 49.90 (11.00)             | 45.94 (12.24)         | 0.020     |         |
| pTi (%)                          |                           |                       | 0.021     |         |
| 0                                | 13 ( 9.7)                 | 5 ( 7.2)              |           |         |
| 1                                | 3 ( 2.2)                  | 2 ( 2.9)              |           |         |
| 2                                | 7 ( 5.2)                  | 1 ( 1.4)              |           |         |
| 3                                | 10 ( 7.5)                 | 4 ( 5.8)              |           |         |
| 4                                | 22 ( 16.4)                | 7 ( 10.1)             |           |         |
| 5                                | 34 ( 25.4)                | 10 ( 14.5)            |           |         |
| 6                                | 41 ( 30.6)                | 29 ( 42.0)            |           |         |
| 7                                | 2 ( 1.5)                  | 8 ( 11.6)             |           |         |
| 8                                | 2 ( 1.5)                  | 2 ( 2.9)              |           |         |
| NA                               | 0 ( 0.0)                  | 1 ( 1.4)              |           |         |
| pNi (%)                          |                           |                       | &lt;0.001 |         |
| 0                                | 71 ( 52.2)                | 18 ( 26.1)            |           |         |
| 1                                | 37 ( 27.2)                | 17 ( 24.6)            |           |         |
| 2                                | 17 ( 12.5)                | 13 ( 18.8)            |           |         |
| 3                                | 11 ( 8.1)                 | 21 ( 30.4)            |           |         |
| pMi = 0 (%)                      | 136 (100.0)               | 69 (100.0)            | NA        |         |
| Tissue.ER (%)                    |                           |                       | 0.286     |         |
| 0                                | 70 ( 51.5)                | 33 ( 48.5)            |           |         |
| 1                                | 23 ( 16.9)                | 19 ( 27.9)            |           |         |
| 2                                | 19 ( 14.0)                | 7 ( 10.3)             |           |         |
| 3                                | 24 ( 17.6)                | 9 ( 13.2)             |           |         |
| Tissue.PR (%)                    |                           |                       | 0.855     |         |
| 0                                | 84 ( 61.8)                | 40 ( 58.8)            |           |         |
| 1                                | 25 ( 18.4)                | 12 ( 17.6)            |           |         |
| 2                                | 16 ( 11.8)                | 11 ( 16.2)            |           |         |
| 3                                | 11 ( 8.1)                 | 5 ( 7.4)              |           |         |
| Tissue.HER2 (%)                  |                           |                       | 0.021     |         |
| 0                                | 1 ( 0.7)                  | 6 ( 8.7)              |           |         |
| 1                                | 3 ( 2.2)                  | 3 ( 4.3)              |           |         |
| 2                                | 17 ( 12.5)                | 8 ( 11.6)             |           |         |
| 3                                | 115 ( 84.6)               | 52 ( 75.4)            |           |         |
| CA153 (mean (sd))                | 11.35 (5.88)              | 38.55 (80.00)         | &lt;0.001 |         |
| CEA (mean (sd))                  | 1.52 (1.15)               | 19.68 (88.18)         | 0.018     |         |
| HER2 (mean (sd))                 | 3.64 (2.19)               | 4.65 (4.21)           | 0.077     |         |
| Index\_duration (median \[IQR\]) | 187.00 \[162.50, 343.00\] | 32.00 \[9.00, 75.00\] | &lt;0.001 | nonnorm |

Build the model to predict breast cancer metastasis at least 3 months in advance
--------------------------------------------------------------------------------

### Set parameters

``` r
seed<-3231
n_times<-50
n_folds<-3
trc<-trainControl(method = "cv", number = n_folds, 
                  classProbs=TRUE, summaryFunction = twoClassSummary)
```

### Get completed data 3 months before the index date

``` r
Over90dData <- TimeVariedData %>% filter(Index_duration>90) %>% 
  arrange(ID,LabName,Index_duration) %>% group_by(ID,LabName) %>% slice(1)
LabWide90d <- spread(Over90dData %>% select(ID,LabName,LabValue),key=LabName,value=LabValue)
LastRecordAfterOP_90<-inner_join(TimeIndepData,LabWide90d, by = "ID")
LastRecordAfterOP_90_rmna <- LastRecordAfterOP_90 %>% filter(complete.cases(LastRecordAfterOP_90))
LastRecordAfterOP_90_rmna_IsRe<-LastRecordAfterOP_90_rmna %>% filter(IsRe=="Recurrence")
LastRecordAfterOP_90_rmna_NotRe<-LastRecordAfterOP_90_rmna%>% filter(IsRe=="Non.Recurrence")
nrow(LastRecordAfterOP_90_rmna_IsRe)
```

    ## [1] 19

``` r
nrow(LastRecordAfterOP_90_rmna_NotRe)
```

    ## [1] 125

### 3 evaluation folds

``` r
datalist<-generate3folds(LastRecordAfterOP_90_rmna_IsRe,LastRecordAfterOP_90_rmna_NotRe,seed)
training_90<-datalist[[1]]
test_90<-datalist[[2]]
```

### Model tuning and evaluation - logistic regression

``` r
glm_perf_90<-NULL
for (k in 1:n_times){
  for (i in 1:n_folds){
    glm_perf_90_tmp<-glm_tune_eval(training_90,test_90,i,k,seed,trc)
    glm_perf_90<-rbind(glm_perf_90,glm_perf_90_tmp)
  }
}
glm_perf_90$Days_before<-"90"
```

### Model tuning and evaluation - Naive bayes

``` r
nb_perf_90<-NULL
for (k in 1:n_times){
  for (i in 1:n_folds){
    nb_perf_90_tmp<-nb_tune_eval(training_90,test_90,i,k,seed,trc)
    nb_perf_90<-rbind(nb_perf_90,nb_perf_90_tmp)
  }
}
nb_perf_90$Days_before<-"90"
```

### Model tuning and evaluation - random forest

``` r
rf_perf_90<-NULL
rf_tree_90<-NULL
rf_imp_90<-NULL
for (k in 1:n_times){
  for (i in 1:n_folds){
    rf_temp_com<-rf_tune_eval(training_90,test_90,i,k,seed,trc)
    rf_perf_90<-rbind(rf_perf_90,rf_temp_com[[1]])
    rf_tree_90<-rbind(rf_tree_90,rf_temp_com[[2]])
    rf_imp_90<-rbind(rf_imp_90,rf_temp_com[[3]])
  }
}
rf_perf_90$Days_before<-"90"
```

Results
-------

### Prediction model evaluation

``` r
AUC <- rbind(glm_perf_90,nb_perf_90,rf_perf_90) %>% dplyr::select(Model,Days_before,folds,times,AUC) %>% unique()
AUCDF<-AUC %>% group_by(Model,Days_before) %>% 
  summarise(Count=n(),Mean=round(mean(AUC),digit=3),
            SE=round(sd(AUC)/150,digit=4))
knitr::kable(AUCDF)
```

| Model | Days\_before |  Count|   Mean|      SE|
|:------|:-------------|------:|------:|-------:|
| GLM   | 90           |    150|  0.581|  0.0006|
| NB    | 90           |    150|  0.693|  0.0006|
| RF    | 90           |    150|  0.744|  0.0006|

``` r
summary(aov(AUC~Model,data=AUC[Days_before=="90"]))
```

    ##              Df Sum Sq Mean Sq F value              Pr(>F)    
    ## Model         2  2.093  1.0467   123.4 <0.0000000000000002 ***
    ## Residuals   447  3.792  0.0085                                
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="90"]))
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = AUC ~ Model, data = AUC[Days_before == "90"])
    ## 
    ## $Model
    ##             diff        lwr       upr     p adj
    ## NB-GLM 0.1123532 0.08734401 0.1373624 0.0000000
    ## RF-GLM 0.1632570 0.13824781 0.1882662 0.0000000
    ## RF-NB  0.0509038 0.02589460 0.0759130 0.0000069

``` r
rf_sen_75 <- rf_perf_90[sen>=0.70 & spe>0][order(sen)][, .SD[1], by=.(folds,times)]
rf_sen_75 %>% summarize(model="Rf",sen = round(mean(sen),3), spe=round(mean(spe),3), 
                        ppv=round(mean(ppv),3), npv=round(mean(npv),3))
```

    ##   model   sen   spe   ppv   npv
    ## 1    Rf 0.797 0.617 0.278 0.948

### Important features for breast cancer metastasis preduction

#### Mean Decrease Gini

``` r
knitr::kable(rf_imp_90[,.(MeanDecreaseGini=mean(MeanDecreaseGini)),by=(rn)][order(-MeanDecreaseGini)])
```

| rn           |  MeanDecreaseGini|
|:-------------|-----------------:|
| OPAge        |         2.1145143|
| CEA          |         1.6635891|
| CA153        |         0.9915120|
| HER2         |         0.9204111|
| Tissue.ER1   |         0.5292963|
| pNi3         |         0.3490936|
| pTi6         |         0.3103191|
| pTi5         |         0.2466921|
| pNi1         |         0.2196941|
| Tissue.PR1   |         0.1873364|
| pTi4         |         0.1851653|
| Tissue.ER3   |         0.1539886|
| pNi2         |         0.1527285|
| Tissue.PR2   |         0.1403161|
| Tissue.HER23 |         0.1226642|
| pTi7         |         0.1215140|
| Tissue.ER2   |         0.1097126|
| pTi3         |         0.1095534|
| Tissue.HER22 |         0.0916811|
| Tissue.PR3   |         0.0811416|
| pTi8         |         0.0727220|
| pTi2         |         0.0638162|
| pTi1         |         0.0549654|
| Tissue.HER21 |         0.0182607|
| pTiNA        |         0.0000000|

#### Number of times being the split variable

``` r
knitr::kable(rf_tree_90[,.N,by=`split var`][order(-N)])
```

| split var    |    N|
|:-------------|----:|
| NA           |  778|
| CEA          |   94|
| OPAge        |   76|
| HER2         |   68|
| CA153        |   65|
| pNi1         |   33|
| pNi3         |   31|
| pTi6         |   30|
| Tissue.ER1   |   27|
| Tissue.ER2   |   19|
| Tissue.ER3   |   17|
| Tissue.PR1   |   16|
| pTi5         |   16|
| pTi4         |   15|
| Tissue.HER22 |   15|
| pTi3         |   15|
| Tissue.HER23 |   15|
| pNi2         |   14|
| pTi1         |   13|
| Tissue.PR2   |   12|
| pTi2         |   11|
| pTi8         |    9|
| Tissue.PR3   |    8|
| pTi7         |    6|
| Tissue.HER21 |    3|

### The effect of time in metastasis prediction

#### 

``` r
summary(aov(AUC~Model,data=AUC[Days_before=="60"]))
TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="60"]))

summary(aov(AUC~Model,data=AUC[Days_before=="30"]))
TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="30"]))

summary(aov(AUC~Days_before,data=AUC[Model=="RF"]))
TukeyHSD(aov(AUC~Days_before,data=AUC[Model=="RF"]))
```
