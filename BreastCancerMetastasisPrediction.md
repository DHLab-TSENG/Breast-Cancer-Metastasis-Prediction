Predicting breast cancer metastasis by using clinicopathological data and machine learning technologies
================
Yi-Ju Tseng, Chuang-En Huang, Po-Yin Lai, Yu-Chen Sun, Chiao-Ni Wen, Hsin-Yao Wang, and Jang-Jih Lu

Set environment
---------------

### Load all librarys

``` r
library(data.table)
library(dplyr)
library(tidyr)
library(tableone)
```

### Load all functions

The ModelFunction.R can be download from [GitHub](https://github.com/DHLab-CGU/Breast-Cancer-Metastasis-Prediction/blob/master/ModelFunction.R).

``` r
source('ModelFunction.R')
```

Load and pre-process the data
-----------------------------

### Load breast cancer data for model development

The clinical data is available from the Ethics Committee of the Chang Gung Memorial Hospital for researchers who meet the criteria for access to confidential data.

``` r
TimeIndepData<-readRDS('TimeIndepData.rds')
TimeVariedData<-readRDS('TimeVariedData.rds')
```

### Convert T stage code

**Table 2.** Conversion table of T stages

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

### Breast cancer data quick view

``` r
# For privacy reasons, we drop identity column 
str(dplyr::select(TimeIndepData,-ID)) 
```

    ## Classes 'data.table' and 'data.frame':   205 obs. of  10 variables:
    ##  $ BRNDAT     : Date, format: "1957-01-04" "1964-02-08" ...
    ##  $ OPAge      : num  43.5 45.8 69.4 69.2 38 ...
    ##  $ pTi        : Factor w/ 10 levels "0","1","2","3",..: 8 6 7 7 7 1 6 7 9 7 ...
    ##  $ pNi        : Factor w/ 4 levels "0","1","2","3": 4 2 1 1 3 1 2 1 2 3 ...
    ##  $ pMi        : Factor w/ 1 level "0": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Tissue.ER  : Factor w/ 4 levels "0","1","2","3": 1 2 1 4 1 3 1 1 1 1 ...
    ##  $ Tissue.PR  : Factor w/ 4 levels "0","1","2","3": 1 2 1 3 1 2 3 1 1 3 ...
    ##  $ Tissue.HER2: Factor w/ 4 levels "0","1","2","3": 4 3 4 4 4 4 4 4 4 4 ...
    ##  $ IsRe       : Factor w/ 2 levels "Non.Recurrence",..: 2 1 2 1 1 1 2 1 1 1 ...
    ##  $ IndexDAT   : Date, format: "2003-02-21" "2016-09-14" ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "index")= int

``` r
# For privacy reasons, we drop identity column 
str(dplyr::select(TimeVariedData,-ID))
```

    ## Classes 'data.table' and 'data.frame':   4321 obs. of  4 variables:
    ##  $ LabName       : chr  "CA153" "CEA" "CA153" "CA153" ...
    ##  $ LabValue      : num  6.7 1.49 18.8 25.2 25.5 14.4 16.3 16.6 13.3 15.4 ...
    ##  $ RCVDAT        : Date, format: "2003-01-16" "2003-01-16" ...
    ##  $ Index_duration: num  36 36 2495 2273 2217 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

Patient characteristics
-----------------------

**Table 3.** Patient characteristics in case and control groups

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

Built the model to predict breast cancer metastasis at least 3 months in advance
--------------------------------------------------------------------------------

### Set parameters

``` r
seed<-3231
n_times<-50
n_folds<-3
trc<-trainControl(method = "cv", number = n_folds, 
                  classProbs=TRUE, summaryFunction = twoClassSummary)
```

### Get completed data 90 days before the index date

``` r
LastRecordAfterOP_90_rmna_IsRe<-getDataForModel(TimeVariedData,TimeIndepData,90,"Recurrence")
LastRecordAfterOP_90_rmna_NotRe<-getDataForModel(TimeVariedData,TimeIndepData,90,"Non.Recurrence")
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

### Model development and evaluation - logistic regression

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

### Model development and evaluation - Naive bayes

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

### Model development and evaluation - random forest

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

### Performance of predictive models

``` r
AUC <- rbind(glm_perf_90,nb_perf_90,rf_perf_90) %>% dplyr::select(Model,Days_before,folds,times,AUC) %>% unique()
AUCDF<-AUC %>% group_by(Model,Days_before) %>% 
  summarise(Count=n(),Mean=round(mean(AUC),digit=3),
            SE=round(sd(AUC)/n(),digit=4))
knitr::kable(AUCDF)
```

| Model | Days\_before |  Count|   Mean|      SE|
|:------|:-------------|------:|------:|-------:|
| GLM   | 90           |    150|  0.581|  0.0006|
| NB    | 90           |    150|  0.693|  0.0006|
| RF    | 90           |    150|  0.744|  0.0006|

``` r
aov90<-summary(aov(AUC~Model,data=AUC[Days_before=="90"]))
knitr::kable(aov90[[1]])
```

|           |   Df|    Sum Sq|    Mean Sq|   F value|  Pr(&gt;F)|
|-----------|----:|---------:|----------:|---------:|----------:|
| Model     |    2|  2.093365|  1.0466823|  123.3846|          0|
| Residuals |  447|  3.791940|  0.0084831|        NA|         NA|

``` r
Tukey90<-TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="90"]))
knitr::kable(Tukey90$Model)
```

|        |       diff|        lwr|        upr|      p adj|
|--------|----------:|----------:|----------:|----------:|
| NB-GLM |  0.1123532|  0.0873440|  0.1373624|  0.0000000|
| RF-GLM |  0.1632570|  0.1382478|  0.1882662|  0.0000000|
| RF-NB  |  0.0509038|  0.0258946|  0.0759130|  0.0000069|

``` r
rf_sen_75 <- rf_perf_90[sen>=0.70 & spe>0][order(sen)][, .SD[1], by=.(folds,times)]
rf_sen_75 %>% summarize(model="Rf",sen = round(mean(sen),3), spe=round(mean(spe),3), 
                        ppv=round(mean(ppv),3), npv=round(mean(npv),3)) %>% knitr::kable()
```

| model |    sen|    spe|    ppv|    npv|
|:------|------:|------:|------:|------:|
| Rf    |  0.797|  0.617|  0.278|  0.948|

### Important features for breast cancer metastasis prediction

#### Mean decrease Gini

``` r
knitr::kable(
  rf_imp_90[,.(MeanDecreaseGini=mean(MeanDecreaseGini)),by=(rn)][order(-MeanDecreaseGini)] %>% 
    head(10), 
  row.names=T)
```

|     | rn         |  MeanDecreaseGini|
|-----|:-----------|-----------------:|
| 1   | OPAge      |         2.1145143|
| 2   | CEA        |         1.6635891|
| 3   | CA153      |         0.9915120|
| 4   | HER2       |         0.9204111|
| 5   | Tissue.ER1 |         0.5292963|
| 6   | pNi3       |         0.3490936|
| 7   | pTi6       |         0.3103191|
| 8   | pTi5       |         0.2466921|
| 9   | pNi1       |         0.2196941|
| 10  | Tissue.PR1 |         0.1873364|

#### Number of times a variable became a split variable

``` r
knitr::kable(rf_tree_90[,.N,by=`split var`][order(-N)] %>% head(10),row.names=T)
```

|     | split var  |    N|
|-----|:-----------|----:|
| 1   | CEA        |   94|
| 2   | OPAge      |   76|
| 3   | HER2       |   68|
| 4   | CA153      |   65|
| 5   | pNi1       |   33|
| 6   | pNi3       |   31|
| 7   | pTi6       |   30|
| 8   | Tissue.ER1 |   27|
| 9   | Tissue.ER2 |   19|
| 10  | Tissue.ER3 |   17|

### Effect of time on metastasis prediction

#### Built and evaluate 60-day model

``` r
LastRecordAfterOP_60_rmna_IsRe<-getDataForModel(TimeVariedData,TimeIndepData,60,"Recurrence")
LastRecordAfterOP_60_rmna_NotRe<-getDataForModel(TimeVariedData,TimeIndepData,60,"Non.Recurrence")
nrow(LastRecordAfterOP_60_rmna_IsRe)
```

    ## [1] 19

``` r
nrow(LastRecordAfterOP_60_rmna_NotRe)
```

    ## [1] 125

``` r
datalist60<-generate3folds(LastRecordAfterOP_60_rmna_IsRe,LastRecordAfterOP_60_rmna_NotRe,seed)
training_60<-datalist60[[1]]
test_60<-datalist60[[2]]


glm_perf_60<-NULL
nb_perf_60<-NULL
rf_perf_60<-NULL
for (k in 1:n_times){
  for (i in 1:n_folds){
    glm_perf_60_tmp<-glm_tune_eval(training_60,test_60,i,k,seed,trc)
    glm_perf_60<-rbind(glm_perf_60,glm_perf_60_tmp)
  }
}
glm_perf_60$Days_before<-"60"

for (k in 1:n_times){
  for (i in 1:n_folds){
    nb_perf_60_tmp<-nb_tune_eval(training_60,test_60,i,k,seed,trc)
    nb_perf_60<-rbind(nb_perf_60,nb_perf_60_tmp)
  }
}
nb_perf_60$Days_before<-"60"

for (k in 1:n_times){
  for (i in 1:n_folds){
    rf_temp_com<-rf_tune_eval(training_60,test_60,i,k,seed,trc)
    rf_perf_60<-rbind(rf_perf_60,rf_temp_com[[1]])
  }
}
rf_perf_60$Days_before<-"60"
```

#### Built and evaluate 30-day model

``` r
LastRecordAfterOP_30_rmna_IsRe<-getDataForModel(TimeVariedData,TimeIndepData,30,"Recurrence")
LastRecordAfterOP_30_rmna_NotRe<-getDataForModel(TimeVariedData,TimeIndepData,30,"Non.Recurrence")
nrow(LastRecordAfterOP_30_rmna_IsRe)
```

    ## [1] 21

``` r
nrow(LastRecordAfterOP_30_rmna_NotRe)
```

    ## [1] 125

``` r
datalist30<-generate3folds(LastRecordAfterOP_30_rmna_IsRe,LastRecordAfterOP_30_rmna_NotRe,seed)
training_30<-datalist30[[1]]
test_30<-datalist30[[2]]

glm_perf_30<-NULL
nb_perf_30<-NULL
rf_perf_30<-NULL
for (k in 1:n_times){
  for (i in 1:n_folds){
    glm_perf_30_tmp<-glm_tune_eval(training_30,test_30,i,k,seed,trc)
    glm_perf_30<-rbind(glm_perf_30,glm_perf_30_tmp)
  }
}
glm_perf_30$Days_before<-"30"

for (k in 1:n_times){
  for (i in 1:n_folds){
    nb_perf_30_tmp<-nb_tune_eval(training_30,test_30,i,k,seed,trc)
    nb_perf_30<-rbind(nb_perf_30,nb_perf_30_tmp)
  }
}
nb_perf_30$Days_before<-"30"

for (k in 1:n_times){
  for (i in 1:n_folds){
    rf_temp_com<-rf_tune_eval(training_30,test_30,i,k,seed,trc)
    rf_perf_30<-rbind(rf_perf_30,rf_temp_com[[1]])
  }
}
rf_perf_30$Days_before<-"30"
```

#### Compare the pperformance of the 90-day, 60-day, and 30-day models

``` r
AUCTime <- rbind(glm_perf_90,nb_perf_90,rf_perf_90,
                 glm_perf_60,nb_perf_60,rf_perf_60,
                 glm_perf_30,nb_perf_30,rf_perf_30) %>% 
  dplyr::select(Model,Days_before,folds,times,AUC) %>% unique()
AUCDFTime<-AUCTime %>% group_by(Model,Days_before) %>% 
  summarise(Count=n(),Mean=round(mean(AUC),digit=3),
            SE=round(sd(AUC)/n(),digit=4))
knitr::kable(AUCDFTime)
```

| Model | Days\_before |  Count|   Mean|      SE|
|:------|:-------------|------:|------:|-------:|
| GLM   | 30           |    150|  0.608|  0.0010|
| GLM   | 60           |    150|  0.629|  0.0007|
| GLM   | 90           |    150|  0.581|  0.0006|
| NB    | 30           |    150|  0.762|  0.0008|
| NB    | 60           |    150|  0.787|  0.0005|
| NB    | 90           |    150|  0.693|  0.0006|
| RF    | 30           |    150|  0.783|  0.0007|
| RF    | 60           |    150|  0.794|  0.0005|
| RF    | 90           |    150|  0.744|  0.0006|

``` r
aov60<-summary(aov(AUC~Model,data=AUCTime[Days_before=="60"]))
knitr::kable(aov60[[1]])
```

|           |   Df|    Sum Sq|    Mean Sq|   F value|  Pr(&gt;F)|
|-----------|----:|---------:|----------:|---------:|----------:|
| Model     |    2|  2.610814|  1.3054071|  174.3157|          0|
| Residuals |  447|  3.347473|  0.0074888|        NA|         NA|

``` r
Tukey60<-TukeyHSD(aov(AUC~Model,data=AUCTime[Days_before=="60"]))
knitr::kable(Tukey60$Model)
```

|        |       diff|         lwr|        upr|      p adj|
|--------|----------:|-----------:|----------:|----------:|
| NB-GLM |  0.1578486|   0.1343508|  0.1813464|  0.0000000|
| RF-GLM |  0.1650696|   0.1415718|  0.1885674|  0.0000000|
| RF-NB  |  0.0072210|  -0.0162768|  0.0307188|  0.7502036|

``` r
aov30<-summary(aov(AUC~Model,data=AUCTime[Days_before=="30"]))
knitr::kable(aov30[[1]])
```

|           |   Df|    Sum Sq|    Mean Sq|   F value|  Pr(&gt;F)|
|-----------|----:|---------:|----------:|---------:|----------:|
| Model     |    2|  2.722357|  1.3611784|  78.96348|          0|
| Residuals |  447|  7.705419|  0.0172381|        NA|         NA|

``` r
Tukey30<-TukeyHSD(aov(AUC~Model,data=AUCTime[Days_before=="30"]))
knitr::kable(Tukey30$Model)
```

|        |       diff|         lwr|        upr|     p adj|
|--------|----------:|-----------:|----------:|---------:|
| NB-GLM |  0.1534249|   0.1177742|  0.1890755|  0.000000|
| RF-GLM |  0.1745346|   0.1388840|  0.2101852|  0.000000|
| RF-NB  |  0.0211097|  -0.0145409|  0.0567604|  0.345719|

``` r
aovRF<-summary(aov(AUC~Days_before,data=AUCTime[Model=="RF"]))
knitr::kable(aovRF[[1]])
```

|              |   Df|     Sum Sq|    Mean Sq|   F value|  Pr(&gt;F)|
|--------------|----:|----------:|----------:|---------:|----------:|
| Days\_before |    2|  0.2075296|  0.1037648|  11.45343|  0.0000141|
| Residuals    |  447|  4.0496913|  0.0090597|        NA|         NA|

``` r
TukeyRF<-TukeyHSD(aov(AUC~Days_before,data=AUCTime[Model=="RF"]))
knitr::kable(TukeyRF$Days_before)
```

|       |        diff|         lwr|         upr|      p adj|
|-------|-----------:|-----------:|-----------:|----------:|
| 60-30 |   0.0110637|  -0.0147815|   0.0369089|  0.5730428|
| 90-30 |  -0.0390046|  -0.0648498|  -0.0131594|  0.0012421|
| 90-60 |  -0.0500683|  -0.0759135|  -0.0242231|  0.0000201|

### Contact

Please feel free to contact `yjtseng [at] mail.cgu.edu.tw` or **open an issue on GitHub** if you have any questions.
