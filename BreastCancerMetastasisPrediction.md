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
  arrange(ID,LabName,-Index_duration) %>% group_by(ID,LabName) %>% slice(1)
LabWide90d <- spread(Over90dData %>% select(ID,LabName,LabValue),key=LabName,value=LabValue)
LastRecordAfterOP_90<-inner_join(TimeIndepData,LabWide90d, by = "ID")
LastRecordAfterOP_90_rmna <- LastRecordAfterOP_90 %>% filter(complete.cases(LastRecordAfterOP_90))
LastRecordAfterOP_90_rmna_IsRe<-LastRecordAfterOP_90_rmna %>% filter(IsRe=="Recurrence")
LastRecordAfterOP_90_rmna_NotRe<-LastRecordAfterOP_90_rmna%>% filter(IsRe=="Non.Recurrence")
```

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
    set.seed(k+seed)
    glm_perf_90_tmp<-glm_tune_eval(training_90,test_90,i,k)
    glm_perf_90<-rbind(glm_perf_90,glm_perf_90_tmp)
  }
}
glm_perf_90$Days_before<-"90"
knitr::kable(head(glm_perf_90))
```

|  folds|  times|   tp|   fp|   tn|   fn|  sen|        spe|        ppv|        npv|        AUC| Model | Days\_before |
|------:|------:|----:|----:|----:|----:|----:|----------:|----------:|----------:|----------:|:------|:-------------|
|      1|      1|    0|    0|   42|    6|  0.0|  1.0000000|        NaN|  0.8750000|  0.5119048| GLM   | 90           |
|      1|      1|    3|   18|   24|    3|  0.5|  0.5714286|  0.1428571|  0.8888889|  0.5119048| GLM   | 90           |
|      1|      1|    3|   19|   23|    3|  0.5|  0.5476190|  0.1363636|  0.8846154|  0.5119048| GLM   | 90           |
|      1|      1|    3|   20|   22|    3|  0.5|  0.5238095|  0.1304348|  0.8800000|  0.5119048| GLM   | 90           |
|      1|      1|    3|   21|   21|    3|  0.5|  0.5000000|  0.1250000|  0.8750000|  0.5119048| GLM   | 90           |
|      1|      1|    3|   22|   20|    3|  0.5|  0.4761905|  0.1200000|  0.8695652|  0.5119048| GLM   | 90           |

### Model tuning and evaluation - Naive bayes

``` r
nb_perf_90<-NULL
for (k in 1:n_times){
  for (i in 1:n_folds){
    set.seed(k+seed)
    nb_perf_90_tmp<-nb_tune_eval(training_90,test_90,i,k)
    nb_perf_90<-rbind(nb_perf_90,nb_perf_90_tmp)
  }
}
nb_perf_90$Days_before<-"90"
knitr::kable(head(nb_perf_90))
```

|  folds|  times|   tp|   fp|   tn|   fn|        sen|        spe|        ppv|        npv|        AUC| Model | Days\_before |
|------:|------:|----:|----:|----:|----:|----------:|----------:|----------:|----------:|----------:|:------|:-------------|
|      1|      1|    0|    0|   42|    6|  0.0000000|  1.0000000|        NaN|  0.8750000|  0.6230159| NB    | 90           |
|      1|      1|    0|    1|   41|    6|  0.0000000|  0.9761905|  0.0000000|  0.8723404|  0.6230159| NB    | 90           |
|      1|      1|    1|    1|   41|    5|  0.1666667|  0.9761905|  0.5000000|  0.8913043|  0.6230159| NB    | 90           |
|      1|      1|    1|    2|   40|    5|  0.1666667|  0.9523810|  0.3333333|  0.8888889|  0.6230159| NB    | 90           |
|      1|      1|    1|    3|   39|    5|  0.1666667|  0.9285714|  0.2500000|  0.8863636|  0.6230159| NB    | 90           |
|      1|      1|    1|    4|   38|    5|  0.1666667|  0.9047619|  0.2000000|  0.8837209|  0.6230159| NB    | 90           |

### Model tuning and evaluation - random forest

``` r
rf_perf_90<-NULL
rf_tree_90<-NULL
rf_imp_90<-NULL
for (k in 1:n_times){
  for (i in 1:n_folds){
    set.seed(k+seed)
    rf_temp_com<-rf_tune_eval(training_90,test_90,i,k)
    rf_perf_90<-rbind(rf_perf_90,rf_temp_com[[1]])
    rf_tree_90<-rbind(rf_tree_90,rf_temp_com[[2]])
    rf_imp_90<-rbind(rf_imp_90,rf_temp_com[[3]])
  }
}
rf_perf_90$Days_before<-"90"
knitr::kable(head(rf_perf_90))
```

|  folds|  times|   tp|   fp|   tn|   fn|  sen|        spe|  ppv|        npv|        AUC| Model | Days\_before |
|------:|------:|----:|----:|----:|----:|----:|----------:|----:|----------:|----------:|:------|:-------------|
|      1|      1|    0|    0|   42|    6|    0|  1.0000000|  NaN|  0.8750000|  0.7579365| RF    | 90           |
|      1|      1|    0|    1|   41|    6|    0|  0.9761905|    0|  0.8723404|  0.7579365| RF    | 90           |
|      1|      1|    0|    2|   40|    6|    0|  0.9523810|    0|  0.8695652|  0.7579365| RF    | 90           |
|      1|      1|    0|    3|   39|    6|    0|  0.9285714|    0|  0.8666667|  0.7579365| RF    | 90           |
|      1|      1|    0|    4|   38|    6|    0|  0.9047619|    0|  0.8636364|  0.7579365| RF    | 90           |
|      1|      1|    0|    5|   37|    6|    0|  0.8809524|    0|  0.8604651|  0.7579365| RF    | 90           |

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
| GLM   | 90           |    150|  0.544|  0.0007|
| NB    | 90           |    150|  0.621|  0.0006|
| RF    | 90           |    150|  0.673|  0.0006|

``` r
summary(aov(AUC~Model,data=AUC[Days_before=="90"]))
```

    ##              Df Sum Sq Mean Sq F value              Pr(>F)    
    ## Model         2  1.253  0.6266   70.75 <0.0000000000000002 ***
    ## Residuals   447  3.959  0.0089                                
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
    ##              diff        lwr        upr     p adj
    ## NB-GLM 0.07667266 0.05111894 0.10222638 0.0000000
    ## RF-GLM 0.12846072 0.10290700 0.15401444 0.0000000
    ## RF-NB  0.05178806 0.02623435 0.07734178 0.0000076

``` r
rf_sen_75 <- rf_perf_90[sen>=0.70 & spe>0][order(sen)][, .SD[1], by=.(folds,times)]
rf_sen_75 %>% summarize(model="Rf",sen = round(mean(sen),3), spe=round(mean(spe),3), 
                        ppv=round(mean(ppv),3), npv=round(mean(npv),3))
```

    ##   model   sen   spe   ppv   npv
    ## 1    Rf 0.795 0.512 0.217 0.934

### Important features for breast cancer metastasis preduction

#### Mean Decrease Gini

``` r
knitr::kable(rf_imp_90[,.(MeanDecreaseGini=mean(MeanDecreaseGini)),by=(rn)][order(-MeanDecreaseGini)])
```

| rn           |  MeanDecreaseGini|
|:-------------|-----------------:|
| OPAge        |         2.3834134|
| CEA          |         1.3846393|
| CA153        |         1.1784172|
| HER2         |         0.9988766|
| Tissue.ER1   |         0.5652976|
| pNi3         |         0.4008728|
| pTi6         |         0.2946136|
| pTi5         |         0.2617994|
| pNi1         |         0.2362085|
| pTi4         |         0.1780165|
| Tissue.PR1   |         0.1741060|
| Tissue.ER3   |         0.1684177|
| pNi2         |         0.1541849|
| pTi7         |         0.1399497|
| Tissue.PR2   |         0.1316868|
| Tissue.ER2   |         0.1183170|
| Tissue.HER23 |         0.1182435|
| pTi3         |         0.1119263|
| Tissue.PR3   |         0.0985570|
| Tissue.HER22 |         0.0914043|
| pTi8         |         0.0880500|
| pTi2         |         0.0601897|
| pTi1         |         0.0441810|
| Tissue.HER21 |         0.0158987|
| pTiNA        |         0.0000000|

#### Number of times being the split variable

``` r
knitr::kable(rf_tree_90[,.N,by=`split var`][order(-N)])
```

| split var    |    N|
|:-------------|----:|
| NA           |  822|
| OPAge        |  113|
| CEA          |   85|
| HER2         |   67|
| CA153        |   61|
| pNi1         |   40|
| Tissue.ER1   |   35|
| pNi3         |   30|
| pTi6         |   29|
| pTi5         |   23|
| Tissue.PR1   |   23|
| pNi2         |   19|
| pTi4         |   18|
| Tissue.ER2   |   15|
| Tissue.HER23 |   15|
| Tissue.PR2   |   14|
| Tissue.ER3   |   13|
| pTi8         |   12|
| pTi3         |   12|
| pTi2         |   12|
| Tissue.HER22 |   11|
| pTi7         |   11|
| Tissue.PR3   |    8|
| pTi1         |    4|
| Tissue.HER21 |    2|

### The effect of time in metastasis prediction

``` r
summary(aov(AUC~Model,data=AUC[Days_before=="60"]))
TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="60"]))

summary(aov(AUC~Model,data=AUC[Days_before=="30"]))
TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="30"]))

summary(aov(AUC~Days_before,data=AUC[Model=="RF"]))
TukeyHSD(aov(AUC~Days_before,data=AUC[Model=="RF"]))
```
