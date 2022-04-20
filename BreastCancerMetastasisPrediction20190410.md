Predicting breast cancer metastasis by using clinicopathological data and machine learning technologies
================
Yi-Ju Tseng, Chuan-En Huang, Chiao-Ni Wen, Po-Yin Lai, Min-Hsien Wu, Yu-Chen Sun, Hsin-Yao Wang, and Jang-Jih Lu

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
ALL_tableOneDF<-data.table(as.matrix(print(ALL_tableOne,nonnormal=c("CA153","CEA","HER2","Index_duration"))),keep.rownames = T)
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
| CA153 (median \[IQR\])           | 10.40 \[7.40, 13.80\]     | 15.00 \[9.60, 38.45\] | &lt;0.001 | nonnorm |
| CEA (median \[IQR\])             | 1.27 \[0.72, 1.89\]       | 2.91 \[1.72, 7.45\]   | &lt;0.001 | nonnorm |
| HER2 (median \[IQR\])            | 3.51 \[2.30, 4.46\]       | 3.70 \[2.30, 4.90\]   | 0.533     | nonnorm |
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

### Model development and evaluation - random forest

``` r
svm_perf_90<-NULL
for (k in 1:n_times){
  for (i in 1:n_folds){
    svm_temp_com<-svm_tune_eval(training_90,test_90,i,k,seed,trc)
    svm_perf_90<-rbind(svm_perf_90,svm_temp_com)
  }
}
svm_perf_90$Days_before<-"90"
```

Results
-------

### Performance of predictive models

``` r
AUC <- rbind(glm_perf_90,nb_perf_90,rf_perf_90,svm_perf_90) %>% dplyr::select(Model,Days_before,folds,times,AUC) %>% unique()

AUCDF<-AUC %>% group_by(Model,Days_before) %>% 
  summarise(Count=n(),Mean=round(mean(AUC),digit=3),
            Median=round(median(AUC),digit=3),
            SE=round(sd(AUC)/n(),digit=4),
            IQR=IQR(AUC),Max=max(AUC),Min=min(AUC))
knitr::kable(AUCDF)
```

| Model | Days\_before |  Count|   Mean|  Median|      SE|        IQR|        Max|        Min|
|:------|:-------------|------:|------:|-------:|-------:|----------:|----------:|----------:|
| GLM   | 90           |    150|  0.581|   0.591|  0.0006|  0.1205616|  0.8095238|  0.2670068|
| NB    | 90           |    150|  0.648|   0.663|  0.0007|  0.1124869|  0.8775510|  0.3333333|
| RF    | 90           |    150|  0.746|   0.756|  0.0006|  0.1402076|  0.9662698|  0.4959350|
| SVM   | 90           |    150|  0.645|   0.687|  0.0012|  0.1013322|  0.9430894|  0.0609756|

### Friedman's test

``` r
AUC90<-AUC[Days_before=="90"]
shapiro.test(AUC90$AUC)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  AUC90$AUC
    ## W = 0.95397, p-value = 0.0000000000009702

``` r
AUC90$Model<-factor(AUC90$Model)
AUC90$FoldTime<-factor(paste(AUC90$folds,AUC90$times))
friedman.test(AUC90$AUC,AUC90$Model,AUC90$FoldTime)
```

    ## 
    ##  Friedman rank sum test
    ## 
    ## data:  AUC90$AUC, AUC90$Model and AUC90$FoldTime
    ## Friedman chi-squared = 144.18, df = 3, p-value <
    ## 0.00000000000000022

``` r
posthoc.friedman.conover.test(AUC90$AUC,AUC90$Model,AUC90$FoldTime, p.adjust="bonferroni")
```

    ## 
    ##  Pairwise comparisons using Conover's test for a two-way 
    ##                     balanced complete block design 
    ## 
    ## data:  AUC90$AUC , AUC90$Model and AUC90$FoldTime 
    ## 
    ##     GLM                 NB                  RF                 
    ## NB  <0.0000000000000002 -                   -                  
    ## RF  <0.0000000000000002 <0.0000000000000002 -                  
    ## SVM <0.0000000000000002 <0.0000000000000002 <0.0000000000000002
    ## 
    ## P value adjustment method: bonferroni

``` r
posthoc.kruskal.conover.test(AUC90$AUC,AUC90$Model,p.adjust="bonferroni")
```

    ## Warning in posthoc.kruskal.conover.test.default(AUC90$AUC, AUC90$Model, :
    ## Ties are present. Quantiles were corrected for ties.

    ## 
    ##  Pairwise comparisons using Conover's-test for multiple  
    ##                          comparisons of independent samples 
    ## 
    ## data:  AUC90$AUC and AUC90$Model 
    ## 
    ##     GLM                  NB                RF               
    ## NB  0.000000010830415    -                 -                
    ## RF  < 0.0000000000000002 0.000000000000011 -                
    ## SVM 0.000000000000027    0.32              0.000000005444884
    ## 
    ## P value adjustment method: bonferroni

### repeated ANOVA

``` r
summary(aov(AUC~Model + Error(FoldTime/Model), data=AUC90))
```

    ## 
    ## Error: FoldTime
    ##            Df Sum Sq Mean Sq F value Pr(>F)
    ## Residuals 149  3.284 0.02204               
    ## 
    ## Error: FoldTime:Model
    ##            Df Sum Sq Mean Sq F value              Pr(>F)    
    ## Model       3  2.110  0.7034   54.13 <0.0000000000000002 ***
    ## Residuals 447  5.809  0.0130                                
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
with(AUC90, pairwise.t.test(AUC, Model, paired = TRUE, p.adjust.method = 'bonferroni'))
```

    ## 
    ##  Pairwise comparisons using paired t tests 
    ## 
    ## data:  AUC and Model 
    ## 
    ##     GLM                  NB                   RF           
    ## NB  0.00000014671        -                    -            
    ## RF  < 0.0000000000000002 < 0.0000000000000002 -            
    ## SVM 0.001                1.000                0.00000000085
    ## 
    ## P value adjustment method: bonferroni

### ANOVA

``` r
aov90<-summary(aov(AUC~Model,data=AUC[Days_before=="90"]))
knitr::kable(aov90[[1]])
```

|           |   Df|    Sum Sq|    Mean Sq|   F value|  Pr(&gt;F)|
|-----------|----:|---------:|----------:|---------:|----------:|
| Model     |    3|  2.110330|  0.7034434|  46.10843|          0|
| Residuals |  596|  9.092746|  0.0152563|        NA|         NA|

``` r
Tukey90<-TukeyHSD(aov(AUC~Model,data=AUC[Days_before=="90"]))
knitr::kable(Tukey90$Model)
```

|         |        diff|         lwr|         upr|      p adj|
|---------|-----------:|-----------:|-----------:|----------:|
| NB-GLM  |   0.0672350|   0.0304908|   0.1039793|  0.0000179|
| RF-GLM  |   0.1659432|   0.1291990|   0.2026874|  0.0000000|
| SVM-GLM |   0.0642997|   0.0275555|   0.1010440|  0.0000465|
| RF-NB   |   0.0987082|   0.0619639|   0.1354524|  0.0000000|
| SVM-NB  |  -0.0029353|  -0.0396795|   0.0338089|  0.9969146|
| SVM-RF  |  -0.1016435|  -0.1383877|  -0.0648993|  0.0000000|

### Sensitivity and Specificity

``` r
rf_sen_75 <- rf_perf_90[sen>=0.70 & spe>0][order(sen)][, .SD[1], by=.(folds,times)]
rf_sen_75 %>% summarize(model="Rf",sen = round(mean(sen),3), spe=round(mean(spe),3), 
                        ppv=round(mean(ppv),3), npv=round(mean(npv),3),
                        acc=round(mean(ACC),3)) %>% knitr::kable()
```

| model |    sen|    spe|    ppv|    npv|    acc|
|:------|------:|------:|------:|------:|------:|
| Rf    |  0.794|  0.621|  0.276|  0.947|  0.643|

``` r
rf_sen_YI <- rf_perf_90[order(Youden,decreasing = T)][, .SD[1], by=.(folds,times)]
rf_sen_YI %>% summarize(model="RF",Count=n(),sen = round(mean(sen),3), spe=round(mean(spe),3), 
                        ppv=round(mean(ppv),3), npv=round(mean(npv),3),
                        acc=round(mean(ACC),3),
                        auc=round(mean(AUC),3))%>% knitr::kable()
```

| model |  Count|    sen|   spe|   ppv|    npv|    acc|    auc|
|:------|------:|------:|-----:|-----:|------:|------:|------:|
| RF    |    150|  0.799|  0.71|  0.36|  0.965|  0.722|  0.746|

``` r
nb_sen_YI <- nb_perf_90[order(Youden,decreasing = T)][, .SD[1], by=.(folds,times)]
nb_sen_YI %>% summarize(model="NB",Count=n(),sen = round(mean(sen),3), spe=round(mean(spe),3), 
                        ppv=round(mean(ppv),3), npv=round(mean(npv),3),
                        acc=round(mean(ACC),3))%>% knitr::kable()
```

| model |  Count|    sen|    spe|    ppv|    npv|    acc|
|:------|------:|------:|------:|------:|------:|------:|
| NB    |    150|  0.641|  0.755|  0.378|  0.939|  0.741|

``` r
glm_sen_YI <- glm_perf_90[order(Youden,decreasing = T)][, .SD[1], by=.(folds,times)]
glm_sen_YI %>% summarize(model="GLM",Count=n(),sen = round(mean(sen),3), spe=round(mean(spe),3), 
                        ppv=round(mean(ppv),3), npv=round(mean(npv),3),
                        acc=round(mean(ACC),3))%>% knitr::kable()
```

| model |  Count|    sen|    spe|  ppv|    npv|    acc|
|:------|------:|------:|------:|----:|------:|------:|
| GLM   |    150|  0.529|  0.688|  NaN|  0.913|  0.667|

``` r
svm_sen_YI <- svm_perf_90[order(Youden,decreasing = T)][, .SD[1], by=.(folds,times)]
svm_sen_YI %>% summarize(model="SVM",Count=n(),sen = round(mean(sen),3), spe=round(mean(spe),3), 
                        ppv=round(mean(ppv),3), npv=round(mean(npv),3),
                        acc=round(mean(ACC),3))%>% knitr::kable()
```

| model |  Count|   sen|   spe|  ppv|    npv|    acc|
|:------|------:|-----:|-----:|----:|------:|------:|
| SVM   |    150|  0.68|  0.74|  NaN|  0.947|  0.733|

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
| 1   | OPAge      |         2.3359430|
| 2   | CEA        |         1.8221788|
| 3   | CA153      |         1.0387600|
| 4   | HER2       |         0.9781090|
| 5   | Tissue.ER1 |         0.5516304|
| 6   | pNi3       |         0.3814608|
| 7   | pTi6       |         0.3255934|
| 8   | pTi5       |         0.2496641|
| 9   | pNi1       |         0.2262818|
| 10  | pTi4       |         0.2009808|

#### Number of times a variable became a split variable

``` r
knitr::kable(rf_tree_90[,.N,by=`split var`][order(-N)] %>% head(10),row.names=T)
```

|     | split var  |    N|
|-----|:-----------|----:|
| 1   | CEA        |  113|
| 2   | OPAge      |  106|
| 3   | CA153      |   60|
| 4   | HER2       |   57|
| 5   | Tissue.ER1 |   41|
| 6   | pNi3       |   30|
| 7   | pTi6       |   25|
| 8   | pTi4       |   19|
| 9   | pNi2       |   19|
| 10  | pNi1       |   18|

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

#### Compare the performance of the 90-day, 60-day, and 30-day models

``` r
AUCTime <- rbind(glm_perf_90,nb_perf_90,rf_perf_90,
                 glm_perf_60,nb_perf_60,rf_perf_60,
                 glm_perf_30,nb_perf_30,rf_perf_30) %>% 
  dplyr::select(Model,Days_before,folds,times,AUC) %>% unique()
AUCDFTime<-AUCTime %>% group_by(Model,Days_before) %>% 
  summarise(Count=n(),Mean=round(mean(AUC),digit=3),
            Median=round(median(AUC),digit=3),
            SE=round(sd(AUC)/n(),digit=4),IQR=IQR(AUC))
knitr::kable(AUCDFTime)
```

| Model | Days\_before |  Count|   Mean|  Median|      SE|        IQR|
|:------|:-------------|------:|------:|-------:|-------:|----------:|
| GLM   | 30           |    150|  0.608|   0.605|  0.0010|  0.2471068|
| GLM   | 60           |    150|  0.629|   0.628|  0.0007|  0.1317820|
| GLM   | 90           |    150|  0.581|   0.591|  0.0006|  0.1205616|
| NB    | 30           |    150|  0.736|   0.723|  0.0008|  0.1280592|
| NB    | 60           |    150|  0.736|   0.782|  0.0009|  0.1288265|
| NB    | 90           |    150|  0.648|   0.663|  0.0007|  0.1124869|
| RF    | 30           |    150|  0.783|   0.774|  0.0008|  0.1749938|
| RF    | 60           |    150|  0.797|   0.798|  0.0005|  0.1032126|
| RF    | 90           |    150|  0.746|   0.756|  0.0006|  0.1402076|

#### Friedman's test

``` r
AUCTime60<-AUCTime[Days_before=="60"]
AUCTime60$Model<-factor(AUCTime60$Model)
AUCTime60$FoldTime<-factor(paste(AUCTime60$folds,AUCTime60$times))
friedman.test(AUCTime60$AUC,AUCTime60$Model,AUCTime60$FoldTime)
```

    ## 
    ##  Friedman rank sum test
    ## 
    ## data:  AUCTime60$AUC, AUCTime60$Model and AUCTime60$FoldTime
    ## Friedman chi-squared = 143.11, df = 2, p-value <
    ## 0.00000000000000022

``` r
posthoc.friedman.conover.test(AUCTime60$AUC,AUCTime60$Model,AUCTime60$FoldTime, p.adjust="bonferroni")
```

    ## 
    ##  Pairwise comparisons using Conover's test for a two-way 
    ##                     balanced complete block design 
    ## 
    ## data:  AUCTime60$AUC , AUCTime60$Model and AUCTime60$FoldTime 
    ## 
    ##    GLM                 NB                 
    ## NB <0.0000000000000002 -                  
    ## RF <0.0000000000000002 <0.0000000000000002
    ## 
    ## P value adjustment method: bonferroni

``` r
AUCTime30<-AUCTime[Days_before=="30"]
AUCTime30$Model<-factor(AUCTime30$Model)
AUCTime30$FoldTime<-factor(paste(AUCTime30$folds,AUCTime30$times))
friedman.test(AUCTime30$AUC,AUCTime30$Model,AUCTime30$FoldTime)
```

    ## 
    ##  Friedman rank sum test
    ## 
    ## data:  AUCTime30$AUC, AUCTime30$Model and AUCTime30$FoldTime
    ## Friedman chi-squared = 143.8, df = 2, p-value <
    ## 0.00000000000000022

``` r
posthoc.friedman.conover.test(AUCTime30$AUC,AUCTime30$Model,AUCTime30$FoldTime, p.adjust="bonferroni")
```

    ## 
    ##  Pairwise comparisons using Conover's test for a two-way 
    ##                     balanced complete block design 
    ## 
    ## data:  AUCTime30$AUC , AUCTime30$Model and AUCTime30$FoldTime 
    ## 
    ##    GLM                 NB                 
    ## NB <0.0000000000000002 -                  
    ## RF <0.0000000000000002 <0.0000000000000002
    ## 
    ## P value adjustment method: bonferroni

``` r
AUCTimeRF<-AUCTime[Model=="RF"]
shapiro.test(AUCTimeRF$AUC)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  AUCTimeRF$AUC
    ## W = 0.9871, p-value = 0.0005122

``` r
AUCTimeRF$Days_before<-factor(AUCTimeRF$Days_before)
AUCTimeRF$FoldTime<-factor(paste(AUCTimeRF$folds,AUCTimeRF$times))
friedman.test(AUCTimeRF$AUC,AUCTimeRF$Days_before,AUCTimeRF$FoldTime)
```

    ## 
    ##  Friedman rank sum test
    ## 
    ## data:  AUCTimeRF$AUC, AUCTimeRF$Days_before and AUCTimeRF$FoldTime
    ## Friedman chi-squared = 46.22, df = 2, p-value = 0.00000000009191

``` r
posthoc.friedman.conover.test(AUCTimeRF$AUC,AUCTimeRF$Days_before,AUCTimeRF$FoldTime, p.adjust="bonferroni")
```

    ## 
    ##  Pairwise comparisons using Conover's test for a two-way 
    ##                     balanced complete block design 
    ## 
    ## data:  AUCTimeRF$AUC , AUCTimeRF$Days_before and AUCTimeRF$FoldTime 
    ## 
    ##    30                  60                 
    ## 60 <0.0000000000000002 -                  
    ## 90 <0.0000000000000002 <0.0000000000000002
    ## 
    ## P value adjustment method: bonferroni

``` r
posthoc.kruskal.conover.test(AUCTimeRF$AUC,AUCTimeRF$Days_before, p.adjust="bonferroni")
```

    ## Warning in posthoc.kruskal.conover.test.default(AUCTimeRF$AUC, AUCTimeRF
    ## $Days_before, : Ties are present. Quantiles were corrected for ties.

    ## 
    ##  Pairwise comparisons using Conover's-test for multiple  
    ##                          comparisons of independent samples 
    ## 
    ## data:  AUCTimeRF$AUC and AUCTimeRF$Days_before 
    ## 
    ##    30     60      
    ## 60 1.0000 -       
    ## 90 0.0031 0.000096
    ## 
    ## P value adjustment method: bonferroni

### repeated ANOVA

``` r
summary(aov(AUC~Days_before + Error(FoldTime/Days_before), data=AUCTimeRF))
```

    ## 
    ## Error: FoldTime
    ##            Df Sum Sq  Mean Sq F value Pr(>F)
    ## Residuals 149  1.453 0.009754               
    ## 
    ## Error: FoldTime:Days_before
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Days_before   2 0.2025 0.10124   11.03 0.000024 ***
    ## Residuals   298 2.7364 0.00918                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
with(AUCTimeRF, pairwise.t.test(AUC, Days_before, paired = TRUE, p.adjust.method = 'bonferroni'))
```

    ## 
    ##  Pairwise comparisons using paired t tests 
    ## 
    ## data:  AUC and Days_before 
    ## 
    ##    30    60                 
    ## 60 0.804 -                  
    ## 90 0.029 <0.0000000000000002
    ## 
    ## P value adjustment method: bonferroni

#### ANOVA

``` r
aov60<-summary(aov(AUC~Model,data=AUCTime[Days_before=="60"]))
knitr::kable(aov60[[1]])
```

|           |   Df|    Sum Sq|    Mean Sq|   F value|  Pr(&gt;F)|
|-----------|----:|---------:|----------:|---------:|----------:|
| Model     |    2|  2.170698|  1.0853489|  90.96065|          0|
| Residuals |  447|  5.333636|  0.0119321|        NA|         NA|

``` r
Tukey60<-TukeyHSD(aov(AUC~Model,data=AUCTime[Days_before=="60"]))
knitr::kable(Tukey60$Model)
```

|        |       diff|        lwr|        upr|      p adj|
|--------|----------:|----------:|----------:|----------:|
| NB-GLM |  0.1074110|  0.0777503|  0.1370717|  0.0000000|
| RF-GLM |  0.1679602|  0.1382995|  0.1976209|  0.0000000|
| RF-NB  |  0.0605492|  0.0308885|  0.0902098|  0.0000065|

``` r
aov30<-summary(aov(AUC~Model,data=AUCTime[Days_before=="30"]))
knitr::kable(aov30[[1]])
```

|           |   Df|    Sum Sq|    Mean Sq|   F value|  Pr(&gt;F)|
|-----------|----:|---------:|----------:|---------:|----------:|
| Model     |    2|  2.451278|  1.2256391|  73.61158|          0|
| Residuals |  447|  7.442588|  0.0166501|        NA|         NA|

``` r
Tukey30<-TukeyHSD(aov(AUC~Model,data=AUCTime[Days_before=="30"]))
knitr::kable(Tukey30$Model)
```

|        |       diff|        lwr|        upr|     p adj|
|--------|----------:|----------:|----------:|---------:|
| NB-GLM |  0.1275599|  0.0925225|  0.1625972|  0.000000|
| RF-GLM |  0.1747265|  0.1396892|  0.2097638|  0.000000|
| RF-NB  |  0.0471666|  0.0121293|  0.0822040|  0.004701|

``` r
aovRF<-summary(aov(AUC~Days_before,data=AUCTime[Model=="RF"]))
knitr::kable(aovRF[[1]])
```

|              |   Df|     Sum Sq|    Mean Sq|   F value|  Pr(&gt;F)|
|--------------|----:|----------:|----------:|---------:|----------:|
| Days\_before |    2|  0.2024872|  0.1012436|  10.80166|  0.0000262|
| Residuals    |  447|  4.1897158|  0.0093730|        NA|         NA|

``` r
TukeyRF<-TukeyHSD(aov(AUC~Days_before,data=AUCTime[Model=="RF"]))
knitr::kable(TukeyRF$Days_before)
```

|       |        diff|         lwr|         upr|      p adj|
|-------|-----------:|-----------:|-----------:|----------:|
| 60-30 |   0.0137624|  -0.0125259|   0.0400506|  0.4354425|
| 90-30 |  -0.0365103|  -0.0627985|  -0.0102221|  0.0033620|
| 90-60 |  -0.0502727|  -0.0765609|  -0.0239844|  0.0000262|

### Contact

Please feel free to contact `yjtseng [at] nycu.edu.tw` or **open an issue on GitHub** if you have any questions.
