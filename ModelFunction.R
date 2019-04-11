library(ROCR)
library(caret)

getPerformance<-function(predClass,fold,time){
  predTable<-data.table(tp=predClass@tp[[1]],fp=predClass@fp[[1]],
                        tn=predClass@tn[[1]],fn=predClass@fn[[1]])
  temp<-data.table(folds=fold, times=time)
  predTable<-cbind(temp,predTable)
  predTable$sen <- predTable$tp/(predTable$fn+predTable$tp)
  predTable$spe <- predTable$tn/(predTable$fp+predTable$tn)
  predTable$ppv <- predTable$tp/(predTable$tp+predTable$fp)
  predTable$npv <- predTable$tn/(predTable$fn+predTable$tn)
  predTable$ACC <- (predTable$tn+predTable$tp)/(predTable$fn+predTable$tn+predTable$tp+predTable$fp)
  predTable$Youden <- 
    ((predTable$tn)/(predTable$tn+predTable$fp))+
    ((predTable$tp)/(predTable$tp+predTable$fn))-1
  AUCdata <- performance(predClass,"auc")
  predTable$AUC<-AUCdata@y.values[[1]]
  predTable
}

glm_tune_eval<-function(trainingData,testData,fold,time,seed,trcpar){
  trainingData<-data.table(trainingData)
  testData<-data.table(testData)
  # Tune logistic regression model
  set.seed(time+seed)
  Glm <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2,
               data = downSample(trainingData[training==fold],trainingData[training==fold]$IsRe),
               method = "glmStepAIC", metric = "ROC",
               family=binomial("logit"), 
               trControl = trcpar,trace = FALSE)
  # Evaluate logistic regression model
  Glm_pr <- predict(Glm, type="prob", newdata=testData[test==fold])[,2]
  Glm_pred <- prediction(Glm_pr,testData[test==fold]$IsRe)
  Glm_Performance<-getPerformance(Glm_pred,fold,time)
  Glm_Performance$Model<-"GLM"
  Glm_Performance
}
nb_tune_eval<-function(trainingData,testData,fold,time,seed,trcpar){
  trainingData<-data.table(trainingData)
  testData<-data.table(testData)
  # Tune model
  set.seed(time+seed)
  Nb <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2 ,
              data = downSample(trainingData[training==i],trainingData[training==i]$IsRe),
              method = "naive_bayes", metric = "ROC",
              trControl = trcpar)
  # Evaluate model
  Nb_pr <- predict(Nb, type="prob", newdata=testData[test==i])[,2]
  Nb_pred <- prediction(Nb_pr,testData[test==i]$IsRe)
  Nb_Performance<-getPerformance(Nb_pred,fold,time)
  Nb_Performance$Model<-"NB"
  Nb_Performance
}

svm_tune_eval<-function(trainingData,testData,fold,time,seed,trcpar){
  trainingData<-data.table(trainingData)
  testData<-data.table(testData)
  # Tune model
  set.seed(time+seed)
  SVM <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2 ,
              data = downSample(trainingData[training==i],trainingData[training==i]$IsRe),
              method = "svmRadial", metric = "ROC",
              trControl = trcpar)
  # https://stackoverflow.com/questions/20461476/svm-with-cross-validation-in-r-using-caret
  # Evaluate model
  SVM_pr <- predict(SVM, type="prob", newdata=testData[test==i])[,2]
  SVM_pred <- prediction(SVM_pr,testData[test==i]$IsRe)
  SVM_Performance<-getPerformance(SVM_pred,fold,time)
  SVM_Performance$Model<-"SVM"
  SVM_Performance
}


rf_tune_eval<-function(trainingData,testData,fold,time,seed,trcpar){
  trainingData<-data.table(trainingData)
  testData<-data.table(testData)
  # Tune model
  set.seed(time+seed)
  Rf <- train(IsRe ~  OPAge + pTi + pNi + Tissue.ER + Tissue.PR + Tissue.HER2 + CA153 + CEA + HER2,
              data = downSample(trainingData[training==i],trainingData[training==i]$IsRe),
              method = "rf", metric = "ROC", importance=TRUE,
              trControl = trcpar,
              tuneGrid = expand.grid(mtry = c(1:24)))
  # Evaluate model
  Rf_pr = predict(Rf, type="prob", newdata=testData[test==i])[,2]
  Rf_pred = prediction(Rf_pr,testData[test==i]$IsRe)
  Rf_Performance<-getPerformance(Rf_pred,fold,time)
  Rf_Performance$Model<-"RF"
  
  TREE<-data.table(randomForest::getTree(Rf$finalModel,labelVar = TRUE))
  TREE<-TREE[`split var`!='NA']
  TREE$folds<-fold
  TREE$times<-time
  
  import<-data.table(data.frame(Rf$finalModel$importance),keep.rownames = T)
  import$folds<-fold
  import$times<-time
  
  list(Perf=Rf_Performance,Tree=TREE,Importance=import)
}
generate3folds<-function(cleanDataRe,cleanDataNotRe,seed){
  smp_size_Re1 <- floor(0.34 * nrow(cleanDataRe))
  set.seed(seed)
  A_ind <- sample(seq_len(nrow(cleanDataRe)), size = smp_size_Re1)
  rmna_IsRe_A <- cleanDataRe[A_ind, ]
  rmna_IsRe_BC <- cleanDataRe[-A_ind, ]
  smp_size_Re2 <- floor(0.5 * nrow(rmna_IsRe_BC))
  set.seed(seed)
  B_ind <- sample(seq_len(nrow(rmna_IsRe_BC)), size = smp_size_Re2)
  rmna_IsRe_B <- rmna_IsRe_BC[B_ind, ]
  rmna_IsRe_C <- rmna_IsRe_BC[-B_ind, ]
  
  smp_size_NotRe1 <- floor(0.34 * nrow(cleanDataNotRe))
  set.seed(seed)
  A_ind <- sample(seq_len(nrow(cleanDataNotRe)), size = smp_size_NotRe1)
  rmna_NotRe_A <- cleanDataNotRe[A_ind, ]
  rmna_NotRe_BC <- cleanDataNotRe[-A_ind, ]
  smp_size_NotRe2 <- floor(0.5 * nrow(rmna_NotRe_BC))
  set.seed(seed)
  B_ind <- sample(seq_len(nrow(rmna_NotRe_BC)), size = smp_size_NotRe2)
  rmna_NotRe_B <- rmna_NotRe_BC[B_ind, ]
  rmna_NotRe_C <- rmna_NotRe_BC[-B_ind, ]
  
  ##Need to be optimized
  test_1<-rbind(rmna_IsRe_A,rmna_NotRe_A)
  test_2<-rbind(rmna_IsRe_B,rmna_NotRe_B)
  test_3<-rbind(rmna_IsRe_C,rmna_NotRe_C)
  
  training_1<-rbind(test_2,test_3)
  training_2<-rbind(test_1,test_3)
  training_3<-rbind(test_1,test_2)
  
  training_1$training<-"1"
  training_2$training<-"2"
  training_3$training<-"3"
  training_90<-rbind(training_1,training_2,training_3)
  
  test_1$test<-"1"
  test_2$test<-"2"
  test_3$test<-"3"
  test_90<-rbind(test_1,test_2,test_3)
  list(training=training_90,test=test_90)
}
getDataForModel<-function(timeVariedData,timeIndepData,days,isRe){
  OverndData <- timeVariedData %>% filter(Index_duration>days) %>% 
    arrange(ID,LabName,Index_duration) %>% group_by(ID,LabName) %>% slice(1)
  LabWidend <- spread(OverndData %>% dplyr::select(ID,LabName,LabValue),key=LabName,value=LabValue)
  LastRecordAfterOP_nday<-inner_join(timeIndepData,LabWidend, by = "ID")
  LastRecordAfterOP_nday_rmna <- LastRecordAfterOP_nday %>% filter(complete.cases(LastRecordAfterOP_nday) & IsRe==isRe)
  LastRecordAfterOP_nday_rmna
}