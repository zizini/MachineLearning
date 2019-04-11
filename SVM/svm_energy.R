library(e1071)
library(xlsx)

filename <- file.choose()
enrg <- read.csv(filename, header=TRUE, sep=";")
View(enrg)

# Assign names to each of the attributes.
colnames(enrg) = c("RelativeCompactness", # Relative Compactness m^2
                   "SurfaceArea", # Surface Area m^2
                   "WallArea", # Wall Area m^2
                   "RoofArea", # Roof Area m^2
                   "OverallHeight",  # Overall Height m
                   "Orientation",  # Orientation 2:Nort 3:East 4:South 5:West
                   "GlazingArea", # Glazing Area 0% 10% 25% 40%
                   "GlazingAreaDistribution", # Glazing Area Distribution 1:Uniform  2:Nort 3:East 4:South 5:West
                   "HeatingLoad", # Heating Load kWh/m^2
                   "CoolingLoad") # Cooling Load kWh/m^2
dim(enrg)
head(enrg)
tail(enrg)
summary(enrg)

en<-enrg
summary(en)
en$RelativeCompactness<-as.character(en$RelativeCompactness)
en$RelativeCompactness<-as.numeric(sub(",", ".",en$RelativeCompactness))
en$SurfaceArea<-as.character(en$SurfaceArea)
en$SurfaceArea<-as.numeric(sub(",", ".",en$SurfaceArea))
en$WallArea<-as.character(en$WallArea)
en$WallArea<-as.numeric(sub(",", ".",en$WallArea))
en$RoofArea<-as.character(en$RoofArea)
en$RoofArea<-as.numeric(sub(",", ".",en$RoofArea))
en$OverallHeight<-as.character(en$OverallHeight)
en$OverallHeight<-as.numeric(sub(",", ".",en$OverallHeight))
en$HeatingLoad<-as.character(en$HeatingLoad)
en$HeatingLoad<-as.numeric(sub(",", ".",en$HeatingLoad))
en$CoolingLoad<-as.character(en$CoolingLoad)
en$CoolingLoad<-as.numeric(sub(",", ".",en$CoolingLoad))
summary(en)

summary(enrg$RelativeCompactness)
enrg$RelativeCompactness<-as.character(enrg$RelativeCompactness)
enrg$RelativeCompactness<-as.numeric(sub(",", ".",enrg$RelativeCompactness))
summary(enrg$RelativeCompactness)

summary(enrg$SurfaceArea)
enrg$SurfaceArea<-as.character(enrg$SurfaceArea)
enrg$SurfaceArea<-as.numeric(sub(",", ".",enrg$SurfaceArea))
summary(enrg$SurfaceArea)
enrg$SurfaceArea   <- enrg$SurfaceArea/808.5
summary(enrg$SurfaceArea)

summary(enrg$WallArea)
enrg$WallArea<-as.character(enrg$WallArea)
enrg$WallArea<-as.numeric(sub(",", ".",enrg$WallArea))
summary(enrg$WallArea)
enrg$WallArea   <- enrg$WallArea/416.5
summary(enrg$WallArea)

summary(enrg$RoofArea)
enrg$RoofArea<-as.character(enrg$RoofArea)
enrg$RoofArea<-as.numeric(sub(",", ".",enrg$RoofArea))
summary(enrg$RoofArea)
enrg$RoofArea   <- enrg$RoofArea/220.5
summary(enrg$RoofArea)

summary(enrg$OverallHeight)
enrg$OverallHeight<-as.character(enrg$OverallHeight)
enrg$OverallHeight<-as.numeric(sub(",", ".",enrg$OverallHeight))
summary(enrg$OverallHeight)
enrg$OverallHeight<- enrg$OverallHeight/7.0
summary(enrg$OverallHeight)

summary(enrg$Orientation)
enrg$Orientation<-as.character(enrg$Orientation)
enrg$Orientation<-as.numeric(sub(",", ".",enrg$Orientation))
summary(enrg$Orientation)
enrg$Orientation<- enrg$Orientation/5.0
summary(enrg$Orientation)
  
summary(enrg$GlazingArea)
enrg$GlazingArea<-as.character(enrg$GlazingArea)
enrg$GlazingArea<-as.numeric(sub(",", ".",enrg$GlazingArea))
summary(enrg$GlazingArea)


summary(enrg$GlazingAreaDistribution)
enrg$GlazingAreaDistribution<-as.character(enrg$GlazingAreaDistribution)
enrg$GlazingAreaDistribution<-as.numeric(sub(",", ".",enrg$GlazingAreaDistribution))
summary(enrg$GlazingAreaDistribution)
enrg$GlazingAreaDistribution<- enrg$GlazingAreaDistribution/5.0
summary(enrg$GlazingAreaDistribution)

summary(enrg$HeatingLoad)
enrg$HeatingLoad<-as.character(enrg$HeatingLoad)
enrg$HeatingLoad<-as.numeric(sub(",", ".",enrg$HeatingLoad))
summary(enrg$HeatingLoad)
enrg$HeatingLoad   <- enrg$HeatingLoad/43.10
summary(enrg$HeatingLoad)

summary(enrg$CoolingLoad)
enrg$CoolingLoad<-as.character(enrg$CoolingLoad)
enrg$CoolingLoad<-as.numeric(sub(",", ".",enrg$CoolingLoad))
summary(enrg$CoolingLoad)
enrg$CoolingLoad   <- enrg$CoolingLoad/48.03
summary(enrg$CoolingLoad)

summary(enrg)


enrg$H<-NULL
for (i in 1:nrow(enrg)){
   if( enrg$HeatingLoad[i]<= 0.3333333) {
     enrg$H[i]<-'LOW'
   }
   else if( enrg$HeatingLoad[i] > 0.6666666) {
    enrg$H[i]<-'HIGH'
   }
   else{
     enrg$H[i]<-'MEDIUM'
   }
} 

enrg$C<-NULL
for (i in 1:nrow(enrg)){
  if( enrg$CoolingLoad[i]<= 0.3333333) {
    enrg$C[i]<-'LOW'
  }
  else if( enrg$CoolingLoad[i] > 0.6666666) {
    enrg$C[i]<-'HIGH'
  }
  else{
    enrg$C[i]<-'MEDIUM'
  }
} 

enrg$output<-NULL
enrg$EnergyPerformance<-NULL
x=0
y=0
for (i in 1:nrow(enrg)){
  if( enrg$C[i] == enrg$H[i] ) {
    enrg$output[i]<-1
    x=x+1
    enrg$EnergyPerformance[i]<-enrg$C[i]
  }
  else{
    enrg$output[i]<-0
    y=y+1
    if((enrg$C[i]=='LOW' && enrg$H[i]=='MEDIUM')||(enrg$C[i]=='MEDIUM' && enrg$H[i]=='LOW'))
    {
      enrg$EnergyPerformance[i]<-'MEDIUM'
    }
    else
    {
      enrg$EnergyPerformance[i]<-'HIGH'
    }
  }
} 
x
y
enrg$EnergyPerformance <- as.factor(enrg$EnergyPerformance)

df <- data.frame(    AccuracyRadialTrain=numeric(),           AccuracyRadialTest=numeric(),               AccuracyPolynomialTrain=numeric(),         
                     AccuracyPolynomialTest=numeric(),        AccuracyLinearTrain=numeric(),              AccuracyLinearTest=numeric(),   
                     AccuracySigmoidTrain=numeric(),          AccuracySigmoidTest=numeric(), 
                     PrecisionRadial_train_HIGH=numeric(),    PrecisionRadial_train_MEDIUM=numeric() ,    PrecisionRadial_train_LOW=numeric(),
                     PrecisionRadial_test_HIGH=numeric(),     PrecisionRadial_test_MEDIUM=numeric(),      PrecisionRadial_test_LOW=numeric(), 
                     PrecisionPolynomial_train_HIGH=numeric(),PrecisionPolynomial_train_MEDIUM=numeric(), PrecisionPolynomial_train_LOW=numeric(),
                     PrecisionPolynomial_test_HIGH=numeric(), PrecisionPolynomial_test_MEDIUM=numeric(),  PrecisionPolynomial_test_LOW=numeric(), 
                     PrecisionLinear_train_HIGH=numeric(),    PrecisionLinear_train_MEDIUM=numeric(),     PrecisionLinear_train_LOW=numeric(),  
                     PrecisionLinear_test_HIGH=numeric(),     PrecisionLinear_test_MEDIUM=numeric(),      PrecisionLinear_test_LOW=numeric(), 
                     PrecisionSigmoid_train_HIGH=numeric(),   PrecisionSigmoid_train_MEDIUM=numeric(),    PrecisionSigmoid_train_LOW=numeric(),  
                     PrecisionSigmoid_test_HIGH=numeric(),    PrecisionSigmoid_test_MEDIUM=numeric(),     PrecisionSigmoid_test_LOW=numeric(),
                     RecallRadialTrain_HIGH=numeric(),        RecallRadialTrain_MEDIUM=numeric(),         RecallRadialTrain_LOW=numeric(),
                     RecallRadialTest_HIGH=numeric(),         RecallRadialTest_MEDIUM=numeric(),          RecallRadialTest_LOW=numeric(),
                     RecallPolynomialTrain_HIGH=numeric(),    RecallPolynomialTrain_MEDIUM=numeric(),     RecallPolynomialTrain_LOW=numeric(),
                     RecallPolynomialTest_HIGH=numeric(),     RecallPolynomialTest_MEDIUM=numeric(),      RecallPolynomialTest_LOW=numeric(),
                     RecallLinearTrain_HIGH=numeric(),        RecallLinearTrain_MEDIUM=numeric(),         RecallLinearTrain_LOW=numeric(),
                     RecallLinearTest_HIGH=numeric(),         RecallLinearTest_MEDIUM=numeric(),          RecallLinearTest_LOW=numeric(),
                     RecallSigmoidTrain_HIGH=numeric(),       RecallSigmoidTrain_MEDIUM=numeric(),        RecallSigmoidTrain_LOW=numeric(),
                     RecallSigmoidTest_HIGH=numeric(),        RecallSigmoidTest_MEDIUM=numeric(),         RecallSigmoidTest_LOW=numeric(),
                     F1RadialTrain_HIGH=numeric(),            F1RadialTrain_MEDIUM=numeric(),             F1RadialTrain_LOW=numeric(), 
                     F1RadialTest_HIGH=numeric(),             F1RadialTest_MEDIUM=numeric(),              F1RadialTest_LOW=numeric(),
                     F1PolynomialTrain_HIGH=numeric(),        F1PolynomialTrain_MEDIUM=numeric(),         F1PolynomialTrain_LOW=numeric(),  
                     F1PolynomialTest_HIGH=numeric(),         F1PolynomialTest_MEDIUM=numeric(),          F1PolynomialTest_LOW=numeric(),
                     F1LinearTrain_HIGH=numeric(),            F1LinearTrain_MEDIUM=numeric(),             F1LinearTrain_LOW=numeric(),
                     F1LinearTest_HIGH=numeric(),             F1LinearTest_MEDIUM=numeric(),              F1LinearTest_LOW=numeric(), 
                     F1SigmoidTrain_HIGH=numeric(),           F1SigmoidTrain_MEDIUM=numeric(),            F1SigmoidTrain_LOW=numeric(),
                     F1SigmoidTest_HIGH=numeric(),            F1SigmoidTest_MEDIUM=numeric(),             F1SigmoidTest_LOW=numeric())

pr <- data.frame(    seed=numeric(),  kernel=numeric(), cost=numeric(), gamma=numeric(), performance=numeric())

for (seed in c(2,50,356,2002,12345))
{
  #set seed
  set.seed(seed)
  
  #split 75% train & 25% test
  index <- 1:nrow(enrg)
  tmp <- sample(index, trunc(length(index)/4))
  test <- enrg[tmp,]
  train <- enrg[-tmp,]

  #unneeded cols
  train$HeatingLoad<-NULL
  train$CoolingLoad<-NULL
  train$H<-NULL
  train$C<-NULL
  train$output<-NULL

  test$HeatingLoad<-NULL
  test$CoolingLoad<-NULL
  test$H<-NULL
  test$C<-NULL
  test$output<-NULL

  
  #subsets of train and test sets
  x <- subset(train, select = -EnergyPerformance)
  y <- train$EnergyPerformance
  t<- subset(test, select = -EnergyPerformance)

  #tune svms (different kernels)
  # 0:linear | 1:polynomial | 2:radial | 3:sigmoid
  svm_tune_radial <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
  parameters <- data.frame(seed,  svm_tune_radial$best.model$kernel, svm_tune_radial$best.parameters$cost, svm_tune_radial$best.parameters$gamma, svm_tune_radial$best.performance)
  pr[nrow(pr) + 1,] = parameters
  svm_tune_radial
  
  svm_tune_polynomial <- tune(svm, train.x=x, train.y=y, kernel="polynomial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
  parameters <- data.frame(seed,  svm_tune_polynomial$best.model$kernel, svm_tune_polynomial$best.parameters$cost, svm_tune_polynomial$best.parameters$gamma, svm_tune_polynomial$best.performance)
  pr[nrow(pr) + 1,] = parameters
  svm_tune_polynomial
  
  svm_tune_linear <- tune(svm, train.x=x, train.y=y, kernel="linear", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
  parameters <- data.frame(seed,  svm_tune_linear$best.model$kernel, svm_tune_linear$best.parameters$cost, svm_tune_linear$best.parameters$gamma, svm_tune_linear$best.performance)
  pr[nrow(pr) + 1,] = parameters
  svm_tune_linear
  
  svm_tune_sigmoid<- tune(svm, train.x=x, train.y=y, kernel="sigmoid", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
  parameters <- data.frame(seed,  svm_tune_sigmoid$best.model$kernel, svm_tune_sigmoid$best.parameters$cost, svm_tune_sigmoid$best.parameters$gamma, svm_tune_sigmoid$best.performance)
  pr[nrow(pr) + 1,] = parameters
  svm_tune_sigmoid

  #svms' models
  svm_model_radial <- svm(x, y, kernel="radial", cost=svm_tune_radial$best.parameters$cost, gamma=svm_tune_radial$best.parameters$gamma)
  svm_model_polynomial <- svm(x, y, kernel="polynomial", cost=svm_tune_polynomial$best.parameters$cost, gamma= svm_tune_polynomial$best.parameters$gamma)
  svm_model_linear <- svm(x, y, kernel="linear", cost=svm_tune_linear$best.parameters$cost, gamma=svm_tune_linear$best.parameters$gamma)
  svm_model_sigmoid <- svm(x, y, kernel="sigmoid", cost=svm_tune_sigmoid$best.parameters$cost, gamma=svm_tune_sigmoid$best.parameters$gamma)

  summary(svm_model_radial)
  summary(svm_model_polynomial)
  summary(svm_model_linear)
  summary(svm_model_sigmoid)

  #predict train data
  pred_radial_train <- predict(svm_model_radial, x)
  pred_polynomial_train <- predict(svm_model_polynomial, x)
  pred_linear_train <- predict(svm_model_linear, x)
  pred_sigmoid_train <- predict(svm_model_sigmoid, x)

  #confusion matrices of train data
  cm_radial_train <-as.matrix(print(table(pred = pred_radial_train, true = train[,9])))
  cm_polynomial_train <-as.matrix(print(table(pred = pred_polynomial_train, true = train[,9])))
  cm_linear_train <-as.matrix(print(table(pred = pred_linear_train, true = train[,9])))
  cm_sigmoid_train <-as.matrix(print(table(pred = pred_sigmoid_train, true = train[,9])))

  #predict test data
  pred_radial_test <- predict(svm_model_radial, t)
  pred_polynomial_test <- predict(svm_model_polynomial, t)
  pred_linear_test <- predict(svm_model_linear, t)
  pred_sigmoid_test <- predict(svm_model_sigmoid, t)

  #confusion matrices of test data
  cm_radial_test <-as.matrix(print(table(pred = pred_radial_test, true = test[,9])))
  cm_polynomial_test <-as.matrix(print(table(pred = pred_polynomial_test, true = test[,9])))
  cm_linear_test <-as.matrix(print(table(pred = pred_linear_test, true = test[,9]))) 
  cm_sigmoid_test<-as.matrix(print(table(pred = pred_sigmoid_test, true = test[,9])))

  #train data accuracy
  n = sum(cm_radial_train) # number of instances
  diag = diag(cm_radial_train) # number of correctly classified instances per class 
  rowsums = apply(cm_radial_train, 1, sum) # number of instances per class
  colsums = apply(cm_radial_train, 2, sum) # number of predictions per class
  accuracy_radial_train = sum(diag) / n 
  precision_radial_train = diag / colsums 
  recall_radial_train = diag / rowsums 
  f1_radial_train = 2 * precision_radial_train * recall_radial_train / (precision_radial_train + recall_radial_train) 
    
  n = sum(cm_polynomial_train) # number of instances
  diag = diag(cm_polynomial_train) # number of correctly classified instances per class 
  rowsums = apply(cm_polynomial_train, 1, sum) # number of instances per class
  colsums = apply(cm_polynomial_train, 2, sum) # number of predictions per class
  accuracy_polynomial_train = sum(diag) / n 
  precision_polynomial_train = diag / colsums 
  recall_polynomial_train = diag / rowsums 
  f1_polynomial_train = 2 * precision_polynomial_train * recall_polynomial_train / (precision_polynomial_train + recall_polynomial_train) 
    
  n = sum(cm_linear_train) # number of instances
  diag = diag(cm_linear_train) # number of correctly classified instances per class 
  rowsums = apply(cm_linear_train, 1, sum) # number of instances per class
  colsums = apply(cm_linear_train, 2, sum) # number of predictions per class
  accuracy_linear_train = sum(diag) / n 
  precision_linear_train = diag / colsums 
  recall_linear_train = diag / rowsums 
  f1_linear_train = 2 * precision_linear_train * recall_linear_train / (precision_linear_train + recall_linear_train)

  n = sum(cm_sigmoid_train) # number of instances
  diag = diag(cm_sigmoid_train) # number of correctly classified instances per class 
  rowsums = apply(cm_sigmoid_train, 1, sum) # number of instances per class
  colsums = apply(cm_sigmoid_train, 2, sum) # number of predictions per class
  accuracy_sigmoid_train = sum(diag) / n 
  precision_sigmoid_train = diag / colsums 
  recall_sigmoid_train = diag / rowsums 
  f1_sigmoid_train = 2 * precision_sigmoid_train * recall_sigmoid_train / (precision_sigmoid_train + recall_sigmoid_train)
  
  #test
  n = sum(cm_radial_test) # number of instances
  diag = diag(cm_radial_test) # number of correctly classified instances per class 
  rowsums = apply(cm_radial_test, 1, sum) # number of instances per class
  colsums = apply(cm_radial_test, 2, sum) # number of predictions per class
  accuracy_radial_test = sum(diag) / n 
  precision_radial_test = diag / colsums 
  recall_radial_test = diag / rowsums 
  f1_radial_test = 2 * precision_radial_test * recall_radial_test / (precision_radial_test + recall_radial_test)
  
  n = sum(cm_polynomial_test) # number of instances
  diag = diag(cm_polynomial_test) # number of correctly classified instances per class 
  rowsums = apply(cm_polynomial_test, 1, sum) # number of instances per class
  colsums = apply(cm_polynomial_test, 2, sum) # number of predictions per class
  accuracy_polynomial_test = sum(diag) / n 
  precision_polynomial_test = diag / colsums 
  recall_polynomial_test = diag / rowsums 
  f1_polynomial_test = 2 * precision_polynomial_test * recall_polynomial_test / (precision_polynomial_test + recall_polynomial_test)

  n = sum(cm_linear_test) # number of instances
  diag = diag(cm_linear_test) # number of correctly classified instances per class 
  rowsums = apply(cm_linear_test, 1, sum) # number of instances per class
  colsums = apply(cm_linear_test, 2, sum) # number of predictions per class
  accuracy_linear_test = sum(diag) / n 
  precision_linear_test = diag / colsums 
  recall_linear_test = diag / rowsums 
  f1_linear_test = 2 * precision_linear_test * recall_linear_test / (precision_linear_test + recall_linear_test)
  
  n = sum(cm_sigmoid_test) # number of instances
  diag = diag(cm_sigmoid_test) # number of correctly classified instances per class 
  rowsums = apply(cm_sigmoid_test, 1, sum) # number of instances per class
  colsums = apply(cm_sigmoid_test, 2, sum) # number of predictions per class
  accuracy_sigmoid_test = sum(diag) / n 
  precision_sigmoid_test = diag / colsums 
  recall_sigmoid_test = diag / rowsums 
  f1_sigmoid_test = 2 * precision_sigmoid_test * recall_sigmoid_test / (precision_sigmoid_test + recall_sigmoid_test)

  unname(precision_radial_train)
  unname(precision_radial_test)
  unname(precision_polynomial_train)
  unname(precision_polynomial_test)
  unname(precision_linear_train)
  unname(precision_linear_test)
  unname(precision_sigmoid_train)
  unname(precision_sigmoid_test)
  
  unname(recall_radial_train)
  unname(recall_radial_test)
  unname(recall_polynomial_test)
  unname(recall_polynomial_train)
  unname(recall_linear_train)
  unname(recall_linear_test)
  unname(recall_sigmoid_train)
  unname(recall_sigmoid_test)
  
  unname(f1_radial_train)
  unname(f1_radial_test)
  unname(f1_polynomial_test)
  unname(f1_polynomial_train)
  unname(f1_linear_train)
  unname(f1_linear_test)
  unname(f1_sigmoid_train)
  unname(f1_sigmoid_test)
  
  data <-     c(accuracy_radial_train,        accuracy_radial_test,         accuracy_polynomial_train, 
                accuracy_polynomial_test,     accuracy_linear_train,        accuracy_linear_test, 
                accuracy_sigmoid_train,       accuracy_sigmoid_test, 
                precision_radial_train[1],    precision_radial_train[3],    precision_radial_train[2],    
                precision_radial_test[1],     precision_radial_test[3],     precision_radial_test[2],    
                precision_polynomial_train[1],precision_polynomial_train[3],precision_polynomial_train[2],
                precision_polynomial_test[1], precision_polynomial_test[3], precision_polynomial_test[2],
                precision_linear_train[1],    precision_linear_train[3],    precision_linear_train[2],
                precision_linear_test[1],     precision_linear_test[3],     precision_linear_test[2],
                precision_sigmoid_train[1],   precision_sigmoid_train[3],   precision_sigmoid_train[2],
                precision_sigmoid_test[1],    precision_sigmoid_test[3],    precision_sigmoid_test[2],
                recall_radial_train[1],       recall_radial_train[3],       recall_radial_train[2],     
                recall_radial_test[1],        recall_radial_test[3],        recall_radial_test[2],     
                recall_polynomial_train[1],   recall_polynomial_train[3],   recall_polynomial_train[2],
                recall_polynomial_test[1],    recall_polynomial_test[3],    recall_polynomial_test[2],
                recall_linear_train[1],       recall_linear_train[3],       recall_linear_train[2],
                recall_linear_test[1],        recall_linear_test[3],        recall_linear_test[2], 
                recall_sigmoid_train[1],      recall_sigmoid_train[3],      recall_sigmoid_train[2],
                recall_sigmoid_test[1],       recall_sigmoid_test[3],       recall_sigmoid_test[2],
                f1_radial_train[1],           f1_radial_train[3],           f1_radial_train[2],
                f1_radial_test[1],            f1_radial_test[3],            f1_radial_test[2],
                f1_polynomial_train[1],       f1_polynomial_train[3],       f1_polynomial_train[2],
                f1_polynomial_test[1],        f1_polynomial_test[3],        f1_polynomial_test[2],
                f1_linear_train[1],           f1_linear_train[3],           f1_linear_train[2],
                f1_linear_test[1],            f1_linear_test[3],            f1_linear_test[2],
                f1_sigmoid_train[1],          f1_sigmoid_train[3],          f1_sigmoid_train[2],
                f1_sigmoid_test[1],           f1_sigmoid_test[3],           f1_sigmoid_test[2]) 
  
  df[nrow(df) + 1,] = data
}

row.names(df) <- c('seed: 2','seed: 50', 'seed: 356','seed: 2002','seed: 12345')
write.xlsx(df, "C:/Users/User/Desktop/machine learning 2/mydata.xlsx")
write.xlsx(pr, "C:/Users/User/Desktop/machine learning 2/myparameters.xlsx") # 0:linear | 1:polynomial | 2:radial | 3:sigmoid
View(df)
View(pr)

plot(svm_model_radial, enrg, SurfaceArea ~ GlazingArea, slice = list(SurfaceArea = 3, GlazingArea = 4))