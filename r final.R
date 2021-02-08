### install package ifrequired ###

        install.packages("data.table")
        install.packages("Rcpp")
        install.packages("curl")
        install.packages("pacman")
        library(pacman)
        install.packages("dplyr")
        install.packages("caret")
        install.packages("VIM")
        install.packages("Boruta")
        install.packages("mlBench")
        install.packages("randomForest")
        install.packages("DMwR")
        library(DMwR)
        library(randomForest)
        library(mlbench)
        library(Boruta)
        library(VIM)
        library(caret)
        library(dplyr)
        library(data.table)
        library(Rcpp)
        library(pacman)
        library(curl)
        p_load(ggplot2)
         p_load(plotly)
         
        
### download data files from source ###

        
        feature <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data')
              

### download labels from source ###

       
        label <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data')
        
        
### binding columns from label and feature ###
        
        x<-cbind(label,feature)       
        
        x<-as.data.frame(x)

  
        
### defining the columname class and time for first two column from the label data and feature in a sequential order ###
        
        colnames(x) <- c("Test", "Time", paste0(rep("Feature", ncol(feature)), seq(1,ncol(feature))))
        
        
###  defining label for values in class ###
        
        x$Test <- factor(x$Test, labels = c("pass", "fail"))

        
###  setting the time format ###
        
        x$Time <-  as.POSIXct(x$Time, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
        
### view data ###
        View(x)
        
        
###descriptive summary of data ##
        
        options(max.print=999999)
        summary(x)      
       data.frame(x)
### percentage of missing value in whole dataset ###
        
        sum(is.na(x))/prod(dim(x))*100
        
        
### percentage of missing value in each column ###
        
        nacol <- apply(x, 2, function(col)sum(is.na(col))/length(col))*100

###plot for na in column###
        
        plot_ly(x = seq(1,ncol(x)), y = nacol, type = "scatter", mode = "markers") %>%
          layout(title = "Variable Missing Values Percentage",
                 xaxis = list(title = "Variable"),
                 yaxis = list(title = "Percentage(%)"))
       
        View(nacol)
        
        
###percentage of missing value in rows ###
        
       
        narow <- apply(x, 1, function(row)sum(is.na(row))/length(row))*100

###plot for  na in rows###
        
        plot_ly(x = seq(1,nrow(x)), y = narow, type = "scatter", mode = "markers") %>%
          layout(title = "Observation Missing Values Percentage",
                 xaxis = list(title = "rownumber"),
                 yaxis = list(title = "Percentage(%)"))
        
        View(narow)


        
        
###maximum missing value in column ###
        max(nacol)
        
###maximum missing value in row ###
        max(narow)
        
###remove variables with missing values more than 55% ###      
        secom.clean<-x[ lapply( x, function(x) sum(is.na(x)) / length(x)*100 ) < 55 ]
        View(secom.clean)
        
### % of missing value in whole data after removing variables with more 55%na###
        
        sum(is.na(secom.clean))/prod(dim(secom.clean))*100
        

### percentage of missing value in each column after removing variable with  55%  nas ###
        
        secom.clean_nacol <- apply(secom.clean, 2, function(col)sum(is.na(col))/length(col))*100   
        View(secom.clean_nacol)
        
###max % missing value in column after removing variable with 55% nas ###
        
        max(secom.clean_nacol)

### plot for percentage of missing value in each column after removing variable with  55%  nas###
      
        plot_ly(x = seq(1,ncol(secom.clean)), y = secom.clean_nacol, type = "scatter", mode = "markers") %>%
          layout(title = "Variable Missing Values Percentage",
                 xaxis = list(title = "Variable"),
                 yaxis = list(title = "Percentage(%)"))
        
### percentage of missing value in each row after removing variable with  55%  nas ###        

        secom.clean.narow <- apply(secom.clean, 1, function(row)sum(is.na(row))/length(row))*100   
        View(secom.clean.narow)
        
###max % missing value in row after removing variable with 55% nas ###
        
        max(secom.clean.narow)
  
        
### plot for percentage of missing value in each row after removing variable with  55%  nas###
        
        plot_ly(x = seq(1,nrow(secom.clean)), y = secom.clean.narow, type = "scatter", mode = "markers") %>%
          layout(title = "Observation Missing Values Percentage",
                 xaxis = list(title = "rownumber"),
                 yaxis = list(title = "Percentage(%)"))   
        
### removing variables with zero variance ###
       
        
        zervar <- apply(secom.clean, 2, function(x) max(na.omit(x)) == min(na.omit(x)))
        View(zervar)
        secom.variance<-secom.clean[zervar==F]
        dim(secom.variance)
        
#### descriptive summary of the data after removing variables with na more than 55 % and zero variance ###
       
         summary(secom.variance)
         

         str(secom.variance)
         
         
### nas in variables   after removing variables with na more than 55 % and zero variance ###
         
         secom.variance_na <- sapply(secom.variance, function(x) sum(is.na(x)))
         View(secom.variance_na)
         
### max and min na in variables   after removing variables with na more than 55 % and zero variance ###
         min(secom.variance_na)
         max(secom.variance_na)

         
### imputing the data with knn ### 
         
         secom.imputed<-kNN(secom.variance,variable = colnames(secom.variance),k=499)
         
         dim(secom.imputed)
         head(secom.imputed)
### subsettng omitting extra variables created during imputation ###         
         secom.imputed<-subset(secom.imputed,select =1:452)
         head(secom.imputed)
         dim(secom.imputed)
         View(secom.imputed)
         summary(secom.imputed)
         sum(is.na(secom.imputed))
         
         
### subsetting data to scale/normalise ###
         
         secom_features<-subset(secom.imputed,select = 3:452)

         View(secom_features)        
         
### subsetting teast and date ###
        
         secom_test<-subset(secom.imputed,select = 1)
         View(secom_test)

### normalising  variable in range -1 to 1 ###
     
         range01 <- function(x){2*(x-min(x))/(max(x)-min(x))-1}
         secom.scale<-range01(secom_features)
         View(secom.scale)
         min(secom.scale)
         max(secom.scale)
         
#### feature selection using boruta ### 
        
         
         
###merge test and features ###         
         
         secom.merge<-cbind(secom_test,secom.scale)
         
         View(secom.merge)
         
         str(secom.merge)
         
### applying baruta feature selection ###         
         set.seed(1)
         
         secom.boruta<-Boruta(Test ~ .,data = secom.merge,doTrace=2,maxRuns =600) 
         print(secom.boruta)

### plot for important and unimportant variables found by baruta ###
        plot(secom.boruta,las=1,cex.axis=0.75)         
         plotImpHistory(secom.boruta)
         print(secom.boruta)
        attStats(secom.boruta)
        
### confirmed attributes by buruta ###
        
        getConfirmedFormula(secom.boruta)
         
### data partition ###
         
         set.seed(2)
         ind<-sample(2,nrow(secom.merge),replace = T,prob = c(.8,.2))
         
### train set ###
         train<-secom.merge[ind==1,]
         
### test set ###
         test<-secom.merge[ind==2,]
         
         
 ## Applying SMOTE function to Oversample Minority class(Fail) and Undersample majority class(Pass) 
         
         set.seed(3)
         data.smote <- SMOTE(Test~ .-Response , data=train, perc.over = 100, perc.under=400)
         table(data.smote$yield)
         View(data.smote)
         
         summary(data.smote$Test)
         summary(train$Test)
         
### random forest model ###
         set.seed(4)
         rf80<- randomForest(Test~.,data = data.smote)
         rf18<-randomForest(Test ~ Feature3 + Feature39 + Feature41 + Feature60 + Feature64 + 
                              Feature65 + Feature104 + Feature133 + Feature154 + Feature198 + 
                              Feature268 + Feature289 + Feature349 + Feature427 + Feature432 + 
                              Feature442 + Feature540 + Feature563,data = data.smote)
         
### prediction and confusion matrix ###
         prediction<-predict(rf80,test)
         confusionMatrix(prediction,test$Test)
         
### prediction and confusion matrix from data boruta find important ###
         
         prediction<-predict(rf18,test)
         confusionMatrix(prediction,test$Test)
         