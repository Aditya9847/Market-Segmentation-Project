#Business Analytics Case Study -1 [Credit card Segmentation]

#Uploading Necessary packages
require(dplyr)
require(psych)
require(GPArotation)
require(writexl)
require(Hmisc)


#Importing ccgeneral data 
Credit_card <- read.csv("C:/Users/Aditya/Documents/R's/RBA/R Business Analytics Case Study1/CC GENERAL.csv")


#Selecting variables for factor analysis
vars<-c("BALANCE","BALANCE_FREQUENCY",
        "PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
        "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY",
        "PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY",
        "CASH_ADVANCE_TRX","PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",
        "MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE")

Credit_card2 <- Credit_card[vars]

###Data preparation###
#Deriving necessary KPI's as new columns in Credit Card Data

#(1) Monthly average purchase and cash advance amount
Credit_card2$MONTHLY_AVG_PURCHASE <- Credit_card2$PURCHASES * Credit_card2$PURCHASES_FREQUENCY
Credit_card2$AVG_CASH_ADV_AMT <- Credit_card2$CASH_ADVANCE * Credit_card2$CASH_ADVANCE_FREQUENCY

#(2) Purchases by type
Credit_card2$PURCHASE_TYPE <- ifelse(Credit_card2$ONEOFF_PURCHASES>0 & 
                                     Credit_card2$INSTALLMENTS_PURCHASES==0,0,
                                     ifelse(Credit_card2$ONEOFF_PURCHASES==0 & 
                                     Credit_card2$INSTALLMENTS_PURCHASES >0,1,2))

#(3)Balance to credit limit ratio
Credit_card2$BALTOCREDIT_RATIO <-Credit_card2$BALANCE/Credit_card2$CREDIT_LIMIT

#(4)Payments to minimum payments ratio
Credit_card2$PAY_TO_MINPAY_RATIO <-Credit_card2$PAYMENTS/Credit_card2$MINIMUM_PAYMENTS

#Creating User Defined Function to calculate basic statistics & Outliers
diag_stats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+2*s
  LC <- m-2*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

#Descriptive Statistics of Credit_card data
desc_statistics<-t(data.frame(sapply(Credit_card2,diag_stats)))
desc_statistics <- data.frame(desc_statistics)

#Exporting the descriptive stats of Credit_card
write_xlsx(desc_statistics,"C:/Users/Aditya/Documents/R's/RBA/R Business Analytics Case Study1/Diag_stats.xlsx")

#Data cleaning
#Outliers Treatment
Credit_card2$BALANCE[Credit_card2$BALANCE>5727.538587]<-5727.538587
Credit_card2$BALANCE_FREQUENCY[Credit_card2$BALANCE_FREQUENCY>1.3510787]<-1.3510787
Credit_card2$PURCHASES[Credit_card2$PURCHASES>5276.474397]<-5276.474397
Credit_card2$ONEOFF_PURCHASES[Credit_card2$ONEOFF_PURCHASES>3912.213206]<-3912.213206
Credit_card2$INSTALLMENTS_PURCHASES[Credit_card2$INSTALLMENTS_PURCHASES>2219.743875]<-2219.743875
Credit_card2$CASH_ADVANCE[Credit_card2$CASH_ADVANCE>5173.198866]<-5173.198866
Credit_card2$ONEOFF_PURCHASES_FREQUENCY[Credit_card2$ONEOFF_PURCHASES_FREQUENCY>0.799129814]<-0.799129814
Credit_card2$CASH_ADVANCE_FREQUENCY[Credit_card2$CASH_ADVANCE_FREQUENCY>0.535386977]<-0.535386697
Credit_card2$CASH_ADVANCE_TRX[Credit_card2$CASH_ADVANCE_TRX>16.8981203]<-16.8981203
Credit_card2$PURCHASES_TRX[Credit_card2$PURCHASES_TRX>64.4251306]<-64.4251306
Credit_card2$CREDIT_LIMIT[Credit_card2$CREDIT_LIMIT>11772.09]<-11772.09
Credit_card2$PAYMENTS[Credit_card2$PAYMENTS>7523.26]<-7523.26
Credit_card2$MINIMUM_PAYMENTS[Credit_card2$MINIMUM_PAYMENTS>5609.1065423]<-5609.1065423
Credit_card2$PRC_FULL_PAYMENT[Credit_card2$PRC_FULL_PAYMENT>0.738713]<-0.738713
Credit_card2$TENURE[Credit_card2$TENURE>14.19398]<-14.19398
Credit_card2$MONTHLY_AVG_PURCHASE[Credit_card2$MONTHLY_AVG_PURCHASE>4872.402] <- 4872.402
Credit_card2$AVG_CASH_ADV_AMT[Credit_card2$AVG_CASH_ADV_AMT>2964.300] <- 2964.300
Credit_card2$BALTOCREDIT_RATIO[Credit_card2$BALTOCREDIT_RATIO >1.16837] <- 1.16837
Credit_card2$PAY_TO_MINPAY_RATIO[Credit_card2$PAY_TO_MINPAY_RATIO >249.923] <- 249.923

#Checking for outliers again
desc_statistics2<-t(data.frame(sapply(Credit_card2,diag_stats)))

#Checking for missing values
sum(is.na(Credit_card2))
summary(Credit_card2)

# Imputing missing values with mean
Credit_card2$CREDIT_LIMIT[which(is.na(Credit_card2$CREDIT_LIMIT))] <- 4494.44
Credit_card2$MINIMUM_PAYMENTS[which(is.na(Credit_card2$MINIMUM_PAYMENTS))] <- 864.20654
Credit_card2$BALTOCREDIT_RATIO[which(is.na(Credit_card2$BALTOCREDIT_RATIO))] <- 0.388926
Credit_card2$PAY_TO_MINPAY_RATIO[which(is.na(Credit_card2$PAY_TO_MINPAY_RATIO))] <- 9.350070


#Rounding off the data to 3 decimal places
Credit_card2 = round(Credit_card2,3)

#Factor analysis 
#CORRELATION MATRIX
corrm = cor(Credit_card2)

corrm = data.frame(corrm)

#Exporting Correlation matrix
require(writexl)
write.csv(corrm,"C:/Users/Aditya/Documents/R's/RBA/R Business Analytics Case Study1/corrm.csv")

#DECIDING NUMBER OF FACTORS USING SCREE PLOT AND EIGEN VALUES
#SCREE PLOT
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) 

#CALCULATING EIGEN VALUES & VARIANCE
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       ,pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       ,cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))   

eigen_values <- round(eigen_values,4)

#EXPORTING EIGEN VALUE SUMMARY
write.csv(eigen_values,"C:/Users/Aditya/Documents/R's/RBA/R Business Analytics Case Study1/Eigen.csv")


#CONDUCTING A 6 FACTOR ANALYSIS BASED ON RESULTS OF EIGEN VALUES
FA<-fa(r=corrm,6, rotate="varimax", fm="ml")               
print(FA)  

#Reordering
FA_SORT<-fa.sort(FA)    

#LISTING OUT THE OBJECTS
ls(FA_SORT)     
FA_SORT$loadings

#Saving and exporting the loadings
Loadings <- data.frame(FA_SORT$loadings[1:ncol(Credit_card2),])

#EXPORTING LOADINGS INTO EXCEL FOR VARIABLE ANALYSIS
write.csv(Loadings,"C:/Users/Aditya/Documents/R's/RBA/R Business Analytics Case Study1/Loadings.csv")

#----------------------------Cluster analysis --------------------------------------#

#Preparing final Data
#Creating a vector of selected variables for clustering
 
Selected_vars <- c("ONEOFF_PURCHASES",
                    "MONTHLY_AVG_PURCHASE",
                    "ONEOFF_PURCHASES_FREQUENCY",
                    "PAYMENTS",
                    "AVG_CASH_ADV_AMT",
                    "CASH_ADVANCE_TRX",
                    "CASH_ADVANCE",
                    "PURCHASES_INSTALLMENTS_FREQUENCY",
                    "BALTOCREDIT_RATIO",
                    "MINIMUM_PAYMENTS", 
                    "INSTALLMENTS_PURCHASES",
                    "CREDIT_LIMIT")

#standardizing the data
Credit_card3 <- data.frame(scale(Credit_card2[Selected_vars]))

#Building clusters using k-means clustering 
cluster_three <- kmeans(Credit_card3,3)
cluster_four <- kmeans(Credit_card3,4)
cluster_five <- kmeans(Credit_card3,5)
cluster_six <- kmeans(Credit_card3,6)


clust_data1<-cbind(Credit_card2,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster
                   ,km_clust_5=cluster_five$cluster,km_clust_6=cluster_six$cluster)


clust_data1<-data.frame(clust_data1)

#----------------------------Profiling --------------------------------------#

#Converting into factors
clust_data1$km_clust_3=factor(clust_data1$km_clust_3)
clust_data1$km_clust_4=factor(clust_data1$km_clust_4)
clust_data1$km_clust_5=factor(clust_data1$km_clust_5)
clust_data1$km_clust_6=factor(clust_data1$km_clust_6)

#preparing profiling sheet

require("tables")
profile<-tabular(1+BALANCE+BALANCE_FREQUENCY+PURCHASES+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES
                 +CASH_ADVANCE+PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY
                 +CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+PURCHASES_TRX+CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+
                 PRC_FULL_PAYMENT+TENURE+MONTHLY_AVG_PURCHASE+AVG_CASH_ADV_AMT+PURCHASE_TYPE+BALTOCREDIT_RATIO+PAY_TO_MINPAY_RATIO~mean+(mean*km_clust_3)+(mean*km_clust_4)
                 +(mean*km_clust_5)+(mean*km_clust_6),data=clust_data1)


colnames(Credit_card2)

profile1<-as.matrix(profile)

profile1<-data.frame(profile1)

profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)
                 +(length*km_clust_6),data=clust_data1)

profile2<-as.matrix(profile)
profile2<-data.frame(profile2)


#Exporting cluster_profiling sheet1
write.csv(profile1,"C:/Users/Aditya/Documents/R's/RBA/R Business Analytics Case Study1/Profile1.csv")
write.csv(profile2,"C:/Users/Aditya/Documents/R's/RBA/R Business Analytics Case Study1/Profile2.csv")
