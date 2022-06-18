library(DataExplorer)
library("readxl")
data<-read_excel("C:\\Users\\Brikena Kokalari\\Dropbox\\data2.xls", col_names = T)
data<-as.data.frame(data)

sapply(data,function(x) sum(is.na(x)))
data$job <- as.factor(data$job)
data$marital <- as.factor(data$marital)
data$education <- as.factor(data$education)
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)
data$contact <- as.factor(data$contact)
data$month <- as.factor(data$month)
data$day_of_week <- as.factor(data$day_of_week)
data$poutcome <- as.factor(data$poutcome)
data$SUBSCRIBED <- as.factor(data$SUBSCRIBED)


df<-data[, c("job", "marital", "education", "previous", "month",
             "cons.price.idx", "cons.conf.idx",
             "euribor3m", "nr.employed", "SUBSCRIBED")]

model <- glm(SUBSCRIBED ~.,family=binomial(link='logit'),data=df)
init<-as.data.frame(summary(model)$coefficients)

table(df$job)
table(df$marital)
table(df$education)
table(df$month)

str(df)
#numeric values
library(dplyr) 
library(purrr)
library(tidyr)
library(ggplot2)

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


par(mfrow=c(3,2))
ggplot(df) + geom_bar(aes(x = month))+
  xlab("Month") +
  ylab("Frequency") +
  ggtitle("Frequency of Months")

ggplot(df) + geom_bar(aes(x = job))+
  xlab("Jobs") +
  ylab("Frequency") +
  ggtitle("Frequency of Jobs type")

ggplot(df) + geom_bar(aes(x = marital))+
  xlab("Marital status") +
  ylab("Frequency") +
  ggtitle("Frequency of Marital Status")

ggplot(df) + geom_bar(aes(x = education))+
  xlab("Education Level") +
  ylab("Frequency") +
  ggtitle("Frequency of Education Level")




#shuffle rows
df <- df[sample(nrow(df)),]

k_split <- function(df, k) {
  folds <- split(sample(nrow(df), replace=F), as.factor(1:k))
  lapply(folds, function(idxs) df[idxs, ])
}

################## 10 splits ##########################################################

############### b coefficients ##################
coeffic_func = function(df) {
  coef(summary(glm(SUBSCRIBED ~.,family=binomial(link='logit'),data=df)))[, 1]
}
set.seed(18)

ten_fold<-k_split(df,10)
coef_10fold = as.data.frame(sapply(ten_fold, FUN=coeffic_func))

#mean per row
coef_10fold$coeff_mean <- rowMeans( coef_10fold[-1], na.rm=TRUE)


############# standard error ####################
se_func = function(df) {
  coef(summary(glm(SUBSCRIBED ~.,family=binomial(link='logit'),data=df)))[, 2]
}

se_10fold = sapply(ten_fold, FUN=se_func)
se_10fold<-as.data.frame(se_10fold)
#mean per row
se_10fold$se_mean <- rowMeans( se_10fold[-1], na.rm=TRUE)

################## 20 splits ###########################################################

set.seed(43)
twenty_fold<-k_split(df,20)

############### b coefficients ##################

coef_20fold = lapply(twenty_fold, FUN=coeffic_func)
nms  <- names(coef_20fold[[1]])
nms

coef_20fold<-do.call(cbind, lapply(coef_20fold,  function(X) X[nms]))
coef_20fold<-as.data.frame(coef_20fold)

#mean per row
coef_20fold$coeff20_mean <- rowMeans( coef_20fold[-1], na.rm=TRUE)

############# standard error ####################

se_20fold = lapply(twenty_fold, FUN=se_func)
nms  <- names(se_20fold[[1]])
nms

se_20fold<-do.call(cbind, lapply(se_20fold,  function(X) X[nms]))
se_20fold<-as.data.frame(se_20fold)

#mean per row
se_20fold$se20_mean <- rowMeans( se_20fold[-1], na.rm=TRUE)



#merge all the data

# stand. Error
mergedf<-merge(init, se_10fold,  by="row.names", all=T)
mergedf<-mergedf[, c("Row.names","Std. Error", "se_mean")]
mergedfse <- mergedf[,-1]
rownames(mergedfse) <- mergedf[,1]

mergedf<-merge(mergedfse, se_20fold,  by="row.names", all=T)
mergedf<-mergedf[, c("Row.names","Std. Error", "se_mean", "se20_mean")]
mergedfse <- mergedf[,-1]
rownames(mergedfse) <- mergedf[,1]

#coefficients
mergedf<-merge(init, coef_10fold,  by="row.names", all=T)
mergedf<-mergedf[, c("Row.names","Estimate", "coeff_mean")]
mergedfcoef <- mergedf[,-1]
rownames(mergedfcoef) <- mergedf[,1]

mergedf<-merge(mergedfcoef, coef_20fold,  by="row.names", all=T)
mergedf<-mergedf[, c("Row.names","Estimate", "coeff_mean", "coeff20_mean")]
mergedfcoef <- mergedf[,-1]
rownames(mergedfcoef) <- mergedf[,1]

######### weights #########

w10<-1/se_10fold
w20<-1/se_20fold


# weighted coefficients
weighted_coeff_10<-as.data.frame(do.call(cbind, w10*coef_10fold))
rownames(weighted_coeff_10) <- rownames(w10)

weighted_coeff_20<-as.data.frame(do.call(cbind, w20*coef_20fold))
rownames(weighted_coeff_20) <- rownames(w10)


weighted_coeff_10$wcoef10 <- rowSums(weighted_coeff_10[,1:10],
                                     na.rm=TRUE)/rowSums(w10[,1:10], na.rm=TRUE)
weighted_coeff_10$se_mean<-NULL

weighted_coeff_20$wcoef20 <- rowSums(weighted_coeff_20[,1:20],
                                     na.rm=TRUE)/rowSums(w20[,1:20], na.rm=TRUE)
weighted_coeff_20$se20_mean<-NULL
#weighted st. error

weighted_se_10<-as.data.frame(do.call(cbind, w10*se_10fold))
weighted_se_20<-as.data.frame(do.call(cbind, w20*se_20fold))
rownames(weighted_se_10) <- rownames(w10)
rownames(weighted_se_20) <- rownames(w10)


weighted_se_10$wse10 <- rowSums(weighted_se_10[1:10],
                                na.rm=TRUE)/(rowSums(w10[1:10], na.rm=TRUE))
weighted_se_10$se_mean<-NULL

weighted_se_20$wse20 <- rowSums(weighted_se_20[1:20],
                                na.rm=TRUE)/(rowSums(w20[1:20], na.rm=TRUE))
weighted_se_20$se20_mean<-NULL


#merge all the data

# stand. Error
mergedf<-merge(mergedfse, weighted_se_10,  by="row.names", all=T)
mergedf<-mergedf[, c("Row.names","Std. Error", "se_mean", "se20_mean", "wse10")]
mergedfse <- mergedf[,-1]
rownames(mergedfse) <- mergedf[,1]

mergedf<-merge(mergedfse, weighted_se_20,  by="row.names", all=T)
mergedf<-mergedf[, c("Row.names","Std. Error", "se_mean", "se20_mean", "wse10", "wse20")]
mergedfse <- mergedf[,-1]
rownames(mergedfse) <- mergedf[,1]

#coefficients
mergedf<-merge(mergedfcoef, weighted_coeff_10,  by="row.names", all=T)
mergedf<-mergedf[, c("Row.names","Estimate", "coeff_mean", "coeff20_mean", "wcoef10")]
mergedfcoef <- mergedf[,-1]
rownames(mergedfcoef) <- mergedf[,1]

mergedf<-merge(mergedfcoef, weighted_coeff_20,  by="row.names", all=T)
mergedf<-mergedf[, c("Row.names","Estimate", "coeff_mean", "coeff20_mean", "wcoef10", "wcoef20")]
mergedfcoef <- mergedf[,-1]
rownames(mergedfcoef) <- mergedf[,1]
#round the data frames

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

mergedfcoef<-round_df(mergedfcoef, 3)
mergedfse<-round_df(mergedfse, 3)

######## plot the data ########  
### standard error 
par(mfrow=c(1,2))
plot( 1:nrow(mergedfse), mergedfse[,1], xaxt="n",
     xlab="", ylab="", 
     main="Standard Error comparison")
legend(1,18 , legend=c("Initial data", "20 splits", "10 splits"),
       col=c("black", "red", "blue"), lty=1, cex=0.8)
axis(side=1, at=1:nrow(mergedfse),
     labels=mergedf$Row.names, las=2, cex.axis=0.5)
par(new=TRUE)
plot(1:nrow(mergedfse), mergedfse[,3], col="red", type="l", pch=1  ,xlab="" ,ylab="", axes = F, add=TRUE)
axis(side = 4)
par(new=TRUE)
plot(1:nrow(mergedfse), mergedfse[,2], col = "blue",type="l", pch=1, xlab="", ylab="", axes = F, add=TRUE)

### weighted standard error 
plot(1:nrow(mergedfse), mergedfse[,1], xaxt="n",
     xlab="", ylab="", 
     main="Weighted Standard Error comparison")
legend(1,18 , legend=c("Initial data", "20 splits", "10 splits"),
       col=c("black", "red", "blue"), lty=1, cex=0.8)
axis(side=1, at=1:nrow(mergedfse),
     labels=mergedf$Row.names, las=2, cex.axis=0.5)
par(new=TRUE)
plot(1:nrow(mergedfse), mergedfse[,5], col="red", type="l", pch=1,  xlab="" ,ylab="", axes = F, add=TRUE)
axis(side = 4)
par(new=TRUE)
plot(1:nrow(mergedfse), mergedfse[,4], col = "blue", type="l", pch=1, xlab="", ylab="", axes = F, add=TRUE)


### coefficients plot
par(mfrow=c(1,2))
plot(1:nrow(mergedfcoef), mergedfcoef[,1], xaxt="n",
     xlab="", ylab="", 
     main="Coefficients comparison")
legend(1,120 , legend=c("Initial data", "20 splits", "10 splits"),
       col=c("black", "red", "blue"), lty=1, cex=0.8)
axis(side=1, at=1:nrow(mergedfcoef),
     labels=mergedf$Row.names, las=2, cex.axis=0.5)
par(new=TRUE)
plot(1:nrow(mergedfcoef), mergedfcoef[,3], col="red",   xlab="" ,ylab="", axes = F, add=TRUE)
axis(side = 4)
par(new=TRUE)
plot(1:nrow(mergedfcoef), mergedfcoef[,2], col = "blue", xlab="", ylab="", axes = F, add=TRUE)

### weighted coefficients plot 
plot(1:nrow(mergedfcoef), mergedfcoef[,1], xaxt="n",
     xlab="", ylab="", 
     main="Weighted Coefficients comparison")
legend(1,120 , legend=c("Initial data", "20 splits", "10 splits"),
       col=c("black", "red", "blue"), lty=1, cex=0.8)
axis(side=1, at=1:nrow(mergedfcoef),
     labels=mergedf$Row.names, las=2, cex.axis=0.5)
par(new=TRUE)
plot(1:nrow(mergedfcoef), mergedfcoef[,5], col="red",   xlab="" ,ylab="", axes = F, add=TRUE)
axis(side = 4)
par(new=TRUE)
plot(1:nrow(mergedfcoef), mergedfcoef[,4], col = "blue", xlab="", ylab="", axes = F, add=TRUE)



library(ggalt)
library(dplyr)
library(magrittr)

#### Dumbbell for Standard Error
par(mfrow=c(1,2))
mergedfse %<>% mutate(diffse10 = se_mean - mergedfse[,1])
mergedfse %>% arrange(se_mean)
ggplot(mergedfse, aes(x=mergedfse[,1], xend=se_mean, y=(diffse10))) + 
  geom_dumbbell(color="purple", 
                size_x=3.5, 
                size_xend = 3.5,
                colour_x="red", 
                colour_xend = "blue")+
  labs(x=NULL, y=NULL, 
       title="Inital data and 10 splits Disparity of Standard Error", 
       subtitle="Difference between initial data (Red) and 10 splits (Blue)")


mergedfse %<>% mutate(diffse20 = se20_mean - mergedfse[,1])
mergedfse %>% arrange(se20_mean)
ggplot(mergedfse, aes(x=mergedfse[,1], xend=se20_mean, y=(diffse20))) + 
  geom_dumbbell(color="purple", 
                size_x=3.5, 
                size_xend = 3.5,
                colour_x="red", 
                colour_xend = "blue")+
  labs(x=NULL, y=NULL, 
       title="Inital data and 20 splits Disparity of Standard Error", 
       subtitle="Difference between initial data (Red) and 20 splits (Blue)")

mergedfse %<>% mutate(diffse10 =abs(diffse10 - mergedfse[,1]))
mergedfse %<>% mutate(diffse20 = abs(diffse20 - mergedfse[,1]))
mergedfse %<>% mutate(diffsew10 =abs(wse10 - mergedfse[,1]))
mergedfse %<>% mutate(diffsew20 = abs(wse20 - mergedfse[,1]))


#### Dumbbell for Coefficients
par(mfrow=c(1,2))
mergedfcoef %<>% mutate(diffcoef10 = coeff_mean - mergedfcoef[,1])
mergedfcoef %>% arrange(coeff_mean)
ggplot(mergedfcoef, aes(x=mergedfcoef[,1], xend=coeff_mean, y=(diffcoef10))) + 
  geom_dumbbell(color="purple", 
                size_x=3.5, 
                size_xend = 3.5,
                colour_x="red", 
                colour_xend = "blue")+
  labs(x=NULL, y=NULL, 
       title="Inital data and 10 splits Disparity of Coefficients", 
       subtitle="Difference between initial data (Red) and 10 splits (Blue)")


mergedfcoef %<>% mutate(diffcoef20 = coeff20_mean - mergedfcoef[,1])
mergedfcoef %>% arrange(coeff20_mean)
ggplot(mergedfcoef, aes(x=mergedfcoef[,1], xend=coeff20_mean, y=(diffcoef20))) + 
  geom_dumbbell(color="purple", 
                size_x=3.5, 
                size_xend = 3.5,
                colour_x="red", 
                colour_xend = "blue")+
  labs(x=NULL, y=NULL, 
       title="Inital data and 20 splits Disparity of Coefficients", 
       subtitle="Difference between initial data (Red) and 20 splits (Blue)")

mergedfcoef %<>% mutate(diffcoef10 = abs(coeff_mean - mergedfcoef[,1]))
mergedfcoef %<>% mutate(diffcoef20 = abs(coeff20_mean - mergedfcoef[,1]))
mergedfcoef %<>% mutate(diffcoefw10 = abs(wcoef10 - mergedfcoef[,1]))
mergedfcoef %<>% mutate(diffcoefw20 = abs(wcoef20 - mergedfcoef[,1]))
rownames(mergedfcoef) <- mergedf[,1]
rownames(mergedfse) <- mergedf[,1]
#sum of absolute difference
mapply(sum,mergedfcoef[,6:9])
mapply(sum,mergedfse[,6:9])

############ variance #################

#variance
#ONLY TO CHECK IF  STANDARD ERROR  IS THE SQUARE ROOT OF VARIANCE

w10s<-(1/se_10fold)^2
w20s<-(1/se_20fold)^2
weighted_se_10v<-as.data.frame(do.call(cbind, w10s*(se_10fold)^2))
weighted_se_20v<-as.data.frame(do.call(cbind, w20s*(se_20fold)^2))
rownames(weighted_se_10) <- rownames(w10)
rownames(weighted_se_20) <- rownames(w10)

weighted_se_10v$se_mean<-NULL
weighted_se_20v$se20_mean<-NULL
w10$se_mean<-NULL
w20$se20_mean<-NULL
weighted_se_10v$wvariance10 <- rowSums(weighted_se_10[1:10],
                                      na.rm=TRUE)/(rowSums(w10[1:10], na.rm=TRUE))^2

weighted_se_20v$wvariance10 <- rowSums(weighted_se_20[1:20],
                                      na.rm=TRUE)/(rowSums(w20[1:20], na.rm=TRUE))^2
