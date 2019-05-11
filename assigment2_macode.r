## This file is now treated as a dependency
## to h2o_final.r
## Paths must be set in that main file. 

library(tidyverse)
library(lubridate)
library(MASS)
library(ROCR)
library(caret)
library(ROSE)
library(mice)

source("./assigment2_functions_macode.r")

telco.dat <- read.csv("telco_train.csv")

str(telco.dat)

colnames(telco.dat) <- colnames(telco.dat) %>% tolower()

## Proportion complete rows and proportions per column
sum(complete.cases(telco.dat))/nrow(telco.dat)

pna.v <- apply(telco.dat, 2, prop_na) %>%
  sort(decreasing=TRUE)
pna.v <- pna.v[pna.v != 0]

## Summarize and dummize fin_state variable
## Can't delete the missing avg variables because
## I'd lose on all customers without a data
## subscription
summary(telco.dat[, names(pna.v)])

## Fin state corresponds pretty much exactly to churn
cor(is.na(telco.dat$fin_state), telco.dat$churn)

##Removing fin_state: is collinear with outcome class
telco.dat <- telco.dat[, colnames(telco.dat) != "fin_state"]

## Survey of time until churn, year level? month level?
telco.dat$start_date <- ymd(telco.dat$start_date)
telco.dat$start_year <- year(telco.dat$start_date)

pchurn_year <- split(telco.dat$churn, telco.dat$start_year) %>%
  lapply(function(x) {
    pchurn = sum(x)/length(x)
    nobs = length(x)
    c(pchurn=pchurn, nobs=nobs)
  }) %>% do.call(what="rbind") %>% as.data.frame()

pchurn_year$year <- rownames(pchurn_year)
colnames(pchurn_year)[1] <- "pchurn"
rownames(pchurn_year) <- NULL

## Time in days since first observation
telco.dat$days_first <- telco.dat$start_date -
  min(telco.dat$start_date)

# ## Increase from about 0.1 to 0.3 through the years
plot(pchurn ~ year, data=pchurn_year, type="b", lwd=2)
lines(lowess(pchurn_year$year, pchurn_year$pchurn))

## Separate dataset into a training and test set
train.rows <- createDataPartition(telco.dat$churn, p=0.7, list=FALSE)
train.dat <- telco.dat[train.rows, ]
test.dat <- telco.dat[-train.rows, ]

## Remove date type variable
telco.dat <- telco.dat[, !colnames(telco.dat) %in% c("start_date",
                                                     "start_year")]
telco.dat$days_first <- as.numeric(telco.dat$days_first)

## Better performance when ID's are removed?
telco.dat <- telco.dat[, colnames(telco.dat) != "id"]

## Partition data into test set and training set (Fit full model
## on both sets once selected specification is tested).
## Isolate outcome variable from data sets

col.rm <- c("id", "start_date", "days_first")

train.dat <- train.dat[, !colnames(train.dat) %in% col.rm]
test.dat <- test.dat[, !colnames(test.dat) %in% col.rm]

##Impute avg_data_3month, count_connections_3month and
##avg_data_1month 

##No connection of avg_data_1month with prepaid
cor(is.na(telco.dat$avg_data_1month), telco.dat$prepaid)

##Prob with these three is that they are always missing
##at the same time. Indeed only two patterns (all there, all absent)
cols.imp <- c("avg_data_3month", "count_connections_3month",
              "avg_data_1month")

sapply(cols.imp, function(x) get_maxcors(x, train.dat))

md.pattern(train.dat[, cols.imp])
md.pattern(test.dat[, cols.imp])

##Find numeric rows with outliers
apply(train.dat, 2, function(x) sum(is_outlier(x, 10)))

##Median imputation, to avoid losing more information
train.mds <- apply(train.dat[, cols.imp], 2, median, na.rm=TRUE)
test.mds <- apply(test.dat[, cols.imp], 2, median, na.rm=TRUE)

trainrows.imp <- !complete.cases(train.dat[, cols.imp])
testrows.imp <- !complete.cases(test.dat[, cols.imp])

train.dat[trainrows.imp, cols.imp] <- train.mds
test.dat[testrows.imp, cols.imp] <- test.mds

## Normalize so that every data point is between 0 and 1
train.dat <- io_scale(data=train.dat)
test.dat <- io_scale(data=test.dat)

orig.dat <- train.dat

## Upsampling to correct for class imbalances (caret package)
train.dat <- upSample(x=dplyr::select(train.dat, -churn),
                      y=factor(train.dat$churn),
                      yname='churn')

## Remove Churn from dataset
Y.train <- train.dat$churn %>% factor(levels=c(0, 1),
                                      labels=c("No", "Yes"))
Y.orig <- orig.dat$churn %>% factor(levels=c(0, 1),
                                    labels=c("No", "Yes"))
Y.test <- test.dat$churn %>% factor(levels=c(0, 1),
                                    labels=c("No", "Yes"))

train.dat <- dplyr::select(train.dat, -churn)
orig.dat <- dplyr::select(orig.dat, -churn)
test.dat <- dplyr::select(test.dat, -churn)

## Save the training and test datasets, along with the
## churn variable
write_rds(train.dat, './train_ma.rds')
write_rds(test.dat, './test_ma.rds')
write_rds(orig.dat, './orig_ma.rds')

write_rds(Y.train, './Y_train.rds')
write_rds(Y.test, './Y_test.rds')
write_rds(Y.orig, './Y_orig.rds')

