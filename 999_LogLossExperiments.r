##------------------------------------------------------------------
## Load the raw data files from the Kaggle website into Rdata files
## http://www.kaggle.com/c/tradeshift-text-classification
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/tradeshift/data/proc")


##------------------------------------------------------------------
## <function> logloss :: evaluation metric
##------------------------------------------------------------------
logloss <- function(y_act, y_pred)
{
    y_pred  <- pmax(1e-15, pmin(1-1e-15, y_pred))
    return(-mean((y_act == 1)*log(y_pred) + (y_act == 0)*log(1-y_pred)))
}


##******************************************************************
## Step 1:  Load data
##******************************************************************

##------------------------------------------------------------------
## load the train/test data
##------------------------------------------------------------------
load("001_trainLabels.Rdata")     ##  1700000 x 146


#   id y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24 y25 y26 y27 y28 y29 y30 y31 y32 y33
#1  1  0  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1
#2  2  0  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0
#3  3  0  0  1  0  0  0  0  0  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0


##******************************************************************
## Step 1:
##******************************************************************

##------------------------------------------------------------------
## isolate the actual values
##------------------------------------------------------------------
yact    <- trainLabels.raw[, -1]

##------------------------------------------------------------------
## create benchmark examples
##------------------------------------------------------------------

## all values set to zero
yhat.all_zeros  <- 0*yact

## all values set to 0.5
yhat.all_halves <- matrix(0.5, nrow=nrow(yact), ncol=ncol(yact))

## all values set to the mean of each column
yhat.col_mean   <- 0*yact
col_mean        <- apply(yact, 2, mean)
for (i in 1:ncol(yhat.col_mean)) {
    yhat.col_mean[,i] <- col_mean[i]
}

## all values set to the mean of each column
yhat.col_med   <- 0*yact
col_med        <- apply(yact, 2, median)
for (i in 1:ncol(yhat.col_med)) {
    yhat.col_med[,i] <- col_med[i]
}

##------------------------------------------------------------------
## compare benchmark examples
##------------------------------------------------------------------

## my estimate = 1.171752; leaderboard = 1.1722138
bm.all_zeros    <- mean(unlist(lapply(1:ncol(yhat.all_zeros), function(x){logloss(yact[,x], yhat.all_zeros[,x])})))

## my estimate = 0.6931472; leaderboard = 0.6931472
bm.all_halves   <- mean(unlist(lapply(1:ncol(yhat.all_halves), function(x){logloss(yact[,x], yhat.all_halves[,x])})))

## my estimate = 0.09420712; James King leaderboard = 0.0943305
bm.col_mean    <- mean(unlist(lapply(1:ncol(yhat.col_mean), function(x){logloss(yact[,x], yhat.col_mean[,x])})))

## my estimate = 1.047095
bm.col_med    <- mean(unlist(lapply(1:ncol(yhat.col_med), function(x){logloss(yact[,x], yhat.col_med[,x])})))


##------------------------------------------------------------------
## test the range of the evaluation metric
##  - use a uniform "actual" vector
##  - the "predicted" vector is unifrom, but varied in small increments
##  - logloss function yields the largest penalty when the predicted
##    value is opposite the actual value (i.e., yact=0 & ypred=1),
##    but the value is non-linear
##  - could attempt to maximize the -log( logloss )
##------------------------------------------------------------------


pred.vec    <- seq(0, 1, 0.01)
y_zero      <- yhat.all_zeros[,1]
res_zero    <- vector(,length=length(pred.vec))

for (i in 1:length(pred.vec))
{
    res_zero[i]  <- logloss(y_zero, y_zero+pred.vec[i])
}
plot(pred.vec, log(res_zero))






