##------------------------------------------------------------------
## Load the raw data files from the Kaggle website into Rdata files
## http://www.kaggle.com/c/tradeshift-text-classification
##------------------------------------------------------------------

##------------------------------------------------------------------
## load libraries
##------------------------------------------------------------------
library(psych)
library(corrplot)

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

##------------------------------------------------------------------
## <function> :: gkTau
##------------------------------------------------------------------
## Compute (asymmetric) correlations for nominal variables.  The
## result of gkTau(a,b) implies a predicts b to a degree specified
## by the returned correlation value [0,1]
##------------------------------------------------------------------
gkTau    <- function(x, y) {
    
    ## convert into a joint probability contingency table
    Pij <- prop.table(table(x, y, useNA="ifany"))
    
    ## compute marginal probabilities
    Pi  <- apply(Pij, 1, sum)
    Pj  <- apply(Pij, 2, sum)
    
    ## compute V(y)
    Vy  <- 1 - sum(Pj^2)
    
    ## compute V(x|y)
    innerSum  <- apply(Pij^2, 1, sum)
    Vybarx    <- 1 - sum(innerSum/Pi)
    
    ## compute the Goodman & Kruskal tau
    return((Vy - Vybarx)/Vy)
    
}

##******************************************************************
## Step 1:  Load data
##******************************************************************

##------------------------------------------------------------------
## load the train/test data
##------------------------------------------------------------------
load("001_trainLabels.Rdata")     ##  1700000 x 146

##------------------------------------------------------------------
## isolate the actual values
##------------------------------------------------------------------
yact        <- trainLabels.raw[, -1]
yact.ncol   <- ncol(yact)


##******************************************************************
## Step 2:  Compute association measures between targets
##******************************************************************

##------------------------------------------------------------------
## compute logloss
##------------------------------------------------------------------
ll.mat  <- matrix(NA, nrow=yact.ncol, ncol=yact.ncol)
for (i in 1:yact.ncol) {
    for (j in 1:yact.ncol) {
        ll.mat[i,j] <- logloss(yact[,i], yact[,j])
    }
}

##------------------------------------------------------------------
## compute phi
##------------------------------------------------------------------
phi.mat  <- matrix(NA, nrow=yact.ncol, ncol=yact.ncol)
for (i in 1:yact.ncol) {
    for (j in 1:yact.ncol) {
        if (i == 14 | j == 14) {
            phi.mat[i,j] <- 0
        } else {
            phi.mat[i,j] <- phi(table(yact[,i], yact[,j]), digits=8)
        }
    }
}


##******************************************************************
## Step 3:  Save results
##******************************************************************
save.image(file="99_TargetCorrelationExperiments.Rdata")






