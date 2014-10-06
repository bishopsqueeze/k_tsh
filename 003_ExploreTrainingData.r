##------------------------------------------------------------------
## Load the raw data files from the Kaggle website into Rdata files
## http://www.kaggle.com/c/tradeshift-text-classification
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(caret)
library(foreach)
library(doMC)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## register cores
##------------------------------------------------------------------
registerDoMC(4)

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/tradeshift/data/proc")
figure.dir  <- "/Users/alexstephens/Development/kaggle/tradeshift/data/figs"


##------------------------------------------------------------------
## Define hased columns
##------------------------------------------------------------------
col.hashed  <- c("x3", "x4", "x34", "x35", "x61", "x64", "x65", "x91", "x94", "x95")


##------------------------------------------------------------------
## <function> logloss :: evaluation metric
##------------------------------------------------------------------
logloss <- function(y_act, y_pred)
{
    y_pred  <- pmax(1e-15, pmin(1-1e-15, y_pred))
    y_act   <- pmax(1e-15, pmin(1 - 1e-15, y_act))
    ll      <- -mean((y_act)*log(y_pred) + (1-y_act)*log(1-y_pred))
    return(ll)
}

##******************************************************************
## Step 1:  Load data
##******************************************************************

##------------------------------------------------------------------
## load the train/test data
##------------------------------------------------------------------
load("001_trainLabels.Rdata")           ##  1700000 x 146
load("002_ConcatModData.Rdata")

##------------------------------------------------------------------
## isolate training data only
##------------------------------------------------------------------
train.mod   <- comb.mod[ which(comb.mod$id <= max(id.train)), ]
train.y     <- trainLabels.raw

## confirm ordering
sum(train.mod$id != train.y$id)

## drop ids
train.mod   <- train.mod[, -1]
train.y     <- train.y[, -1]

## list classes
train.mod.class <- sapply(train.mod, class)


##******************************************************************
## Step 2:  Plot basic histograms of predictors/targets
##******************************************************************

## break predictor columns into type
factor.cols     <- colnames(train.mod)[which(train.mod.class %in% "factor")]
integer.cols    <- colnames(train.mod)[which(train.mod.class %in% "integer")]
numeric.cols    <- colnames(train.mod)[which(train.mod.class %in% "numeric")]
character.cols  <- colnames(train.mod)[which(train.mod.class %in% "character")] ## hashed text

predictor.cols  <- colnames(train.mod)
target.cols     <- colnames(train.y)


##------------------------------------------------------------------
## plot factor variable barplots
##------------------------------------------------------------------
for (i in 1:length(factor.cols))
{
    tmp.var         <- factor.cols[i]
    tmp.filename    <- paste0("factor_plot_variable_",tmp.var,".pdf")
    tmp.title       <- paste0("Variable ",tmp.var)
    
    pdf(file=paste0(figure.dir,"/predictors/",tmp.filename))
        plot(train.mod[, tmp.var], col="steelblue", main=tmp.title)
    dev.off()
}

##------------------------------------------------------------------
## plot integer variable histograms
##------------------------------------------------------------------
for (i in 1:length(integer.cols))
{
    tmp.var         <- integer.cols[i]
    tmp.filename    <- paste0("integer_plot_variable_",tmp.var,".pdf")
    tmp.title       <- paste0("Variable ",tmp.var)
    
    pdf(file=paste0(figure.dir,"/predictors/",tmp.filename))
        hist(train.mod[, tmp.var], breaks=100, col="steelblue", main=tmp.title)
    dev.off()
}


##------------------------------------------------------------------
## plot numeric variable histograms
##------------------------------------------------------------------
for (i in 1:length(numeric.cols))
{
    tmp.var         <- numeric.cols[i]
    tmp.filename    <- paste0("numeric_plot_variable_",tmp.var,".pdf")
    tmp.title       <- paste0("Variable ",tmp.var)
    
    pdf(file=paste0(figure.dir,"/predictors/",tmp.filename))
        hist(train.mod[, tmp.var], breaks=100, col="steelblue", main=tmp.title)
    dev.off()
}


##------------------------------------------------------------------
## plot character variable tables
##------------------------------------------------------------------
for (i in 1:length(character.cols))
{
    tmp.var         <- character.cols[i]
    tmp.filename    <- paste0("character_plot_variable_",tmp.var,".pdf")
    tmp.title       <- paste0("Variable ",tmp.var)
    
    pdf(file=paste0(figure.dir,"/predictors/",tmp.filename))
        plot(prop.table(sort(table(train.mod[, tmp.var]))), col="steelblue", main=tmp.title)
    dev.off()
}



## Will need to define buckets for the character variables

## Will need to compute quantiles for each


## Will want to compute WOE for each of the factor, character variables
charVarWoe  <- function(x, y)
{
    tmp.tbl     <- table(x, y)
    tmp.tbl     <- as.data.frame(tmp.tbl[order(-tmp.tbl[,1]),])
    colnames(tmp.tbl)   <- c("X0","X1")
    
    tmp.tbl$tot  <- apply(tmp.tbl, 1, sum)
    tmp.tbl$D0  <- tmp.tbl$X0 / tmp.tbl$tot
    tmp.tbl$D1  <- tmp.tbl$X1 / tmp.tbl$tot
    tmp.tbl$woe <- log(tmp.tbl$D1 / tmp.tbl$D0)*100
    
    return(tmp.tbl)
}


# Stacked Bar Plot with Colors and Legend
counts <- prop.table(table(train.y[,33],train.mod[, tmp.var]))
barplot(counts, main="X vs Y",
xlab="Factor", col=c("steelblue","red"),
legend = rownames(counts))







