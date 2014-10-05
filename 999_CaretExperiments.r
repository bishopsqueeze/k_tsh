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
## register cores
##------------------------------------------------------------------
registerDoMC(4)

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
## <function>
##------------------------------------------------------------------
twoClassLogLoss <- function (data, lev = NULL, model = NULL)
{
    ##------------------------------------------------------------------
    ## set-up
    ##------------------------------------------------------------------
    require(pROC)
    if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
    {
        stop("levels of observed and predicted data do not match")
    }
    
    ##------------------------------------------------------------------
    ## copmpute the vanilla roc
    ##------------------------------------------------------------------
    rocObject   <- try(pROC::roc(data$obs, data[, lev[2]]), silent = TRUE)
    rocAUC      <- if (class(rocObject)[1] == "try-error") { NA } else { rocObject$auc }

    ##------------------------------------------------------------------
    ## compute the logloss
    ##------------------------------------------------------------------
    y_pred      <- data[, lev[2]]                           ## level == "one"
    y_act       <- ifelse(data[, "obs"] == "zero", 0, 1)    ## convert from a factor
    y_pred      <- pmax(1e-15, pmin(1 - 1e-15, y_pred))
    y_act       <- pmax(1e-15, pmin(1 - 1e-15, y_act))
    ll          <- -mean((y_act)*log(y_pred) + (1-y_act)*log(1-y_pred))
    nlll        <- -log(ll)
    
    ##------------------------------------------------------------------
    ## return results
    ##------------------------------------------------------------------
    out         <- c(rocAUC, ll, nlll)
    names(out)  <- c("ROC", "LogLoss", "NegLogLogLoss")
    return(out)
}


##******************************************************************
## Step 1:  Load data
##******************************************************************


## !!! move pre-work stuff into a separate script

##------------------------------------------------------------------
## load the train/test data
##------------------------------------------------------------------
load("001_trainLabels.Rdata")           ##  1700000 x 146
load("002_ConcatenatedModData.Rdata")


##------------------------------------------------------------------
## isolate training data only
##------------------------------------------------------------------
train.mod   <- comb.out[ which(comb.out$id <= max(id.train)), ]
train.y     <- trainLabels.raw

## confirm ordering
sum(train.mod$id != train.y$id)

## drop ids
train.mod   <- train.mod[, -1]
train.y     <- train.y[, -1]

## list classes
train.mod.class <- sapply(train.mod, class)

## drop character classes/integer classes for this experiment
train.mod   <- train.mod[, -which(train.mod.class %in% c("character", "integer"))]

##------------------------------------------------------------------
## create a data partition indices
##------------------------------------------------------------------

## USE A SMALL SAMPLE FOR THE TESTS
set.seed(3456)
train.y.pidx   <- lapply(train.y, function(x) { createDataPartition(factor(x), p = 0.10, list = FALSE, times = 1) })

prop.table(table(train.y[,1]))
prop.table(table(train.y[train.y.pidx[[1]],1]))


##------------------------------------------------------------------
## model training
##------------------------------------------------------------------

train.num   <- ncol(train.y)

gbmGrid     <-  expand.grid(interaction.depth = c(3), n.trees = (1:30)*100, shrinkage = 0.1)

fitControl  <- trainControl(method = "cv",
                            number = 4,
                            returnData = TRUE,
                            classProbs = TRUE,
                            summaryFunction = twoClassLogLoss,
                            allowParallel = TRUE)

fitResults  <- list()

for (i in 1:train.num)
{
    ## extract
    idx             <- train.y.pidx[[i]]
    train.x         <- train.mod[idx, ]
    class.y         <- factor(train.y[idx, i])
    levels(class.y) <- c("zero","one")
    
    
    if (i != 14) {
        set.seed(88888888)
        tmpFit <- train(    x=train.x,
                            y=class.y,
                            method = "gbm",
                            trControl = fitControl,
                            metric="ROC",
                            tuneGrid=gbmGrid)
                            
        fitResults[[i]] <- tmpFit
        
        plot(tmpFit, title=paste0("Plot #",i))
        pdf(file=paste0("gbm_shrink01_x",i,".pdf"))
            plot(tmpFit)
        dev.off()
    }
}

save(fitResults, file="999_CaretExperiments.Rdata")
#a <- predict(ldaFit1, newdata = train.x, type = "prob")
#c <- predict(ldaFit1, newdata = train.x)
#b <- logloss(y_act=train.y[ train.y.pidx[[33]],  33], y_pred=a[, "one"])
#d <- logloss(y_act=train.y[,c("y33")], y_pred=rep(mean(train.y[,c("y33")]), length(train.y[,c("y33")])))

