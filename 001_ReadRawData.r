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
setwd("/Users/alexstephens/Development/kaggle/tradeshift/data/raw")


##******************************************************************
## Step 1:  Convert each .csv file into an .Rdata file
##******************************************************************

##------------------------------------------------------------------
## define the processed data directory
##------------------------------------------------------------------
proc.dir    <- "/Users/alexstephens/Development/kaggle/tradeshift/data/proc/"

##------------------------------------------------------------------
## load and save each of the files indivually
##------------------------------------------------------------------

## read the trainLabels file
trainLabels.raw <- read.csv("trainLabels.csv", header = TRUE, sep = ",")
save(trainLabels.raw, file=paste0(proc.dir, "001_trainLabels.Rdata"))
rm(trainLabels.raw)

## read the test file
test.raw <- read.csv("test.csv", header = TRUE, sep = ",")
save(test.raw, file=paste0(proc.dir, "001_test.Rdata"))
rm(test.raw)

## read the train file
train.raw <- read.csv("train.csv", header = TRUE, sep = ",")
save(train.raw, file=paste0(proc.dir, "001_train.Rdata"))
rm(train.raw)