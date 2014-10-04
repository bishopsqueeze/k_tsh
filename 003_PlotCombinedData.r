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


##******************************************************************
## Step 1:  Convert each .csv file into an .Rdata file
##******************************************************************

##------------------------------------------------------------------
## load the train/test data
##------------------------------------------------------------------
load("001_train.Rdata")     ##  1700000 x 146
load("001_test.Rdata")      ##  545082 x 146

##------------------------------------------------------------------
## concatenate the datasets
##------------------------------------------------------------------
comb.raw    <- rbind(train.raw, test.raw)   ## ;rm(train.raw, test.raw)
comb.out    <- comb.raw                     ## output dataset

##------------------------------------------------------------------
## variables columns
##------------------------------------------------------------------
data.cols   <- colnames(comb.raw)[ grep("[x]", colnames(comb.raw)) ]
num.cols    <- length(data.cols)
data.class  <- sapply(comb.raw[, data.cols], class)

##------------------------------------------------------------------
## train/test id values
##------------------------------------------------------------------
id.train    <- train.raw[, c("id")]
id.test     <- test.raw[, c("id")]


##------------------------------------------------------------------
## loop over each variable; concatenate train/test; simplify factors
##------------------------------------------------------------------
for (i in 1:num.cols) {
    
    tmp.col     <- data.cols[i]
    tmp.dat     <- comb.raw[, tmp.col]
    tmp.class   <- data.class[tmp.col]
    
    ##------------------------------------------------------------------
    ## factor
    ##------------------------------------------------------------------
    if (tmp.class == "factor") {
        
        ## number of factor levels
        tmp.lvl.num     <- nlevels(tmp.dat)
        
        ## yes/no/blank
        if (tmp.lvl.num == 3) {
            tmp.red <- ifelse(tmp.dat == "", -1, ifelse(tmp.dat == "YES", 1, 0))
            
        ## yes/no
        } else if (tmp.lvl.num == 2) {
            tmp.red <- ifelse(tmp.dat == "YES", 1, 0)
            
        ## hashed text field
        } else {
            tmp.red     <- factor(formatC(as.numeric(tmp.dat), width = 6, format = "d", flag = "0"))
        }
        
    ##------------------------------------------------------------------
    ## integer
    ##------------------------------------------------------------------
    } else if (tmp.class == "integer") {
        
        ## for now, no modifications
        tmp.red <- tmp.dat
        
    ##------------------------------------------------------------------
    ## numeric
    ##------------------------------------------------------------------
    } else if (tmp.class == "numeric") {
        
        ## for now, no modifications
        tmp.red <- tmp.dat
    }
    
    ##------------------------------------------------------------------
    ## update output dataset
    ##------------------------------------------------------------------
    comb.out[, tmp.col]    <- tmp.red
}


##------------------------------------------------------------------
## save the results
##------------------------------------------------------------------
save(comb.out, id.train, id.test, data.class, file="002_combined.Rdata")




