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
comb.out    <- comb.raw

##------------------------------------------------------------------
## variables columns
##------------------------------------------------------------------
data.cols   <- colnames(comb.raw)[ grep("[x]", colnames(comb.raw)) ]
num.cols    <- length(data.cols)
data.class  <- sapply(comb.raw[, data.cols], class)

##------------------------------------------------------------------
## train/test ids
##------------------------------------------------------------------
id.train    <- train.raw[, c("id")]
id.test     <- test.raw[, c("id")]


##******************************************************************
## Step 2:  Examine hashed text fields for repeats across columns
##******************************************************************

##------------------------------------------------------------------
## set-up
##------------------------------------------------------------------
col.hashed  <- c("x3", "x4", "x34", "x35", "x61", "x64", "x65", "x91", "x94", "x95")
num.hashed  <- length(col.hashed)
mat.hashed  <- matrix(NA, nrow=num.hashed, ncol=num.hashed)

##------------------------------------------------------------------
## list of (sorted) unique hashes in each column
##------------------------------------------------------------------
unq.hashed  <- lapply(comb.raw[, col.hashed], function(x){ as.character(sort(unique(x))) })

##------------------------------------------------------------------
## loop over each hashed column & identify any intersections
##------------------------------------------------------------------
for (i in 1:num.hashed) {
    for (j in 1:num.hashed) {
        mat.hashed[i, j]    <- length(intersect(unq.hashed[[i]], unq.hashed[[j]]))
    }
}
colnames(mat.hashed) <- col.hashed
rownames(mat.hashed) <- col.hashed

##------------------------------------------------------------------
## create a complete set of unique hashes
##------------------------------------------------------------------
for (i in 1:num.hashed) {
    if (i == 1) {
        all.hashed  <- unq.hashed[[1]]
    } else {
        all.hashed  <- c(all.hashed, unq.hashed[[i]])
    }
}
unq.all.hashed.char <- sort(unique(all.hashed))
unq.all.hashed.fac  <- factor(unq.all.hashed.char)

##------------------------------------------------------------------
## create a matrix holding the unique hashes (and an index)
##------------------------------------------------------------------
unq.all.hashed.mat      <- matrix(NA, nrow=length(unq.all.hashed.fac), ncol=2)
unq.all.hashed.mat[,1]  <- unq.all.hashed.char
unq.all.hashed.mat[,2]  <- as.numeric(unq.all.hashed.fac)

##------------------------------------------------------------------
## loop over the text fields and create the
##------------------------------------------------------------------
for (i in 1:num.hashed) {
    comb.out[, col.hashed[i]] <-  formatC(
                                    match(as.character(comb.raw[, col.hashed[i]]), unq.all.hashed.mat[,1]),
                                    width = 7,
                                    format = "d",
                                    flag = "0")
}


##******************************************************************
## Step 3:  Modify remaining factor columns
##******************************************************************

##------------------------------------------------------------------
## loop over each column and make modifications as needed
##------------------------------------------------------------------
for (i in 1:num.cols) {
    
    tmp.col     <- data.cols[i]
    tmp.dat     <- comb.out[, tmp.col]
    tmp.class   <- data.class[tmp.col]
    
    ##------------------------------------------------------------------
    ## factor
    ##------------------------------------------------------------------
    if (tmp.class == "factor") {
        
        ## number of factor levels
        tmp.lvl.num     <- nlevels(tmp.dat)
        
        ## yes/no/blank
        if (tmp.lvl.num == 3) {
            
            tmp.red <- as.integer(ifelse(tmp.dat == "", -1, ifelse(tmp.dat == "YES", 1, 0)))
            
        ## yes/no
        } else if (tmp.lvl.num == 2) {
            
            tmp.red <- as.integer(ifelse(tmp.dat == "YES", 1, 0))
            
        ## no change for remaining factor fields
        } else {
            
            tmp.red <- tmp.dat
        }
        
    ##------------------------------------------------------------------
    ## integer
    ##------------------------------------------------------------------
    } else if (tmp.class == "integer") {
        
        tmp.red <- tmp.dat  ## for now, no modifications
        
    ##------------------------------------------------------------------
    ## numeric
    ##------------------------------------------------------------------
    } else if (tmp.class == "numeric") {
        
        tmp.red <- tmp.dat  ## for now, no modifications
    }
    
    ##------------------------------------------------------------------
    ## update output dataset
    ##------------------------------------------------------------------
    comb.out[, tmp.col]    <- tmp.red
}



##------------------------------------------------------------------
## save the results
##------------------------------------------------------------------
save(comb.raw, id.train, id.test, file="002_ConcatenatedRawData.Rdata")
save(comb.out, id.train, id.test, file="002_ConcatenatedModData.Rdata")



