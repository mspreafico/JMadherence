
##############################
# Code - Section 4.2 - 4.2.2 #
##############################

# 4.2 Joint models for time-varying consumption and adherence to ACE/ARB therapy
#     4.2.2 Comparison of the two approaches

# This code allows to perform 10-fold cross-validation in order to assess the predictive 
# performances of models M2 and M4 (see Table 2) in terms of calibration and discrimination.
# In particular, it computes the integrated prediction errors IPE(u|t) and the dynamic 
# discrimination indexes dynC(u,delta_t) at time points u = 12 months (1 year), i.e.
# mid-term mortality, and u = 60 months (5 years), i.e. long-term mortality, for the 10-folds.
# Indixes means and standard deviations for the two models are then computed in Table 5.
# For further details see Section 4.2.2 of the manuscript.


rm( list = ls() )
library(data.table)
library(doSNOW)
library(foreach)
library(JMbayes)
library(MASS)
library(parallel)
library(splines)

# RStudio: Session -> Set Working Directory -> To Source File Location
directory_path <- "XXXX/Code" ## change to your working directory
setwd(directory_path)

# Load auxiliary functions CrossValJM_M2 and CrossValJM_M4.
source('utils_functions/utils_cross_validation.R')

# Load data
load('data/fake_data.Rdata')
data <- data.table(fake_data)
# Remove standardized age and comorbidity:
# they must be standardized on training data.
data <- data[, age := NULL]
data <- data[, n_com := NULL]


# Create K=10 random folds
K <- 10
IDs <- unique(data$ID)
n <- length(IDs)
set.seed(123)
splits <- split(seq_len(n), sample(rep(seq_len(K), length.out = n)))


## Performances of model M2

# Setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores-1) #not to overload your computer
registerDoSNOW(cl)

# Progress bar
pb <- txtProgressBar(max = K, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

IPE_dynC_M2 <- foreach(k=1:K, .combine=rbind,
                       .options.snow = opts) %dopar% {
                         
                         # Training dataset
                         trainingData <-  data[!(data$ID %in% IDs[unlist(splits[k])]),]
                         # Testing dataset: fold k
                         testingData <-  data[data$ID %in% IDs[unlist(splits[k])],]
                         
                         out_matrix <- CrossValJM_M2(k, trainingData, testingData)
                         out_matrix
                         
                       }

# Stop cluster and close progress bar
stopCluster(cl)
close(pb)


## Performances of model M4

# Setup parallel backend to use many processors
cl <- makeCluster(cores-1)
registerDoSNOW(cl)

# Progress bar
pb <- txtProgressBar(max = K, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

IPE_dynC_M4 <- foreach(k=1:K, .combine=rbind,
                       .options.snow = opts) %dopar% {

                       # Training dataset
                       trainingData <-  data[!(data$ID %in% IDs[unlist(splits[k])]),]
                       # Testing dataset: fold k
                       testingData <-  data[data$ID %in% IDs[unlist(splits[k])],]
                       
                       out_matrix <- CrossValJM_M4(k, trainingData, testingData)
                       out_matrix
                      }

# Stop cluster and close progress bar
stopCluster(cl)
close(pb)


#---------#
# Table 5 #
#---------#

Table_5 <- cbind(round(colMeans(IPE_dynC_M2[,-1]),4),
                 round(apply(IPE_dynC_M2[,-1], 2, sd),4),
                 round(colMeans(IPE_dynC_M4[,-1]),4),
                 round(apply(IPE_dynC_M4[,-1], 2, sd),4)
)
rownames(Table_5) <- c('IPE(12|3)', 'IPE(60|12)', 'dynC(12,1)', 'dynC(60,6)')
colnames(Table_5) <- c('M2 - Mean', 'M2 - s.d.','M4 - Mean', 'M4 - s.d.')
Table_5





