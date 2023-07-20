
##############################
# Code - Section 4.2 - 4.2.1 #
##############################

# 4.2 Joint models for time-varying consumption and adherence to ACE/ARB therapy
#     4.2.1 Results

# This code allows to fit joints models M1, M2, M3, M4, M5 and M6 of Table 2
# of the manuscript, as explained in Section 4.2 and Section 4.2.1, on the
# fake dataset 'data.Rdata' in 'data' sub-folder.

rm( list = ls() )
library(data.table)
library(JMbayes)
library(MASS)
library(splines)

# RStudio: Session -> Set Working Directory -> To Source File Location
directory_path <- "XXXX/Code" ## change to your working directory
setwd(directory_path)
load('data/fake_data.Rdata')

# Long-format & wide-format (time=0) dataset
data <- data.table(fake_data)
data_id <- data[!duplicated(data$ID), ]


# Event process with only baseline covariates age, gender, n_com
# (necessary to run JM with jointModelBayes function)
event_proc_base <- coxph( Surv(fup, death) ~ age + gender + n_com, 
                          data = data_id, x = TRUE)


#######################################################
# Joint models for continuous time-varying cum_months #
#######################################################

# Longitudinal process (4)
long_proc_C <- lme(cum_months ~ ns(time,4) + n_com, random = ~ ns(time,4) | ID, 
                   data = data, control = lmeControl(opt = 'optim'))

# M1: Fit JM with longitudinal process (4) and event process (6)
M1 <- jointModelBayes(long_proc_C, event_proc_base, timeVar = "time",
                      n.iter = 30000, n.burnin = 3000)

# M2: Fit JM with longitudinal process (4) and event process (7)
dForm <- list(fixed = ~ 0 + dns(time, 4), random = ~ 0 + dns(time, 4), 
              indFixed = 2:5, indRandom = 2:5)
M2 <- update(M1, param = 'td-both', extraForm = dForm)

# M3: Fit JM with longitudinal process (4) and event process (8)
dForm <- list(fixed = ~ 0 + dns(time, 4), random = ~ 0 + dns(time, 4), 
              indFixed = 2:5, indRandom = 2:5)
M3 <- update(M1, param = 'td-extra', extraForm = dForm)



#######################################################
# Joint models for dichotomous time-varying adherence #
#######################################################

# Longitudinal process (5)
long_proc_D <- glmmPQL(adherence ~ time + n_com, random = ~ time | ID, family = binomial, 
                       data = data, niter=20)

# M4: Fit JM with longitudinal process (5) and  event process (6)
dLongBin <- function(y, eta.y, scale, log = FALSE, data) {
  dbinom(x = y, size = 1L, prob = plogis(eta.y), log = log)
}
M4 <- jointModelBayes(long_proc_D, event_proc_base, timeVar = "time", 
                      densLong = dLongBin, n.iter=30000)

# M5: Fit JM with longitudinal process (5) and  event process (7)
dForm <- list(fixed = ~ 1, random = ~ 1, indFixed = 2, indRandom = 2)
M5 <- update(M4, param = 'td-both', extraForm = dForm)


# M6: Fit JM with longitudinal process (5) and  event process (8)
dForm <- list(fixed = ~ 1, random = ~ 1, indFixed = 2, indRandom = 2)
M6 <- update(M4, param = 'td-extra', extraForm = dForm)


# Save JM results (necessary to run file '03_jm_tab_fig.R')
rm(list = setdiff(ls(), c('M1','M2','M3','M4','M5','M6')))
save.image('joint_models_1_6.Rdata')



