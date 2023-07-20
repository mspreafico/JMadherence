
# Auxiliary functions for file 04_cross_validation.R

# CrossValJM_M2 (or CrossValJM_M4) function fits model M2 (or M4) on trainingData 
# and computes the integrated prediction errors IPE(u|t) and the dynamic 
# discrimination indexes dynC(u,delta_t) at time points u = 12 months (1 year),
# i.e. mid-term  mortality, and u = 60 months (5 years), i.e. long-term mortality,
# for the testingData (k-th fold).

CrossValJM_M2 <- function(k, trainingData, testingData) {
  library("JMbayes")
  library("splines")
  
  trainingData.id <- trainingData[!duplicated(trainingData$ID), ]
  
  # Standardize age_in and comorbidity in training dataset
  mean_age <- mean(trainingData.id$age_in)
  sd_age <- sd(trainingData.id$age_in)
  mean_com <- mean(trainingData.id$comorbidity)
  sd_com <- sd(trainingData.id$comorbidity)
  trainingData[, age := (age_in - mean_age)/sd_age]
  trainingData[, n_com := (comorbidity-mean_com)/sd_com]
  trainingData.id <- trainingData[!duplicated(trainingData$ID), ]
  
  # Fit joint model M2 on trainingData
  event_proc_base <- coxph(Surv(fup, death) ~ age + gender + n_com, 
                   data = trainingData.id, x = TRUE)
  
  ctrl <- lmeControl(opt='optim')
  long_proc_C <- lme(cum_months ~ ns(time,4) + n_com , random = ~ ns(time,4) |ID, 
                data=trainingData, control = ctrl)
  
  dForm <- list(fixed = ~ 0 + dns(time,4), random = ~ 0 + dns(time,4), indFixed = 2:5, indRandom = 2:5)
  M2 <- jointModelBayes(long_proc_C, event_proc_base, timeVar = "time", n.iter = 30000, n.burnin = 3000,
                        param = 'td-both', extraForm = dForm)
  
  # Standardize age_in and comorbidity according to training dataset
  testingData[, age := (age_in - mean_age)/sd_age]
  testingData[, n_com := (comorbidity - mean_com)/sd_com]
  
  # Calibration IPE(Thoriz|Tstart) for testingData
  ipe12 <- prederrJM(M2, newdata = testingData, Tstart = 3, Thoriz = 12, idVar = 'ID', interval = T)$prederr
  ipe60 <- prederrJM(M2, newdata = testingData, Tstart = 12, Thoriz = 60, idVar = 'ID', interval = T)$prederr
  
  # Discrimination dynC(t.max, Dt) for testingData
  dynC12 <- dynCJM(M2, newdata = testingData, Dt = 1, t.max = 12, idVar = 'ID')$dynC
  dynC60 <- dynCJM(M2, newdata = testingData, Dt = 6, t.max = 60, idVar = 'ID')$dynC
  
  
  out <- cbind(k, ipe12, ipe60, dynC12, dynC60)
  colnames(out) <- c('k', 'IPE(12|3)', 'IPE(60|12)', 'dynC(12,1)', 'dynC(60,6)')
  
  return(out)
}


CrossValJM_M4 <- function(k, trainingData, testingData) {
  library("JMbayes")
  library("MASS")
  
  trainingData.id <- trainingData[!duplicated(trainingData$ID), ]
  
  # Standardize age_in and comorbidity in training dataset
  mean_age <- mean(trainingData.id$age_in)
  sd_age <- sd(trainingData.id$age_in)
  mean_com <- mean(trainingData.id$comorbidity)
  sd_com <- sd(trainingData.id$comorbidity)
  trainingData[, age := (age_in - mean_age)/sd_age]
  trainingData[, n_com := (comorbidity-mean_com)/sd_com]
  trainingData.id <- trainingData[!duplicated(trainingData$ID), ]
  
  # Fit joint model M4 on trainingData
  event_proc_base <- coxph(Surv(fup, death) ~ age + gender + n_com, 
                   data = trainingData.id, x = TRUE)
  
  long_proc_D <- glmmPQL(adherence ~ time + n_com, random = ~ time | ID, 
                    family = binomial, data = trainingData, niter=20)
  
  dLongBin <- function(y, eta.y, scale, log = FALSE, data) {
    dbinom(x = y, size = 1L, prob = plogis(eta.y), log = log)
  }
  M4 <- jointModelBayes(long_proc_D, event_proc_base, timeVar = "time", densLong = dLongBin, n.iter=30000)
  
  # Standardize age_in and comorbidity according to training dataset
  testingData[, age := (age_in - mean_age)/sd_age]
  testingData[, n_com := (comorbidity - mean_com)/sd_com]
  
  # Calibration IPE(Thoriz|Tstart) for testingData
  ipe12 <- prederrJM(M4, newdata = testingData, Tstart = 3, Thoriz = 12, idVar = 'ID', interval = T)$prederr
  ipe60 <- prederrJM(M4, newdata = testingData, Tstart = 12, Thoriz = 60, idVar = 'ID', interval = T)$prederr
  
  # Discrimination dynC(t.max, Dt) for testingData
  dynC12 <- dynCJM(M4, newdata = testingData, Dt = 1, t.max = 12, idVar = 'ID')$dynC
  dynC60 <- dynCJM(M4, newdata = testingData, Dt = 6, t.max = 60, idVar = 'ID')$dynC
  
  out <- cbind(k, ipe12, ipe60, dynC12, dynC60)
  colnames(out) <- c('k', 'IPE(12|3)', 'IPE(60|12)', 'dynC(12,1)', 'dynC(60,6)')
  
  return(out)
}