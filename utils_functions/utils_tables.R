
# Auxiliary functions for 03_jm_tab_fig.R

table_C <- function(M1, M2, M3){
  
  # Function for the generation of Table 3 of the manuscript.
  
  #-------------#
  # Left column #
  #-------------#
  value <- rbind(t(t(M1$postMeans$betas)),
                 t(t(M1$postMeans$sigma)),
                 t(t(M1$postMeans$gammas)),
                 t(t(M1$postMeans$alphas)),
                 NA)
  lower <- rbind(t(t(M1$CIs$betas[1,])),
                 t(t(M1$CIs$sigma[1,])),
                 t(t(M1$CIs$gammas[1,])),
                 t(t(M1$CIs$alphas[1,])),
                 NA)
  upper <- rbind(t(t(M1$CIs$betas[2,])),
                 t(t(M1$CIs$sigma[2,])),
                 t(t(M1$CIs$gammas[2,])),
                 t(t(M1$CIs$alphas[2,])),
                 NA)
  intervals <- cbind(value,lower,upper)
  colnames(intervals) <- c('value', 'lower', 'upper')
  CI <- NULL
  for(i in 1:12){
    CI <- c(CI,paste0('(',round(lower[i],4),';',round(upper[i],4),')'))
  }
  TM1 <- data.frame('Mean' = round(value,4), CI)
  colnames(TM1) <- c('Mean', '95% CI')
  rownames(TM1) <- c('Intercept', '$B_n(t,lambda_1)$', '$B_n(t,lambda_2)$',
                     '$B_n(t,lambda_3)$', '$B_n(t,lambda_4)$', 'comorbidity', 'sigma_eps',
                     'Age', 'Gender (Male)', 'Comorbidity', 'Current Value', 'Slope')
  
  #----------------#
  # Central column #
  #----------------#
  value <- rbind(t(t(M2$postMeans$betas)),
                 t(t(M2$postMeans$sigma)),
                 t(t(M2$postMeans$gammas)),
                 t(t(M2$postMeans$alphas)),
                 t(t(M2$postMeans$Dalphas)))
  lower <- rbind(t(t(M2$CIs$betas[1,])),
                 t(t(M2$CIs$sigma[1,])),
                 t(t(M2$CIs$gammas[1,])),
                 t(t(M2$CIs$alphas[1,])),
                 t(t(M2$CIs$Dalphas[1,])))
  upper <- rbind(t(t(M2$CIs$betas[2,])),
                 t(t(M2$CIs$sigma[2,])),
                 t(t(M2$CIs$gammas[2,])),
                 t(t(M2$CIs$alphas[2,])),
                 t(t(M2$CIs$Dalphas[2,])))
  intervals <- cbind(value,lower,upper)
  colnames(intervals) <- c('value', 'lower', 'upper')
  CI <- NULL
  for(i in 1:12){
    CI <- c(CI,paste0('(',round(lower[i],4),';',round(upper[i],4),')'))
  }
  TM2 <- data.frame('Mean' = round(value,4), CI)
  colnames(TM2) <- c('Mean', '95% CI')
  rownames(TM2) <- c('Intercept', '$B_n(t,lambda_1)$', '$B_n(t,lambda_2)$',
                     '$B_n(t,lambda_3)$', '$B_n(t,lambda_4)$', 'comorbidity', 'sigma_eps',
                     'Age', 'Gender (Male)', 'Comorbidity', 'Current Value', 'Slope')
  
  #--------------#
  # Right column #
  #--------------#
  value <- rbind(t(t(M3$postMeans$betas)),
                 t(t(M3$postMeans$sigma)),
                 t(t(M3$postMeans$gammas)),
                 NA,
                 t(t(M3$postMeans$Dalphas)))
  lower <- rbind(t(t(M3$CIs$betas[1,])),
                 t(t(M3$CIs$sigma[1,])),
                 t(t(M3$CIs$gammas[1,])),
                 NA,
                 t(t(M3$CIs$Dalphas[1,])))
  upper <- rbind(t(t(M3$CIs$betas[2,])),
                 t(t(M3$CIs$sigma[2,])),
                 t(t(M3$CIs$gammas[2,])),
                 NA,
                 t(t(M3$CIs$Dalphas[2,])))
  intervals <- cbind(value,lower,upper)
  colnames(intervals) <- c('value', 'lower', 'upper')
  CI <- NULL
  for(i in 1:12){
    CI <- c(CI,paste0('(',round(lower[i],4),';',round(upper[i],4),')'))
  }
  TM3 <- data.frame('Mean' = round(value,4), CI)
  colnames(TM3) <- c('Mean', '95% CI')
  rownames(TM3) <- c('Intercept', '$B_n(t,lambda_1)$', '$B_n(t,lambda_2)$',
                     '$B_n(t,lambda_3)$', '$B_n(t,lambda_4)$', 'comorbidity', 'sigma_eps',
                     'Age', 'Gender (Male)', 'Comorbidity', 'Current Value', 'Slope')
  
  #---------#
  # Table C #
  #---------#
  summaryJM <- cbind(TM1,TM2,TM3)
  DIC <- c(M1$DIC,M2$DIC,M3$DIC)
  names(DIC)<-c('M1', 'M2', 'M3')
  
  return(list('summaryJM' = summaryJM, 'DIC' = DIC))
  
}

table_D <- function(M4, M5, M6){
  
  # Function for the generation of Table 4 of the manuscript.
  
  #-------------#
  # Left column #
  #-------------#
  value <- rbind(t(t(M4$postMeans$betas)),
                 t(t(M4$postMeans$gammas)),
                 t(t(M4$postMeans$alphas)),
                 NA)
  lower <- rbind(t(t(M4$CIs$betas[1,])),
                 t(t(M4$CIs$gammas[1,])),
                 t(t(M4$CIs$alphas[1,])),
                 NA)
  upper <- rbind(t(t(M4$CIs$betas[2,])),
                 t(t(M4$CIs$gammas[2,])),
                 t(t(M4$CIs$alphas[2,])),
                 NA)
  intervals <- cbind(value,lower,upper)
  colnames(intervals) <- c('value','lower','upper')
  CI <- NULL
  for(i in 1:8){
    CI <- c(CI,paste0('(',round(lower[i],4),';',round(upper[i],4),')'))
  }
  TM4 <- data.frame('Mean' = round(value,4), CI)
  colnames(TM4) <- c('Mean','95% CI')
  rownames(TM4) <- c('Intercept','time', 'comorbidity',
                     'Age', 'Gender (Male)', 'Comorbidity', 'Current Value', 'Slope')
  
  #----------------#
  # Central column #
  #----------------#
  value <- rbind(t(t(M5$postMeans$betas)),
                 t(t(M5$postMeans$gammas)),
                 t(t(M5$postMeans$alphas)),
                 t(t(M5$postMeans$Dalphas)))
  lower <- rbind(t(t(M5$CIs$betas[1,])),
                 t(t(M5$CIs$gammas[1,])),
                 t(t(M5$CIs$alphas[1,])),
                 t(t(M5$CIs$Dalphas[1,])))
  upper <- rbind(t(t(M5$CIs$betas[2,])),
                 t(t(M5$CIs$gammas[2,])),
                 t(t(M5$CIs$alphas[2,])),
                 t(t(M5$CIs$Dalphas[2,])))
  intervals <- cbind(value,lower,upper)
  colnames(intervals) <- c('value','lower','upper')
  CI <- NULL
  for(i in 1:8){
    CI <- c(CI,paste0('(',round(lower[i],4),';',round(upper[i],4),')'))
  }
  TM5 <- data.frame('Mean' =round(value,4), CI)
  colnames(TM5) <- c('Mean','95% CI')
  rownames(TM5) <- c('Intercept','time', 'comorbidity',
                     'Age', 'Gender (Male)', 'Comorbidity', 'Current Value', 'Slope')
  
  #--------------#
  # Right column #
  #--------------#
  value <- rbind(t(t(M6$postMeans$betas)),
                 t(t(M6$postMeans$gammas)),
                 NA,
                 t(t(M6$postMeans$Dalphas)))
  lower <- rbind(t(t(M6$CIs$betas[1,])),
                 t(t(M6$CIs$gammas[1,])),
                 NA,
                 t(t(M6$CIs$Dalphas[1,])))
  upper <- rbind(t(t(M6$CIs$betas[2,])),
                 t(t(M6$CIs$gammas[2,])),
                 NA,
                 t(t(M6$CIs$Dalphas[2,])))
  intervals <- cbind(value,lower,upper)
  colnames(intervals) <- c('value','lower','upper')
  CI <- NULL
  for(i in 1:8){
    CI <- c(CI,paste0('(',round(lower[i],4),';',round(upper[i],4),')'))
  }
  TM6 <- data.frame('Mean' = round(value,4), CI)
  colnames(TM6) <- c('Mean','95% CI')
  rownames(TM6) <- c('Intercept','time', 'comorbidity',
                     'Age', 'Gender (Male)', 'Comorbidity', 'Current Value', 'Slope')
  
  
  #---------#
  # Table D #
  #---------#
  summaryJM <- cbind(TM4, TM5, TM6)
  DIC <- c(M4$DIC, M5$DIC, M6$DIC)
  names(DIC)<-c('M4', 'M5', 'M6')
  
  return(list('summaryJM' = summaryJM, 'DIC' = DIC))
  
}

