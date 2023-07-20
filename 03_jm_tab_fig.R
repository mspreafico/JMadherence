
##############################
# Code - JM Tables & Figures #
##############################

## !!! Run file 02_fit_joint_models.R at least once before proceed.

# Code for tables and figures related to the joint modelling analysis,
# i.e. Table 3, Table 4, Figure 5, Figure 6 and Figure 7.

rm( list = ls() )
library(data.table)
library(ggplot2)
library(JMbayes)
library(splines)

# RStudio: Session -> Set Working Directory -> To Source File Location
directory_path <- "XXXX/Code" ## change to your working directory
setwd(directory_path)

source('utils_functions/utils_tables.R')
source('utils_functions/utils_myJMsurvfit.R')
load('data/fake_data_ptsABCDE.Rdata')
load('joint_models_1_6.Rdata') # Generated running 02_fit_joint_models.R


#---------#
# Table 3 #
#---------#
Table_3 <- table_C(M1, M2, M3)
Table_3$summaryJM
Table_3$DIC
# Alternatively you can look at summary(M1), summary(M2) and summary(M3)

#---------#
# Table 4 #
#---------#
Table_4 <- table_D(M4, M5, M6)
Table_4$summaryJM
Table_4$DIC
# Alternatively you can look at summary(M4), summary(M5) and summary(M6)


#----------#
# Figure 5 #
#----------#
sfit.A <- survfitJM(M2, newdata = pts_data[ID=='A'], idVar = 'ID')
sfit.B <- survfitJM(M2, newdata = pts_data[ID=='B'], idVar = 'ID')
x11()
par(mfrow=c(1,2))
myJMsurvfit(sfit.A, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = 'Survival Probability',
            ylab2 = 'Cumulative months at time t', main = "Patient A - Model 2",
            ymin = 0, ylim.y = c(0, 12), col = 'black',
            cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
            cex.lab.z = 1.2, cex.axis.z = 1.05)
myJMsurvfit(sfit.B, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = 'Survival Probability',
            ylab2 = 'Cumulative months at time t', main = "Patient B - Model 2",
            ymin = 0, ylim.y = c(0, 12), col = 'black',
            cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
            cex.lab.z = 1.2, cex.axis.z = 1.05)


#----------#
# Figure 6 #
#----------#
sfit.C <- survfitJM(M4, newdata = pts_data[ID=='C'], idVar = 'ID')
sfit.D <- survfitJM(M4, newdata = pts_data[ID=='D'], idVar = 'ID')
x11()
par(mfrow=c(1,2))
myJMsurvfit(sfit.C, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = 'Survival Probability',
            ylab2 = 'Adherence at time t', main = "Patient C - Model 4",
            ymin = 0, ylim.y = c(0, 1), invlink=sigm, col = 'black',
            cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
            cex.lab.z = 1.2, cex.axis.z = 1.05)
myJMsurvfit(sfit.D, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = 'Survival Probability',
            ylab2 = 'Adherence at time t', main = "Patient D - Model 4",
            ymin = 0, ylim.y = c(0, 1), invlink=sigm, col = 'black',
            cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
            cex.lab.z = 1.2, cex.axis.z = 1.05)




#----------#
# Figure 7 #
#----------#
sfit.M2.1 <- survfitJM(M2, newdata = pts_data[ID=='E'][1:5,], idVar = 'ID')
sfit.M2.2 <- survfitJM(M2, newdata = pts_data[ID=='E'][1:9,], idVar = 'ID')
sfit.M2.3 <- survfitJM(M2, newdata = pts_data[ID=='E'], idVar = 'ID')
sfit.M4.1 <- survfitJM(M4, newdata = pts_data[ID=='E'][1:5,], idVar = 'ID')
sfit.M4.2 <- survfitJM(M4, newdata = pts_data[ID=='E'][1:9,], idVar = 'ID')
sfit.M4.3 <- survfitJM(M4, newdata = pts_data[ID=='E'], idVar = 'ID')

x11()
par(mfrow=c(2,3))
myJMsurvfit(sfit.M2.1, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = '',
            ylab2 = 'Cumulative months at time t',
            main = "Patient E - Model 2 - t = 4", ymin = 0, ylim.y = c(0, 12),
            cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.3, cex.lab.z = 1,
            cex.axis.z = 1.3, col = 'black')
myJMsurvfit(sfit.M2.2, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = '',
            ylab2 = 'Cumulative months at time t',
            main = "Patient E - Model 2 - t = 8", ymin = 0, ylim.y = c(0, 12),
            cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.3, cex.lab.z = 1,
            cex.axis.z = 1.3, col = 'black')
myJMsurvfit(sfit.M2.3, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = '',
            ylab2 = 'Cumulative months at time t',
            main = "Patient E - Model 2 - t = 12", ymin = 0, ylim.y = c(0, 12),
            cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.3, cex.lab.z = 1,
            cex.axis.z = 1.3, col = 'black')

myJMsurvfit(sfit.M4.1, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = '',
            ylab2 = 'Adherence at time t',
            main = "Patient E - Model 4 - t = 4",
            ymin = 0,  ylim.y = c(0, 1), invlink = sigm,
            cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.3,
            cex.lab.z = 1, cex.axis.z = 1.3, col = 'black')
myJMsurvfit(sfit.M4.2, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = '',
            ylab2 = 'Adherence at time t',
            main = "Patient E - Model 4 - t = 8",
            ymin = 0,  ylim.y = c(0, 1), invlink = sigm,
            cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.3,
            cex.lab.z = 1, cex.axis.z = 1.3, col = 'black')
myJMsurvfit(sfit.M4.3, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = '',
            ylab2 = 'Adherence at time t',
            main = "Patient E - Model 4 - t = 12",
            ymin = 0,  ylim.y = c(0, 1), invlink = sigm,
            cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.3,
            cex.lab.z = 1, cex.axis.z = 1.3, col = 'black')


