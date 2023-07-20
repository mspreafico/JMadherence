
######################
# Code - Section 3.3 #
######################

# 3.3 Pharmacological time-varying covariates for ACE/ARB therapy
# See Section 3.3 of the manuscript for further details.

# This code is an example of pharmacological time-varying covariates for
# ACE/ARB therapy for the random patient 'E' mentioned in the manuscript
# with pharmacological history reported in dataset
# 'administrative_data_pt_E.Rdata' in 'data' sub-folder.
# Code for Table 1 and Figure 3.

rm( list = ls() )
library(data.table)
library(ggplot2)
library(ggpubr)

# RStudio: Session -> Set Working Directory -> To Source File Location
directory_path <- "XXXX/Code" ## change to your working directory
setwd(directory_path)

source('utils_functions/utils_time_var_covariates.R')
load('data/administrative_data_pt_E.Rdata')

# Starting administrative data
adm_pt_E

# Select anagraphic covariates
baseline_cov <- adm_pt_E[, 1:6]
baseline_cov <- baseline_cov[!duplicated(baseline_cov)]

# Compute cumulative coverage days
time_var_cov<- cumulative_days(data = adm_pt_E,
                               anagraphic = baseline_cov,
                               therapy = c('ACE', 'ARB'),
                               idVar = 'ID')

# Convert days into months [ 1 month = 30.4375 days ]
time_var_cov<- cumulative_months(time_var_cov)

# Dichotomize cum_months into adherence with tau = 0.80
time_var_cov<- adherence(time_var_cov,
                         tau = 0.80)
# Final data
time_var_cov



#---------#
# Table 1 #
#---------#
Table_1 <- time_var_cov
# We round variables 'time' and 'cum_months' for a better visualization 
# in Table 1 (not necessary for the analysis)
Table_1[time<0.04, time := round(time, digits=3)]
Table_1[time>0.04, time := round(time, digits=0)]
Table_1[, cum_months := round(cum_months, digits=3)]
Table_1


#----------#
# Figure 3 #
#----------#
points <- Table_1$adherence+1
p1 <- ggplot(Table_1, aes(x=time, y=cum_months)) + 
  geom_point(size=4,shape=c(17,19)[points]) +
  geom_line(size=1,linetype = "dashed") +
  scale_y_continuous(limits = c(0, 12),breaks=seq(0,12,by=1)) +
  scale_x_continuous(breaks=seq(0,12,by=1)) +
  ggtitle("Time-varying consumption and adherence to ACE/ARB therapy") +
  labs(x=NULL,y='Cumulative months') +
  theme_minimal() +
  theme(axis.text=element_text(size=rel(1.1)),
        axis.title=element_text(size=rel(1.2)),
        plot.title = element_text(face="bold", size=rel(1.5)))
p2 <- ggplot(Table_1, aes(x=time, y=adherence)) + 
  geom_point(size=4,shape=c(17,19)[points]) +
  scale_y_continuous(limits = c(-0.3, 1.3),breaks=seq(0,1,by=1)) +
  scale_x_continuous(breaks=seq(0,12,by=1)) +
  labs(x='Time [months]',y='Adherence') +
  theme_minimal() +
  theme(axis.text=element_text(size=rel(1.1)),
        axis.title=element_text(size=rel(1.2)),
        plot.title = element_text(face="bold", size=rel(1.5)))
x11()
ggarrange(p1, p2, heights = c(2, 0.7), ncol = 1, nrow = 2, align='v')
