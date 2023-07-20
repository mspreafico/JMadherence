# JMadherence

Code for (i) computation of time-varying covariates for cumulative months covered by drug consumption and adherence to drug treatment; (ii) joint models for time-dependent consumption/adherence and survival, with selection of the best model by cross-validation.
[This code is also freely available as Supporting Information in the main reference paper.]

### Reference
Spreafico, M. & Ieva, F. (2021). Dynamic monitoring of the effects of adherence to medication on survival in heart failure patients: A joint modelling approach exploiting time-varying covariates. *Biometrical Journal*, **62**(2):305–322. [https://link.springer.com/article/10.1007/s10260-022-00647-0](https://onlinelibrary.wiley.com/doi/full/10.1002/bimj.201900365)


### Data Availability
We cannot provide the original administrative data due to confidentiality.
We provide some fake datasets in order to allow researchers who want to replicate the same analysis to properly get how the code has to be run and how results are displayed and should be read.

## Description

- Files:
  - **01_time_varying_covariates.R**: Computation of pharmacological time-varying covariates for ACE/ARB therapy (Section 3.3 of the manuscript Table 1 and Figure 3 included) for patient E with the pharmacological administrative history reported in dataset 'administrative_data_pt_E.Rdata' in 'data' sub-folder. Covariates legend can be found in 'administrative_data_legend.txt' in 'data'sub-folder. [If you use your data, please note that dataset must contain the following variables in order to run, with dates formatted using as.Date(date, format='%Y-%m-%d): 'ID', 'event_type', 'index_date', 'hosp_adm_date', 'LOS', 'ATC_class', 'purchase_date', 'days_pharma']
  - **02_fit_joint_models**: Joints models M1, M2, M3, M4, M5 and M6 specified in Table 2 of the manuscript, as explained in Section 4.2 and Section 4.2.1, on the fake dataset 'fake_data.Rdata' in 'data' sub-folder. [If you use your data, please note that datasets must contain variables reported in the legend file 'pharma_data_legend.txt' in 'data' sub-folder.]
  - **03_jm_tab_fig**: Code for tables and figures realted to the joint modelling analysis, i.e., Table 3, Table 4, Figure 5, Figure 6 and Figure 7 of the manuscript. Run file '02_fit_joint_models.R' at least once before proceed with this code.
  - **04_cross_validation**: 10-fold cross-validation on the fake dataset 'fake_data.Rdata' in order to assess the predictive performances of models M2 and M4 in terms of calibration and discrimination (Section 4.2.2 of the manuscript), summarizing them in Table 5. [If you use your data, please note that datasets must contain variables reported in the legend file 'pharma_data_legend.txt' in 'data' sub-folder.
    
- Sub-folder **./utils_functions/** contains some auxiliary functions to run the code.
  
- Sub-folder **./data/** contains fake datasets along with their legends:
  - **administrative_data_pt_E.Rdata**:Administrative data related to the random patient E mentioned in the manuscript. Used as example for the computation of pharmacological time-varying covariates for ACE/ARB therapy (Section 3.3).
	- **administrative_data_legend.txt**: Variables legend of dataset 'administrative_data_pt_E.Rdata'.
	- **fake_data.Rdata**: Dataset related to 150 fake patients with pharmacological time-varying covariates for ACE/ARB therapy already computed.
	- **fake_data_ptsABCDE.Rdata**: Dataset related to fake patients A, B, C, D and E mentioned in the manuscript with pharmacological time-varying covariates for ACE/ARB therapy already computed.
	- **pharma_data_legend.txt**: Variables legend of datasets 'fake_data.Rdata' and 'fake_data_ptsABCDE.Rdata'

## Software
- R software.
- File **sessionInfo.txt** contains a list of code configurations [software version (incl. package versions), platform].

(Last update: July 20th, 2023)
