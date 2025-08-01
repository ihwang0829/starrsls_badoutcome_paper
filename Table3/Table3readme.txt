
Table 3 results are from all the code below:

#############Unemployment panel####################

Go into the "EM" folder

-Total sample model:
		
 data set up and CV AUCs	                _4_EM_outcome_makedata_for_R_draft.sas
 Lasso PP, ICI, Brier scores	                EM_lasso_using_R.R
 Lasso ICIC/Brier scores by stack (year 1/2/3)	EM_lasso_lapply_bystack.R
		
-Year 1 sample model (in the subfolder "yr1"):
 Lasso PP, ICI, Brier scores	                EM_lasso_using_R_yr1.R

-note: there are CVSL versions of the code, these were run to compare with the LASSO results but not used in the final output


#############Homelessness####################


Go into the "HL" folder

-the subfolders and file names have the exact same format, replace "EM" with "HL"

#############Suicide Attempt####################

Go into the "SA" folder

-the subfolders and file names have the exact same format, replace "EM" with "SA"
 

#############Code that gets all the results####################

-note: ICI/Brier scores are from the individual R output files above (see notes that say "ICI,Brier scores")

-the  AUC in Table 3 are generated from these 2 codes

 _5a1_all_cv_lasso_results_bystack.sas 
 _5a_all_cv_lasso_results.sas 



variable names for outcome used in the code :
 
 -Unemployment:    unempoutcome
 -Homelessness:    hl_final
 -Suicide Attempt: SA_outcomef