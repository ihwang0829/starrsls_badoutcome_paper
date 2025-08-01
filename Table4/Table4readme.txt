 

The same two code that calculates the AUCs for Table 3 were used to calculate the Table 4 statistics:

 _5a1_all_cv_lasso_results_bystack.sas 
 _5a_all_cv_lasso_results.sas 

top part of the code calculates AUCs, the macro underneath calculates sensitivity and PPV for each outcome

variable names for outcome used in the code :
 
 -Unemployment:    unempoutcome
 -Homelessness:    hl_final
 -Suicide Attempt: SA_outcomef