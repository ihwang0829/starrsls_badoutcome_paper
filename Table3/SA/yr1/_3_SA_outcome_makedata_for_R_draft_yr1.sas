
OPTIONS SPOOL NOFMTERR NODATE CENTER MPRINT FULLSTIMER NOQUOTELENMAX LINESIZE=MAX PAGESIZE=MAX;

/***************************
Detect OS and then set paths
***************************/
%MACRO GetPath;

%GLOBAL sys pathJ pathO pathU;

%IF &sysscpl = Linux %THEN %DO;
    %LET sys = Linux;
    %LET pathJ = /home/user/waichiu;
    %LET pathO = /home/data;
    %LET pathU = /home/user/petukhova;
%END;
%ELSE %IF &sysscp = WIN %THEN %DO;
    %LET sys = Windows;
    %LET pathJ = U:\echiu;
    %LET pathO = O:;
    %LET pathU = U:\petukhova;
%END; 
%MEND GetPath;

%GetPath;

/*************
1. Set LIBNAME
2. Add path
*************/
/*1. Set LIBNAME*/
/*NOTE: Since O Drive is a shared drive, it's better to add ACCESS=READONLY in LIBNAME to avoid from overwriting the source*/
LIBNAME source0 "&pathO./ArmySTARRS/LSW3/DataDelivery/Replicate 1-14/" ACCESS=READONLY;
LIBNAME lspastn "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/2_PastSurveyVariables/DATA/";
LIBNAME irvlsw1 "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/3_LSW1_Variables/DATA/" ACCESS=READONLY;
LIBNAME irvlsw2 "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/4_LSW2_Variables/DATA/" ACCESS=READONLY;
LIBNAME irvlsw3 "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/5_LSW3_variables/DATA/" ACCESS=READONLY;
LIBNAME irvlsw4 "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/DATA/"; 
LIBNAME mashaw0 "&pathU./STARRSLS/weights_LS3_with_LS2/data/" ACCESS=READONLY; 
LIBNAME mashawt "&pathU./STARRSLS/weights_LS3_allrep/data/" ACCESS=READONLY; 
LIBNAME out "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/DATA/imps/";
LIBNAME aagmc21 "&pathO./VARIABLES/APDS_VARS/AAGtoUM/Dec2021";
LIBNAME aagvars "&pathO./VARIABLES/APDS_VARS/AAGtoUM/Dec2019";
LIBNAME aagvar16 "&pathO./VARIABLES/APDS_VARS/AAGtoUM/Dec2016";
LIBNAME master   "&pathO./VARIABLES/APDS_VARS/HARVARD_VARS/TAIHOD_EQUIVALENT/DATA" ACCESS=READONLY;
libname mp "&pathO./Harvard/Hwang/APDS/DATA/";
LIBNAME ndata "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/DATA/" ACCESS=READONLY;
LIBNAME l4wgt "&pathO./VARIABLES/STARRSLS_VARS/HARVARD/6_LSW4_Variables/DATA/weights/" ACCESS=READONLY;
LIBNAME hdata "&pathO./Harvard/Hwang/STARRSLS/Separation/DATA/";
LIBNAME sudata "&pathO./Harvard/Hwang/SurveyDATA/"; 

libname ls1sep "&pathU./LS1/separation_ls1/data";
libname ls2sep "&pathU./LS2/separation_ls2/data";

libname sepdate "&pathO./Harvard/king/ls3/SA/forjs/";
libname adres "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/temps/";
libname sepfile "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/";
libname out "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/SA/yr1/";
 

%INCLUDE "&pathO./Harvard/Hwang/SAS_MACROS/code_nested_standardize_stabilize.sas";
%LET output=&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/temps/; 
%LET output2=&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/SA/yr1/; 

  


data  full_data_with_fold ;
 set sepfile.full_data_with_fold_SA;
 if stack=1;
run;

 
/*
drop rares

binary: drop <5 non-zero realization of outcome
continuous: drop <5 non-zero realization of outcome
categorical: code rare category to other
*/

/*first check small categories for categorical*/
%macro checkcats;

proc sort data=full_data_with_fold out=base;by masterid;
proc sort data=sepfile._predictorlist_for_R out=preds; by masterid;
data fullfile;
 merge base (in=a)
       preds ;
 by masterid;
 if a;
run;

%let catvars=
edcat_admin
rank_admin
parented_cat0
parented_cat1
parented_cat2
parented_cat3
datingstatus_sep1
datingstatus_sep2
datingstatus_sep3
datingstatus_sep4
rwhite
rblk
rhisp
rother
warrantofficer
commofficer
deer_pm_v001
deer_pm_v084
deer_pm_v083
deployed_wot
deployed_wot_prev
deployed_wot_nev
mast_pm_v134
mast_pm_v135
mast_pm_v136
mast_pm_v137
mast_pm_v138
mast_pm_v139
mast_pm_v140
duty_moscat1
duty_moscat2
duty_moscat3
dischargetype_cat1
dischargetype_cat2
dischargetype_cat3
dischargetype_cat4
;
 

/*
proc freq data=fullfile  nlevels;
tables  &catvars &contvar &dichvar;
ods output nlevels=nlevels_ne; 
run; 
proc freq;
tables nlevels;
run;
*/
proc freq data=fullfile;
tables SA_outcomef* (&catvars)
 
/list missing;
ods output list=_outcats;
run;
/*print out frequencies less than 5, then manually re-code*/
proc print data=_outcats;
where frequency<5;
run;


/*
combine dischargetype_cat2,3,4
code mast_pm_v137 into mast_pm_v139
code deployed_wot into deployed_wot_prev (only 1 currently deployed, code into current or previously)

edcat_admin combine 1 and 2
edcat_admin combine 3 and 4
 <5  officers with outcome, combine with E5-9

combine rother with rhisp
*/

data fullfile1;
 set fullfile;
if edcat_admin in (1,2) then edcat_admin=2;
if edcat_admin in (3,4) then edcat_admin=3;
if rank_admin in (2,3,4) then rank_admin=2;  
if deployed_wot=1 then deployed_wot_prev=1;
if mast_pm_v137=1 then mast_pm_v139=1;
if rother=1 then rhisp=1;

if dischargetype_cat2=1 or dischargetype_cat3=1 or dischargetype_cat4=1 then dischargetype_cat2=1; else dischargetype_cat2=0;
run;
 





%mend;
 
%checkcats;



/*after re-coding categorical, update list for categorical variables and auto-check rare dichotomy/continuous*/
%macro droprares;

 

%let catvars=
edcat_admin
rank_admin
parented_cat0
parented_cat1
parented_cat2
parented_cat3
datingstatus_sep1
datingstatus_sep2
datingstatus_sep3
datingstatus_sep4
rwhite
rblk
rhisp 
deer_pm_v001
deer_pm_v084
deer_pm_v083 
deployed_wot_prev
deployed_wot_nev
mast_pm_v134
mast_pm_v135
mast_pm_v136 
mast_pm_v138
mast_pm_v139
mast_pm_v140
duty_moscat1
duty_moscat2
duty_moscat3
dischargetype_cat1
dischargetype_cat2 
;
%let contvar=
mast_pm_v318
deployed_wot_num_p
cd_odd_scale
age_separation
scale_anger
mast_pm_v310
scale_borderline
scale_antisocial
scale_resilience
numattempts_sep_new
numdeps_admin
LE_PYrStressAreas_j_sep
religiosity
deployed_wot_num
pain_rating_sep
rel_sum_fundamental
parentbornus_num
;


%let dichvar=
 mde_sep_new
gad_sep_new
pts_sep_new
mania_sep_new
ied_sep_new
pd_sep_new
sub_sep_new
add_sep_new
attempt_sep_new
plan_sep_new
ideation_sep_new
attempt_past2year
ideation_past2year
any_tbi_sep
sn_nonhetero
any_biochildren_sep
any_stepchildren_sep
any_children_sep
ds_ls4_combatpatrol
ds_ls4_firerounds
ds_ls4_getwounded
ds_ls4_closecall
ds_ls4_enemydeath
ds_ls4_noncombatdeath
ds_ls4_allydeath
ds_ls4_savelife
ds_ls4_svillagedestroy
ds_ls4_exposesevere
ds_ls4_witnessviol
ds_ls4_physassualt
ds_ls4_sexassualt
ds_ls4_bullied
ds_ls4_other
ce_pdied
ce_psd
ce_psuicide
ce_pprison
ce_juv
ce_foster
ce_pmental
ce_substance
ce_insultn0
ce_homelessn0
ce_welfaren0
ce_dangerousn0
ce_takecaren0
ce_neglectn0
ce_sharrassedn0
ce_haten0
ce_physicalabusen0
ce_hithardn0
ce_emoabusen0
ce_sexualabusen0
any_lte_trauma_sep
any_accident_sep
any_disaster_sep
any_illinjury_sep
any_loveonedeath_sep
any_physassault_sep
any_sexsassault_sep
any_physsex_assault_sep
any_anyother_sep
CA_BornUS_d
mast_pm_v002
marriageend_12mo
married_3p
any_dep_05
any_dep_013
any_dep_017
special_ops_ever
Vict_maj_phys_viol_12m_yn
Vict_maj_phys_viol_EVR_yn
Vict_non_viol_offense_12m_yn
Vict_non_viol_offense_EVR_yn
Vict_any_offense_12m_yn
Vict_any_offense_EVR_yn
lt_ww
lt_wtu
num_acc_12m_yn
;
 
 

data varnamelist;
length nonrarevars $40;
run; 

%LET indepvars= &contvar &dichvar;

%LET i=1;
%LET varname = %SCAN(&indepvars,&i);

%DO %WHILE (&varname ne );


data numouts;
 set fullfile1;
 if SA_outcomef=1 and &varname >=1;
run; 
proc contents;
ods output attributes=_att&i;
run;
data _att&i;
 set _att&i;
 if label2="Observations";
run;
PROC SQL NOPRINT;
	SELECT nValue2
	INTO :num
	FROM _att&i ;
QUIT;
 
data var&i;
nonrarevars="&varname";
num=&num;

data varnamelist;
 set varnamelist var&i;
run; 
 

  %LET i = %EVAL(&i + 1);
  %LET varname = %SCAN(&indepvars,&i);
 

%END; 

/*
add categorical list
*/


%LET indepvars2= &catvars;

%LET j=1;
%LET varname2 = %SCAN(&indepvars2,&j);

%DO %WHILE (&varname2 ne );

 
data var&j;
nonrarevars="&varname2";
num=5;

data varnamelist;
 set varnamelist var&j;
run; 
 

  %LET j = %EVAL(&j + 1);
  %LET varname2 = %SCAN(&indepvars2,&j);
 

%END; 

data __varnamelist;
 set varnamelist;
 if num<5 then delete;
run;
%mend;
 
%droprares;

data sepfile._sa_varlist_forcvauc;
 set __varnamelist;
run;


proc sql;
 select nonrarevars
 into: saauc_varlist
 separated by ' '
 from sepfile._sa_varlist_forcvauc; 
quit; 
%put &saauc_varlist;

data out._sa_forcvauc;
 set fullfile1 (keep= masterid  stack  SA_outcomef  weight_lsw3_or_lsw4_norm1  str  secu  nowgt  fold 
                             &saauc_varlist);
run;
proc sort; by masterid stack;run;
 /*
proc contents varnum;run; 
*/




%let DSET_MODEL= out._sa_forcvauc; **<-- USER MUST EDIT; 
  ** &DSET_MODEL set must contain predictors, 
		a dichotomous outcome variable 
		a FOLD variable, 
		a WEIGHT variable (sum of weights may vary across folds) ;


%let DEPVAR = SA_outcomef ; **<-- USER MUST EDIT; *dichotomous outcome variable ; 

%let WEIGHT = weight_lsw3_or_lsw4_norm1;   **<--- USER MAY EDIT ; * normalized weight variable;

%let FOLDVAR = fold ;  **<--- USER MAY EDIT ; *fold identifier;
%let NFOLDS = 10 ;     **<--- USER MAY EDIT ;  *number of folds;

%let BASEMODEL =   ;   
                    *<-- USER MAY EDIT (base model control variables); 
					*(optional), leave blank if none ; 

%let BASECLASSVARS =     ; **<-- USER MAY EDIT (base model class variables) ;
						*(optional) leave blank if no class controls; 

%let DROPVARS=  &DEPVAR &WEIGHT &FOLDVAR &BASEMODEL  masterid stack str  secu  nowgt ;  **<-- (optional) add to this list any other variables to be excluded from the cv-AUC screening ; 
 

** OUTPUT DATA SETS: ;
%let OUT_Converge_fitted_ALL = OUT.sa_lst_converge_st_fit_all ; **<- convergence status of all fitted models ; 

%let OUT_UNIV_FITTED = OUT.sa_univ_model_fitted ; **<- output data set for all logistic model parameters;
												**optional data set for troubleshooting ;

%let OUT_UNIV_SCORED =  OUT.sa_univ_model_scored ; **<- output data set for all predicted probabilities;
												** optional data set for troubleshooting;

%let OUT_AUC_VFOLDS_ALL = out.sa_univ_vfold_auc_all ; **<-- output data set for validation fold AUCs for univariate model ; 
%let OUT_AUC_STATS =  OUT.sa_univ_model_auc_stats ;  **<- output data set for auc contrast pvalues ;


** automate selection of univariate predictors PREDICTORS ** ; 
 * all variables in the LST_VARIABLES data set will be univariate predictors ;
 * add code to the data step to exclude any variables from univariate modeling; ;


* drop model parameters and any variables not to run as a univariate predictor ;
** the cv-AUC will be generated for every variable listed in this data set; 
proc contents data= &DSET_MODEL (DROP= &DROPVARS) NOPRINT 
 out= lst_variables (keep= VARNUM NAME TYPE LABEL where=(TYPE=1)) ; run ;
proc sort data= lst_variables ; by VARNUM ; run ; 


** create macro variable for each univariate predictors ;
data lst_variables ; set lst_variables (rename=(NAME=variable)); 
 if _n_=1 then VARNUM = . ; VARNUM + 1 ; run ; 

** create macro variables &PRED1 - &PREDn;
proc sql NOPRINT; 
 select count(*) into: NUMPREDS from lst_variables ; quit ; 
%let NUMPREDS = &NUMPREDS ; *strips blanks from the macro variable;
%let NUMPREDS_ALL = &NUMPREDS ; *copy of NUMPREDS for troubleshooting ; 


proc sql NOPRINT ; 
 select variable into :PRED1 - :PRED&NUMPREDS
  from lst_variables ; quit ; 

** output macro variable assignment to the log file ;
%macro ck ; 
 %do i = 1 %to &NUMPREDS ; %put PRED&i =  &&PRED&i ; %end ; %mend ; 
%ck;


** MACRO TO GENERATE validation fold AUCs ;
options mprint ; 
%macro auc_contrasts  ; 
proc delete data= &OUT_Converge_fitted_ALL ; run ; 
proc delete data= &OUT_UNIV_FITTED ; run ;
proc delete data= &OUT_UNIV_SCORED ; run ; 

proc delete data= &OUT_AUC_VFOLDS_ALL ; run ; 
proc delete data= &OUT_AUC_STATS ; run ;  




* estimate basemodel for the entire sample ; 
ODS OUTPUT  
  ConvergenceStatus = tmp_convergence_basemodel
  ResponseProfile= tmp_Resp_full_basemodel
  ROCASSOCIATION = tmp_roc_full_basemodel (where=(ROCModel="full model") 
				rename=(Area=AUC_allobs StdErr=StdErr_allobs
						LowerArea=LowerArea_allobs  UpperArea=UpperArea_allobs)) ; 
proc logistic data= &DSET_MODEL 
		 ROCOPTIONS(WEIGHTED) 
		 NAMELEN=32 ; 
	class &BASECLASSVARS  ; 
  	model &DEPVAR (event='1') = &BASEMODEL  ;
	weight &WEIGHT  ;
    roc "full model"  &BASEMODEL;  run ;  run ; 


*create summary statistics for the entire data set ;

* FOLD &i summary data set created with 1 row and 6 columns:
    N, Cases_N, Controls_N, Sum_Weights, Cases_Weight, Controls_Weight ; 
 data tmp_Resp_full_basemodel ; 
  set tmp_Resp_full_basemodel  END=lastobs; 
  length N Cases_N Controls_N  Sum_Weights Cases_Weight Controls_Weight 8 ; 
  retain N Cases_N Controls_N  Sum_Weights Cases_Weight Controls_Weight ; 
  if input(outcome,8.)=0 then do ; Controls_N+Count; Controls_Weight+totalweight ; end ; 
  if input(outcome,8.)=1 then do ; Cases_N+count; Cases_Weight+totalweight ; end ; 
 if lastobs then do ; 
   N=sum(Controls_N, Cases_N) ;
   Sum_Weights=(Controls_Weight + Cases_Weight) ; 
   output ; end ; 
 drop orderedvalue outcome count totalweight ; 
 run ; 


data out.sa_auc_basemodel_fullsample ; length ROCModel $12 controls $60 N Sum_Weights 8 ;
merge 
 tmp_roc_full_basemodel 
 tmp_Resp_full_basemodel ; 
 ROCModel= "base model" ; 
 controls  = "&BASEMODEL" ; run ; 




************************;
** UNIVARIATE MODELS ** ;
************************;


** BEGIN LOOP THROUGH ALL FOLDS AND ALL PREDICTORS ; 


 %do p = 1 %to &NUMPREDS ;  **<--- EDIT the START/STOP values to limit the job to a few variables;
 /* %do p = 7 %to 7 ;   */ **<- to run a test job on variables 12-15 variables, comment above line and run this line instead;


  %do i = 1 %to &NFOLDS ;  


ods output 
    ConvergenceStatus = tmp_converge_f&i._v&p 
  	NObs= tmp_sum_weights 
	ParameterEstimates= tmp_univ_Param_fold&i._v&p ; 
proc logistic data= &DSET_MODEL 
		 (where=(&FOLDVAR ne &i)) 
		 ROCOPTIONS(WEIGHTED) 
		 NAMELEN=32 ; 
	class &BASECLASSVARS /ref=first  ; 
  	model &DEPVAR (event='1') = &BASEMODEL &&PRED&p ;
	weight &WEIGHT  ; 

 **  prob_uni_r&j._fold&i._v&p is the scored fold &i data set, containing
	   all observations and variables for fold &i
	   plus 4 variables: F_&&pred&p (actual value), I_&&pred&p (scored value), P_0, and P_1;
	score data= &DSET_MODEL  (where=(&FOLDVAR eq &i))
			out= prob_univ_f&i._v&p ;  run ;
 

******************;
** ROC , by fold ** ;
******************;

** proc logistic with NOFIT option does not re-estimate the model, no independent variables needed ;

 * AUC for validation fold &i is calculated and output to tmp_ROC_fit_f&i._v&p ; 
 * output AUC data set contains one row and 8 columns: 
		ROCModel $4  Area StdErr LowerArea UpperArea SomersD Gamma TauA 8 ; 
 ods graphics off; 
 ODS OUTPUT  
  ResponseProfile= tmp_Resp_univ_f&i._v&p
  ROCASSOCIATION = tmp_roc_univ_f&i._v&p; 
 proc logistic data= prob_univ_f&i._v&p  ROCOPTIONS(WEIGHTED)   ; 
 model &DEPVAR (event='1')=   /nofit ;
 weight &WEIGHT ; 
 roc "univ model"  pred=p_1;  run ; 


* FOLD &i summary data set created with 1 row and 6 columns:
    N, Cases_N, Controls_N, Sum_Weights, Cases_Weight, Controls_Weight ; 
 data tmp_Resp_univ_f&i._v&p.b ; 
  set tmp_Resp_univ_f&i._v&p  END=lastobs; 
  length N Cases_N Controls_N  Sum_Weights Cases_Weight Controls_Weight 8 ; 
  retain N Cases_N Controls_N  Sum_Weights Cases_Weight Controls_Weight ; 
  if input(outcome,8.)=0 then do ; Controls_N+Count; Controls_Weight+totalweight ; end ; 
  if input(outcome,8.)=1 then do ; Cases_N+count; Cases_Weight+totalweight ; end ; 
 if lastobs then do ; 
   N=sum(Controls_N, Cases_N) ;
   Sum_Weights=(Controls_Weight + Cases_Weight) ; 
   output ; end ; 
 drop orderedvalue outcome count totalweight ; 
 run ; 


** final 1-row summary of training sample data, with:
 	 all 6 columns from above FOLD summary data set (N, Cases_N, Controls_N, Sum_Weights, Cases_Weight, Controls_Weight),
     all 8 columns from above ROC output variables (ROCModel, Area, StdErr, LowerArea, UpperArea, SomersD, Gamma, TauT),
     plus fold #, varnum of predictor, name of Predictor variable, Contrast (='intercept'),
       Estimate (=AUC-0.5), SE_squared (= SE * SE) ; 
 data sum_roc_univ_f&i._v&p ; 
   length  VarNum  8 
		Predictor  $32 
		&FOLDVAR  8
		Area LowerArea UpperArea 8
		SE   SE_squared 8
		delta_AUC    
		Contrast $9 
		N  Cases_N Sum_Weights Cases_Weight 8 ; 
   set tmp_roc_univ_f&i._v&p (rename=(StdErr=SE)) ; 
    if _n_=1 then set tmp_resp_univ_f&i._v&p.b ;
	 &FOLDVAR = &i ; 
	 VARNUM = &p ; 
	 PREDICTOR = "&&PRED&p" ; 
  	 WeightVar = "&WEIGHT" ;
	 delta_AUC = Area - .5 ; 
	 Contrast = 'intercept' ;
     SE_squared = SE*SE ; 
	 foldwt_x_AUC = sum_weights * Area ; 
	 foldwt_x_SE_squared = sum_weights * SE * SE ; 

	 label delta_AUC=  "AUC - 0.5" ; drop contrast ; run ; 



* 3 variables added to output scored data set ; 
data prob_univ_f&i._v&p  ;
 length DepVar  Predictors  WeightVar $32   &FOLDVAR 8 ; 
 set prob_univ_f&i._v&p (keep=  &FOLDVAR &DEPVAR &WEIGHT  P_1) ;
  DepVar = "&DEPVAR" ; 
  Predictors= "&&PRED&p" ; 
  WeightVar = "&WEIGHT" ; run ; 


* 3 variables added to output parameters data set ; 
data tmp_univ_Param_fold&i._v&p ; 
 length Predictors  WeightVar $32  FOLD 8 Variable $32 ClassVal0 $5 ;  
 set tmp_univ_Param_fold&i._v&p  ;
  FOLD = &i ;  
  Predictors= "&&PRED&p" ; 
  WeightVar = "&WEIGHT" ; 
 label ClassVal0= "Level of CLASS Variable 1 for Variable" ; run ; 
 

* add 3 varaibles to convergence status data set ;
data tmp_converge_f&i._v&p ;
 length Predictors  WeightVar $32  FOLD 8  ; 
 set tmp_converge_f&i._v&p ;
  FOLD = &i ;  
  Predictors= "&&PRED&p" ; 
  WeightVar = "&WEIGHT" ; run ; 


 proc append data= prob_univ_f&i._v&p       	BASE= &OUT_UNIV_SCORED       FORCE ; run ; 
 proc append data= tmp_univ_Param_fold&i._v&p  BASE= &OUT_UNIV_FITTED  		FORCE ; run ; 
 proc append data= sum_roc_univ_f&i._v&p  	  	  BASE= &OUT_AUC_VFOLDS_ALL   	FORCE ; run ; 
 proc append data= tmp_converge_f&i._v&p 		BASE= &OUT_Converge_fitted_ALL FORCE ; run ; 

 proc delete data= prob_univ_f&i._v&p  ; run ; 
 proc delete data= tmp_resp_univ_f&i._v&p ; run ;
 proc delete data= tmp_resp_univ_f&i._v&p.b ; run ;
 proc delete data= tmp_univ_Param_fold&i._v&p ; run ;
 proc delete data= tmp_roc_univ_f&i._v&p ; run ; 
 proc delete data= sum_roc_univ_f&i._v&p ; run ; 
 proc delete data= tmp_converge_f&i._v&p ; run ; 
%end ;  
%end ;



*******************************************;
** AVERAGE AUC, SE, and test statistics ** ;
*******************************************;

%let OUT_AUC_VFOLDS_ALL = out.sa_univ_vfold_auc_all ; **<-- output data set for validation fold AUCs for univariate model ; 
%let OUT_AUC_STATS =  OUT.sa_univ_model_auc_stats ;  **<- output data set for auc contrast pvalues ;


** after all validation fold AUCs are generated, calculate SE-cvAUC ; 

proc sql ; create table sum_auc_var&p as 
 select "&DEPVAR" as outcome, "&WEIGHT" as weight, 
  &NFOLDS as Folds, 
  varnum, predictor, 
  count(*) as n,
  sum(sum_weights) as sum_weights_all_folds,
  sum(foldwt_x_AUC)/CALCULATED sum_weights_all_folds as cv_AUC, 
  sum(foldwt_x_SE_squared) as sum_foldwt_x_SE_squared,
  sqrt( CALCULATED sum_foldwt_x_SE_squared /(&NFOLDS * CALCULATED sum_weights_all_folds)) as SE_cv_AUC 
 from &OUT_AUC_VFOLDS_ALL 
 group by varnum, predictor ; quit ; 


 data sum_auc_var&p ; 
 set sum_auc_var&p ;
  if cv_AUC > 0 then do ; 
  WaldChiSq = (cv_AUC/SE_cv_AUC)**2 ;
  ProbChiSq = 1-cdf('CHISQUARE', WaldChiSq,1) ; 
  Zscore = cv_AUC/SE_cv_AUC ; 
  ProbZ_onetail= (1-cdf('Normal',abs(zscore))) ; 
  if ProbZ_onetail < .001 then prob_onetail = .001 ; 
  else if probz_onetail < .01 then prob_onetail = .01 ;
  else if probz_onetail < .05 then prob_onetail = .05 ;
  else if probz_onetail < .10 then prob_onetail = .10 ; 
  else if probz_onetail < .10 then prob_onetail = .10 ;  end ; 
  drop sum_weights_all_folds sum_foldwt_x_SE_squared ; 
  format cv_AUC best8.3 SE_cv_AUC best8.5 
		ProbChiSq  probz_onetail  PVALUE6.4 WaldChiSq  Zscore 8.2  prob_onetail best8.3 ; run ; 
  
*proc print data= sum_auc_var&p ; run ; 
proc append data= sum_auc_var&p  BASE= &OUT_AUC_STATS FORCE ; run ; 

*proc delete data= sum_auc_var&p ; run ; 


proc print data= &OUT_Converge_fitted_ALL; 
 title "the following models did not converge" ; 
 var predictors fold reason ; where status ne 0 and nullmodel=0 ; run ; title ; 

%mend ;   

***************************************;
**									** ;
**									** ;
**									** ;
**       -- END OF MACRO  --        ** ; 
**									** ;
**									** ;
**									** ;
***************************************;


**    SUBMIT MACRO        **; 

%auc_contrasts  ;   

proc sort data=out.sa_univ_model_auc_stats  out=aucstats; BY DESCENDING cv_AUC;

 

data aucstats1 ;
 set aucstats ;
 lb=cv_AUC -(1.96*SE_cv_AUC);
 if cv_AUC >= 0.51 or lb > 0.5 then keep=1;
run;
 
proc sort ; by predictor;
proc contents data=out._sa_forcvauc;
ods output variables=_cont1;
proc sort ; by variable;
run;
data aucstats1;
 retain outcome	Predictor	Label	cv_AUC	SE_cv_AUC	lb	keep;
 merge aucstats1 (in=a) _cont1 (keep=variable label rename=(variable=predictor));
 by predictor;
if a;

proc sort data= aucstats1; BY DESCENDING cv_AUC;
run; 
ODS HTML STYLE=sasweb FILE="&output2._SA_totalsamp_cv_AUC.html";

proc print data=aucstats1 ;
run; 
ODS HTML CLOSE;
data out.sa_univ_model_auc_stats_f;
 set aucstats1;
run; 

data out.sa_cvaucs (keep=predictor cv_auc keep);
set out.sa_univ_model_auc_stats_f;
if keep=1;
run;

proc print data=out.sa_cvaucs ;
run;
