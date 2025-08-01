
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
 

%INCLUDE "&pathO./Harvard/Hwang/SAS_MACROS/code_nested_standardize_stabilize.sas";
%INCLUDE "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/cv_auc_macro.sas";
%INCLUDE "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/cvSE.sas";

%LET output=&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/Analysis/RESULTS/output/; 
 
 

%MACRO calc_lasso(type=,dir=,pre=,outcome=, pp=, csv=, outfile1=, outfile2=,);
libname out "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/&dir";
%LET output2=&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/&dir; 
 
PROC IMPORT OUT= out.all_learners_&pre 
            DATAFILE= "&output2.&pre.n_cv_preds.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= out.lrnr_xwalk_&pre
            DATAFILE= "&output2.&pre.n_learner_xwalk.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/*
data variables masterid fold unempoutcome weight_lsw3_or_lsw4_norm1
*/
%runcvauc_smacro(indat=out.all_learners_&pre, depvar=&outcome, weight=weight_lsw3_or_lsw4_norm1, fold=fold, dropvars=masterid, pre=&pre );


/*
merge label of lrnr onto auc file
*/
data lrnrs_&pre;
 length predictor $32;
 set out.lrnr_xwalk_&pre;
 predictor=var;
run;
proc sort; by predictor;

data out.&pre._univ_model_auc_stats1;
 set &pre._univ_model_auc_stats;


 auc_lb=cv_AUC - (1.96*SE_cv_AUC);
 auc_ub=cv_AUC + (1.96*SE_cv_AUC);
run;
 
data lrnrs_&pre;
 length predictor $32;
 set out.lrnr_xwalk_&pre;
 predictor=var;
run;
proc sort; by predictor;
proc sort data= out.&pre._univ_model_auc_stats1; by predictor;


data  out.&pre._univ_model_auc_stats1a;
 length testtype $32;
 retain outcome predictor lrnr testcat testtype cv_AUC SE_cv_AUC auc_lb auc_ub;
 merge  out.&pre._univ_model_auc_stats1 lrnrs_&pre;
 by predictor;

 testtype=substr(lrnr,1,6);

 

 if testtype="SL.glm" then testcat=1;
 if testtype="SL.nne" then testcat=2;
 if testtype="SL.ran" then testcat=3;
 if testtype="SL.xgb" then testcat=4;
 if testtype="SL.wsv" then testcat=5;
 if testtype="SL.pol" then testcat=6;
 if testtype="LASSO_" then testcat=7;


proc sort data=out.&pre._univ_model_auc_stats1a; by descending  testcat cv_AUC;
run;
 

data largestauc;
 set out.&pre._univ_model_auc_stats1a;
 by descending testcat cv_AUC;
 if last.testcat;

 run;
 proc sort; by testcat;run;
 proc print;run;
ODS HTML STYLE=sasweb FILE="&output2._&outfile1..html";
 
proc print data=out.&pre._univ_model_auc_stats1a; 
 proc print data=largestauc;run;
 run;
ODS HTML CLOSE;
%MEND; 
%calc_lasso(type=lasso,dir=EM/,    pre=em,outcome=unempoutcome,     pp=emcv_pred_lasso, csv=emcv_pred_lasso, outfile1=em_alllrnr_cvauc, outfile2=em_total_CRtables)
%calc_lasso(type=lasso,dir=SA/,    pre=sa,outcome=SA_outcomef,      pp=sacv_pred_lasso, csv=sacv_pred_lasso, outfile1=sa_alllrnr_cvauc, outfile2=sa_total_CRtables) 
%calc_lasso(type=lasso,dir=HL/,    pre=hl,outcome=hl_final,         pp=cv_pred_lasso,   csv=cv_pred_lasso,   outfile1=hl_alllrnr_cvauc, outfile2=hl_total_CRtables)
