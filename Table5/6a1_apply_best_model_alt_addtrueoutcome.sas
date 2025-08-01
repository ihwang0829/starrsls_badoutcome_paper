

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
libname sepfile "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/";  
LIBNAME dout "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/apc_hads_preds/DATA/";
%INCLUDE "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/cv_auc_macro.sas";

/*
final base file with plvl outcomes on full 7188 sample

and all predictors
*/

data SAoutcomeplvl;
 set sepfile.stackf_SA_outcome;
 if SA_outcomef=1;
proc sort nodupkey; by masterid;
run;

data HLoutcomeplvl;
 set sepfile._outcomesf_HL;
 if hl_final=1;
proc sort nodupkey; by masterid;
run;

data EMoutcomeplvl;
 set sepfile._outcomesf_EM;
 if unempoutcome=1;
proc sort nodupkey; by masterid;
run; 
 

proc sort data=sepfile.allvars_for_shap out=shapfile;
 by masterid;

data shapfile;
 merge shapfile
       SAoutcomeplvl (keep=masterid SA_outcomef)
	    HLoutcomeplvl (keep=masterid hl_final);
 by masterid;
 if SA_outcomef^=1 then SA_outcomef=0;
 if hl_final^=1 then hl_final=0;
 run;
data sepfile.allvars_for_shap1 (DROP=mast_pm_v010);
retain masterid str secu  weight_lsw3_or_lsw4_norm1 fold riskcat  risk_saonly    risk_hlonly  risk_sdonly  risk_2plus SA_outcomef hl_final;
 set shapfile;
 run;
 data _temp;
 set sepfile.allvars_for_shap1 (drop=masterid);
 proc means nmiss;
 var _all_;
 run;




