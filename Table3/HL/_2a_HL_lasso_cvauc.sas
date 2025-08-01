  
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
libname out "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/HL/";
 

%INCLUDE "&pathO./Harvard/Hwang/SAS_MACROS/code_nested_standardize_stabilize.sas";
%INCLUDE "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/cv_auc_macro.sas";


%INCLUDE "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/cvSE.sas";

%LET output=&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/temps/; 
%LET output2=&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/HL/; 

PROC IMPORT OUT= WORK._hl_lasso_pp 
            DATAFILE= "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/HL/cv_pred_lasso.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc sort ; by masterid stack;run;
PROC IMPORT OUT= WORK._hl_cvsl_pp 
            DATAFILE= "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/cv_pred_cvsl.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc sort ; by masterid stack; 
run;               
proc sort data=out._hl_forcvauc out=hldat;
 by masterid stack;

data hldatnew (keep= masterid stack str  secu  nowgt hl_final weight_lsw3_or_lsw4_norm1  fold   masterid stack str  secu  nowgt cv_pred_lasso cv_pred);
 merge hldat _hl_lasso_pp(keep=masterid stack cv_pred_lasso)  _hl_cvsl_pp (keep=masterid stack cv_pred) ;
 by masterid stack;
run;
/*

data hldatnew (keep= masterid stack str  secu  nowgt hl_final weight_lsw3_or_lsw4_norm1  fold   masterid stack str  secu  nowgt cv_pred_lasso  );
 merge hldat _hl_lasso_pp(keep=masterid stack cv_pred_lasso)   ;
 by masterid stack;
run;

*/
%runcvauc_smacro(indat=hldatnew, depvar=hl_final, weight=weight_lsw3_or_lsw4_norm1, fold=fold, dropvars=masterid stack str  secu  nowgt );

proc print data=hl_univ_model_auc_stats;
run;

/*
%MACRO foldAUCs;
%DO i = 1 %TO 10;
    PROC LOGISTIC DATA=hldatnew
        ROCOPTIONS(WEIGHTED);
        WEIGHT weight_lsw3_or_lsw4_norm1;
        WHERE fold = &i;
        MODEL hl_final(EVENT='1') = / NOFIT;
        ROC  "fold&i" PRED=cv_pred_lasso;
    RUN;
%END;
%MEND;
%foldAUCs;
*/
/*
cross-validated concentration of risk calculation

% of each ventile
% of outcome within each ventile
sensitivity
cumulative sensitivity
specificity
cumulative specificity
PPV per 100k (calculate PPV and multiply by 1000)
cumulative PPV per 100k
*/
 


%macro runall(type=, pp=,);
 

%macro cvrisks(indata=,ppvar=,fold=,);
data hldatnew_f&fold;
 set &indata (where=(fold=&fold));
run;

proc surveymeans data=hldatnew_f&fold sumwgt;
 STRATA str; CLUSTER secu;
 WEIGHT weight_lsw3_or_lsw4_norm1 ;
 var fold;
 ods output statistics=_out;
 run;
proc sql;
select SumWgt 
into :sumweight
from _out  ;
quit;
%put &sumweight;

/*
% of each ventile
*/

proc univariate data = hldatnew_f&fold;
weight  weight_lsw3_or_lsw4_norm1;
var &pp;
output out = outuni pctlpts = 99 pctlpts = 5 to 100 by 5 pctlpre = ptile;
run;

data data1;
if _N_ = 1 then set outuni;
set hldatnew_f&fold;
if &ppvar > ptile95 then tier = 1;
else if &ppvar > ptile90 then tier = 2;
else if &ppvar > ptile85 then tier = 3;
else if &ppvar > ptile80 then tier = 4;
else if &ppvar > ptile75 then tier = 5;
else if &ppvar > ptile70 then tier = 6;
else if &ppvar > ptile65 then tier = 7;
else if &ppvar > ptile60 then tier = 8;
else if &ppvar > ptile55 then tier = 9;
else if &ppvar > ptile50 then tier = 10;
else if &ppvar > ptile45 then tier = 11;
else if &ppvar > ptile40 then tier = 12;
else if &ppvar > ptile35 then tier = 13;
else if &ppvar > ptile30 then tier = 14;
else if &ppvar > ptile25 then tier = 15;
else if &ppvar > ptile20 then tier = 16;
else if &ppvar > ptile15 then tier = 17;
else if &ppvar > ptile10 then tier = 18;
else if &ppvar > ptile5 then tier = 19;
else if &ppvar ne . then tier = 20;
run;


proc sort data=data1; by str secu;
run;

PROC SURVEYFREQ DATA=data1;
 STRATA str; CLUSTER secu;
 WEIGHT weight_lsw3_or_lsw4_norm1 ;
 TABLE tier ;  
 ods output oneway=_outcol1;
 RUN;
data _outcol1a;
 set _outcol1;
 keep tier percent stderr;
 if tier^=.;
run;
 

/*
% outcome by ventile

ods trace on;
*/ 
PROC SURVEYFREQ DATA=data1;
 STRATA str; CLUSTER secu;
 WEIGHT weight_lsw3_or_lsw4_norm1 ;
 TABLE tier*hl_final / ROW;
 ods output crosstabs=_outcol2;
 RUN;
data _outcol2a;
 set _outcol2;
 if tier^=. and hl_final=1;
 if RowPercent=. then do; RowPercent=0; RowStdErr=0; end;
run;

/*
sensitivity 

=% of pp threshold = yes / outcome = yes
*/ 

data data2;
set data1  ;
if tier = 1 then top1 = 1; else top1 = 0;
if tier = 2 then top2 = 1; else top2 = 0;
if tier = 3 then top3 = 1; else top3 = 0;
if tier = 4 then top4 = 1; else top4 = 0;
if tier = 5 then top5 = 1; else top5 = 0;
if tier = 6 then top6 = 1; else top6 = 0;
if tier = 7 then top7 = 1; else top7 = 0;
if tier = 8 then top8 = 1; else top8 = 0;
if tier = 9 then top9 = 1; else top9 = 0;
if tier = 10 then top10 = 1; else top10 = 0;
if tier = 11 then top11 = 1; else top11 = 0;
if tier = 12 then top12 = 1; else top12 = 0;
if tier = 13 then top13 = 1; else top13 = 0;
if tier = 14 then top14 = 1; else top14 = 0;
if tier = 15 then top15 = 1; else top15 = 0;
if tier = 16 then top16 = 1; else top16 = 0;
if tier = 17 then top17 = 1; else top17 = 0;
if tier = 18 then top18 = 1; else top18 = 0;
if tier = 19 then top19 = 1; else top19 = 0;
if tier = 20 then top20 = 1; else top20 = 0;

if tier <= 2 then topc2 = 1; else topc2 = 0;
if tier <= 3 then topc3 = 1; else topc3 = 0;
if tier <= 4 then topc4 = 1; else topc4 = 0;
if tier <= 5 then topc5 = 1; else topc5 = 0;
if tier <= 6 then topc6 = 1; else topc6 = 0;
if tier <= 7 then topc7 = 1; else topc7 = 0;
if tier <= 8 then topc8 = 1; else topc8 = 0;
if tier <= 9 then topc9 = 1; else topc9 = 0;
if tier <= 10 then topc10 = 1; else topc10 = 0;
if tier <= 11 then topc11 = 1; else topc11 = 0;
if tier <= 12 then topc12 = 1; else topc12 = 0;
if tier <= 13 then topc13 = 1; else topc13 = 0;
if tier <= 14 then topc14 = 1; else topc14 = 0;
if tier <= 15 then topc15 = 1; else topc15 = 0;
if tier <= 16 then topc16 = 1; else topc16 = 0;
if tier <= 17 then topc17 = 1; else topc17 = 0;
if tier <= 18 then topc18 = 1; else topc18 = 0;
if tier <= 19 then topc19 = 1; else topc19 = 0;
if tier <= 20 then topc20 = 1; else topc20 = 0;
run;
/*
proc freq data=data2;
 tables top1*hl_final top2*hl_final;
 weight weight_lsw3_or_lsw4_norm1;
 run;
*/
proc surveymeans data=data2;
 domain  hl_final;
 strata str; cluster secu;
 var top1 top2 top3 top4 top5 top6 top7 top8 top9 top10 top11 top12 top13 top14 top15 top16 top17 top18 top19 top20;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol3;
run;
data _outcol3a;
 set _outcol3;
if hl_final=1;
tier+1;
sens=mean*100;
sens_se=stderr*100;
run;

/*
cumulative sensitivity
*/
  
proc surveymeans data=data2;
 domain  hl_final;
 strata str; cluster secu;
 var top1 topc2 topc3 topc4 topc5 topc6 topc7 topc8 topc9 topc10 topc11 topc12 topc13 topc14 topc15 topc16 topc17 topc18 topc19 topc20;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol4;
run;
data _outcol4a;
 set _outcol4;
if hl_final=1;
tier+1;
csens=mean*100;
csens_se=stderr*100;
run;


/*
specificity

=% of pp threshold = no / outcome = no
*/ 

data data2a;
set data1  ;
if tier = 1 then top1 = 0; else top1 = 1;
if tier = 2 then top2 = 0; else top2 = 1;
if tier = 3 then top3 = 0; else top3 = 1;
if tier = 4 then top4 = 0; else top4 = 1;
if tier = 5 then top5 = 0; else top5 = 1;
if tier = 6 then top6 = 0; else top6 = 1;
if tier = 7 then top7 = 0; else top7 = 1;
if tier = 8 then top8 = 0; else top8 = 1;
if tier = 9 then top9 = 0; else top9 = 1;
if tier = 10 then top10 = 0; else top10 = 1;
if tier = 11 then top11 = 0; else top11 = 1;
if tier = 12 then top12 = 0; else top12 = 1;
if tier = 13 then top13 = 0; else top13 = 1;
if tier = 14 then top14 = 0; else top14 = 1;
if tier = 15 then top15 = 0; else top15 = 1;
if tier = 16 then top16 = 0; else top16 = 1;
if tier = 17 then top17 = 0; else top17 = 1;
if tier = 18 then top18 = 0; else top18 = 1;
if tier = 19 then top19 = 0; else top19 = 1;
if tier = 20 then top20 = 0; else top20 = 1;

if tier <= 2 then topc2 = 0; else topc2 = 1;
if tier <= 3 then topc3 = 0; else topc3 = 1;
if tier <= 4 then topc4 = 0; else topc4 = 1;
if tier <= 5 then topc5 = 0; else topc5 = 1;
if tier <= 6 then topc6 = 0; else topc6 = 1;
if tier <= 7 then topc7 = 0; else topc7 = 1;
if tier <= 8 then topc8 = 0; else topc8 = 1;
if tier <= 9 then topc9 = 0; else topc9 = 1;
if tier <= 10 then topc10 = 0; else topc10 = 1;
if tier <= 11 then topc11 = 0; else topc11 = 1;
if tier <= 12 then topc12 = 0; else topc12 = 1;
if tier <= 13 then topc13 = 0; else topc13 = 1;
if tier <= 14 then topc14 = 0; else topc14 = 1;
if tier <= 15 then topc15 = 0; else topc15 = 1;
if tier <= 16 then topc16 = 0; else topc16 = 1;
if tier <= 17 then topc17 = 0; else topc17 = 1;
if tier <= 18 then topc18 = 0; else topc18 = 1;
if tier <= 19 then topc19 = 0; else topc19 = 1;
if tier <= 20 then topc20 = 0; else topc20 = 1;
run;
proc surveymeans data=data2a;
 domain  hl_final;
 strata str; cluster secu;
 var top1 top2 top3 top4 top5 top6 top7 top8 top9 top10 top11 top12 top13 top14 top15 top16 top17 top18 top19 top20;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol5;
run;
data _outcol5a;
 set _outcol5;
if hl_final=0;
tier+1;
spec=mean*100;
spec_se=stderr*100;
run;

/*
cumulative specificity
*/
  
proc surveymeans data=data2a;
 domain  hl_final;
 strata str; cluster secu;
 var top1 topc2 topc3 topc4 topc5 topc6 topc7 topc8 topc9 topc10 topc11 topc12 topc13 topc14 topc15 topc16 topc17 topc18 topc19 topc20;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol6;
run;
data _outcol6a;
 set _outcol6;
if hl_final=0;
tier+1;
cspec=mean*100;
cspec_se=stderr*100;
run;

/*
ppv

=% of pp threshold = yes and outcome = yes / pp threshold = yes
*/ 

data data3;
set data1  ; 

if tier = 1 then top1 = 1; else top1 = 0;
if tier = 2 then top2 = 1; else top2 = 0;
if tier = 3 then top3 = 1; else top3 = 0;
if tier = 4 then top4 = 1; else top4 = 0;
if tier = 5 then top5 = 1; else top5 = 0;
if tier = 6 then top6 = 1; else top6 = 0;
if tier = 7 then top7 = 1; else top7 = 0;
if tier = 8 then top8 = 1; else top8 = 0;
if tier = 9 then top9 = 1; else top9 = 0;
if tier = 10 then top10 = 1; else top10 = 0;
if tier = 11 then top11 = 1; else top11 = 0;
if tier = 12 then top12 = 1; else top12 = 0;
if tier = 13 then top13 = 1; else top13 = 0;
if tier = 14 then top14 = 1; else top14 = 0;
if tier = 15 then top15 = 1; else top15 = 0;
if tier = 16 then top16 = 1; else top16 = 0;
if tier = 17 then top17 = 1; else top17 = 0;
if tier = 18 then top18 = 1; else top18 = 0;
if tier = 19 then top19 = 1; else top19 = 0;
if tier = 20 then top20 = 1; else top20 = 0;

if tier <= 2 then topc2 = 1; else topc2 = 0;
if tier <= 3 then topc3 = 1; else topc3 = 0;
if tier <= 4 then topc4 = 1; else topc4 = 0;
if tier <= 5 then topc5 = 1; else topc5 = 0;
if tier <= 6 then topc6 = 1; else topc6 = 0;
if tier <= 7 then topc7 = 1; else topc7 = 0;
if tier <= 8 then topc8 = 1; else topc8 = 0;
if tier <= 9 then topc9 = 1; else topc9 = 0;
if tier <= 10 then topc10 = 1; else topc10 = 0;
if tier <= 11 then topc11 = 1; else topc11 = 0;
if tier <= 12 then topc12 = 1; else topc12 = 0;
if tier <= 13 then topc13 = 1; else topc13 = 0;
if tier <= 14 then topc14 = 1; else topc14 = 0;
if tier <= 15 then topc15 = 1; else topc15 = 0;
if tier <= 16 then topc16 = 1; else topc16 = 0;
if tier <= 17 then topc17 = 1; else topc17 = 0;
if tier <= 18 then topc18 = 1; else topc18 = 0;
if tier <= 19 then topc19 = 1; else topc19 = 0;
if tier <= 20 then topc20 = 1; else topc20 = 0;
run;

proc surveymeans data=data3;
 domain  top1 topc2 topc3 topc4 topc5 topc6 topc7 topc8 topc9 topc10 topc11 topc12 topc13 topc14 topc15 topc16 topc17 topc18 topc19 topc20;
 strata str; cluster secu;
 var hl_final;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol7;
run;
data _outcol7a;
 set _outcol7;
if top1=1 or topc2=1 or topc3=1 or topc4=1 or topc5=1 or topc6=1 or topc7=1 or topc8=1 or topc9=1 or topc10=1 or
   topc11=1 or topc12=1 or topc13=1 or topc14=1 or topc15=1 or topc16=1 or topc17=1 or topc18=1 or topc19=1 or topc20=1;
data _outcol7a;
 set _outcol7a;
 tier+1;
 
cumppv=mean*100000;
cumppv_se=stderr*100000;
run;
 data _hl_table_fold&fold;
  retain tier percent stderr RowPercent RowStdErr sens sens_se csens csens_se spec spec_se cspec cspec_se ppv ppv_se  cumppv cumppv_se;
  merge _outcol1a (keep=tier percent stderr)
        _outcol2a (keep=tier RowPercent RowStdErr)
        _outcol3a (keep=tier sens sens_se)
        _outcol4a (keep=tier csens csens_se)
        _outcol5a (keep=tier spec spec_se)
        _outcol6a (keep=tier cspec cspec_se)
        _outcol7a (keep=tier cumppv cumppv_se)
 ;
 by tier;

 ppv=RowPercent*1000 ;
 ppv_se=RowStdErr*1000 ;


 fold=&fold;
 sumweight=&sumweight;
run;

%mend;
%cvrisks(indata=hldatnew, ppvar=&pp, fold=1);
%cvrisks(indata=hldatnew, ppvar=&pp, fold=2);
%cvrisks(indata=hldatnew, ppvar=&pp, fold=3);
%cvrisks(indata=hldatnew, ppvar=&pp, fold=4);
%cvrisks(indata=hldatnew, ppvar=&pp, fold=5);
%cvrisks(indata=hldatnew, ppvar=&pp, fold=6);
%cvrisks(indata=hldatnew, ppvar=&pp, fold=7);
%cvrisks(indata=hldatnew, ppvar=&pp, fold=8);
%cvrisks(indata=hldatnew, ppvar=&pp, fold=9);
%cvrisks(indata=hldatnew, ppvar=&pp, fold=10);

data out._hl_table_&type._nCV;
 set _hl_table_fold1 _hl_table_fold2 _hl_table_fold3 _hl_table_fold4 _hl_table_fold5 _hl_table_fold6 _hl_table_fold7 _hl_table_fold8 _hl_table_fold9 _hl_table_fold10;
run;



/*
calculate mean and SE across fold for all cells
*/
 

%macro calccell(cellnum=,colnum=,tier=,perc=,se=,);

data cell&cellnum;
 set out._hl_table_&type._nCV (keep=tier &perc &se fold sumweight where=(tier=&tier));
run;

%cvSE(indata=cell&cellnum,outdata=cell&cellnum._se,	fold=fold,	fold_weight=sumweight, 	fold_se=&se)

PROC MEANS DATA=cell&cellnum mean  ;
VAR &perc;
ods output summary=_m1;
run;

data _cell_&cellnum (keep=mean&colnum cvSE&colnum);
 merge _m1 cell&cellnum._se;

 mean&colnum=&perc._Mean;
 cvSE&colnum=cvSE;

run;

%mend;

%macro runcol(colnum=,perc=,se=,);
 
%calccell(cellnum=_1_&colnum,colnum=&colnum,tier=1,perc=&perc,se=&se);
%calccell(cellnum=_2_&colnum,colnum=&colnum,tier=2,perc=&perc,se=&se);
%calccell(cellnum=_3_&colnum,colnum=&colnum,tier=3,perc=&perc,se=&se);
%calccell(cellnum=_4_&colnum,colnum=&colnum,tier=4,perc=&perc,se=&se);
%calccell(cellnum=_5_&colnum,colnum=&colnum,tier=5,perc=&perc,se=&se);
%calccell(cellnum=_6_&colnum,colnum=&colnum,tier=6,perc=&perc,se=&se);
%calccell(cellnum=_7_&colnum,colnum=&colnum,tier=7,perc=&perc,se=&se);
%calccell(cellnum=_8_&colnum,colnum=&colnum,tier=8,perc=&perc,se=&se);
%calccell(cellnum=_9_&colnum,colnum=&colnum,tier=9,perc=&perc,se=&se);
%calccell(cellnum=_10_&colnum,colnum=&colnum,tier=10,perc=&perc,se=&se);
%calccell(cellnum=_11_&colnum,colnum=&colnum,tier=11,perc=&perc,se=&se);
%calccell(cellnum=_12_&colnum,colnum=&colnum,tier=12,perc=&perc,se=&se);
%calccell(cellnum=_13_&colnum,colnum=&colnum,tier=13,perc=&perc,se=&se);
%calccell(cellnum=_14_&colnum,colnum=&colnum,tier=14,perc=&perc,se=&se);
%calccell(cellnum=_15_&colnum,colnum=&colnum,tier=15,perc=&perc,se=&se);
%calccell(cellnum=_16_&colnum,colnum=&colnum,tier=16,perc=&perc,se=&se);
%calccell(cellnum=_17_&colnum,colnum=&colnum,tier=17,perc=&perc,se=&se);
%calccell(cellnum=_18_&colnum,colnum=&colnum,tier=18,perc=&perc,se=&se);
%calccell(cellnum=_19_&colnum,colnum=&colnum,tier=19,perc=&perc,se=&se);
%calccell(cellnum=_20_&colnum,colnum=&colnum,tier=20,perc=&perc,se=&se); 

data out._hlcr_&type._column_&colnum;
 set _cell__1_&colnum
     _cell__2_&colnum
     _cell__3_&colnum
     _cell__4_&colnum
     _cell__5_&colnum
     _cell__6_&colnum
     _cell__7_&colnum
     _cell__8_&colnum
     _cell__9_&colnum
     _cell__10_&colnum
     _cell__11_&colnum
     _cell__12_&colnum
     _cell__13_&colnum
     _cell__14_&colnum
     _cell__15_&colnum
     _cell__16_&colnum
     _cell__17_&colnum
     _cell__18_&colnum
     _cell__19_&colnum
     _cell__20_&colnum;
run;

%mend;

%runcol(colnum=1,perc=percent,se=stderr)
%runcol(colnum=2,perc=RowPercent,se=RowStdErr )
%runcol(colnum=3,perc=sens,se=sens_se )
%runcol(colnum=4,perc=csens,se=csens_se )
%runcol(colnum=5,perc=spec,se=spec_se )
%runcol(colnum=6,perc=cspec,se=cspec_se )
%runcol(colnum=7,perc=ppv,se=ppv_se  )
%runcol(colnum=8,perc=cumppv,se=cumppv_se)

data out._hlcr_&type._CRtable;
 retain tier;
 merge out._hlcr_&type._column_1 
out._hlcr_&type._column_2 
out._hlcr_&type._column_3 
out._hlcr_&type._column_4 
out._hlcr_&type._column_5 
out._hlcr_&type._column_6 
out._hlcr_&type._column_7  
out._hlcr_&type._column_8;
tier+1;
 run;
ODS HTML STYLE=sasweb FILE="&output2._HL_&type._CRtable.html";
 proc print data=out._hlcr_&type._CRtable ;run;
 proc print data=out._hl_table_&type._nCV; run;
ODS HTML CLOSE;
 %mend;
 
 
%runall(type=lasso, pp=cv_pred_lasso);
%runall(type=cvsl, pp=cv_pred );
