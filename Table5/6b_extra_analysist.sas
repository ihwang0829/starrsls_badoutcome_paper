

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
%INCLUDE "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/cvSE.sas";
LIBNAME out "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/data/imps/";
 

proc sort data= sepfile._file_with_outcome_PPsf  out=seppps; by masterid;
proc sort data= sepfile.allvars_for_shap         out=shapsfile; by masterid;

data sepdata;
 merge shapsfile
       seppps (keep=masterid sa_top20 hl_top10 sd_top10 SA_outcomef  hl_final);
 by masterid;
 /*2x2x2 riskcat with top20 SA instead*/
 if sa_top20=0 and hl_top10=0 and sd_top10=0 then riskcat2=1;
 if sa_top20=1 and hl_top10=0 and sd_top10=0 then riskcat2=2;
 if sa_top20=2 and hl_top10=0 and sd_top10=0 then riskcat2=2;
 if sa_top20=0 and hl_top10=1 and sd_top10=0 then riskcat2=3;
 if sa_top20=0 and hl_top10=0 and sd_top10=1 then riskcat2=4;
 if sa_top20=0 and hl_top10=1 and sd_top10=1 then riskcat2=5;
 if sa_top20=1 and hl_top10=1 and sd_top10=0 then riskcat2=6;
 if sa_top20=2 and hl_top10=1 and sd_top10=0 then riskcat2=6;
 if sa_top20=1 and hl_top10=0 and sd_top10=1 then riskcat2=7;
 if sa_top20=2 and hl_top10=0 and sd_top10=1 then riskcat2=7;
 if sa_top20=1 and hl_top10=1 and sd_top10=1 then riskcat2=8;
 if sa_top20=2 and hl_top10=1 and sd_top10=1 then riskcat2=8;

 if sa_top20>0 then sa_topr=1; else sa_topr=0;

 if  riskcat2=1 then  riskcat2_1=1; else  riskcat2_1=0;
 if  riskcat2=2 then  riskcat2_2=1; else  riskcat2_2=0;
 if  riskcat2=3 then  riskcat2_3=1; else  riskcat2_3=0;
 if  riskcat2=4 then  riskcat2_4=1; else  riskcat2_4=0;
 if  riskcat2=5 then  riskcat2_5=1; else  riskcat2_5=0;
 if  riskcat2=6 then  riskcat2_6=1; else  riskcat2_6=0;
 if  riskcat2=7 then  riskcat2_7=1; else  riskcat2_7=0;
 if  riskcat2=8 then  riskcat2_8=1; else  riskcat2_8=0;

 run;
proc freq   data=sepdata;
tables  riskcat2*sa_topr* hl_top10*sd_top10
/list missing;
weight  weight_lsw3_or_lsw4_norm1;
run;


proc freq  data=sepdata;
tables   sa_topr* hl_top10*sd_top10 
/list missing;
weight  weight_lsw3_or_lsw4_norm1;
run;
proc sort data=sepdata ; by str  secu;run;

proc surveyfreq data=sepdata;
strata str; cluster secu;
tables  sa_topr* hl_top10*sd_top10 ;
weight  weight_lsw3_or_lsw4_norm1;
run;

proc sort data=sepdata;
 by str secu ;
 run;
proc freq  data=sepdata;
tables   fold*riskcat2  fold*SA_outcomef fold*hl_final
/list missing;
weight  weight_lsw3_or_lsw4_norm1;
run;
/*
 row %s of SA/HL
*/

 proc surveymeans data=sepdata;
 domain riskcat2;
 strata str; cluster secu;
 var SA_outcomef  hl_final; 
weight  weight_lsw3_or_lsw4_norm1;
ods output domain=_outing2;
run; 
data _outing2_sa;
 set _outing2;
 if VarName="SA_outcomef";
data _outing2_hl;
 set _outing2;
 if VarName="hl_final";
 run;
proc print data=_outing2_sa;
proc print data=_outing2_hl;
run;

/*
% of risk cat among SA/HL
*/
 proc surveymeans data=sepdata;
 domain SA_outcomef;
 strata str; cluster secu;
 var riskcat2_1 riskcat2_2 riskcat2_3 riskcat2_4 riskcat2_5 riskcat2_6 riskcat2_7 riskcat2_8; 
weight  weight_lsw3_or_lsw4_norm1;
ods output domain=_outing3a;
run; 
 proc surveymeans data=sepdata;
 domain hl_final;
 strata str; cluster secu;
 var riskcat2_1 riskcat2_2 riskcat2_3 riskcat2_4 riskcat2_5 riskcat2_6 riskcat2_7 riskcat2_8; 
weight  weight_lsw3_or_lsw4_norm1;
ods output domain=_outing3b;
run; 
proc print data=_outing3a;
proc print data=_outing3b;
run;

proc freq data=sepdata;
tables mast_pm_v134*
mast_pm_v135*
mast_pm_v136*
mast_pm_v137*
mast_pm_v138*
mast_pm_v139*mast_pm_v140
/list missing;
run;

/*
CR tables with the risk cat group
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


libname out "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/data/imps/SA/";
*/
 



  


%MACRO calc_lasso(type=,dir=,pre=,outcome=, pp=, csv=, outfile1=, outfile2=,);
libname out "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/data/imps/&dir";
%LET output2=&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/data/imps/&dir; 


%macro runall(type=, pp=,);
 

%macro cvrisks(indata=,ppvar=,fold=,);
data &pre.datnew_f&fold;
 set &indata (where=(fold=&fold));
run;

proc surveymeans data=&pre.datnew_f&fold sumwgt;
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
 

data data1; 
set &pre.datnew_f&fold; 

if riskcat2=8 then tier=1;
if riskcat2=7 then tier=2;
if riskcat2=6 then tier=3;
if riskcat2=5 then tier=4;
if riskcat2=4 then tier=5;
if riskcat2=3 then tier=6;
if riskcat2=2 then tier=7;
if riskcat2=1 then tier=8;

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
 TABLE tier*&outcome / ROW;
 ods output crosstabs=_outcol2;
 RUN;
data _outcol2a;
 set _outcol2;
 if tier^=. and &outcome=1;
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

if tier <= 2 then topc2 = 1; else topc2 = 0;
if tier <= 3 then topc3 = 1; else topc3 = 0;
if tier <= 4 then topc4 = 1; else topc4 = 0;
if tier <= 5 then topc5 = 1; else topc5 = 0;
if tier <= 6 then topc6 = 1; else topc6 = 0;
if tier <= 7 then topc7 = 1; else topc7 = 0;
if tier <= 8 then topc8 = 1; else topc8 = 0; 
run;
/*
proc freq data=data2;
 tables top1*&outcome top2*&outcome;
 weight weight_lsw3_or_lsw4_norm1;
 run;
*/
proc surveymeans data=data2;
 domain  &outcome;
 strata str; cluster secu;
 var top1 top2 top3 top4 top5 top6 top7 top8  ;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol3;
run;
data _outcol3a;
 set _outcol3;
if &outcome=1;
tier+1;
sens=mean*100;
sens_se=stderr*100;
run;

/*
cumulative sensitivity
*/
  
proc surveymeans data=data2;
 domain  &outcome;
 strata str; cluster secu;
 var top1 topc2 topc3 topc4 topc5 topc6 topc7 topc8 ;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol4;
run;
data _outcol4a;
 set _outcol4;
if &outcome=1;
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

if tier <= 2 then topc2 = 0; else topc2 = 1;
if tier <= 3 then topc3 = 0; else topc3 = 1;
if tier <= 4 then topc4 = 0; else topc4 = 1;
if tier <= 5 then topc5 = 0; else topc5 = 1;
if tier <= 6 then topc6 = 0; else topc6 = 1;
if tier <= 7 then topc7 = 0; else topc7 = 1;
if tier <= 8 then topc8 = 0; else topc8 = 1; 
run;
proc surveymeans data=data2a;
 domain  &outcome;
 strata str; cluster secu;
 var top1 top2 top3 top4 top5 top6 top7 top8  ;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol5;
run;
data _outcol5a;
 set _outcol5;
if &outcome=0;
tier+1;
spec=mean*100;
spec_se=stderr*100;
run;

/*
cumulative specificity
*/
  
proc surveymeans data=data2a;
 domain  &outcome;
 strata str; cluster secu;
 var top1 topc2 topc3 topc4 topc5 topc6 topc7 topc8  ;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol6;
run;
data _outcol6a;
 set _outcol6;
if &outcome=0;
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

if tier <= 2 then topc2 = 1; else topc2 = 0;
if tier <= 3 then topc3 = 1; else topc3 = 0;
if tier <= 4 then topc4 = 1; else topc4 = 0;
if tier <= 5 then topc5 = 1; else topc5 = 0;
if tier <= 6 then topc6 = 1; else topc6 = 0;
if tier <= 7 then topc7 = 1; else topc7 = 0;
if tier <= 8 then topc8 = 1; else topc8 = 0; 
run;

proc surveymeans data=data3;
 domain  top1 topc2 topc3 topc4 topc5 topc6 topc7 topc8  ;
 strata str; cluster secu;
 var &outcome;
 weight weight_lsw3_or_lsw4_norm1;
ods output domain=_outcol7;
run;
data _outcol7a;
 set _outcol7;
if top1=1 or topc2=1 or topc3=1 or topc4=1 or topc5=1 or topc6=1 or topc7=1 or topc8=1  ;
data _outcol7a;
 set _outcol7a;
 tier+1;
 
cumppv=mean*100000;
cumppv_se=stderr*100000;
run;
 data _&pre._table_fold&fold;
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
%cvrisks(indata=sepdata, ppvar=&pp, fold=1);
%cvrisks(indata=sepdata, ppvar=&pp, fold=2);
%cvrisks(indata=sepdata, ppvar=&pp, fold=3);
%cvrisks(indata=sepdata, ppvar=&pp, fold=4);
%cvrisks(indata=sepdata, ppvar=&pp, fold=5);
%cvrisks(indata=sepdata, ppvar=&pp, fold=6);
%cvrisks(indata=sepdata, ppvar=&pp, fold=7);
%cvrisks(indata=sepdata, ppvar=&pp, fold=8);
%cvrisks(indata=sepdata, ppvar=&pp, fold=9);
%cvrisks(indata=sepdata, ppvar=&pp, fold=10);

data out._&pre._table_&type._nCV;
 set _&pre._table_fold1 _&pre._table_fold2 _&pre._table_fold3 _&pre._table_fold4 _&pre._table_fold5 _&pre._table_fold6 _&pre._table_fold7 _&pre._table_fold8 _&pre._table_fold9 _&pre._table_fold10;
run;



/*
calculate mean and SE across fold for all cells
*/
 

%macro calccell(cellnum=,colnum=,tier=,perc=,se=,);

data cell&cellnum;
 set out._&pre._table_&type._nCV (keep=tier &perc &se fold sumweight where=(tier=&tier));
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
data out._&pre.cr_&type._column_&colnum;
 set _cell__1_&colnum
     _cell__2_&colnum
     _cell__3_&colnum
     _cell__4_&colnum
     _cell__5_&colnum
     _cell__6_&colnum
     _cell__7_&colnum
     _cell__8_&colnum ;
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

data out._&pre.cr_&type._CRtable;
 retain tier;
 merge out._&pre.cr_&type._column_1 
out._&pre.cr_&type._column_2 
out._&pre.cr_&type._column_3 
out._&pre.cr_&type._column_4 
out._&pre.cr_&type._column_5 
out._&pre.cr_&type._column_6 
out._&pre.cr_&type._column_7  
out._&pre.cr_&type._column_8;
tier+1;
 run;
ODS HTML STYLE=sasweb FILE="&output2._&outfile2..html";
 proc print data=out._&pre.cr_&type._CRtable ;run;
 proc print data=out._&pre._table_&type._nCV; run;
ODS HTML CLOSE;
 %mend;
 
 
%runall(type=&type, pp=&pp);
/*%runall(type=cvsl, pp=cv_pred );*/

%MEND;
%calc_lasso(type=lasso,dir=SA/,    pre=sa,outcome=SA_outcomef,     pp=sacv_pred_lasso, csv=sacv_pred_lasso, outfile1=sa_total_ppauc, outfile2=sa_total_CRtables)
 
%calc_lasso(type=lasso,dir=HL/,    pre=hl,outcome=hl_final,        pp=cv_pred_lasso, csv=cv_pred_lasso, outfile1=hl_total_ppauc, outfile2=hl_total_CRtables) 
/*
%calc_lasso(type=lasso,dir=SA/,    pre=sa,outcome=SA_outcomef,     pp=sacv_pred_lasso, csv=sacv_pred_lasso, outfile1=sa_total_ppauc, outfile2=sa_total_CRtables)*/
