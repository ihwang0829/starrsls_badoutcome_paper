

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
LIBNAME out2 "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/data/tables/";


%LET output2=&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/data/tables/; 

proc sort data= sepfile._file_with_outcome_PPsf  out=seppps; by masterid;
proc sort data= sepfile.allvars_for_shap         out=shapsfile; by masterid;
proc sort data=dout.sepsamp_allhads_allimpd out=adminvars; by masterid;
  
 run;
data sepdata;
 merge shapsfile
       adminvars (keep=masterid  )
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


/*
predictors

   18-21	age_separation, code to correct range
  22-24	
  25-27	
  28+	
    ?2	
Gender	
  Female	not mast_pm_v001
  Male	mast_pm_v001
    ?2	
Race/ethnicity	
  Non-Hispanic Black	rblk
  Non-Hispanic White	rwhite
  Hispanic	rhisp
  Other	rother
    ?2	
Education	
  Less than HS	edcat_s (1,2,3,4) code to dummies
  High school	
  Some college	
  College graduate	
    ?2	
Marital status	
  Married	deer_pm_v001
  Previously married	deer_pm_v084
  Never married	deer_pm_v083
    ?2	
Years in service	
  1 or less	yrs_srvc_9cat in (1,2)
  2	yrs_srvc_9cat=3
  3-4	yrs_srvc_9cat in (4,5)
  5 or more	yrs_srvc_9cat>5
    ?2	
Rank	
  Junior enlisted (E1-E3)	rankcat 1-3
  NCOs (E4-E8)	rankcat 4-8
  Officer (warrant or commissioned)	rankcat>8
    ?2	
Number of combat deployments	
  0	deployed_wot_num=0
  1	deployed_wot_num=1
  2	deployed_wot_num=2
  3+	deployed_wot_num=3+
    ?2	
Duty-MOS	
  Direct combat arms	duty_moscat4=1
  Indirect combat arms	duty_moscat4=2
  Combat support	duty_moscat4=3
  Combat service support	duty_moscat4=4
    ?2	
Command	
  FORSCOM	mast_pm_v134=1
  TRADOC	mast_pm_v135=1
  Area Commands	mast_pm_v136=1
  Special Ops	mast_pm_v137=1
  MEDCOM	mast_pm_v138=1
  AMC, unknown	mast_pm_v139=1

*/
     if age_separation<=21 then agecats=1;
else if age_separation<=24 then agecats=2;
else if age_separation<=27 then agecats=3;
else                            agecats=4;
 
if agecats=1 then agecats1=1; else agecats1=0;
if agecats=2 then agecats2=1; else agecats2=0;
if agecats=3 then agecats3=1; else agecats3=0;
if agecats=4 then agecats4=1; else agecats4=0;

if mast_pm_v001^=1 then sexcats=1;
else sexcats=2;

if sexcats=1 then sexcats1=1; else sexcats1=0;
if sexcats=2 then sexcats2=1; else sexcats2=0;

if edcat_s=0 then edcat_s1=1; else edcat_s1=0;
if edcat_s=1 then edcat_s2=1; else edcat_s2=0;
if edcat_s=2 then edcat_s3=1; else edcat_s3=0;
if edcat_s=3 then edcat_s4=1; else edcat_s4=0;

if deer_pm_v001=0 and deer_pm_v084=0 and deer_pm_v083=0 then deer_pm_v083=1; 
if deer_pm_v001=1 then marcat=1;
if deer_pm_v084=1 then marcat=2;
if deer_pm_v083=1 then marcat=3;
 
 if rblk=1 then racecat=1;
 if rwhite=1 then racecat=2;
 if rhisp=1 then racecat=3;
 if rother=1 then racecat=4; 

 if yrs_srvc_9cat<=2 then yrsservcat=1;
 else if yrs_srvc_9cat<=3 then yrsservcat=2;
 else if yrs_srvc_9cat<=5 then yrsservcat=3;
 else                          yrsservcat=4;
 
 if yrsservcat=1 then yrsservcat1=1; else yrsservcat1=0;
 if yrsservcat=2 then yrsservcat2=1; else yrsservcat2=0;
 if yrsservcat=3 then yrsservcat3=1; else yrsservcat3=0;
 if yrsservcat=4 then yrsservcat4=1; else yrsservcat4=0;

      if rank_cat<=3 then rankcats=1;
 else if rank_cat<=8 then rankcats=2;
 else                    rankcats=3;

  if rankcats=1 then rankcats1=1; else rankcats1=0;
  if rankcats=2 then rankcats2=1; else rankcats2=0;
  if rankcats=3 then rankcats3=1; else rankcats3=0;

 if deployed_wot_num=0 then deploynum=0;
 else if deployed_wot_num=1 then  deploynum=1;
 else if deployed_wot_num=2 then  deploynum=2;
 else if deployed_wot_num>2 then  deploynum=3;
 
 if deploynum=0 then deploynumcat0=1; else deploynumcat0=0;
 if deploynum=1 then deploynumcat1=1; else deploynumcat1=0;
 if deploynum=2 then deploynumcat2=1; else deploynumcat2=0;
 if deploynum=3 then deploynumcat3=1; else deploynumcat3=0;

 if duty_moscat4=1 the duty_moscat4_1=1; else duty_moscat4_1=0;
 if duty_moscat4=2 the duty_moscat4_2=1; else duty_moscat4_2=0;
 if duty_moscat4=3 the duty_moscat4_3=1; else duty_moscat4_3=0;
 if duty_moscat4=4 the duty_moscat4_4=1; else duty_moscat4_4=0;

 if mast_pm_v134=1 then commandcat=1;
 if mast_pm_v135=1 then commandcat=2;
 if mast_pm_v136=1 then commandcat=3;
 if mast_pm_v137=1 then commandcat=4;
 if mast_pm_v138=1 then commandcat=5;
 if mast_pm_v139=1 then commandcat=6;
 if mast_pm_v140=1 then commandcat=7;
 run;
proc freq   data=sepdata;
tables  age_separation*agecats*agecats1*agecats2*agecats3*agecats4
        mast_pm_v001*sexcats*sexcats1*sexcats2
        racecat*rblk*	rwhite*	rhisp*rother
        edcat_s*edcat_s1*edcat_s2*edcat_s3*edcat_s4
		marcat*deer_pm_v001*deer_pm_v084*deer_pm_v083 
		yrs_srvc_9cat*yrsservcat*yrsservcat1*yrsservcat2*yrsservcat3*yrsservcat4
		rank_cat*rankcats*rankcats1*rankcats2*rankcats3
		deployed_wot_num*deploynum*deploynumcat1* deploynumcat2*deploynumcat3
		duty_moscat4*duty_moscat4_1*duty_moscat4_2*duty_moscat4_3*duty_moscat4_4
        commandcat*mast_pm_v134* mast_pm_v135*mast_pm_v136*mast_pm_v137*mast_pm_v138*mast_pm_v139* mast_pm_v140
/list missing;
weight  weight_lsw3_or_lsw4_norm1;
run;

/*
merge predictors to stack dataset with outcomes



 %getdistso(infile=sepfile._outcomesf_SA, outcome=SA_outcomef)
 %getdistso(infile=sepfile._outcomesf_HL, outcome=hl_final)
 %getdistso(infile=sepfile._outcomesf_EM, outcome=unempoutcome)
*/
proc contents data=sepfile._outcomesf_SA varnum; run;

%macro getdistso(infile=,outcome=,);
proc sort data=&infile out=base; by masterid;
proc sort data=sepdata; by masterid;

data _t1dat_&outcome;
 merge base (in=a)
       sepdata (keep=masterid age_separation agecats agecats1 agecats2 agecats3 agecats4
        mast_pm_v001 sexcats sexcats1 sexcats2
        racecat rblk 	rwhite 	rhisp rother
        edcat_s edcat_s1 edcat_s2 edcat_s3 edcat_s4
		marcat deer_pm_v001 deer_pm_v084 deer_pm_v083 
		yrs_srvc_9cat yrsservcat yrsservcat1 yrsservcat2 yrsservcat3 yrsservcat4
		rank_cat rankcats rankcats1 rankcats2 rankcats3
		deployed_wot_num deploynum deploynumcat0 deploynumcat1  deploynumcat2 deploynumcat3
		duty_moscat4 duty_moscat4_1 duty_moscat4_2 duty_moscat4_3 duty_moscat4_4
        commandcat mast_pm_v134  mast_pm_v135 mast_pm_v136 mast_pm_v137 mast_pm_v138 mast_pm_v139  mast_pm_v140);
 by masterid;
 if a;
 total=1;
run;


proc freq   data=_t1dat_&outcome;
tables  age_separation*agecats*agecats1*agecats2*agecats3*agecats4
        mast_pm_v001*sexcats*sexcats1*sexcats2
        racecat*rblk*	rwhite*	rhisp*rother
        edcat_s*edcat_s1*edcat_s2*edcat_s3*edcat_s4
		marcat*deer_pm_v001*deer_pm_v084*deer_pm_v083 
		yrs_srvc_9cat*yrsservcat*yrsservcat1*yrsservcat2*yrsservcat3*yrsservcat4
		rank_cat*rankcats*rankcats1*rankcats2*rankcats3
		deployed_wot_num*deploynum*deploynumcat0*deploynumcat1* deploynumcat2*deploynumcat3
		duty_moscat4*duty_moscat4_1*duty_moscat4_2*duty_moscat4_3*duty_moscat4_4
        commandcat*mast_pm_v134* mast_pm_v135*mast_pm_v136*mast_pm_v137*mast_pm_v138*mast_pm_v139* mast_pm_v140
/list missing;
weight  weight_lsw3_or_lsw4_norm1;
run;

proc sort; by str secu;
run;

%mend;

 %getdistso(infile=sepfile._outcomesf_SA, outcome=SA_outcomef)
 %getdistso(infile=sepfile._outcomesf_HL, outcome=hl_final)
 %getdistso(infile=sepfile._outcomesf_EM, outcome=unempoutcome)

ods trace off;
/*
PROC FREQ DATA=_t1dat_SA_outcomef;
 TABLES SA_outcomef*(agecats sexcats racecat edcat_s marcat  
        yrsservcat 
        rank_cat 
        deploynum
        duty_moscat4 
        commandcat) /CHISQ;
 WEIGHT weight_lsw3_or_lsw4_norm1; 
 ODS OUTPUT  ChiSq=_allchsq;
run;
 ODS OUTPUT DOMAIN=_domain_&outcome;
RUN;
*/
 /*
ods trace on;
ods trace off;
*/ 
/*
proc surveylogistic data=_t1dat_SA_outcomef;
class agecats ;
STRATA str; CLUSTER secu;
MODEL SA_outcomef (EVENT='1') = agecats;
 WEIGHT weight_lsw3_or_lsw4_norm1;   
 ods output GlobalTests=__logtest;
run;
proc surveylogistic data=_t1dat_SA_outcomef;
class rankcats ;
STRATA str; CLUSTER secu;
MODEL SA_outcomef (EVENT='1') =rankcats;
 WEIGHT weight_lsw3_or_lsw4_norm1;   
 ods output GlobalTests=__logtest1;
run;
proc surveylogistic data=_t1dat_SA_outcomef;
class duty_moscat4 ;
STRATA str; CLUSTER secu;
MODEL SA_outcomef (EVENT='1') = duty_moscat4;
 WEIGHT weight_lsw3_or_lsw4_norm1;   
 ods output GlobalTests=__logtest2;
run;
*/

%macro make_table1(infile=,outcome=,);

%let varlist=
agecats1
agecats2
agecats3
agecats4
sexcats1
sexcats2
rblk
rwhite
rhisp
rother
edcat_s1
edcat_s2
edcat_s3
edcat_s4
deer_pm_v001
deer_pm_v084
deer_pm_v083 
yrsservcat1
yrsservcat2
yrsservcat3
yrsservcat4
rankcats1
rankcats2
rankcats3
deploynumcat0
deploynumcat1
deploynumcat2
deploynumcat3
duty_moscat4_1
duty_moscat4_2
duty_moscat4_3
duty_moscat4_4
mast_pm_v134
mast_pm_v135
mast_pm_v136
mast_pm_v137
mast_pm_v138
mast_pm_v139
mast_pm_v140
;

PROC SURVEYMEANS DATA=&infile;
 DOMAIN total;
 STRATA str; CLUSTER secu;
 VAR &varlist;
 WEIGHT weight_lsw3_or_lsw4_norm1;
 ODS OUTPUT DOMAIN=_domaintot_&outcome;
RUN;
 
 
PROC SURVEYMEANS DATA=&infile;
 DOMAIN &outcome;
 STRATA str; CLUSTER secu;
 VAR &varlist;
 WEIGHT weight_lsw3_or_lsw4_norm1;
 ODS OUTPUT DOMAIN=_domain_&outcome;
RUN;


data totcol;
 set _domaintot_&outcome (keep=VarName mean mean stderr);
 obs=_N_;
 mtot_&outcome=mean*100;
 setot_&outcome=stderr*100;
run;
proc sort; by VarName;
run;
data yescol;
 set _domain_&outcome (keep=VarName &outcome mean mean stderr 
                        where=(&outcome=1) 
                      ); 
 myes_&outcome=mean*100;
 seyes_&outcome=stderr*100;
run;
proc sort; by VarName;
run;

data nocol;
 set _domain_&outcome (keep=VarName &outcome mean mean stderr 
                        where=(&outcome^=1) 
                       ); 
 mno_&outcome=mean*100;
 seno_&outcome=stderr*100;
run;
proc sort; by VarName;
run;

data __prevtable_&outcome (drop=mean stderr &outcome);
 merge totcol 
       yescol 
       nocol;
 by varname;
run;
proc sort; by obs;
run;


/*
chisquare test
*/
PROC SURVEYFREQ DATA=&infile;
STRATA str; CLUSTER secu;

 TABLES &outcome*(agecats sexcats racecat edcat_s marcat  
        yrsservcat 
        rankcats
        deploynum
        duty_moscat4 
        commandcat) /LLCHISQ;
 WEIGHT weight_lsw3_or_lsw4_norm1;   
 ODS OUTPUT WLLChiSq=_allchsq_&outcome;
run;
data _chisqs;
 set _allchsq_&outcome (where=(Label1="Chi-Square"));
data _pvals;
 set _allchsq_&outcome (where=(Label1="Pr > F"));
run;
data __sigtests_&outcome;
 merge  _chisqs (keep=Table nvalue1 rename=(nvalue1=chi)) 
        _pvals (keep= nvalue1 rename=(nvalue1=p)) ;
run;


data __sigtests_&outcome.1;
 retain obs;
 set __sigtests_&outcome (rename=(table=varname chi=myes_&outcome p=seyes_&outcome)); 

  if _N_=1 then obs=4.5;
  if _N_=2 then obs=6.5;
  if _N_=3 then obs=10.5;
  if _N_=4 then obs=14.5;
  if _N_=5 then obs=17.5;
  if _N_=6 then obs=21.5;
  if _N_=7 then obs=24.5;
  if _N_=8 then obs=28.5;
  if _N_=9 then obs=32.5;
  if _N_=10 then obs=39.5; 


run;
data out2.__table1_&outcome;
   set __prevtable_&outcome  
       __sigtests_&outcome.1;
run;
proc sort; by obs;
run;
 
%MEND;
%make_table1(infile=_t1dat_SA_outcomef, outcome=SA_outcomef)
%make_table1(infile=_t1dat_hl_final,    outcome=hl_final)
%make_table1(infile=_t1dat_unempoutcome,outcome=unempoutcome)
 
ODS HTML STYLE=sasweb FILE="&output2._table1.html";
 proc print data=out2.__table1_SA_outcomef;run;
 proc print data=out2.__table1_hl_final; run;
 proc print data=out2.__table1_unempoutcome; run;
ODS HTML CLOSE;
