/*

  final HL outcome code is in 
     "O:\Harvard\Hwang\STARRSLS\AnalyticFiles_NEW\6_LSW4_variables\Separation_analysis_makedata\_0e1_outcome_xtabs.sas"

  final emp outcome code is in
     "O:\Harvard\Hwang\STARRSLS\AnalyticFiles_NEW\6_LSW4_variables\Separation_analysis_makedata\_0e_emp_variables_updated.sas"

  final SA outcome code is in
     "O:\Harvard\Hwang\STARRSLS\AnalyticFiles_NEW\6_LSW4_variables\Separation_analysis_makedata\_0f_outcome_SA_final.sas"

outcomes

unemployment: 


data: irvlsw4.emp_outcome
outcome: unempoutcome
stack variable: stack

data: irvlsw4.homeless_outcome
outcome: hl_final 
stack variable: stack
 
 


SA outcome saved:
sepfile.stackf_SA_outcome 
outcome: SA_outcomef
stack variable: int

 
*/

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
LIBNAME out "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_Variables/DATA/imps/";
LIBNAME aagmc21 "&pathO./VARIABLES/APDS_VARS/AAGtoUM/Dec2021";
LIBNAME aagvars "&pathO./VARIABLES/APDS_VARS/AAGtoUM/Dec2019";
LIBNAME aagvar16 "&pathO./VARIABLES/APDS_VARS/AAGtoUM/Dec2016";
LIBNAME master   "&pathO./VARIABLES/APDS_VARS/HARVARD_VARS/TAIHOD_EQUIVALENT/DATA" ACCESS=READONLY;
libname mp "&pathO./Harvard/Hwang/APDS/DATA/";
LIBNAME ndata "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/DATA/" ACCESS=READONLY;
LIBNAME l4wgt "&pathO./VARIABLES/STARRSLS_VARS/HARVARD/6_LSW4_Variables/DATA/weights/" ACCESS=READONLY;
LIBNAME hdata "&pathO./Harvard/Hwang/STARRSLS/Separation/DATA/";

libname ls1sep "&pathU./LS1/separation_ls1/data";
libname ls2sep "&pathU./LS2/separation_ls2/data";

libname sepdate "&pathO./Harvard/king/ls3/SA/forjs/";

libname sepfile "&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/";


%LET output=&pathO./Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/DATA/; 


/*
final outcome files with weight and design variables
*/


/*
suicide attempt
*/
proc sort data=sepfile.stackf_SA_outcome out=saout; by masterid;
data saout1  ;
 merge saout (in=a) irvlsw4.ls4sepdat_wgt;
 by masterid;
 if a; 
proc sort; by masterid stack;
run;

proc contents varnum;
run;
proc freq ;
tables lslast_septyp*moact_lastls    
/list missing;
run;


proc freq data=saout1;
tables stack stack*SA_outcomef
	/list missing;
	run;
proc freq data=saout1;
tables  stack*SA_outcomef
	/list missing;
weight weight_lsw3_or_lsw4_norm1;


run;

data sepfile._outcomesf_SA;
 retain masterid stack SA_outcomef  weight_lsw3_or_lsw4_norm1 nowgt str secu;
 set saout1 (keep=masterid stack SA_outcomef  weight_lsw3_or_lsw4_norm1  str secu);
 nowgt=1;
run;
/*
homeless
*/
proc sort data=irvlsw4.homeless_outcome out=hlout; by masterid;
data hlout1  ;
 merge hlout (in=a) irvlsw4.ls4sepdat_wgt;
 by masterid;
 if a; 
proc sort; by masterid stack;
run;

proc contents varnum;
run; 


data sepfile._outcomesf_HL;
 retain masterid stack hl_final  weight_lsw3_or_lsw4_norm1 str secu;
 set hlout1 (keep=masterid stack hl_final  weight_lsw3_or_lsw4_norm1 str secu);
 nowgt=1;
run;

/*
unemployment
*/
proc sort data=irvlsw4.emp_outcome out=emout; by masterid;
data emout1  ;
 merge emout (in=a) irvlsw4.ls4sepdat_wgt;
 by masterid;
 if a; 
proc sort; by masterid stack;
run;

proc contents varnum;
run; 
data sepfile._outcomesf_EM;
 retain masterid stack unempoutcome  weight_lsw3_or_lsw4_norm1 str secu;
 set emout1 (keep=masterid stack unempoutcome  weight_lsw3_or_lsw4_norm1  str secu);
 nowgt=1;
run;


/***********
distributions

sepfile._outcomesf_SA
************/

proc freq data=sepfile._outcomesf_SA;
tables stack stack*SA_outcomef
/list missing;
run;

/*
SA_outcomef

SA_outcomef
*/  
 %macro getdistso(infile=,outcome=,);

proc sort data=&infile out=outcome1;
 by str secu;

/*total*/
 
proc surveymeans data=outcome1 nobs mean stderr  ; 
 strata str; cluster secu;
 var &outcome;
 weight weight_lsw3_or_lsw4_norm1;
 ods output statistics=_tot; 

proc freq data=outcome1  ;
tables  &outcome
/list missing;
ods output onewayfreqs=_totn;
data _totn1;
 set _totn;
 if &outcome=1;
run;
data totrow;
 retain varname n frequency mean stderr;
  merge _tot _totn1 (keep=frequency);
  mean=mean*100;
  stderr=stderr*100;
run;


proc surveymeans data=outcome1 nobs mean stderr  ;
 domain stack;
 strata str; cluster secu;
 var &outcome;
 weight weight_lsw3_or_lsw4_norm1;
 ods output domain=_out; 
data _out;
 set _out;
  mean=mean*100;
  stderr=stderr*100;
run;

proc freq data=outcome1  ;
tables stack*&outcome
/list missing;
ods output list=_outn;
data _outn1;
 set _outn;
 if &outcome=1;
run; 
data stackrow;
 retain varname stack n frequency mean stderr;
  merge _out _outn1 (keep=frequency); 
run;

data _dists_&outcome;
 retain varname stack n frequency mean stderr;
 set totrow stackrow;
run;

%mend;

 %getdistso(infile=sepfile._outcomesf_SA, outcome=SA_outcomef)
 %getdistso(infile=sepfile._outcomesf_HL, outcome=hl_final)
 %getdistso(infile=sepfile._outcomesf_EM, outcome=unempoutcome)


 proc print data=_dists_SA_outcomef;
 proc print data=_dists_hl_final;
 proc print data=_dists_unempoutcome;
 run;
