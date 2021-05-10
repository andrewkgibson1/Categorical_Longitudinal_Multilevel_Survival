*make library to store data in;
libname dig '/folders/myfolders/Survival DA/SDA Project/Data';

data dig.digdata; 
	set dig.dig;	 
run;

/*
STEP 1: Find an appropriate outcome variable
*/

*first, find most frequent hospitalization from all outcomes; 
proc freq data=dig.digdata;
	tables WHF DIG MI UANG STRK SVA VENA CREV OCVD RINF OTH;
run;

/* 
WHF and OTH were most frequent
WHF - hospitalizations due to worsening heart failure
OTH - hospitalizations due to other problems
Neither reach 50% of the risk set getting the outcome, no way to report median survival
*/

*derived outcomes;
proc freq data=dig.digdata;
	tables CVD HOSP;
run;

/*
CVD - First hospitalization for CVD cause (combination of: Worsening heart failure, Arrythmia,
	Digoxin Toxicity, MI, Unstable angina, Stroke, Coronary revascularization, 
	Cardiac transplantation, other cardiovascular)
HOSP - First hospitalization for any cause

Both reach over 50% of risk set getting the outcome
Choose HOSP since it will be easier to report median outcome
*/

/*
STEP 2: Find appropriate confounding variables
*/

/*
confounders to try:

EJF_PER - ejection fraction %: <.25, .25-.45
DIGUSE - ? previous use of digoxin (within last week): yes, no
CHFETIOL - ? cause of heart failure: ischemic, nonischemic
CHESTX - cardiothoracic ratio: <=0.55, >0.55
FUNCTCLS - NYHA class: I or II, III or IV
DIGDOSE - Dosage of digoxin/placebo: continuous (can categorize)
AGE
SEX
RACE
*/

*checking signficance;
proc phreg data=dig.digdata;
   	model HOSPDAYS*HOSP(0)= EJF_PER; 
Run;

proc phreg data=dig.digdata;
   	model HOSPDAYS*HOSP(0)= CHESTX; 
Run;

*both EJF_PER & CHESTX work;

*checking significance of variables all in model together;
proc phreg data=dig.digdata;
   	model HOSPDAYS*HOSP(0)= EJF_PER CHESTX;
   	strata TRTMT;
Run;

*all still significant;

/*
STEP 3: Descriptive statistics
*/

proc freq data=dig.digdata;
	tables TRTMT HOSP;
run;

proc means data=dig.digdata;
	var EJF_PER CHESTX HOSPDAYS;
run;

/*
STEP 4: Log-rank test 

Conduct log-rank test to test for the differences in survivor curves for the outcome variables 
stratified by the treatment variable. Graphically display the Kaplan-Meier survivor curves. 
Show the risk set and Log-rank test on the plot. Compare and interpret the median differences 
between the survivor curves based on the statistical results.
*/

proc sort data=dig.digdata;
	by TRTMT;
	
*log-rank test;
proc lifetest data=dig.digdata plots=s(test atrisk);
	time HOSPDAYS*HOSP(0);
	strata TRTMT;
	title "K-M curve";
run;

/*
*lifetable method;
PROC LIFETEST data= dig.digdata Method=life PLOTS=s INTERVALS=0 TO 2000 BY 50;
	Time HOSPDAYS*HOSP(0); 
	strata TRTMT;
 	Title "Lifetable";
run;
*/

/*
RESULTS:
log-rank test signficant, meaning there is a difference between the two curves
placebo median survival time = 565 days
treatment median survival time = 615 days
*/

/*
STEP 5: Proportional hazards assumption

Construct a final Cox model including all independent variables. Evaluate the proportional 
hazards assumption using the following methods:
 
a) 	Use a graphical approach by producing log-negative-log (LLS) plots against log-transformed 
	survival time to evaluate the proportional hazards assumption for the treatment variable. 
	Interpret the plot results.

b) 	Use Schoenfeld residuals to evaluate the PH assumption for EACH independent variable. 
	Pearson’s correlation coefficient test is used to examine whether the 
	Schoenfeld residuals are independent of ranked log-transformed follow-up time. 
	Plot the Schoenfeld residuals against the ranked log-transformed follow-up time. 
	Interpret statistical results and draw your conclusion whether each independent 
	variable meets the proportional hazards assumption.
*/

*a): lls plot;	
proc lifetest data=dig.digdata plots=lls notable;
	time HOSPDAYS*HOSP(0);
	strata TRTMT;
	title 'Checking the PH assumption using the log-log survival plots for treatment';
run; 	

*curves cross, treatment violates the PH assumption, treatment interacts with time;

*b): Schoenfeld residuals;

*step i - get residuals;
proc phreg data=dig.digdata;
   	model HOSPDAYS*HOSP(0) = EJF_PER CHESTX/ties=efron; 
   	
   	*output new data set with residuals;
	*ressch = schoenfeld residuals, gives order of residuals (first residual = first independent variable);
   	output out=sch ressch = sch_EJF_PER sch_CHESTX;  
	title 'Checking the PH assumption of independent variables using the Schoenfeld residuals';
RUN;

*step ii - log transform time
*Create a function of survival time;
Data schoenfeld;
  	set sch;
  	if HOSP=1;
  	Ldays=log(HOSPDAYS);
  	days2=HOSPDAYS**2;
run;

*rank events based on log time;
Proc Rank data=schoenfeld out=ranked1 ties=mean;
  	*The var to be ranked;
  	Var ldays; 
  	*assign the ldays var with a new rank name;
  	Ranks ldaysrank; 
run;

*sort by ranked time;
proc sort data=ranked1; 
	by ldaysrank;
run;

*step iii - correlate residuals to ranked log time & test;
Proc corr data=ranked1 nosimple;  
	var sch_EJF_PER sch_CHESTX; 
	with ldaysrank;
run;

*result: CHESTX is correlated with time, not EJF_PER

*step iv: Graph Schoenfeld residuals against survival time;
proc sgplot data=ranked1; 
    loess x = Ldaysrank y = sch_EJF_PER/ 	clm LINEATTRS=(COLOR=Black) 
    										CLMATTRS=(CLMFILLATTRS=(COLOR=Red))
    										MARKERATTRS=(size=2);
    title 'Checking the PH assumption of independent variables using the Schoenfeld residuals';
run;

*result: line seems staight, meaining EJF_PER is time-indpendednt, but not sure;

proc sgplot data=ranked1; 
    loess x = Ldaysrank y = sch_CHESTX / 	clm LINEATTRS=(COLOR=Black) 
    										CLMATTRS=(CLMFILLATTRS=(COLOR=Red))
    										MARKERATTRS=(size=2);
	title 'Checking the PH assumption of independent variables using the Schoenfeld residuals';
run;

*result: line seems to be decreasing, meaning CHESTX is time-dependent, but not sure;

*reconfirm results by checking significance of interaction terms with time;
proc phreg data=dig.digdata;

   	*model with interaction terms (defined below);
   	model HOSPDAYS*HOSP(0)= EJF_PER CHESTX t_EJF_PER t_CHESTX/rl ties=efron; 
   	strata TRTMT;
   	
   	*make interaction terms of covariates with time;
   	t_EJF_PER = EJF_PER*HOSPDAYS;
    t_CHESTX = CHESTX*HOSPDAYS;
	
	*null hyp: none of the interaction terms are significant;
	*alt hyp: at least 1 of the covariates changes with time; 
	*(check max likelihood est to see which one(s));
	Global_proportionality_test: test t_EJF_PER, t_CHESTX; 
    
	title 'Checking the PH assumption using time-dependent covariate';
RUN;

*results: CHESTX varies with time, not EJF_PER;

/*
STEP 6: Final Cox Model

Specify a final Cox model including all independent variables. 
Appropriate time interaction terms (i.e. involving followup time variable) may be included 
if any independent variable violates the proportional hazards assumption in #2. 
	You can use a calculator or Microsoft Excel to calculate those HRs involved in the 
	interactions. For those ORs, 95%CIs are not required. 
For other independent variables that are involved in any interaction terms, present 
and interpret the adjusted HR and 95%CI. Draw your conclusions for the relationship between 
the treatment and the outcome.
*/

proc phreg data=dig.digdata;
   	model HOSPDAYS*HOSP(0)= EJF_PER CHESTX TRTMT T_TRTMT t_CHESTX/rl ties=efron; 
    t_CHESTX = CHESTX*HOSPDAYS;
    T_TRTMT = TRTMT*HOSPDAYS;
RUN;

