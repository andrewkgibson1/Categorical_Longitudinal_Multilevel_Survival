*make library to store working datasets;
libname Data '/folders/myfolders/Categorical DA/CDA Project/Data';

/*
STEP 1:
convert XPT data into SAS dataset;
*/

*Questionnaire data - smoking data;
libname xportin xport '/folders/myfolders/Categorical DA/CDA Project/Data/SMQ_I.XPT';
libname target '/folders/myfolders/Categorical DA/CDA Project/Data';
proc copy in=xportin out=target;
	select SMQ_I;
run;

*Demographics data;
libname xportin xport '/folders/myfolders/Categorical DA/CDA Project/Data/DEMO_I.XPT';
libname target '/folders/myfolders/Categorical DA/CDA Project/Data';
proc copy in=xportin out=target;
	select DEMO_I;
run;

/*
STEP 2:
Get relevant variables out of datasets;
*/

*Questionnaire dataset -> ID & Dependent variable;
*SEQN = ID number of individuals;
*SMQ040 = Do you now smoke cigarettes? (1 - Everyday, 2 - Some Days, 3 - Not At All, . - Missing(4579));
data Data.smoke;
	set Data.smq_i(keep=SEQN SMQ040);
run;

proc sort data=Data.smoke;
	by SEQN;
run;

*Demographics dataset -> Age, Gender, & Education;
*RIAGENDR - Gender (1 - Male, 2 - Female) - should be no missing;
*RIDAGEYR - Age (continuous) - anyone above 80 y/o is coded as 80;
*DMDEDUC2 - Education 20+ y/o (1 - <9th grade, 2 - 9-11th grade, 3 - HS grad/GED, 
	4 - Some college/AA, 5 - >=college grad, 9 - don't know(5), . - missing(4252));
*SDMVPSU, SDMVSTRA, WTINT2YR - used as weights for model later;
data Data.demo;
	set Data.demo_i(keep=SEQN RIAGENDR RIDAGEYR DMDEDUC2 SDMVPSU SDMVSTRA WTINT2YR);
run;

proc sort data=Data.demo;
	by SEQN;
run;

/*
STEP 3:
Merge datasets into one with all variables of interest;
*/

*merge two tables together;
data Data.comb;
	merge Data.demo Data.smoke;
	by SEQN;
run;

*check to make sure all variables of interest are included;
proc contents data=Data.comb;
run;

/*
STEP 4:
Data management,
a. rename variables to make them easier to work with
b. recode refused and unknown to missing data for each variable
c. recode missing smoke data into new variable
d. only include data for adults age 20+ for each variable
e. recode any variables with low sample in certain categories
*/

*a. rename variables;
data Data.comb1;
	rename 	SEQN = id
			SMQ040 = smoke
			RIAGENDR = sex
			RIDAGEYR = age
			DMDEDUC2 = edu
			SDMVPSU = cluster_var
			SDMVSTRA = strata_var
			WTINT2YR = weight_var;
	set Data.comb;
run;

*make sure there are no huge amounts of missing data;
*descriptives of categorical variables in model;
Proc surveyfreq data=Data.comb1;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var; 
  	table smoke sex edu/col row nostd nowt; 
Run;

*descriptives for continuous variables;
Proc surveymeans data=Data.comb1 nobs mean stderr;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var;
  	var age;
Run;

*limit to just 20+ adults subpopulation and then check descriptives for huge missing;
data Data.comb2;
	set Data.comb1;
	if age < 20 then delete;
run;

Proc surveyfreq data=Data.comb2;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var; 
  	table smoke sex edu/col row nostd nowt; 
Run;

Proc surveymeans data=Data.comb2 nobs mean stderr;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var;
  	var age;
Run;

*Over half missing in smoke category;
*This corresponds to people who smoked <100 cigarettes in life (an earlier question in the survey);

*b. recode refused and unknown to missing data
*only education has 5 counts of unknown;
data Data.comb3;
	set Data.comb2;
	edu1 = .;
	if edu = 9 then edu1 = .;
	else edu1 = edu;
run;

*check to make sure it worked;
Proc surveyfreq data=Data.comb3;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var; 
  	table edu1/col row nostd nowt; 
Run;

*c. recode missing smoke data into new variable;
data Data.comb4;
	set Data.comb3;
	smoke1 = .;
	if smoke = '.' then smoke1 = 4;
	else smoke1 = smoke;
run;

*check to make sure it worked;
Proc surveyfreq data=Data.comb4;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var; 
  	table smoke1/col row nostd nowt; 
Run;

*d. only include data that has no missing values;
*reminder, only edu1 has missing values now;
data Data.comb5;
	set Data.comb4;
	if edu1 = '.' then delete;
run;

*e. recode any variables with few or 0 samples in certain categories;
*check data for missing or 0 samples in categories;
Proc surveyfreq data=Data.comb5;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var; 
  	table smoke1 sex edu1/col row nostd nowt; 
Run;

Proc surveymeans data=Data.comb5 nobs mean stderr;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var;
  	var age;
Run;

*all categories have a good amount of observations and no missing data;

/*
STEP 5:
Descriptive statistics for each variable,
frequency, percentage, mean, and standard deviation
(Hints: different statistics for continuous and categorical variables)
*/

*descriptives of categorical variables in model;
Proc surveyfreq data=Data.comb5;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var; 
  	table smoke1 sex edu1 / nowt cl ; 
Run;

*descriptives for continuous variables;
Proc surveymeans data=Data.comb5;
  	strata strata_var;
  	cluster cluster_var;
  	weight weight_var;
  	var age;
Run;

/*
STEP 6:
Proportional Odds Model,
1. Score chi square test for checking the proportional odds assumption
2. Wald chi square test for regression coefficients for each independent variable  
3. Odds ratio (OR) and 95% confidence interval (CI) for each independent variable

All independent variables included in the final model must be statistically significant. 
Students may take several rounds of variable selection to meet this criterion

In write up:
Write and test the null hypothesis for each variable
If proportional odds assumption is not met, make a statement for the implication of that conclusion
Then, interpret the rest of model, "assuming the proportional odds assumption is met...", 
	interpret odds ratios, independent variables, etc.
*/

proc surveylogistic data=Data.comb5;
	strata strata_var;
	cluster cluster_var;
	weight weight_var;
	class sex(ref='1') edu1(ref='1');
	model smoke1(ref='4') = sex edu1 age;
run;

*NOT GIVING score chi-square for proportional odds assumption;

*use proc logistic instead to get score test;
proc logistic data=Data.comb5;
	class sex(ref='1') edu1(ref='1');
	model smoke1(ref='4') = sex edu1 age / scale=none aggregate;
run;

/*
STEP 7:
Multinomial Logit Model
*/
