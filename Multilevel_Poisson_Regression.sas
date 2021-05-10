/*
example 1, slide 9 (lecture 12), Poisson Regression (1-level only)
*/

*dataset;
data one;
  input Cytology $ Residence $ Age $ y pop;
  lpop=log(pop); /* offset:log of population size */ 
  rate=death/pop; /*death rate*/ 
cards;
L R 0-5  38 103857
L R 6-14 13 155786
L U 0-5  51 135943
L U 6-14 37 203914
M R 0-5   5 103857
M R 6-14  8 155786
M U 0-5  13 135943
M U 6-14 20 203914
;

*descriptive;
proc sgplot data = one;
  histogram rate/scale = percent showbins;
run;

*regression model;
proc genmod data=one;
  Class Cytology(ref='L') Residence(ref='R') age(ref='0-5')/param=ref;
  model y=Cytology Residence age/dist=poisson link=log offset=lpop;
run;

*check for overdispersion;
data pvalue;
  *df = # observations - # parameters in model = 8-4;
  df=4; 
  *from output of regression model;
  chisq=13.9; 
  pvalue=1-probchi(chisq,df);
run;
proc print data=pvalue noobs;
run;
*result: significant, there is overdispersion;

*add scale=pearson to adjust for overdispersion;
*estimate beta coefficients & test for significance after exponentiating;
proc genmod data=one;
  class Cytology(ref='L') Residence(ref='R') age(ref='0-5')/param=ref;
  model y=Cytology Residence age/dist=poisson link=log offset=lpop scale=pearson CL; 
  estimate 'L vs M' Cytology 1 -1/exp;
  estimate 'R vs U' Residence 1 -1/exp;
  estimate '0-5 vs 6-14' age 1 -1/exp;
run;

/*
Multilevel Poisson Regression, slide 51 (Lecture 12)
*/

*MODEL 1: random-intercept only;
proc glimmix data=seizure noitprint noclprint;
  class subject;
  model seizure = / solution dist = poisson link = log;
  random int / subject=subject type=un;
  COVTEST / WALD;
run;
*Fit Stats: Generalized chi-square/df > 1, meaning overdispersion;

*MODEL 2: correct overdispersion in model 1 (random _residual_ option);
proc glimmix data=seizure noitprint noclprint;
  class subject;
  model seizure =  /solution dist=poisson link=log;
  random int /subject=subject type=un;
  random _residual_ /subject=subject;
  COVTEST / WALD; 
run;

*MODEL 3: Adding level-1 variable;
proc glimmix data=seizure noitprint noclprint;
  class subject;
  model seizure =visit / solution dist=poisson link=log;
  random int /subject=subject type=un;
  random _residual_ /subject=subject;
  estimate 'visit' visit 1 /exp cl;
  COVTEST / WALD;
run;

*MODEL 4: Random slope of level-1 variable;
proc glimmix data=seizure noitprint noclprint;
  class subject ;
  model seizure =visit / solution dist=poisson link=log;
  random int visit/subject=subject type=un;
  random _residual_ /subject=subject;
  estimate 'visit' visit 1 /exp cl;
  COVTEST / WALD;
run;
*result: variance is non-estimable due to the divergence of the algorithm, don't assess random slope;

*MODEL 5: Remove random slope, add level-2 variables;
proc format; value treatment 1='1:Treatment' 0='2:Placebo';  run;
proc glimmix data=seizure noitprint noclprint;
  class subject treatment;
  model seizure = base age treatment visit/solution dist=poisson link=log;
  random int /subject=subject type=un;
  random _residual_ /subject=subject;
  estimate 't vs p' treatment 1 -1/exp cl;
  estimate 'base' base 1 /exp cl;
  estimate 'age' age 1 /exp cl;
  estimate 'visit' visit 1  /exp cl;
  format treatment treatment.;
  COVTEST / WALD;
run;
*results: slide 65 (lecture 12)

