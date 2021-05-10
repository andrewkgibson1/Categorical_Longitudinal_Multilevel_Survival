/*
Nominal outcomes
*/

*MODEL 1: intercept-only model;
*laplace estimator;
proc glimmix data = one noclprint METHOD=laplace noclprint;
  class ID y;
  model y(ref='1') = / solution DIST=MULTINOMIAL LINK=GLOGIT oddsratio(DIFF=LABEL); 
  random int /subject=ID type=un GROUP=Y;
  COVTEST / WALD; 
run;

*MODEL 2: add time variable;
proc glimmix data=one noclprint METHOD=laplace noclprint;
  class ID y;
  model y(ref='1') = time / solution DIST=MULTINOMIAL LINK=GLOGIT oddsratio(LABEL); 
  random int /subject=ID type=un GROUP=Y;
  COVTEST / WALD;
run;

*test model 2 to model 1;
data pvalue;
	df = 2; 
	chisq = 2664.09- 2420.41;
	pvalue = 1 - probchi(chisq, df);
run;

*MODEL 3: random slope of time variable;
proc glimmix data=one noclprint METHOD=laplace noclprint;
  class ID y section;
  model y(ref='1') = time / solution DIST=MULTINOMIAL LINK=GLOGIT oddsratio(LABEL); 
  random int time /subject=ID type=un GROUP=Y;
  COVTEST / WALD;
run;
*result: cannot test this, model does not converge, Gmatrix is not positive definite;

*MODEL 4: add level 2 variables;
proc format; value section 0='2=No' 1='1=Yes'; run;
proc glimmix data=one noclprint METHOD=laplace noclprint;
  class ID y section;
  model y(ref='1') = time section / solution DIST=MULTINOMIAL 
    LINK=GLOGIT oddsratio(LABEL); 
  random int  /subject=ID type=un GROUP=Y;
  format section section.;
  COVTEST / WALD; 
run;

*test model 4 to model 2;
data pvalue;
df =2; chisq = 2420.41-2303.69;
pvalue = 1 - probchi(chisq, df); run;
proc print data = pvalue noobs; run;

/*
Can do the previous with:
quadrature (METHOD=quad)
pseudo-likelihood (ddfm=bw, no method option) - doesn't normally work
*/

/*
Ordinal outcomes
*/

*MODEL 1: Random-intercept only;
proc glimmix data=one METHOD=laplace noitprint noclprint;
  class Id;
  model y (desc) = / solution DIST=MULTINOMIAL LINK=CLOGIT;
  random int /subject=ID type=un;
  COVTEST / WALD;
run;

*MODEL 2: Add time variable;
proc glimmix data=one METHOD=laplace noitprint noclprint;
  class Id;
  model y (desc) = week / solution DIST=MULTINOMIAL LINK=CLOGIT oddsratio(LABEL); 
  random int / subject=ID type=un;
  COVTEST / WALD;
run;

*MODEL 3: Add random slope of time variable;
proc glimmix data=one METHOD=laplace noitprint noclprint;
  class Id;
  model y (desc) = week / solution DIST=MULTINOMIAL LINK=CLOGIT oddsratio(LABEL); 
  random int week/subject=ID type=un;
  COVTEST / WALD;
run;

*MODEL 4: Add level-2 treatment variable;
proc format; value treatment 0='2=Placebo' 1='1=Drug'; run;
proc glimmix data=one METHOD=laplace noitprint noclprint;
  class Id;
  model y (desc) = week|treatment/ solution DIST=MULTINOMIAL LINK=CLOGIT oddsratio(LABEL); 
  random int week/subject=ID type=un;
  COVTEST / WALD;
run;
*oddsratio(LABEL at week=0 unit week=1);
*oddsratio(LABEL at week=1 unit week=1);
*oddsratio(LABEL at week=2 unit week=1);

/*
Treating ordinal outcome as continuous
when outcome has 5+ levels
*/

data two; 
	set one;
	if CC=1 and TV=0 then trt='1=CC';
	if CC=0 and TV=1 then trt='2=TV';
	if CC=1 and TV=1 then trt='3=CC&TV';;
	if CC=0 and TV=0 then trt='4=Control';;
run;

*MODEL 1: Intercept-only;
proc mixed data=two method=ml noitprint noclprint covtest ;
	class School Class   ;
	model ypost =  / solution ddfm=satterth ; 
	RANDOM INTERCEPT/ SUBJECT=class(school)   type=un;
	RANDOM INTERCEPT/ SUBJECT= school  type=un;
run;

*MODEL 2: add level-1 variable;
proc mixed data=two method=ml noitprint noclprint covtest;
  class School Class;
  model ypost = ypre / solution ddfm=satterth ; 
  RANDOM INTERCEPT/ SUBJECT=class(school)   type=un;
  RANDOM INTERCEPT/ SUBJECT= school  type=un;
run;

*MODEL 3: random slope of level-1 variable;
proc mixed data=two method=ml noitprint noclprint covtest;
  class School Class;
  model ypost = ypre / solution ddfm=satterth; 
  RANDOM INTERCEPT ypre/ SUBJECT=class(school)  type=un;
  RANDOM INTERCEPT/ SUBJECT= school  type=un;
run;

*MODEL 4: remove random slope, add level-2 variable;
proc mixed data=two method=ml noitprint noclprint covtest ;
  class School Class trt ;
  model ypost = ypre trt/ solution ddfm=satterth ; 
  RANDOM INTERCEPT/ SUBJECT=class(school)   type=un;
  RANDOM INTERCEPT/ SUBJECT= school  type=un;
run;

/*
both ordinal logistic regression and quadrature methods do not work with so many levels
*/
