*ordinary logistic regression;
Proc logistic data=one desc; 
  model hrtatt = aspirin; 
run;

/*
2-level logit modeling with default METHOD=RSPL;
*/

*intercept-only;
proc glimmix data=two noclprint noitprint;
  class id;
  *need 'desc' to model 1 vs. 0 (have disease compared to not having) or 'ref=first' or 'ref=0';
  *dist=binary and link=logit are both necessary;
  model ri(desc) = / ddfm=bw solution dist=binary link=logit oddsratio(LABEL);
  random intercept /subject=ID type=UN;
  random  _residual_ /subject=ID v vcorr;
  COVTEST / WALD;
run; 

*add level-1 variable;
proc glimmix data=two noclprint noitprint;
  class id;
  model ri(desc) = visit/ ddfm=bw solution dist=binary link=logit oddsratio(LABEL);
  random  intercept /subject=ID type=UN;
  random  _residual_ /subject=ID v vcorr;
  COVTEST / WALD; 
run;

*cannot compare model fit with current default method (METHOD=RSPL)

*add level-2 variables and random slope for level-1;
proc glimmix data=two noclprint noitprint;
  class id gender vitamin(ref='0');
  Model ri(desc) = visit age gender vitamin / ddfm=bw solution dist=binary link=logit  
  oddsratio(LABEL);
  random intercept visit /subject=ID type=UN;
  random _residual_ /subject=ID v vcorr;
  COVTEST / WALD;
run;

/*
GEE Modeling;
*/

proc genmod data=two DESCENDING;
  class ID gender vitamin(ref='0');
  model ri(desc) = gender visit vitamin age/ dist = bin link = logit;
  repeated subject=ID / type=un corrw;
  *repeated subject=ID / type=exch corrw;
  *repeated subject=ID / type=ar(1) corrw;                 
  *repeated subject=ID / type=toep(5) corrw;                                 
run;

*compare QIC values between the four correlation matrices;

/*
2-level logit modeling with METHOD=LAPLACE;
exclude v & vcorr matrices by not including 'random _residual_' option;
assume R is identity matrix;
*/

*MODEL 1: intercept-only;
proc glimmix data=two noclprint method = laplace;
  class id;
  model ri(desc)= / ddfm=bw solution dist=binary link=logit oddsratio(LABEL);
  random int /subject=ID type=un;
  COVTEST / WALD;
run;

*MODEL 2: add level-1 variable;
proc glimmix data=two noclprint method=laplace;
  class id;
  model ri(desc) = visit/ddfm=bw solution dist=binary link=logit oddsratio(LABEL);
  random int /subject=ID type=un;
  COVTEST / WALD; 
run;

*compare -2LL between models;
data pvalue;
	df =1; 
	chisq = 1361.3- 1356.8;
	pvalue = 1 - probchi(chisq, df);
run;
*signficant difference between models;

*MODEL 3: add random slope of level-1 variable;
proc glimmix data=two noclprint method=laplace;
  class id;
  model ri(desc)=visit/ddfm=bw solution dist=binary link=logit oddsratio(label);
  random int visit/subject=ID type=un;
  COVTEST / WALD;
run;
*variance of slope not significant so keep slope invariant (UN(2,2) > 0.05);

*should test model 3 to model 2, especially if UN(1,2) < 0.05;

*MODEL 4: add level-2 variables;
proc glimmix data=two noclprint method=laplace;
  class id gender vita(ref=first);
  model ri(desc) =visit vita age gender/ddfm=bw solution dist=binary link=logit oddsratio(label);
  random int /subject=ID type=un;
  COVTEST / WALD;
run;

*compare model 4 to model 2;
data pvalue;
	df =3; 
	chisq = 1356.8-1344.7;
	pvalue = 1 - probchi(chisq, df);
run;
*significantly different;
