*lecture 4 slide 15;

*step 1;
*random intercept model;
proc mixed data = one noclprint method=ml covtest noitprint;
  class hospital ward ;
  model stress = /solution ddfm=bw ;
  random intercept /sub=hospital type=un;
  random intercept /sub=ward(hospital) type=un;
run;

*step 2;
*add level-1 variables, slopes fixed (not random);
proc mixed data=two noclprint method=ml covtest noitprint;
  class hospital ward ;
  model stress = age gender experience /solution ddfm=bw ;
  random intercept /sub=ward(hospital) type=un;
  random intercept /sub=hospital type=un;
run;

*step 3;
*add level-2 variables, slopes fixed;
proc mixed data=two noclprint method=ml covtest noitprint;
  class hospital ward ;
  model stress = age gender experience ward_type ExpCon/solution ddfm=bw ;
  random intercept /sub=ward(hospital) type=un;
  random intercept /sub=hospital type=un;
run;

*step 4;
*add level-3 variable, slope fixed;
proc mixed data=two noclprint method=ml covtest noitprint;
  class hospital ward ;
  model stress = age gender	experience ward_type ExpCon hospital_size /solution ddfm=bw ;
  random intercept /sub=ward(hospital) type=un;
  random intercept /sub=hospital type=un;
Run;

*step 5;
*add slope variation for ExpCon (experimental condition);
*ExpCon (level-2) different across hospital size (level-3);
proc mixed data=two noclprint method=ml covtest noitprint;
  class hospital ward ;
  model stress = age gender	experience ward_type ExpCon|hospital_size /solution ddfm=bw ;
  random intercept /sub=ward(hospital) type=un;
  random intercept ExpCon /sub=hospital type=un; 
run;

*step 6;
*take out insignificant variables;
proc mixed data=two noclprint method=ml covtest noitprint;
  class hospital ward ;
  model stress = age gender experience ExpCon|hospital_size /solution ddfm=bw ;
  random intercept /sub=ward(hospital) type=un;
  random intercept ExpCon /sub=hospital type=un; 
run;

*run chi-square to compare tests;
data pvalue;
df =1; chisq = 10.9;
pvalue = 1 - probchi(chisq, df);run;
proc print data = pvalue noobs;run;
