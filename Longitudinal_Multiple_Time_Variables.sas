*random intercept;
*'v=4' choose the fourth person to print their v matrix (because first three people have some missing data);
proc mixed data = a2 noclprint method=ml covtest noitprint;
  class ID;
  model BMI = / solution ddfm=bw;
  random intercept/subject=ID type=un v=4 vcorr=4 g;
  repeated/subject=ID R=4 ; 
run;

*level-1 variables;
proc mixed data=a2 noclprint method=ml covtest noitprint;
  class ID;
  model BMI=age beer cig/solution ddfm=bw;
  random intercept /subject=ID type=un v=4 vcorr=4 g;
  repeated/subject=ID R=4; 
run;

*random slope of level-1 variables, one at a time;
proc mixed data=a2 noclprint method=ml covtest noitprint;
  class ID  ;
  model BMI = age beer cig /solution ddfm=bw;
  random intercept age/subject=ID type=un v=4 vcorr=4 g;
  repeated /subject=ID R=4; 
run;
proc mixed data=a2 noclprint method=ml covtest noitprint;
  class ID  ;
  model BMI = age beer cig /solution ddfm=bw;
  random intercept beer/subject=ID type=un v=4 vcorr=4 g;
  repeated /subject=ID R=4; 
run;
proc mixed data=a2 noclprint method=ml covtest noitprint;
  class ID  ;
  model BMI = age beer cig /solution ddfm=bw;
  random intercept cig/subject=ID type=un v=4 vcorr=4 g;
  repeated /subject=ID R=4; 
run;

*random slope of significant level-1 variables together;
proc mixed data = a2 noclprint method=ml covtest noitprint;
  class ID  ;
  model BMI = age beer cig/solution ddfm=bw;
  random intercept age cig /subject=ID type=un v=4 vcorr=4 g;
  repeated /subject=ID R=4; 
run;

*level 2 variables;
proc mixed data = a2 noclprint method=ml covtest noitprint;
  class ID gender ed marital;
  model BMI=age beer cig ed gender marital/solution ddfm=bw;
  random intercept age cig /subject=ID type=un v=4 vcorr=4 g;
  repeated /subject=ID R=4; 
run;

*GEE models;
proc genmod data = a2; 
  class ID gender marital;
  model BMI = age beer cig gender marital/type3 dist=normal;
  repeated subject = ID/type=cs corrw; 
  *repeated subject = ID/type=un corrw;
  *repeated subject = ID/type=ar(1) corrw;
  *repeated subject = ID/type=toep corrw;
 run; 

