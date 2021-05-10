*take 1000 samples of size 1000 from standard normal distribution;
%let numsample=1000;  
%let samplesize=1000;
%let mu=0; 
%let sigma=1;
data normalsamp (keep=sample id x);
    do sample=1 to &numsample; 
        seed=ceil(ranuni(54321)*1000);
        do id=1 to &samplesize; 
            x= &mu + &sigma*rannor(seed); 
            output; 
        end;
    end;
run;

*compute statistics for each sample;
ods exclude all;
proc means data= normalsamp alpha=0.05 mean lclm uclm   
      noprint;
    by sample;
    var x;
    output out=statistics 
    mean=SampleMean 
    std=s 
    lclm=Lower 
    uclm=Upper;
run;
ods exclude none;

*compute # CI with 0 mean;
data CI95; set statistics;
  paramInCI = (Lower<0 & Upper>0); **indicator variable;
  label paramInCI = "Parameter in CI";
run;

*compute proportion of CI with 0 mean; 
proc freq data=CI95;
   tables ParamInCI / out=PropCI95; 
run;

*sample from standard normal distribution;
%LET mu=20;
%LET std=110;
data normal(keep=id x);
    call streaminit(3251);
    *number of samples (200 in this case);
    do id=1 to 200;
        x=&mu + &std*rand("Normal");
       /*x=rand(“Normal”,&mu,&std);*/
        output;
    end;
run;
proc print data=normal(obs=10); run;

*sample from bernoulli distribution;
%LET N=200;
%LET p=1/2;
data bernoulli(keep=id x);
    call streaminit(3251);
    do id=1 to &N;
        x=rand("Bernoulli",&p);
        output;
    end;
run;
proc freq data=bernoulli; tables x;
proc print data=Bernoulli(obs=10); run;

*sample from binomial distibution;
%LET N=200;
%LET M=10;
%LET p=1/2;
data binomail(keep=id x);
    call streaminit(3251);
    do id=1 to &N;
        x=rand("Binomial",&p,&m);
        output;
    end;
run;
proc print data=binomail(obs=10); run;

*geometric dist;
%LET N=200;
%LET p=1/2;
data geometric(keep=id x);
    call streaminit (3251);
    do id=1 to &N;
        x=rand("Geometric",&p);
        output;
    end;
run;
proc print data=geometric(obs=10); run;

*poisson dist;
%LET N=200;
%LET Lambda=4;
data Poisson(keep=id x);
    call streaminit(3251);
    do id=1 to &N;
        x=rand("Poisson",&lambda);
        output;
    end;
run;
proc freq data=Poisson; tables x; run;
proc print data=Poisson(obs=10); run;

*uniform - discrete dist;
%LET N=200;
%LET K=6;
data uniform(keep=id x);
    call streaminit(3251);
    do id=1 to &N;
        x=ceil(&k*rand("Uniform"));
        output;
    end;
run;
proc freq data=uniform; tables x; run;
proc print data=uniform(obs=10); run;

*uniform - continuous U(a,b) dist;
%LET N=200;
%LET a=5;
%LET b=10;
data uniform(keep=id x);
    call streaminit(3251);
    do id=1 to &N;
        x=&a + (&b-&a)*rand("Uniform");
        output;
    end;
run;
proc print data=uniform(obs=10); run;

*exponential dist;
%LET N=200;
%LET sigma=10;
data Exponential(keep=id x);
    call streaminit(3251);
    do id=1 to &N;
        x=sigma*rand("Exponential");
        output;
    end;
run;
proc print data= Exponential(obs=10); run;

*chi-square dist;
%LET N=200;
%LET DF=1;
Data chisquare(keep=id x);
    call streaminit(3251);
    do id=1 to &N;
        x=rand("Chisquare",&df);
        output;
    end;
run;
proc print data= chisquare(obs=10); run;

*qq plot - slide 26;
proc univariate data=Exponential; 
    var x; 
    qqplot x /exp; 
run;

/*
simulate linear regression model:
𝑦=2.7−1.3𝑥+0.5𝑧+𝑒
where 
x ~ Bernoulli trial with p = 0.25
z ~ uniform distribution over (0,1)
e ~ N(0, 4) 
assuming x and z are independent of each other
slide 28
*/

%let N=520;  **sample size;
%let mu=2.7;  **intercept;
%let beta1=-1.3; **coefficient of X;
%let beta2=0.5;  **coefficient of Z;
%let p=0.25;  **success rate of the Bernoulli trial;
%let sigma=2;  **standard deviation of the normal distribution; 
data regdata;
 call streaminit(2145);
 do i=1 to &N;
     x=rand("Bernoulli", &p);
     z=rand("Uniform"); **uniform distribution over (0, 1);
     e=rand("Normal",0,&sigma);
     y=&mu+&beta1*x+&beta2*z+e;
     output;
 end;
run;

*run linear regression model from simulated data - should give answers close to original equation;
ods graphics off;
ods exclude all;
Proc reg data=regdata;
    model y = x z;
    ods output 
    ParameterEstimates=RegParameterEst(drop=Model Dependent); 
run;
ods graphics on;
ods exclude none;

/*
Simulate data from Logistic Regression Model
logit(𝑝)=log⁡(𝑝/(1−𝑝))=2+0.8𝑋+0.2𝑍
x ~ Bernoulli trial with p = 0.3
z ~ Normal distribution
slide 34
*/

%let N=1000;
%let mu=2;
%let beta1=0.8;
%let beta2=0.2;
%let p=0.3;
data logisticdata;
 call streaminit(2145);
 do id=1 to &n;
     x=rand("Bernoulli", &p);
     z=rand("Normal"); **standard normal;
     eta=&mu+&beta1*x+&beta2*z; **logit value;
     lambda=exp(eta)/(1+exp(eta)); **transform by inverse logit;
     y=rand("Bernoulli",lambda);  **binary response;
     output;
 end;
run;

*run logistic regression model from simulated data - should be close to original model;
ods graphics off;
ods exclude all;
Proc logistic data=logisticdata;
    class x (ref='0') / param=ref;
    model y = x z;
    ods output 
    ParameterEstimates=LogParameterEst(drop=Model Dependent); 
run;
ods graphics on;
ods exclude none;

/*
Power analysis using simulation for two sample t-test
slide 41
*/

*Step 1: simulate data;
%let n1 = 10;  %let n2 = 10;
%let NumSamples = 10000;               /* number of samples */
%let mu1=0;  **mean of pop 1;
%let sigma1=1;  **variance of pop 1;
%let mu2=1.6;  **mean of pop 1;
%let sigma2=1;  **variance of pop 1;                         
data PowerSim(drop=id);
   call streaminit(321);
   do SampleID = 1 to &NumSamples;
      c = 1; do id = 1 to &n1; x1 = rand("Normal",&mu1,&sigma1); output; end;
      c = 2; do id = &n1+1 to &n1+&n2; x1 = rand("Normal",&mu2,&sigma2); output; end;
   end;
run;

*Step 2: t-test for each sample;
ods graphics off;
ods exclude all;
ods noresults;
proc ttest data=PowerSim; 
   by SampleID; 
   class c; 
   var x1; 
   ods output ttests=TTests(where=(method="Pooled")); 
run;
ods graphics on;
ods exclude none;
ods results;

*Step 3: count number of observations reject null hypothesis;
data Results; 
   set TTests; 
   RejectH0 = (Probt <= 0.05); 
run; 

*Step 4: compute proportion of observations rejecting null hypothesis;
proc freq data=Results noprint; 
   tables RejectH0 / out=SimPower(where=(RejectH0=1));
run;
*The power of the two sample t test is 0.92;

/*
Power analysis for multilevel model
slide 51

Hypothesized a two-level model:
𝑌_𝑖𝑗=1+0.7∗𝑋_1+0.3∗𝑋_2+𝜇_0𝑗+𝑒_𝑖𝑗

𝑖: individuals
𝑗: clusters
𝑌_𝑖𝑗: continuous outcome variable
𝑋_1: level-1 continuous variable following 𝑁(0.5, 0.3)
𝑋_2: level-2 binary variable
𝜇_0𝑗: level-2 random intercept effect following 𝑁(0, 0.1)
𝑒_𝑖𝑗: level-1 residual following 𝑁(0, 0.9)

ICC = var(mu_0j) / [var(mu_0j)+var(e_ij)] = 0.1 / [0.1+0.9] = 0.1

Compute the power of detecting the effect size (0.3) of X2 and the effect size (0.7) of X1, 
given sample size: 
Total number of clusters = 20 
Number of subjects per cluster = 10
*/

%let sample=100; %let cluster=20; %let clustersize=10;
data one;
  do sim=1 to &sample; 
    do i=1 to &cluster; cluster=i;
/*At level 2, simulate a binary variable (X2:treatment) following a binomial distribution with p=.5*/
      x2=ranbin(12345,1,0.5); 
/*At level 2, simulate the random error following a normal distribution with mean 0 and level-2 variance 0.1*/
      u=SQRT(.1)*NORMAL(1000);
      do id=1 to &clustersize;
/*At level 1,simulate a continuous level-1 covariate X1  following a normal distribution with mean 0.5 and variance 0.3*/
        x1=.5+SQRT(.3)*NORMAL(1000);
/*At level 1, simulate the random error following a normal distribution with mean 0 and level-1 variance 0.9*/
        e=SQRT(.9)*NORMAL(10000); 
/*The 2-level model with a random intercept and fixed slope parameters as 1*/
        y= 1 + 0.3*x2 + 0.7*x1 + u + e;
        output;
      end;
    end;
  end;
  keep sim cluster y x1 x2; 
run;
ods listing close;

PROC MIXED DATA=one noclprint covtest noitprint method=ml;  
/**Print out estimated regression coefficients**/
  ods output SolutionF =tests;  
  CLASS cluster;
  MODEL y = x1 x2/SOLUTION ddfm=bw;
  RANDOM INTERCEPT/ SUB=cluster TYPE=UN  ;
  by sim; 
run;

*Count the data sets that result in a significant effects;
data tests; set tests; issig = Probt < .05; run;
proc freq data=tests; where Effect='x2';
  tables issig / binomial(level='1');
run;
/*power for detecting the effect (0.3) of X2 
(the percentage of times the null hypothesis is rejected) is only 42%*/
proc freq data=tests; where Effect='x1';
  tables issig / binomial(level='1');
run;
/*power for detecting the effect (0.7) of X1 
(the percentage of times the null hypothesis is rejected) is 100%*/

/*Increasing the total number of clusters to 50, and keeping the cluster size per cluster at 10
The power increases to 74%*/

%let sample=100; %let cluster=50; %let clustersize=10;
data one;
  do sim=1 to &sample; 
    do i=1 to &cluster; cluster=i;
/*At level 2, simulate a binary variable (X2:treatment) following a binomial distribution with p=.5*/
      x2=ranbin(12345,1,0.5); 
/*At level 2, simulate the random error following a normal distribution with mean 0 and level-2 variance 0.1*/
      u=SQRT(.1)*NORMAL(1000);
      do id=1 to &clustersize;
/*At level 1,simulate a continuous level-1 covariate X1  following a normal distribution with mean 0.5 and variance 0.3*/
        x1=.5+SQRT(.3)*NORMAL(1000);
/*At level 1, simulate the random error following a normal distribution with mean 0 and level-1 variance 0.9*/
        e=SQRT(.9)*NORMAL(10000); 
/*The 2-level model with a random intercept and fixed slope parameters as 1*/
        y= 1 + 0.3*x2 + 0.7*x1 + u + e;
        output;
      end;
    end;
  end;
  keep sim cluster y x1 x2; 
run;
ods listing close;

/*
Increasing the cluster size per cluster to 20 with a total number of clusters being 50
The power increases to 82%
*/

%let sample=100; %let cluster=50; %let clustersize=20;
data one;
  do sim=1 to &sample; 
    do i=1 to &cluster; cluster=i;
/*At level 2, simulate a binary variable (X2:treatment) following a binomial distribution with p=.5*/
      x2=ranbin(12345,1,0.5); 
/*At level 2, simulate the random error following a normal distribution with mean 0 and level-2 variance 0.1*/
      u=SQRT(.1)*NORMAL(1000);
      do id=1 to &clustersize;
/*At level 1,simulate a continuous level-1 covariate X1  following a normal distribution with mean 0.5 and variance 0.3*/
        x1=.5+SQRT(.3)*NORMAL(1000);
/*At level 1, simulate the random error following a normal distribution with mean 0 and level-1 variance 0.9*/
        e=SQRT(.9)*NORMAL(10000); 
/*The 2-level model with a random intercept and fixed slope parameters as 1*/
        y= 1 + 0.3*x2 + 0.7*x1 + u + e;
        output;
      end;
    end;
  end;
  keep sim cluster y x1 x2; 
run;
ods listing close;

/*
3-level model
slide 64
*/

/*Generate 100 data stes of 3 levels. Each has 25 sites and 4 clusters with  
cluster size of 10.*/
data one;
  do sim=1 to 100; 
    do k=1 to 25; site=k;
/**At level 3,simulate a binary level-3 covariate X3 following 
a binomial distribution**/
      x3=ranbin(12345,1,0.5 );
/*At level 3, simulate the random error following a normal distribution
with mean 0 and level-3 variance 0.2*/
      v=SQRT(.2)*NORMAL(1000);
      do i=1 to 4; cluster=i;
/*At level 2, simulate the random error following a normal distribution
with mean 0 and level-2 variance 0.1*/
        u=SQRT(.1)*NORMAL(1000) ;
        do id=1 to 10;
/*At level 1, simulate the random error following a normal distribution
with mean 0 and level-1 variance 0.9*/
          e=SQRT(.7)*NORMAL(10000); 
/*The 3-level model with a random intercept with
the fixed intercept paprameter as 1*/
          y= 1 + 0.3*x3 + e + u + v;
          output;
        end;
      end;
    end;
  end;
  keep sim site cluster id y x3;
run;

ods listing close;
PROC MIXED DATA=one METHOD=ML COVTEST;  
  ods output SolutionF=tests; 
  CLASS cluster site;
  MODEL y=x2/SOLUTION ddfm=bw;
  RANDOM INTERCEPT/ SUB=cluster(site) TYPE=UN;
  RANDOM INTERCEPT/ SUB=site TYPE=UN;
  by sim; 
run;

data tests;set tests; issig = Probt < .05; run;
proc freq data=tests; where Effect='x2';
  tables issig / binomial(level='1');
run;
/*
Simulating 100 balanced data sets of 25 sites and 4 clusters with a cluster size of 10 given 
ICC=0.1 at level 2 and .2 at level 3 and ES=0.3. There is no covariate at both levels. 
The resulting power is only 35%
*/

/*
increase the total number of sites to 30 and total number of clusters to 10. 
This results in a small increase in power, 40%  
*/

/*
Now increase effect size(ES) to 0.5. This results in a substantial increase in power, 83%
*/
