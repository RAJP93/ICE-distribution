
libname in "...";

data derive;
set in.derive;
keep ID memaxdemaxratio fattyliver age sex smoke drinksperweek diab sbp hrx lipidrx chol hdl trig glucose;
run;

proc means data=derive median;
var age drinksperweek sbp chol hdl trig glucose;
run;


data derive;
set derive;
fattyliverC=1-fattyliver;
sexC=1-sex;
smokeC=1-smoke;
diabC=1-diab;
hrxC=1-hrx;
lipidrxC=1-lipidhrx;
AGEb=(AGE>49);
DPWb=(DrinksPerWeek>1.5);
DPWbC=1-DPWb;
SBPb=(SBP>120);
CHOLb=(chol>189);
HDLb=(HDL>52);
TRIGb=(trig>98);
GLUb=(Glucose>96);
run;

PROC STANDARD DATA=derive STD=1 MEAN=0 OUT=deriveS;
  VAR age drinksperweek sbp chol hdl trig glucose;
RUN;


proc sort data = deriveS; by fattyliver; run;


ods output SolutionF=outputF CovParms=outputR;
proc mixed data = deriveS;
model memaxdemaxratio = fattyliver age sex diab sbp hrx/solution;
random fattyliver ageb sex diab sbpb hrx/subject=ID;
run;


*Flexible ICE disribution;
%Global n;
%let n=5;

ods output PostSummaries=test4FR;
proc mcmc data=deriveS seed=333 nbi=100000 nmc=500000 thin=100 statistics=summary DIC monitor=( betaF betaC tauC p pe H He SD SDe) outpost=outFixed3FR; 
*proc mcmc data=deriveS seed=2021 nbi=100000 nmc=500000 thin=100 statistics=summary DIC monitor=( betaF betaC tauC p pe H He SD SDe) outpost=outFixed4FR; 
*proc mcmc data=deriveS seed=3031993 nbi=100000 nmc=500000 thin=100 statistics=summary DIC monitor=( betaF betaC tauC p pe H He SD SDe) outpost=outFixed5FR;
*proc mcmc data=deriveS seed=23092011 nbi=100000 nmc=500000 thin=100  statistics=summary DIC monitor=( betaF betaC tauC p pe H He SD SDe) outpost=outFixed7FR;
array p[&n];
array pe[3];
array de[3];
array d[&n];

do j = 1 to &n;
d[j]=0.5;
end;

do j = 1 to 3;
de[j]=0.5;
end;

array H[&n];
array SD[&n];
array He[2];
array SDe[3];

array betaC[5];
array tauC[5];

parms p pe H1=0.60 H2=0.61 H3=0.62 H4=0.63 H5=0.64 SD1-SD&n=0.64 He1=-0.1 He2=0 SDe1-SDe3=1 beta0=5.9 betaC1=0.36 betaC2=0.67 betaC3=0.56 betaC4=0.33 betaC5=0.43 s0=1.06 tauC1=0.5 tauC2=0.85 tauC3=0.55 tauC4=0.52 tauC5=1.88;

*Set priors;
prior beta0  ~ normal(0, var=10000);
prior betaC: ~ normal(0, var=1000); 
prior tauC: ~igamma(shape=0.01, scale=0.01);

prior  s0: ~ uniform(0, 10);
prior H: ~ normal(0, var=10000);
prior SD: ~ uniform(0, 10);

prior p ~ dirich(d);
prior pe ~ dirich(de);


if ((H1 < H2) AND (H2<H3) AND (H3<H4) AND (H4<H5)) then
ll=log(p[1]*exp(lpdfnorm(ICE, H[1], SD[1]))+p[2]*exp(lpdfnorm(ICE, H[2], SD[2]))+p[3]*exp(lpdfnorm(ICE, H[3], SD[3]))+p[4]*exp(lpdfnorm(ICE, H[4], SD[4]))+p[5]*exp(lpdfnorm(ICE, H[5], SD[5])));
else
ll=.;

if ((He1 < He2) AND (He2<(-pe[1]*He[1]-pe[2]*He[2])/pe[3])) then
lle=log(pe[1]*exp(lpdfnorm(e, He[1], SDe[1]))+pe[2]*exp(lpdfnorm(e, He[2], SDe[2]))+pe[3]*exp(lpdfnorm(e, (-pe[1]*He[1]-pe[2]*He[2])/pe[3], SDe[3])));
else
lle=.;


random ICE ~ general(ll) subject=ID INIT=0.62 monitor=(ICE_4);
random e ~ general(lle) subject=ID INIT=0 monitor=(e_4);

random U1 ~ normal(0, sd=tauC1) subject=ID;
random U2 ~ normal(0, sd=tauC2) subject=ID;
random U3 ~ normal(0, sd=tauC3) subject=ID;
random U4 ~ normal(0, sd=tauC4) subject=ID;
random U5 ~ normal(0, sd=tauC5) subject=ID;

mu = beta0  + ICE*fattyliver + betaC1*age +U1*ageb+(betaC2+U2)*sex +(betaC3+U3)*diab+ betaC4*sbp+U4*sbpb +(betaC5+U5)*hrx;

if ((He1 < He2) AND (He2<(-pe[1]*He[1]-pe[2]*He[2])/pe[3])) then
ll2=log(pe[1]*exp(lpdfnorm(memaxdemaxratio-mu, He[1], SDe[1]))+pe[2]*exp(lpdfnorm(memaxdemaxratio-mu, He[2], SDe[2]))+pe[3]*exp(lpdfnorm(memaxdemaxratio-mu, (-pe[1]*He[1]-pe[2]*He[2])/pe[3], SDe[3])));
else
ll2=.;
model memaxdemaxratio ~ general(ll2);

run;


libname out "...";

*chain 1;

data check;
set outFixed3FR;
keep iteration betaC1 betaC2 betaC3 betaC4 betaC5 s0 ICE: U1: U2: U3: U4: U5: e:;
run;


data check0;
set check;
keep iteration betaC1 betaC2 betaC3 betaC4 betaC5 s0;
run;

proc transpose data=check out=checka;
by iteration;
var ICE_:;
run;

data checka;
  set checka (rename=(col1=ICE));
  ID=input(substr(_name_, 5), 5.);
  drop _name_;
run; 


proc transpose data=check out=checkb;
by iteration;
var U1_:;
run;

data checkb;
  set checkb (rename=(col1=U1));
  ID=input(substr(_name_, 4), 5.);
  drop _name_;
run; 

proc transpose data=check out=checkc;
by iteration;
var U2_:;
run;

data checkc;
  set checkc (rename=(col1=U2));
  ID=input(substr(_name_, 4), 5.);
  drop _name_;
run; 

proc transpose data=check out=checkd;
by iteration;
var U3_:;
run;

data checkd;
  set checkd (rename=(col1=U3));
  ID=input(substr(_name_, 4), 5.);
  drop _name_;
run; 

proc transpose data=check out=checke;
by iteration;
var U4_:;
run;

data checke;
  set checke (rename=(col1=U4));
  ID=input(substr(_name_, 4), 5.);
  drop _name_;
run; 

proc transpose data=check out=checkf;
by iteration;
var U5_:;
run;

data checkf;
  set checkf (rename=(col1=U5));
  ID=input(substr(_name_, 4), 5.);
  drop _name_;
run; 

proc transpose data=check out=checkg;
by iteration;
var e_:;
run;

data checkg;
  set checkg (rename=(col1=e));
  ID=input(substr(_name_, 3), 5.);
  drop _name_;
run; 
proc sort data=checka; by ID iteration; run;
proc sort data=checkb; by ID iteration; run;
proc sort data=checkc; by ID iteration; run;
proc sort data=checkd; by ID iteration; run;
proc sort data=checke; by ID iteration; run;
proc sort data=checkf; by ID iteration; run;
proc sort data=checkg; by ID iteration; run;


data checkfinal;
merge checka checkb checkc checkd checke checkf checkg;
by ID iteration;
run;

proc sort data=checkfinal; by iteration; run;
proc sort data=check0; by iteration; run;

data checkfinal;
merge checkfinal check0;
by iteration;
run;

data covariates;
set DeriveS;
keep ID fattyliver memaxdemaxratio age ageb sex diab sbp sbpb hrx;
run;

proc sort data=checkfinal; by ID; run;
proc sort data=covariates; by ID; run;

data checkfinal;
merge checkfinal covariates;
by ID;
run;

data checkfinal;
set checkfinal;
mu = ICE*fattyliver + betaC1*age +U1*ageb+(betaC2+U2)*sex +(betaC3+U3)*diab+ betaC4*sbp+U4*sbpb +(betaC5+U5)*hrx;
DROP U: beta: ;
run;

data out.CFIVE333;
set checkfinal;
run;



*IPTW;
proc logistic data=deriveS outmodel=pout;
  class fattyliver sex smoke diab diab hrx lipidrx;
  model fattyliver(event='1')= age sex smoke drinksperweek diab sbp hrx lipidrx chol hdl trig glucose;
run;

proc logistic inmodel=pout;
  score clm data = deriveS out=pred ;
run;

data deriveS2;
set pred;
if fattyliver=1 then iptWeight=1/P_1;
else iptWeight=1/(1-P_1);
run;

proc mixed data= deriveS2;
	class fattyliver(ref='0');
     weight iptWeight;
     model memaxdemaxratio = fattyliver /solution;
run;

*Bootstrap SE IPTW;

PROC SURVEYSELECT DATA=deriveS OUT=outboot_NonParm
SEED=1993
METHOD=URS
SAMPRATE=1 /*get the sample of the same size as the original dataset*/
OUTHITS
REP=10000;
RUN;



%Macro BOOTSTRAP_SE(data=, nend=);
data input;
set &data;
where replicate=1;
run;

proc logistic data=input outmodel=pout;
  class fattyliver sex smoke diab diab hrx lipidrx;
  model fattyliver(event='1')= age sex smoke drinksperweek diab sbp hrx lipidrx chol hdl trig glucose;
run;

proc logistic inmodel=pout;
  score clm data = deriveS out=pred ;
run;

data intermediate;
set pred;
if fattyliver=1 then iptWeight=1/P_1;
else iptWeight=1/(1-P_1);
run;

ods output SolutionF=output;
proc mixed data= intermediate;
	class fattyliver(ref='0');
    model memax = fattyliver /solution;
	weight iptWeight;
run;

data output;
set output;
where fattyliver=1;
replicate=1;
keep replicate Estimate StdErr;
run;

data outputb;
set output;
run;

%DO i=2 %to &nend;
data input;
set &data;
where replicate=&i;
run;

proc logistic data=input outmodel=pout;
  class fattyliver sex smoke diab diab hrx lipidrx;
  model fattyliver(event='1')= age sex smoke drinksperweek diab sbp hrx lipidrx chol hdl trig glucose;
run;

proc logistic inmodel=pout;
  score clm data = deriveS out=pred ;
run;

data intermediate;
set pred;
if fattyliver=1 then iptWeight=1/P_1;
else iptWeight=1/(1-P_1);
run;

ods output SolutionF=output;
proc mixed data= intermediate;
	class fattyliver(ref='0');
    model memax = fattyliver /solution;
	weight iptWeight;
run;

data output;
set output;
where fattyliver=1;
replicate=&i;
keep replicate Estimate StdErr;
run;

data outputb;
set outputb output;
run;
%end;
%mend;

options NONOTES;
options nosource;
ods exclude all;
%BOOTSTRAP_SE(data=Outboot_nonparm, nend=10000);
ods exclude none;
options source;
options NOTES;
