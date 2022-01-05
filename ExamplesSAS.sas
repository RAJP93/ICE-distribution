
Libname SAS "...";


%macro simdata(nsim=, n=, pC=, a0=, aC=, bC=, b0=, b1=, sigma=, tau0=, tau1=, tau2=, tau3=, tau4=, mu1=, mu2=, mu3=, pM=);
data SEM;
call streaminit(1993);       /* set random number seed */
DO sim=1 to &nsim by 1;
	DO ID=1 to &n by 1;
		
		pC=&pC;
		
		a0=&a0;
		aC=&aC;
		
		bC=&bC;
		b0=&b0;
		b1=&b1;
		sigma=&sigma;

		tau0=&tau0;
		tau1=&tau1;
		tau2=&tau2;
		tau3=&tau3;
		tau4=&tau4;
		mu1=&mu1;
		mu2=&mu2;
		mu3=&mu3;
		pM=&pM;

		if(&tau0>0) then	u0= rand('Normal',0,&tau0);
		else u0=0;

		u1= rand('Normal',0,&tau1);
		u2= exp(rand('Normal',&mu1,&tau2))-exp(&mu1+(&tau2**2)/2);

		M= rand('Bernoulli',pM);
		u3= M*rand('Normal',&mu2,&tau3)+(1-M)*rand('Normal',&mu3,&tau4)-(&pM*&mu2+(1-&pM)*&mu3);
		
		C= rand('Bernoulli',pC);
			if(C=0) then do;
			C=-&pC;
			end;
			else do;
			C=(1-&pC);
				end;

			pA = exp(&a0+&aC*(C))/(1+exp(&a0+&aC*(C)));
			uA = rand('UNIFORM');
			A= (pA>uA);

			e_0=rand('Normal',0,&sigma);
			Y1= &b0 + u0 + (&b1+u1)*A + (&bC)*C+ e_0;
			Y2= &b0 + u0 + (&b1+u2)*A + (&bC)*C+ e_0;
			Y3= &b0 + u0 + (&b1+u3)*A + (&bC)*C+ e_0;
	
			Y1c= &b0 + u0 + (&b1+u1)*(1-A) + (&bC)*C+ e_0;
			Y2c= &b0 + u0 + (&b1+u2)*(1-A) + (&bC)*C+ e_0;
			Y3c= &b0 + u0 + (&b1+u3)*(1-A) + (&bC)*C+ e_0;
			ICE1=(&b1+u1);
			ICE2=(&b1+u2);
			ICE3=(&b1+u3);
			OUTPUT;
	END;	
END;  
run;

%mend simdata;


*example 1 - 3;
%simdata(nsim=100, n=1000, pC=0.3, a0=-3, aC=0.7, bC=5, b0=120, b1=-15, sigma=5, tau0=5, tau1=10, tau2=sqrt(0.5), tau3=10, tau4=5, mu1=4, mu2=-30, mu3=10, pM=0.6);


%Global n;
%let n=5;

ods select none;
ods output PostSummaries=test1;
proc mcmc data=SEM seed=5235 nbi=10000 nmc=100000 thin=1000 statistics=summary  monitor=(beta0 betaC s0 p H SD ICE)outpost=outFixed1(keep= sim iteration p: H: SD: ICE:) diag=none plots=none ;
*Set starting values;
array p[&n];
array d[&n];

do j = 1 to &n;
d[j]=0.5;
end;

array H[&n];
array SD[&n];
parms p H1-H&n SD1-SD&n beta0=120 betaC=5 s0=7;

*Set priors;
prior beta0 betaC : ~ normal(0, var=1e6);
prior  s0: ~ uniform(0, 100);
prior H: ~ normal(0, var=1e6);
prior SD: ~ uniform(0, 100);
prior p ~ dirich(d);


l=0;
do j = 1 to &n;
l=l+p[j]*exp(lpdfnorm(ICE, H[j], SD[j]));
end;
ll=log(l);

random ICE ~ general(ll) subject=ID INIT=0;

mu = beta0  + ICE*A + betaC*C;
model Y1 ~ Normal(mu,sd=s0);
*Change to Y2 and Y3 respectively for other examples;
by sim;
run;

data want1;
   set Outfixed1(keep= sim iteration ICE_:);
run;

proc transpose data=want1 out=ICE_long1;
by sim iteration;
run;

data ICE_long1;
  set ICE_long1 (rename=(col1=ICE));
  ID=input(substr(_name_, 5), 5.);
  drop _name_;
run; 

