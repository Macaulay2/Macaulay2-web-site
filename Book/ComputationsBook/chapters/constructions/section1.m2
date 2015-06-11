-----------------------
----1st section: Curves:
-----------------------

--Plane curves:

randomPlanePoints = (delta,R) -> (
     k:=ceiling((-3+sqrt(9.0+8*delta))/2); eps:=delta-binomial(k+1,2);
     if k-2*eps>=0 
     then minors(k-eps,random(R^(k+1-eps),R^{k-2*eps:-1,eps:-2}))
     else minors(eps,random(R^{k+1-eps:0,2*eps-k:-1},R^{eps:-2})));


distinctPoints = (J) -> (
     singJ:=minors(2,jacobian J)+J;
     codim singJ==3);


randomNodalCurve = method();

randomNodalCurve (ZZ,ZZ,Ring) := (d,g,R) -> (
     delta:=binomial(d-1,2)-g;
     K:=coefficientRing R;
     if (delta==0) 
     then (	--no double points   
	  ideal random(R^1,R^{-d}))
     else (      --delta double points     	  
     	  Ip:=randomPlanePoints(delta,R);
     	  --choose the curve
     	  Ip2:=saturate Ip^2;
     	  ideal (gens Ip2 * random(source gens Ip2, R^{-d}))));

isNodalCurve = (I) -> (
	  singI:=ideal jacobian I +I;
	  delta:=degree singI;d:=degree I;g:=binomial(d-1,2)-delta;
     	  {distinctPoints(singI),delta,g});




---- Space Curves:

randomGenus11Curve = (R) -> (
     correctCodimAndDegree:=false;
     while not correctCodimAndDegree do (
     	  Mt=coker random(R^{3:8},R^{6:7,2:6});
     	  M=coker (transpose (res Mt).dd_4);
     	  Gt:=transpose (res M).dd_3;
     	  I:=ideal syz (Gt*random(source Gt,R^{6:5}));
     	  correctCodimAndDegree=(codim I==2 and degree I==12););
     I);


isSmoothSpaceCurve = (I) -> (
     singI:=I+minors(codim I,jacobian I);
     codim singI==4);


randomGenus12Curve = (R) -> (
     correctCodimAndDegree:=false;
     while not correctCodimAndDegree do (
     	  M:=coker random(R^{3:-2},R^{7:-3});
     	  Gt:=transpose (res M).dd_3;
     	  I:=ideal syz (Gt*random(source Gt,R^{7:5}));
     	  correctCodimAndDegree=(codim I==2 and degree I==12););
     I);


randomGenus13Curve = (R) -> (
     kappa:=koszul(3,vars R);
     correctCodimAndDegree:=false;
     while not correctCodimAndDegree do (
     	  test:=false;while test==false do ( 
     	       alpha:=random(R^{4:-2},R^{6:-2});
     	       beta:=random(R^{4:-2},R^{5:-3});
     	       M:=coker(alpha*kappa|beta);
     	       test=(codim M==4 and degree M==16););
     	  Gt:=transpose (res M).dd_3;
	  --up to change of basis we can reduce to this form of phi
	  phi:=random(R^6,R^3)++id_(R^6);
	  I:=ideal syz(Gt_{1..12}*phi);
     	  correctCodimAndDegree=(codim I==2 and degree I==13););
     I);
     

testModulesForGenus14Curves = (N,p) ->(
     x:=symbol x;R:=(ZZ/p)[x_0..x_3];
     i:=0;j:=0;
     kappa:=koszul(3,vars R);
     kappakappa:=kappa++kappa;
     usedtime:=timing while (i<N) do (
	  test:=false;
 	  alpha:=random(R^{5:-2},R^{12:-2});
	  beta:=random(R^{5:-2},R^{3:-3});
 	  M:=coker (alpha*kappakappa|beta);
	  fM:=res (M,DegreeLimit =>3);
 	  if (tally degrees fM_2)_{5}==3 then (
	       --further checks to be sure to pick up the right module
	       test=(tally degrees fM_2)_{4}==2 and
	       codim M==4 and degree M==23;);
  	  i=i+1;if test==true then (j=j+1;););
     timeForNModules:=usedtime#0; numberOfGoodModules:=j;
     {timeForNModules,numberOfGoodModules})     


randomGenus14Curve = (R) -> (
     kappa:=koszul(3,vars R);
     kappakappa:=kappa++kappa;
     correctCodimAndDegree:=false;
     count:=0;while not correctCodimAndDegree do (
     	  test:=false;
     	  t:=timing while test==false do (
	       alpha=random(R^{5:-2},R^{12:-2});
	       beta=random(R^{5:-2},R^{3:-3});
	       M:=coker (alpha*kappakappa|beta);
	       fM:=res (M,DegreeLimit =>3);
	       if (tally degrees fM_2)_{5}==3 then (
   	       	    --further checks to be sure to pick up the right module
	       	    test=(tally degrees fM_2)_{4}==2 and
       	            codim M==4 and degree M==23;);
  	       count=count+1;);
     	  Gt:=transpose (res M).dd_3;
     	  I:=ideal syz (Gt_{5..17});
     	  correctCodimAndDegree=(codim I==2 and degree I==14););
     <<"    --"<<t#0<<" seconds used for "<<count<<" modules"<<endl;
     I);


--the time to compute the smoothness of N examples: not in the article
smoothSample = (N,R)-> (
     t=0;i=0;while i<N do (
     I:=randomGenus14Curve(R);
     t=t+(timing(codim(I+minors(2,jacobian I));))#0;
     i=i+1;);    
     t)

--function to produce the smoothness table for genus 14
countGenus14Curves = (p,N) -> (
     r:=symbol r;R=ZZ/p[r_0..r_3];
     nSmooth:=0;n1Nodal:=0;nMoreSingular:=0;nNonReduced:=0;
     n:=0;t:=timing while nSmooth<N do (
     	  I:=randomGenus14Curve(R);
     	  n=n+1;
	  singI:=I+minors(2,jacobian I);
	  if codim singI==4 then (nSmooth=nSmooth+1;);
	  if codim singI==3 then (
	       if degree singI==1 then (n1Nodal=n1Nodal+1;) else (
		    nMoreSingular=nMoreSingular+1;);); 
	  if codim singI==2 then nNonReduced=nNonReduced+1;
	  collectGarbage());
     {p,N,t#0,n,nSmooth,n1Nodal,nMoreSingular,nNonReduced,p^3})
--All the results:
--{2, 100, 7357.63, 1482, 100, 75, 1012, 295, 8}
--{3, 100, 3068.36, 302, 100, 53, 142, 7, 27}
--{5, 100, 2726.4, 155, 100, 31, 24, 0, 125}
--{7, 100, 3395.71, 127, 100, 16, 11, 0, 343}
--{11, 100, 6488.11, 112, 100, 10, 2, 0, 1331}
--{13, 100, 9461.17, 108, 100, 8, 0, 0, 2197}
