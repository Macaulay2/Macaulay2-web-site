     -- just run this through M2 to see my comments in action

kk=ZZ/32003
ringP3=kk[x_0..x_3]
ringP1=kk[s,t]
cubicMap = map(ringP1,ringP3,matrix{{s^3, s^2*t, s*t^2, t^3}})
idealCubic=kernel cubicMap
idealCubic2 = monomialCurveIdeal(ringP3,{1,2,3})
M=matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}}
idealCubic3 = minors(2, M)
codim idealCubic
degree idealCubic
dim idealCubic
gens idealCubic
0==(gens idealCubic)%(gens idealCubic3)
     -- Why not just ask whether the ideals are equal like this?
     -- You're only checking one containment.
          idealCubic == idealCubic3
     --
     --

f=map(ringP3^1,ringP3^{4:-1},{{x_0,x_1,x_2,x_3}})
     -- why not just use this?
     	    f = vars ringP3
     --
     --

OmegaP3=kernel f
generators OmegaP3
presentation OmegaP3
G=res coker f
G.dd
G.dd_2
degrees source G.dd_2
betti G
m=matrix{{x_0^3, x_1^2, x_2,x_3},{x_1^3,x_2^2,x_3,0}}
i=minors(2,m)
F=res(ringP3^1/i)
betti F
betti G
OmegaP3res = kernel (f**(ringP3^1/idealCubic))
J1=jacobian(idealCubic)
J2=J1 // (gens OmegaP3res)
J=map(OmegaP3res, module idealCubic, J1 // (gens OmegaP3res))
     -- why not use this?
           J = map(OmegaP3res, module idealCubic, J2)
     --
     --

OmegaCubic=prune coker J
prune HH^0(sheaf OmegaCubic)
ringP4=kk[x_0..x_4]
idealX=ideal(x_1+x_3, x_2+x_4)
idealL1=ideal(x_1,x_2)
idealL2=ideal(x_3,x_4)
idealY=intersect(idealL1,idealL2)
degree(idealX+idealY)
degree Tor_0(ringP4^1/idealX, ringP4^1/idealY)
degree Tor_1(ringP4^1/idealX, ringP4^1/idealY)
degree Tor_2(ringP4^1/idealX, ringP4^1/idealY)
res (ringP4^1/idealX)
ringP3=kk[x_0..x_3]

idealX= ideal(x_1^4-2*x_0*x_1^2*x_3-x_1^2*x_2*x_3+x_0^2*x_3^2,
         x_0^2*x_1^2+10669*x_0*x_1^2*x_2+10667*x_0^3*x_3-
             10668*x_0^2*x_2*x_3+10668*x_0*x_2^2*x_3+10668*x_1*x_3^3,
         x_0*x_1^3*x_2-6400*x_1^3*x_2^2+12801*x_0^3*x_1*x_3-
             6401*x_0^2*x_1*x_2*x_3+12801*x_0*x_1*x_2^2*x_3+
             6400*x_1*x_2^3*x_3+6401*x_1^2*x_3^3+6400*x_0*x_3^4,
         x_0*x_1^2*x_2^2+5819*x_0^4*x_3+2909*x_0^3*x_2*x_3-
             14547*x_0^2*x_2^2*x_3+5820*x_0*x_2^3*x_3+
             14546*x_1^3*x_3^2-8727*x_0*x_1*x_3^3+5820*x_1*x_2*x_3^3,
         x_0^5+5*x_0^2*x_2^3+5*x_0*x_2^4-3*x_0*x_1^3*x_3-4*x_1^3*x_2*x_3+
             4*x_0^2*x_1*x_3^2+10*x_0*x_1*x_2*x_3^2+5*x_1*x_2^2*x_3^2,
         x_1^2*x_2^4+11637*x_0^4*x_2*x_3+5819*x_0^3*x_2^2*x_3+
             2909*x_0^2*x_2^3*x_3+11635*x_0*x_2^4*x_3-x_2^5*x_3+
             2*x_0*x_1^3*x_3^2-2907*x_1^3*x_2*x_3^2-x_0^2*x_1*x_3^3+
            14545*x_0*x_1*x_2*x_3^3+11635*x_1*x_2^2*x_3^3+x_3^6)
codim idealX
degree idealX
codim singularLocus idealX
decompose idealX
     -- This turns out to have just one component, but can the reader tell?
     -- Ask for
     	    # (decompose idealX)
     -- and the reader can tell.

rank source basis(0,HH^0(sheaf (ringP3/idealX)))
idealX == saturate idealX
basis(0, HH^1(sheaf(ringP3^1/idealX)))
g=rank source basis(0, HH^1(sheaf(ringP3^1/idealX)))
HH^1(sheaf(ringP3^{1}/idealX))
omegaX=Ext^(codim idealX)(ringP3^1/idealX, ringP3^{-4})
dualModule = Hom(omegaX, ringP3^1/idealX)
betti generators dualModule
choice = transpose matrix{{1_ringP3,9:0}}
f = homomorphism map(dualModule, ringP3^{-3}, choice)
     --
 	 isHomogeneous f
     -- this f would be homgeneous if -3 were replaced by -5
     -- even better, replace the two lines above by this
         f = homomorphism dualModule_{0}
     --
     -- let's check whether it's homgeneous
 	 isHomogeneous f
     --

canGens=f*basis(0,omegaX)
ringX=ringP3/idealX
ringP5=kk[x_0..x_5]
     -- the ring map below would be homogeneous if we made the variables of this ring degree 5
           ringP5'=kk[x_0..x_5,Degrees => {6:5}]
     -- Let's check:
     	   isHomogeneous map(ringX, ringP5', substitute(matrix canGens,ringX))
     -- That might be better for subsequent algorithms, such as 'kernel'.
     -- But it makes the betti displays below grossly enlarged, and we'd have to change 
     -- the computation of deg2places below.
     --
idealXcan = trim kernel map(ringX, ringP5, substitute(canGens,ringX))
     -- I've had to replace 'canGens' by 'matrix canGens' here:
     --
     	  idealXcan = trim kernel map(ringX, ringP5, substitute(matrix canGens,ringX))
     --
     -- Explanation:  f is a map whose target is 'ringP3^1/idealX'
     -- and thus so is canGens.  I've changed 'substitute' to
     -- distinguish between submodules and quotient modules of free
     -- modules, giving an error in the latter case.  I've introduced
     -- a tensor product notation to handle the latter case.

     -- Also, I'll make it error to try to use a homomorphism between
     -- non-free modules when trying to make a ring homomorphism.
     
betti res idealXcan
deg2places = positions(degrees source gens idealXcan,
                            i->first i==2)
     	  -- Why not check
	  --	      i -> i == {2}
	  -- instead?  It would be clearer.

     	  -- Also, I see that the locution
	       	degrees source gens idealXcan
	  -- is inconvenient and common, so I'm introducing the following shortcut
	       	degrees idealXcan
     	  -- Here is the new code which you can run soon...
		deg2places = positions(degrees idealXcan, i->i=={2})
     	  --

idealS= ideal (gens idealXcan)_deg2places
codim singularLocus idealS
omegaS=Ext^(codim idealS)(ringP5^1/idealS, ringP5^{-6})
OS=ringP5^1/idealS
omegaS**omegaS
omega2S=Hom(Hom(omegaS**omegaS, OS),OS)
L= Hom(omegaS, OS**(ringP5^{-1}))
dualModule = Hom(L, OS)
betti generators dualModule
choice = transpose matrix{{1_ringP5,2:0}}
g = homomorphism map(dualModule, ringP5^{0}, choice)
     -- should replace 0 by -1 to get a homogeneous map.
     -- But better is to replace two lines by
     	  g = homomorphism dualModule_{0} 
     -- as before

toP2=g*basis(0,L)
ringXcan = ringP5/idealXcan
ringP2=kk[x_0..x_2]
idealXplane = trim kernel map(ringXcan, ringP2, 
                                  substitute(toP2,ringXcan))
     	       -- had to insert matrix as explained above
		idealXplane = trim kernel map(ringXcan, ringP2, 
						  substitute(matrix toP2,ringXcan))
     	       -- 
ringP2 = kk[x_0..x_2]
idealC2=ideal(x_0^5+x_1^5+x_2^5)
ringC2=ringP2/idealC2
ringP5 = kk[x_0..x_5]
idealC5=trim kernel map(ringC2, ringP5, gens (ideal vars ringC2)^2)
ringC5=ringP5/idealC5
ringP3 = kk[x_0..x_3]
use ringC5
idealC=trim kernel map(ringC5, ringP3,matrix{{x_0+x_1,x_2,x_3,x_5}})
codim idealC
degree idealC
codim singularLocus idealC
