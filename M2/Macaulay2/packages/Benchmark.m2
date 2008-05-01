newPackage (
     "Benchmark",
     Headline => "a standard Macaulay 2 benchmark",
     Authors => {
	  {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"},
	  {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	  },
     HomePage => "http://www.math.uiuc.edu/Macaulay2/",
     Version => "1.0"
     )

-- we should remove the dependence of this on the unix "date" command!

export {runBenchmarks}

benchmarks = new HashTable from {
     "res39" => "res of a generic 3 by 9 matrix over ZZ/101" => () -> (
	  rr := ZZ/101[Variables => 52, MonomialSize => 16];
	  (ti,re) := toSequence timing res coker genericMatrix(rr,3,9);
	  vv := apply(8,i->rank re_i);
	  assert( vv == {3, 9, 126, 378, 504, 360, 135, 21} );
	  ti),
     "resG25" => "res of the coordinate ring of Grassmannian(2,5)" => () -> (
	  SS := ZZ/101[Variables => 20, MonomialSize => 16];
	  MM := SS^1/Grassmannian(2,5,SS);
	  (ti,re) := toSequence timing res MM;
	  vv := apply(11,i->rank re_i);
	  assert( vv == {1, 35, 140, 301, 735, 1080, 735, 301, 140, 35, 1} );
	  ti),
     "gbB148" => "gb of Bayesian graph ideal #148" => () -> (
	  p := local p;
	  R := ZZ/32003[reverse(p_(1,1,1,1,1)..p_(2,2,2,2,2)), MonomialSize=>8];
	  J := ideal(
	      -p_(1,1,2,1,1)*p_(2,1,1,1,1)+p_(1,1,1,1,1)*p_(2,1,2,1,1),
	      -p_(1,1,2,1,2)*p_(2,1,1,1,2)+p_(1,1,1,1,2)*p_(2,1,2,1,2),
	      -p_(1,1,2,2,1)*p_(2,1,1,2,1)+p_(1,1,1,2,1)*p_(2,1,2,2,1),
	      -p_(1,1,2,2,2)*p_(2,1,1,2,2)+p_(1,1,1,2,2)*p_(2,1,2,2,2),
	      -p_(1,2,2,1,1)*p_(2,2,1,1,1)+p_(1,2,1,1,1)*p_(2,2,2,1,1),
	      -p_(1,2,2,1,2)*p_(2,2,1,1,2)+p_(1,2,1,1,2)*p_(2,2,2,1,2),
	      -p_(1,2,2,2,1)*p_(2,2,1,2,1)+p_(1,2,1,2,1)*p_(2,2,2,2,1),
	      -p_(1,2,2,2,2)*p_(2,2,1,2,2)+p_(1,2,1,2,2)*p_(2,2,2,2,2),
	      -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
	      -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
	      -p_(1,1,1,2,1)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,1),
	      -p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2),
	      -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
	      -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
	      -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
	      -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
	      -p_(1,1,2,2,1)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,1),
	      -p_(1,1,2,2,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,2),
	      -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
	      -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
	      -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
	      -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
	      -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
	      -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
	      -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
	      -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
	      -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
	      -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
	      -p_(1,1,1,1,2)*p_(1,1,1,2,1)+p_(1,1,1,1,1)*p_(1,1,1,2,2)
		+p_(1,1,1,2,2)*p_(1,1,2,1,1)-p_(1,1,1,2,1)*p_(1,1,2,1,2)
		-p_(1,1,1,1,2)*p_(1,1,2,2,1)-p_(1,1,2,1,2)*p_(1,1,2,2,1)
		+p_(1,1,1,1,1)*p_(1,1,2,2,2)+p_(1,1,2,1,1)*p_(1,1,2,2,2)
		+p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,2,2,2)*p_(1,2,1,1,1)
		-p_(1,1,1,2,1)*p_(1,2,1,1,2)-p_(1,1,2,2,1)*p_(1,2,1,1,2)
		-p_(1,1,1,1,2)*p_(1,2,1,2,1)-p_(1,1,2,1,2)*p_(1,2,1,2,1)
		-p_(1,2,1,1,2)*p_(1,2,1,2,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2)
		+p_(1,1,2,1,1)*p_(1,2,1,2,2)+p_(1,2,1,1,1)*p_(1,2,1,2,2)
		+p_(1,1,1,2,2)*p_(1,2,2,1,1)+p_(1,1,2,2,2)*p_(1,2,2,1,1)
		+p_(1,2,1,2,2)*p_(1,2,2,1,1)-p_(1,1,1,2,1)*p_(1,2,2,1,2)
		-p_(1,1,2,2,1)*p_(1,2,2,1,2)-p_(1,2,1,2,1)*p_(1,2,2,1,2)
		-p_(1,1,1,1,2)*p_(1,2,2,2,1)-p_(1,1,2,1,2)*p_(1,2,2,2,1)
		-p_(1,2,1,1,2)*p_(1,2,2,2,1)-p_(1,2,2,1,2)*p_(1,2,2,2,1)
		+p_(1,1,1,1,1)*p_(1,2,2,2,2)+p_(1,1,2,1,1)*p_(1,2,2,2,2)
		+p_(1,2,1,1,1)*p_(1,2,2,2,2)+p_(1,2,2,1,1)*p_(1,2,2,2,2));
	  (ti,re) := toSequence timing gens gb (J,MaxReductionCount => 3000);
	  assert ( rank source re == 3626 );
	  ti),
     "gb3445" => "gb of a random ideal with elements of degree 3,4,4,5 in 8 variables" => ()  -> (
	  R := ZZ/101[a..h,MonomialSize=>8];
	  I := ideal random(R^1, R^{-3,-4,-4,-5});
	  J := ideal "a3,b4,c4,d5";
	  installHilbertFunction(I, poincare J);
	  (ti,re) := toSequence timing gens gb(I);
	  assert( tally degrees source re === new Tally from { {12} => 3, {13} => 1, {3} => 1, {4} => 2, {5} => 3, {6} => 5, {7} => 6, {8} => 8, {9} => 10, {10} => 9, {11} => 6} );
	  ti),
     "gb4by4comm" => "gb of the ideal of generic commuting 4 by 4 matrices over ZZ/101" => () -> (
	  R = ZZ/101[vars(0..31),MonomialOrder=>ProductOrder{8,12,12},MonomialSize=>8];
	  I = ideal( -j*o+i*p-v*A+u*B-x*C+w*D, -a*p+b*o+c*p-d*o+k*B-l*A+m*D-n*C, -a*B+b*A+e*B-f*A+p*q-o*r-z*C+y*D, -a*D+b*C+g*D-h*C+p*s-o*t+B*E-A*F,
	       a*j-b*i-c*j+d*i-q*v+r*u-s*x+t*w, j*o-i*p-l*q+k*r-n*s+m*t, -c*r+d*q+e*r-f*q-i*B+j*A-s*z+t*y, -c*t+d*s+g*t-h*s-i*D+j*C-q*F+r*E,
	       a*v-b*u-e*v+f*u-j*k+i*l-x*E+w*F, c*l-d*k-e*l+f*k+m*F-n*E+o*v-p*u, l*q-k*r+v*A-u*B-z*E+y*F, -e*F+f*E+g*F-h*E+l*s-k*t+v*C-u*D,
	       a*x-b*w-g*x+h*w-j*m+i*n-v*y+u*z, c*n-d*m-g*n+h*m+k*z-l*y+o*x-p*w, e*z-f*y-g*z+h*y+n*q-m*r+x*A-w*B, n*s-m*t+x*C-w*D+z*E-y*F);
	  (ti,re) := toSequence timing gens gb(I, Strategy => LongPolynomial, Algorithm => Homogeneous2);
	  assert( tally degrees source re === new Tally from {{2} => 15, {3} => 23, {4} => 22, {5} => 67, {6} => 72, {7} => 85, {8} => 10} );
	  ti),
     "deg2generic" => "gb of a generic ideal of codimension 2 and degree 2" => () -> (
	  R1 = ZZ/32003[vars(0..39),MonomialSize=>8];
          I1 = ideal"b2de+bd2f+a2dg+abdh+ad2i+b2cj+bc2k+bcdl+a2cm+abcn+ac2o+acdp,
	       b2dq+bd2r+a2ds+abdt+ad2u+b2cv+bc2w+bcdx+a2cy+abcz+ac2A+acdB,
	       b2dC+bd2D+a2dE+abdF+ad2G+b2cH+bc2I+bcdJ+a2cK+abcL+ac2M+acdN";
	  (ti,re) := toSequence timing gens gb(I1, MaxReductionCount=>3000);
	  --(ti,re) := toSequence timing gens gb(I1, Algorithm=>LinearAlgebra);
	  assert(44 === numgens source gens gb I1);
	  ti),
     "yang-subring" => "an example of Yang-Hui He" => () -> (
     	  kk = ZZ/101;
	  R=kk[vars(0..12)];
	  fterms = ideal"4ef2-4dfg+10ah+3bh+3ch+3aj+5bj+5cj+7al+4bl+3cl+7f,2e2f-2deg-13ghi+18fi2-7gij-15ghk+24fik-11gjk+20fk2-11gil-12gkl-12ghm+16fim-8gjm+26fkm-19glm+8fm2+7e,3eh-3di+5ej-5dk+4el-4dm,18g2h-12fgi+13g2j-15fgk+13g2l-5fgm+10ae+3be+3ce,3eh-3di+5ej-5dk+3el-3dm,9g2h-5fgi+2g2j-4fgk+14g2l-17fgm+3ae+5be+5ce,8g2h-17fgi+8g2j-16fgk+18g2l-13fgm+7ae+4be+3ce,10eh-10di+3ej-3dk+7el-7dm,-16efg+16dg2-10ai-3bi-3ci-3ak-5bk-5ck-7am-4bm-3cm-7g,-6def+6d2g+16gh2-19fhi+28ghj-12fij+16gj2-5fhk-2fjk+10ghl-10fil+24gjl-10fkl+18gl2-7fhm-13fjm-10flm-7d,-12fgh+6f2i-10fgj+8f2k-10fgl+11f2m-10ad-3bd-3cd,-13fgh+11f2i-12fgj+10f2k-13fgl+14f2m-3ad-5bd-5cd,-20fgh+13f2i-13fgj+16f2k-3fgl+16f2m-7ad-4bd-3cd";
	  dterms = matrix"gh-fi,gj-fk,gl-fm,ef-dg,aij-ahk,bij-bhk,cij-chk,ail-ahm,bil-bhm,cil-chm,akl-ajm,bkl-bjm,ckl-cjm,aeh-adi,beh-bdi,ceh-cdi,aej-adk,bej-bdk,cej-cdk,ael-adm,bel-bdm,cel-cdm";
	  S=kk[y_1..y_22, Degrees=>degrees source dterms];
	  A = R/fterms;
	  F = map(A,S,sub(dterms,A));
	  (ti,re) := toSequence timing ker F;
	  assert(tally degrees source gens re === new Tally from {{2} => 1, {3} => 12, {5} => 3, {6} => 16, {7} => 27});
	  ti),
     "yang-gb1" => "an example of Yang-Hui He arising in string theory" => () -> (
	  -- see also bugs/mike/1-yang-gbexamples.m2
	  kk = QQ;
	  R1=kk[vars(0..47), MonomialSize=>8];
	  J1 = ideal"dgjm-chjm-dfkm+bhkm+cflm-bglm-dgin+chin+dekn-ahkn-celn+agln+dfio-bhio-dejo+ahjo+belo-aflo-cfip+bgip+cejp-agjp-bekp+afkp,dgjq-chjq-dfkq+bhkq+cflq-bglq-dgir+chir+dekr-ahkr-celr+aglr+dfis-bhis-dejs+ahjs+bels-afls-cfit+bgit+cejt-agjt-bekt+afkt,dgnq-chnq-dfoq+bhoq+cfpq-bgpq-dgmr+chmr+deor-ahor-cepr+agpr+dfms-bhms-dens+ahns+beps-afps-cfmt+bgmt+cent-agnt-beot+afot,dknq-clnq-djoq+bloq+cjpq-bkpq-dkmr+clmr+dior-alor-cipr+akpr+djms-blms-dins+alns+bips-ajps-cjmt+bkmt+cint-aknt-biot+ajot,hknq-glnq-hjoq+floq+gjpq-fkpq-hkmr+glmr+hior-elor-gipr+ekpr+hjms-flms-hins+elns+fips-ejps-gjmt+fkmt+gint-eknt-fiot+ejot,dgju-chju-dfku+bhku+cflu-bglu-dgiv+chiv+dekv-ahkv-celv+aglv+dfiw-bhiw-dejw+ahjw+belw-aflw-cfix+bgix+cejx-agjx-bekx+afkx,dgnu-chnu-dfou+bhou+cfpu-bgpu-dgmv+chmv+deov-ahov-cepv+agpv+dfmw-bhmw-denw+ahnw+bepw-afpw-cfmx+bgmx+cenx-agnx-beox+afox,dknu-clnu-djou+blou+cjpu-bkpu-dkmv+clmv+diov-alov-cipv+akpv+djmw-blmw-dinw+alnw+bipw-ajpw-cjmx+bkmx+cinx-aknx-biox+ajox,hknu-glnu-hjou+flou+gjpu-fkpu-hkmv+glmv+hiov-elov-gipv+ekpv+hjmw-flmw-hinw+elnw+fipw-ejpw-gjmx+fkmx+ginx-eknx-fiox+ejox,dgru-chru-dfsu+bhsu+cftu-bgtu-dgqv+chqv+desv-ahsv-cetv+agtv+dfqw-bhqw-derw+ahrw+betw-aftw-cfqx+bgqx+cerx-agrx-besx+afsx,dkru-clru-djsu+blsu+cjtu-bktu-dkqv+clqv+disv-alsv-citv+aktv+djqw-blqw-dirw+alrw+bitw-ajtw-cjqx+bkqx+cirx-akrx-bisx+ajsx,hkru-glru-hjsu+flsu+gjtu-fktu-hkqv+glqv+hisv-elsv-gitv+ektv+hjqw-flqw-hirw+elrw+fitw-ejtw-gjqx+fkqx+girx-ekrx-fisx+ejsx,doru-cpru-dnsu+bpsu+cntu-botu-doqv+cpqv+dmsv-apsv-cmtv+aotv+dnqw-bpqw-dmrw+aprw+bmtw-antw-cnqx+boqx+cmrx-aorx-bmsx+ansx,horu-gpru-hnsu+fpsu+gntu-fotu-hoqv+gpqv+hmsv-epsv-gmtv+eotv+hnqw-fpqw-hmrw+eprw+fmtw-entw-gnqx+foqx+gmrx-eorx-fmsx+ensx,loru-kpru-lnsu+jpsu+kntu-jotu-loqv+kpqv+lmsv-ipsv-kmtv+iotv+lnqw-jpqw-lmrw+iprw+jmtw-intw-knqx+joqx+kmrx-iorx-jmsx+insx,ay+bz+cA+dB,ey+fz+gA+hB,iy+jz+kA+lB,my+nz+oA+pB,qy+rz+sA+tB,uy+vz+wA+xB,aC+bD+cE+dF,eC+fD+gE+hF,iC+jD+kE+lF,mC+nD+oE+pF,qC+rD+sE+tF,uC+vD+wE+xF,aG+bH+cI+dJ,eG+fH+gI+hJ,iG+jH+kI+lJ,mG+nH+oI+pJ,qG+rH+sI+tJ,uG+vH+wI+xJ,aK+bL+cM+dN,eK+fL+gM+hN,iK+jL+kM+lN,mK+nL+oM+pN,qK+rL+sM+tN,uK+vL+wM+xN,BEHK-AFHK-BDIK+zFIK+ADJK-zEJK-BEGL+AFGL+BCIL-yFIL-ACJL+yEJL+BDGM-zFGM-BCHM+yFHM+zCJM-yDJM-ADGN+zEGN+ACHN-yEHN-zCIN+yDIN,aO+bP+cQ+dR,eO+fP+gQ+hR,iO+jP+kQ+lR,mO+nP+oQ+pR,qO+rP+sQ+tR,uO+vP+wQ+xR,BEHO-AFHO-BDIO+zFIO+ADJO-zEJO-BEGP+AFGP+BCIP-yFIP-ACJP+yEJP+BDGQ-zFGQ-BCHQ+yFHQ+zCJQ-yDJQ-ADGR+zEGR+ACHR-yEHR-zCIR+yDIR,BELO-AFLO-BDMO+zFMO+ADNO-zENO-BEKP+AFKP+BCMP-yFMP-ACNP+yENP+BDKQ-zFKQ-BCLQ+yFLQ+zCNQ-yDNQ-ADKR+zEKR+ACLR-yELR-zCMR+yDMR,BILO-AJLO-BHMO+zJMO+AHNO-zINO-BIKP+AJKP+BGMP-yJMP-AGNP+yINP+BHKQ-zJKQ-BGLQ+yJLQ+zGNQ-yHNQ-AHKR+zIKR+AGLR-yILR-zGMR+yHMR,FILO-EJLO-FHMO+DJMO+EHNO-DINO-FIKP+EJKP+FGMP-CJMP-EGNP+CINP+FHKQ-DJKQ-FGLQ+CJLQ+DGNQ-CHNQ-EHKR+DIKR+EGLR-CILR-DGMR+CHMR,aS+bT+cU+dV,eS+fT+gU+hV,iS+jT+kU+lV,mS+nT+oU+pV,qS+rT+sU+tV,uS+vT+wU+xV,BEHS-AFHS-BDIS+zFIS+ADJS-zEJS-BEGT+AFGT+BCIT-yFIT-ACJT+yEJT+BDGU-zFGU-BCHU+yFHU+zCJU-yDJU-ADGV+zEGV+ACHV-yEHV-zCIV+yDIV,BELS-AFLS-BDMS+zFMS+ADNS-zENS-BEKT+AFKT+BCMT-yFMT-ACNT+yENT+BDKU-zFKU-BCLU+yFLU+zCNU-yDNU-ADKV+zEKV+ACLV-yELV-zCMV+yDMV,BILS-AJLS-BHMS+zJMS+AHNS-zINS-BIKT+AJKT+BGMT-yJMT-AGNT+yINT+BHKU-zJKU-BGLU+yJLU+zGNU-yHNU-AHKV+zIKV+AGLV-yILV-zGMV+yHMV,FILS-EJLS-FHMS+DJMS+EHNS-DINS-FIKT+EJKT+FGMT-CJMT-EGNT+CINT+FHKU-DJKU-FGLU+CJLU+DGNU-CHNU-EHKV+DIKV+EGLV-CILV-DGMV+CHMV,BEPS-AFPS-BDQS+zFQS+ADRS-zERS-BEOT+AFOT+BCQT-yFQT-ACRT+yERT+BDOU-zFOU-BCPU+yFPU+zCRU-yDRU-ADOV+zEOV+ACPV-yEPV-zCQV+yDQV,BIPS-AJPS-BHQS+zJQS+AHRS-zIRS-BIOT+AJOT+BGQT-yJQT-AGRT+yIRT+BHOU-zJOU-BGPU+yJPU+zGRU-yHRU-AHOV+zIOV+AGPV-yIPV-zGQV+yHQV,FIPS-EJPS-FHQS+DJQS+EHRS-DIRS-FIOT+EJOT+FGQT-CJQT-EGRT+CIRT+FHOU-DJOU-FGPU+CJPU+DGRU-CHRU-EHOV+DIOV+EGPV-CIPV-DGQV+CHQV,BMPS-ANPS-BLQS+zNQS+ALRS-zMRS-BMOT+ANOT+BKQT-yNQT-AKRT+yMRT+BLOU-zNOU-BKPU+yNPU+zKRU-yLRU-ALOV+zMOV+AKPV-yMPV-zKQV+yLQV,FMPS-ENPS-FLQS+DNQS+ELRS-DMRS-FMOT+ENOT+FKQT-CNQT-EKRT+CMRT+FLOU-DNOU-FKPU+CNPU+DKRU-CLRU-ELOV+DMOV+EKPV-CMPV-DKQV+CLQV,JMPS-INPS-JLQS+HNQS+ILRS-HMRS-JMOT+INOT+JKQT-GNQT-IKRT+GMRT+JLOU-HNOU-JKPU+GNPU+HKRU-GLRU-ILOV+HMOV+IKPV-GMPV-HKQV+GLQV";
	  --(ti,re) := toSequence timing gb(J1, Algorithm=>LinearAlgebra);
	  (ti,re) := toSequence timing gb(J1);
	  ti)
     }

runBenchmark = n -> (
     ti := benchmarks#n#1();
     << "-- " << n << ": " << benchmarks#n#0 << ": " <<  toString ti << " seconds" << endl;
     )

runBenchmarks0 = method()
runBenchmarks0 List := x -> (
     << "-- beginning computation " << get "!date";
     << "-- " << first lines get "!uname -a" << endl;
     << "-- Macaulay2 " << version#"VERSION";
     << ", compiled with " << version#"compiler";
     << endl;
     scan(x,runBenchmark))

installMethod(runBenchmarks0, () -> runBenchmarks {"res39","resG25","gbB148"})

runBenchmarks = Command runBenchmarks0

<< "Benchmark: type \"runBenchmarks\" to run the three standard benchmarks" << endl
<< "Benchmark: type \"runBenchmarks {m,n,...}\" to run benchmarks m,n,..." << endl
<< "Benchmarks available:" << endl
scan(pairs benchmarks, (n,x) -> << "       " << toExternalString n << ": " << x#0 << endl)

end

Here is another possible benchmark, but it doesn't work for us yet:

    R = ZZ/5[a..d,MonomialSize=>16];
    I = ideal{a^3-2*a^2*b-a*b^2-2*b^3+a*b*c-2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d
	      -2*b^2*d-a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3, a^125, b^125,
	      c^125, d^125}
    time gb I;

-- Results:

---- a MacBook Pro (core 2 duo 2.4 GHZ, 4 GB ram):
  --- Mac OS X side:
    -- beginning computation Mon Sep 17 21:52:30 EDT 2007
    -- Darwin indigo.local 8.10.1 Darwin Kernel Version 8.10.1: Wed May 23 16:33:00 PDT 2007; root:xnu-792.22.5~1/RELEASE_I386 i386 i386
    -- Macaulay2 0.9.97, compiled with gcc 4.0.1
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.25 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 2.43 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 19.15 seconds
  --- 32 bit GNU/Linux side, under vmware:
    -- beginning computation Mon Sep 17 21:56:26 EDT 2007
    -- Linux indigo-ubuntu32 2.6.20-16-generic #2 SMP Thu Jun 7 20:19:32 UTC 2007 i686 GNU/Linux
    -- Macaulay2 0.9.97, compiled with gcc 4.1.2
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.204013 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 2.77217 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 19.4452 seconds
  --- 64 bit GNU/Linux side, under vmware:
    -- beginning computation Thu Aug  9 20:23:47 EDT 2007
    -- Linux indigo-ubuntu 2.6.20-16-generic #2 SMP Thu Jun 7 19:00:28 UTC 2007 x86_64 GNU/Linux
    -- Macaulay2 0.9.97, compiled with gcc 4.1.2
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.188012 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 2.54816 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 19.8732 seconds

-- beginning computation Thu Jan  4 20:25:24 EST 2007
-- Darwin bayer.math.columbia.edu 8.8.1 Darwin Kernel Version 8.8.1: Mon Sep 25 19:42:00 PDT 2006; root:xnu-792.13.8.obj~1/RELEASE_I386 i386 i386 iMac5,1 Darwin
-- Macaulay2 0.9.96, compiled with gcc 4.0.1
-- res39: 0.21 seconds
-- resG25: 2.51 seconds

-- Lenovo/IBM Thinkpad R61i with Core 2 Duo at 1.5 Ghz
  -- Linux gallium 2.6.22-14-generic #1 SMP Sun Oct 14 23:05:12 GMT 2007 i686 GNU/Linux
    -- beginning computation Wed Nov 14 18:58:46 CET 2007
    -- Macaulay2 1.0beta, compiled with gcc 4.1.3
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.352022 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 4.10826 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 31.818 seconds
  -- Linux gallium 2.6.22-14-generic #1 SMP Sun Oct 14 21:45:15 GMT 2007 x86_64 GNU/Linux
    -- beginning computation Wed Nov 14 10:52:29 CST 2007
    -- Macaulay2 1.0beta, compiled with gcc 4.1.3
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.252016 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 3.66823 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 32.8221 seconds
  -- CYGWIN_NT-6.0 gallium 1.5.25(0.156/4/2) 2007-12-14 19:21 i686 Cygwin 
       -- this one is TOO SLOW
    -- beginning computation Tue Mar 18 15:14:38 GMTST 2008
    -- CYGWIN_NT-6.0 gallium 1.5.25(0.156/4/2) 2007-12-14 19:21 i686 Cygwin
    -- Macaulay2 1.1, compiled with gcc 3.4.4
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: .811 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 12.496 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 47.565 seconds

-- Lenovo Thinkpad X61, new
  -- Linux
    -- beginning computation Mon Sep 17 15:03:59 CDT 2007
    -- Linux dubnium 2.6.20-16-generic #2 SMP Fri Aug 31 00:55:27 UTC 2007 i686 GNU/Linux
    -- Macaulay2 0.9.97, compiled with gcc 4.1.2
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.304019 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 3.02419 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 22.0254 seconds
  -- Windows Vista
    -- beginning computation Mon Sep 17 20:37:59 GMTDT 2007
    -- CYGWIN_NT-6.0 LENOVO-PC 1.5.24(0.156/4/2) 2007-01-31 10:57 i686 Cygwin
    -- Macaulay2 0.9.97, compiled with gcc 3.4.4
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.578 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 8.424 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 32.916 seconds

-- beginning computation Tue Apr 11 20:14:38 EDT 2006
-- Linux mathvader1.math.cornell.edu 2.6.9-34.ELsmp #1 SMP Fri Feb 24 16:56:28 EST 2006 x86_64 x86_64 x86_64 GNU/Linux
-- Macaulay2 0.9.8, compiled with gcc 3.4.5
-- res39: 0.260961 seconds
-- resG25: 3.89541 seconds

-- beginning computation Mon Dec 23 00:06:16 CST 2002
-- Linux u126.math.uiuc.edu 2.4.18-18.7.xsmp #1 SMP Wed Nov 13 19:01:42 EST 2002 i686 unknown
--    Dell_dualX/2600 
-- Macaulay2 0.9, compiled with gcc 3.0
-- res39: 0.285138 seconds
-- resG25: 4.98438 seconds
--loaded benchmark.m2 from /home/17/dan/tmp/M2-linux/Macaulay2-0.9/lib/Macaulay2-0.9/packages/benchmark.m2

-- beginning computation Mon Dec 31 19:22:33 CST 2001
-- Linux capybara 2.4.18 #25 Sun Jul 28 15:59:31
-- Macaulay2 0.9.2, compiled with gcc 2.95
-- res39: 0.38 seconds
-- resG25: 6.05 seconds

-- Dell Latitude C840
-- beginning computation Mon Sep 17 15:10:14 CDT 2007
-- Linux rhodium 2.6.20-16-generic #2 SMP Thu Jun 7 20:19:32 UTC 2007 i686 GNU/Linux
-- Macaulay2 0.9.97, compiled with gcc 4.1.2
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.584037 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 6.90443 seconds
-- gbB148: gb of Bayesian graph ideal #148: 52.2593 seconds

-- beginning computation Tue Sep 30 22:05:23 CDT 2003
-- Linux rhodium 2.4.20 #2 Mon Mar 17 22:02:15 PST 2003 i686 unknown
-- Macaulay2 0.9.2, compiled with gcc 3.2
-- res39: 0.37 seconds
-- resG25: 6.1 seconds

-- beginning computation Mon Jan  8 11:44:18 EST 2007
-- Linux rhodium 2.6.18.1 #1 SMP PREEMPT Sat Nov 11 02:11:17 CET 2006 i686 pentium4 i386 GNU/Linux
-- Macaulay2 0.9.96, compiled with gcc 4.1.1
-- res39: 0.48003 seconds
-- resG25: 6.56441 seconds
-- gbB148: 59.9957 seconds
-- beginning computation Tue Jan  9 06:57:58 EST 2007
-- res39: 0.520033 seconds
-- resG25: 6.66042 seconds
-- gbB148: 63.472 seconds
-- res39: 0.532033 seconds
-- resG25: 6.65642 seconds
-- gbB148: 64.004 seconds
----- a bit of optimization to find_divisors:
-- res39: 0.504031 seconds
-- resG25: 6.72442 seconds
-- gbB148: 62.4079 seconds
-- res39: 0.516032 seconds
-- resG25: 6.81643 seconds
-- gbB148: 61.9039 seconds
-- res39: 0.528033 seconds
-- resG25: 6.76842 seconds
-- gbB148: 61.6319 seconds
---- start letting the matches percolate up, and insert new ones at the end
-- res39: 0.536033 seconds
-- resG25: 6.89243 seconds
-- gbB148: 57.1316 seconds
-- res39: 0.532034 seconds
-- resG25: 7.17245 seconds
-- gbB148: 51.5232 seconds
-- res39: 0.540033 seconds
-- resG25: 7.08444 seconds
-- gbB148: 52.1673 seconds
-- res39: 0.528033 seconds
-- resG25: 6.94443 seconds
-- gbB148: 52.2953 seconds
---- safe::add using long long
-- res39: 0.500032 seconds
-- resG25: 6.74442 seconds
-- gbB148: 51.5032 seconds
-- res39: 0.516033 seconds
-- resG25: 6.85243 seconds
-- gbB148: 51.8792 seconds
-- res39: 0.548034 seconds
-- resG25: 6.83643 seconds
-- gbB148: 52.0313 seconds
---- safe: back to not using long long
-- res39: 0.508031 seconds
-- resG25: 6.86843 seconds
-- gbB148: 52.3553 seconds
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.524033 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 6.84043 seconds
-- gbB148: gb of Bayesian graph ideal #148: 51.9993 seconds
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.532034 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 6.86843 seconds
-- gbB148: gb of Bayesian graph ideal #148: 52.1553 seconds
--   rework gb-default.cpp for local orderings
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.552035 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 7.42846 seconds
-- gbB148: gb of Bayesian graph ideal #148: 151.697 seconds
--   oops, re-enable the use of the previous divisor during reduction
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.532033 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 7.74848 seconds
-- gbB148: gb of Bayesian graph ideal #148: 84.3813 seconds
--   use the old code in the global case
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.544034 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 7.68448 seconds
-- gbB148: gb of Bayesian graph ideal #148: 54.1754 seconds <-- not quite as good

-- Darwin habanero.local 7.7.0 Darwin Kernel Version 7.7.0: Sun Nov  7 16:06:51 PST 2004; root:xnu/xnu-517.9.5.obj~1/RELEASE_PPC  Power Macintosh powerpc
-- Macaulay2 0.9.5, compiled with gcc 3.3.0
-- res39: 0.72 seconds
-- resG25: 6.82 seconds

-- beginning computation Fri May  5 14:27:51 CDT 2006
-- Linux u123.math.uiuc.edu 2.4.22-1.2199.4.legacy.nptl #1 Sun Feb 20 18:21:21 EST 2005 i686 i686 i386 GNU/Linux
-- Macaulay2 0.9.8, compiled with gcc 4.1.0
-- res39: 0.5 seconds
-- resG25: 6.99 seconds

-- beginning computation Fri May  5 13:38:03 CDT 2006
-- Linux u123.math.uiuc.edu 2.4.22-1.2199.4.legacy.nptl #1 Sun Feb 20 18:21:21 EST 2005 i686 i686 i386 GNU/Linux
-- Macaulay2 0.9.8, compiled with gcc 3.3.2
-- res39: 0.56 seconds
-- resG25: 7.38 seconds

-- beginning computation Thu Oct 17 00:26:02 CEST 2002
-- Linux abuch.imf.au.dk 2.4.9-34smp #1 SMP Sat Jun 1 05:54:57 EDT 2002 i686 unknown
-- Macaulay2 0.9.2, compiled with gcc 2.96
-- res39: 0.47 seconds
-- resG25: 7.82 seconds

-- beginning computation Sat May 25 17:27:49 CDT 2002
-- Linux lisboa.ks.uiuc.edu 2.4.16 #9 Tue Feb 19 14:11:28 CST 2002 i686 unknown
-- model name      : Intel(R) Pentium(R) 4 CPU 1700MHz
-- Macaulay2 0.9, compiled with gcc 3.0
-- res39: 0.44 seconds
-- resG25: 8.43 seconds

-- FreeBSD euclid.math.purdue.edu 4.5-RELEASE FreeBSD i386
-- Macaulay2 0.9.2, compiled with gcc 2.95
-- res39: 0.523437 seconds
-- resG25: 8.14062 seconds

-- Linux hn03 2.4.9-31pctr #1 SMP Thu May 9 13:22:43 CDT 2002 i686 unknown
-- Macaulay2 0.9.2, compiled with gcc 3.0
-- res39: 0.66 seconds
-- resG25: 10.23 seconds

-- OSF1 cosimo.medicis.polytechnique.fr V4.0 1229 alpha
-- 0.683216 seconds, Macaulay2 0.8.57, compiled with gcc 2.95

-- OSF1 agnesi.matematik.su.se V4.0 1091 alpha
-- 0.708576 seconds, Macaulay2 0.8.57, compiled with gcc 2.95

-- OSF1 despina.ks.uiuc.edu V4.0 1229 alpha
-- 0.75152 seconds, Macaulay2 0.8.52

-- CYGWIN_NT-5.0 NIOBIUM 1.3.2(0.39/3/2) 2001-05-20 23:28 i686 unknown (1 Ghz pentium III)
-- Macaulay2 0.9, compiled with gcc 2.95
-- res39: 0.802 seconds
-- resG25: 10.936 seconds

-- beginning computation Tue Apr 11 19:53:29 EDT 2006
-- Linux rhodium 2.6.11 #3 SMP Tue Jun 28 07:39:29 CDT 2005 i686 unknown unknown GNU/Linux
-- Macaulay2 0.9.8, compiled with gcc 4.0.0
-- res39: 0.749886 seconds
-- resG25: 11.1823 seconds

-- Linux rhenium 2.2.16 #24 Sat Jun 10 15:07:27 CDT 2000 i686 unknown
-- Macaulay2 0.8.61, compiled with gcc 2.95
-- res39: 1.12 seconds
-- resG25: 18.45 seconds

-- Linux noether.matematik.su.se 2.1.121 #3 Wed Sep 16 10:05:16 CEST 1998 alpha unknown (600 mHz)
-- 1.20106 seconds, 0.8.46

-- SunOS sg0.math.cornell.edu 5.7 Generic_106541-08 sun4u sparc SUNW,Ultra-Enterprise
-- Macaulay2 0.8.60, compiled with gcc 2.95
-- res39: 1.37 seconds
-- resG25: 18.29 seconds

-- Linux localhost.localdomain 2.2.1 #101 Fri Feb 5 16:17:12 EST 1999 ppc unknown
-- (250 MHz Macintosh Powerbook G3)
-- 1.67 seconds, Macaulay 2 version 0.8.52

-- beginning computation Thu Aug  9 18:07:41 CDT 2001
-- Linux rhenium 2.4.6 #61 Fri Jul 13 09:17:38 CDT 2001 i686 unknown
-- Macaulay2 0.9, compiled with gcc 2.95
-- res39: 1.71 seconds
-- resG25: 25. seconds

-- beginning computation Sun Sep  9 19:33:49 CDT 2001
-- Linux rhenium 2.4.9 #67 Mon Sep 3 11:10:40 CDT 2001 i686 unknown
-- Macaulay2 0.9, compiled with gcc 3.0.1  (faster than 3.0 below)
-- res39: 1.71 seconds
-- resG25: 27.27 seconds

-- Linux rhenium 2.4.4 #55 Tue Jun 26 13:44:25 CDT 2001 i686 unknown
-- Macaulay2 0.9, compiled with gcc 3.0
-- res39: 1.93 seconds
-- resG25: 28.47 seconds

-- Linux hypatia.matematik.su.se 2.0.34 #2 Thu May 7 10:48:04 EDT 1998 alpha unknown
-- 2.01366 seconds, Macaulay2 0.8.52a, compiled with gcc 2.90 -- with DEBUG on!

-- SunOS orion.math.uiuc.edu 5.5.1 Generic_103640-26 sun4u sparc SUNW,Ultra-4
-- 2.14 seconds, Macaulay2 0.8.53, compiled with gcc 2.8

-- Linux yttrium 2.2.6 #82 Sun Apr 18 15:06:16 CDT 1999 i686 unknown
-- e-machine: Cyrix M II - 333 MMX
-- 2.45 seconds, Macaulay2 0.8.53, compiled with gcc 2.91

-- SunOS andy 5.5.1 Generic_103640-29 sun4u sparc SUNW,Ultra-2
-- Macaulay2 0.8.60, compiled with gcc 2.95
-- res39: 2.7 seconds
-- resG25: 37.77 seconds

-- Linux geometry 2.3.18 #2 Thu Sep 16 17:50:47 CDT 1999 i586 unknown
-- 3.46 seconds, Macaulay2 0.8.55, compiled with gcc 2.95

-- Linux geometry 2.1.121 #33 SMP Tue Sep 15 21:44:25 CDT 1998 i586
-- 4.01 seconds, Macaulay 2 version 0.8.47, compiled with gcc

-- Linux geometry 2.2.0-pre4 #65 Mon Jan 4 20:14:06 CST 1999 i586 unknown
-- 4.17 seconds, Macaulay 2 version 0.8.50, compiled with gcc 2.8.1

----- with SHAREDLIBS, including interpeter but not engine:
-- Linux geometry 2.2.0-pre4 #65 Mon Jan 4 20:14:06 CST 1999 i586 unknown
-- 4.27 seconds, Macaulay 2 version 0.8.50

-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 4.31 seconds, Macaulay2 0.8.53, compiled with gcc 2.91, statically linked

-- CYGWIN_NT-4.0 GEOMETRY 20.1 (0.3/1/1) 1998-12-3 20:39:18 i586 unknown
-- 4.327 seconds, Macaulay 2 version 0.8.52
-- gcc version egcs-2.91.60 19981201 (egcs-1.1.1 release)

-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 4.36 seconds, Macaulay2 0.8.53, compiled with gcc 2.8, dynamically linked

-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 4.38 seconds, Macaulay2 0.8.53, compiled with gcc 2.91

----- with SHAREDLIBS, including engine and interpeter:
-- Linux geometry 2.2.0-pre4 #65 Mon Jan 4 20:14:06 CST 1999 i586 unknown
-- 4.81 seconds, Macaulay 2 version 0.8.50

-- IRIX illi 5.3 12201932 IP22 mips
-- 	5.49 seconds		0.8.10

-- with SHAREDLIBS and DEBUG and egcs compiler
-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 5.65 seconds, Macaulay2 0.8.52, compiled with gcc 2.91

-- HP-UX ux1 B.10.10 U 9000/819 65611361 unlimited-user license
-- 	6.26 seconds

-- NeXTstep mms-hal 3.2 2 i586
-- 	6.74973 seconds

-- SunOS saturn.math.uiuc.edu 5.5.1 Generic sun4m sparc SUNW,SPARCstation-5
-- 7.71 seconds, Macaulay 2 version 0.8.41

-- SunOS orion.math.uiuc.edu 5.5 Generic sun4d sparc SUNW,SPARCserver-1000
-- 	8.41 seconds		0.8.46

-- SunOS kilburn 4.1.4 2 sun4m
-- Macaulay 2 version 0.8.46
-- 10.17 seconds

-- HP-UX skolem A.09.05 A 9000/750 2013387634 two-user license
-- 	10.25 seconds

-- Linux homotopy 2.0.14, NEC Versa E, i486 laptop, DX4/100, 25 mhz
-- 	Macaulay 2, version 0.8.26
--      	22.04 seconds

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages NAMEOFPACKAGE=Benchmark install-one"
-- End:
