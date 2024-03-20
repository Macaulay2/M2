kk = ZZ/101;
R1=kk[vars(0..47), MonomialSize=>8];
J1 = ideal"dgjm-chjm-dfkm+bhkm+cflm-bglm-dgin+chin+dekn-ahkn-celn+agln+dfio-bhio-dejo+ahjo+belo-aflo-cfip+bgip+cejp-agjp-bekp+afkp,dgjq-chjq-dfkq+bhkq+cflq-bglq-dgir+chir+dekr-ahkr-celr+aglr+dfis-bhis-dejs+ahjs+bels-afls-cfit+bgit+cejt-agjt-bekt+afkt,dgnq-chnq-dfoq+bhoq+cfpq-bgpq-dgmr+chmr+deor-ahor-cepr+agpr+dfms-bhms-dens+ahns+beps-afps-cfmt+bgmt+cent-agnt-beot+afot,dknq-clnq-djoq+bloq+cjpq-bkpq-dkmr+clmr+dior-alor-cipr+akpr+djms-blms-dins+alns+bips-ajps-cjmt+bkmt+cint-aknt-biot+ajot,hknq-glnq-hjoq+floq+gjpq-fkpq-hkmr+glmr+hior-elor-gipr+ekpr+hjms-flms-hins+elns+fips-ejps-gjmt+fkmt+gint-eknt-fiot+ejot,dgju-chju-dfku+bhku+cflu-bglu-dgiv+chiv+dekv-ahkv-celv+aglv+dfiw-bhiw-dejw+ahjw+belw-aflw-cfix+bgix+cejx-agjx-bekx+afkx,dgnu-chnu-dfou+bhou+cfpu-bgpu-dgmv+chmv+deov-ahov-cepv+agpv+dfmw-bhmw-denw+ahnw+bepw-afpw-cfmx+bgmx+cenx-agnx-beox+afox,dknu-clnu-djou+blou+cjpu-bkpu-dkmv+clmv+diov-alov-cipv+akpv+djmw-blmw-dinw+alnw+bipw-ajpw-cjmx+bkmx+cinx-aknx-biox+ajox,hknu-glnu-hjou+flou+gjpu-fkpu-hkmv+glmv+hiov-elov-gipv+ekpv+hjmw-flmw-hinw+elnw+fipw-ejpw-gjmx+fkmx+ginx-eknx-fiox+ejox,dgru-chru-dfsu+bhsu+cftu-bgtu-dgqv+chqv+desv-ahsv-cetv+agtv+dfqw-bhqw-derw+ahrw+betw-aftw-cfqx+bgqx+cerx-agrx-besx+afsx,dkru-clru-djsu+blsu+cjtu-bktu-dkqv+clqv+disv-alsv-citv+aktv+djqw-blqw-dirw+alrw+bitw-ajtw-cjqx+bkqx+cirx-akrx-bisx+ajsx,hkru-glru-hjsu+flsu+gjtu-fktu-hkqv+glqv+hisv-elsv-gitv+ektv+hjqw-flqw-hirw+elrw+fitw-ejtw-gjqx+fkqx+girx-ekrx-fisx+ejsx,doru-cpru-dnsu+bpsu+cntu-botu-doqv+cpqv+dmsv-apsv-cmtv+aotv+dnqw-bpqw-dmrw+aprw+bmtw-antw-cnqx+boqx+cmrx-aorx-bmsx+ansx,horu-gpru-hnsu+fpsu+gntu-fotu-hoqv+gpqv+hmsv-epsv-gmtv+eotv+hnqw-fpqw-hmrw+eprw+fmtw-entw-gnqx+foqx+gmrx-eorx-fmsx+ensx,loru-kpru-lnsu+jpsu+kntu-jotu-loqv+kpqv+lmsv-ipsv-kmtv+iotv+lnqw-jpqw-lmrw+iprw+jmtw-intw-knqx+joqx+kmrx-iorx-jmsx+insx,ay+bz+cA+dB,ey+fz+gA+hB,iy+jz+kA+lB,my+nz+oA+pB,qy+rz+sA+tB,uy+vz+wA+xB,aC+bD+cE+dF,eC+fD+gE+hF,iC+jD+kE+lF,mC+nD+oE+pF,qC+rD+sE+tF,uC+vD+wE+xF,aG+bH+cI+dJ,eG+fH+gI+hJ,iG+jH+kI+lJ,mG+nH+oI+pJ,qG+rH+sI+tJ,uG+vH+wI+xJ,aK+bL+cM+dN,eK+fL+gM+hN,iK+jL+kM+lN,mK+nL+oM+pN,qK+rL+sM+tN,uK+vL+wM+xN,BEHK-AFHK-BDIK+zFIK+ADJK-zEJK-BEGL+AFGL+BCIL-yFIL-ACJL+yEJL+BDGM-zFGM-BCHM+yFHM+zCJM-yDJM-ADGN+zEGN+ACHN-yEHN-zCIN+yDIN,aO+bP+cQ+dR,eO+fP+gQ+hR,iO+jP+kQ+lR,mO+nP+oQ+pR,qO+rP+sQ+tR,uO+vP+wQ+xR,BEHO-AFHO-BDIO+zFIO+ADJO-zEJO-BEGP+AFGP+BCIP-yFIP-ACJP+yEJP+BDGQ-zFGQ-BCHQ+yFHQ+zCJQ-yDJQ-ADGR+zEGR+ACHR-yEHR-zCIR+yDIR,BELO-AFLO-BDMO+zFMO+ADNO-zENO-BEKP+AFKP+BCMP-yFMP-ACNP+yENP+BDKQ-zFKQ-BCLQ+yFLQ+zCNQ-yDNQ-ADKR+zEKR+ACLR-yELR-zCMR+yDMR,BILO-AJLO-BHMO+zJMO+AHNO-zINO-BIKP+AJKP+BGMP-yJMP-AGNP+yINP+BHKQ-zJKQ-BGLQ+yJLQ+zGNQ-yHNQ-AHKR+zIKR+AGLR-yILR-zGMR+yHMR,FILO-EJLO-FHMO+DJMO+EHNO-DINO-FIKP+EJKP+FGMP-CJMP-EGNP+CINP+FHKQ-DJKQ-FGLQ+CJLQ+DGNQ-CHNQ-EHKR+DIKR+EGLR-CILR-DGMR+CHMR,aS+bT+cU+dV,eS+fT+gU+hV,iS+jT+kU+lV,mS+nT+oU+pV,qS+rT+sU+tV,uS+vT+wU+xV,BEHS-AFHS-BDIS+zFIS+ADJS-zEJS-BEGT+AFGT+BCIT-yFIT-ACJT+yEJT+BDGU-zFGU-BCHU+yFHU+zCJU-yDJU-ADGV+zEGV+ACHV-yEHV-zCIV+yDIV,BELS-AFLS-BDMS+zFMS+ADNS-zENS-BEKT+AFKT+BCMT-yFMT-ACNT+yENT+BDKU-zFKU-BCLU+yFLU+zCNU-yDNU-ADKV+zEKV+ACLV-yELV-zCMV+yDMV,BILS-AJLS-BHMS+zJMS+AHNS-zINS-BIKT+AJKT+BGMT-yJMT-AGNT+yINT+BHKU-zJKU-BGLU+yJLU+zGNU-yHNU-AHKV+zIKV+AGLV-yILV-zGMV+yHMV,FILS-EJLS-FHMS+DJMS+EHNS-DINS-FIKT+EJKT+FGMT-CJMT-EGNT+CINT+FHKU-DJKU-FGLU+CJLU+DGNU-CHNU-EHKV+DIKV+EGLV-CILV-DGMV+CHMV,BEPS-AFPS-BDQS+zFQS+ADRS-zERS-BEOT+AFOT+BCQT-yFQT-ACRT+yERT+BDOU-zFOU-BCPU+yFPU+zCRU-yDRU-ADOV+zEOV+ACPV-yEPV-zCQV+yDQV,BIPS-AJPS-BHQS+zJQS+AHRS-zIRS-BIOT+AJOT+BGQT-yJQT-AGRT+yIRT+BHOU-zJOU-BGPU+yJPU+zGRU-yHRU-AHOV+zIOV+AGPV-yIPV-zGQV+yHQV,FIPS-EJPS-FHQS+DJQS+EHRS-DIRS-FIOT+EJOT+FGQT-CJQT-EGRT+CIRT+FHOU-DJOU-FGPU+CJPU+DGRU-CHRU-EHOV+DIOV+EGPV-CIPV-DGQV+CHQV,BMPS-ANPS-BLQS+zNQS+ALRS-zMRS-BMOT+ANOT+BKQT-yNQT-AKRT+yMRT+BLOU-zNOU-BKPU+yNPU+zKRU-yLRU-ALOV+zMOV+AKPV-yMPV-zKQV+yLQV,FMPS-ENPS-FLQS+DNQS+ELRS-DMRS-FMOT+ENOT+FKQT-CNQT-EKRT+CMRT+FLOU-DNOU-FKPU+CNPU+DKRU-CLRU-ELOV+DMOV+EKPV-CMPV-DKQV+CLQV,JMPS-INPS-JLQS+HNQS+ILRS-HMRS-JMOT+INOT+JKQT-GNQT-IKRT+GMRT+JLOU-HNOU-JKPU+GNPU+HKRU-GLRU-ILOV+HMOV+IKPV-GMPV-HKQV+GLQV";
end

restart
load "mathicGBExample.m2"
gbTrace = 2
elapsedTime gens gb (J1,Algorithm => LinearAlgebra);  -- ~20 sec
J1 = ideal J1_*;
errorDepth = 0
elapsedTime groebnerBasis (J1,Strategy => "F4", "MGBOptions" => hashTable {"Threads" => 1,"Log" => "F4"});  -- ~62 sec

restart
kk = ZZ/101;
R = kk[a..g, MonomialSize=>8];
randomSeed = 872742873
J = ideal random(R^1, R^{-6,-6,-6,-7});
gbTrace=2
K = ideal (a^6,b^6,c^6,d^7)
elapsedTime gens gb(J,Hilbert => poincare K); -- long time without Hilbert hint; 
elapsedTime gens gb (J,Algorithm => LinearAlgebra);  -- ~125 secs

J = ideal J_*;
elapsedTime groebnerBasis (J,Strategy => "F4", "MGBOptions" => hashTable {"Threads" => 1,"Log" => "F4"});
--***** Constructing matrix *****
--F4[43,626 by 201,249]
--F4MatrixBuild2 time recorded:        7.378s (real)
--F4MatReduceTop time recorded:        0.823s (real)
--F4RedBottomRight time recorded:        0.000s (real)
 -- 56.1944 seconds elapsed

J = ideal J_*;
elapsedTime groebnerBasis (J,Strategy => "F4", "MGBOptions" => hashTable {"Threads" => 8,"Log" => "F4"});  --
--***** Constructing matrix *****
--F4[43,626 by 201,249]
--F4MatrixBuild2 time recorded:        2.812s (real)
--F4MatReduceTop time recorded:        0.300s (real)
--F4RedBottomRight time recorded:        0.000s (real)
 -- 20.827 seconds elapsed

restart
kk = ZZ/101
n = 4
R = kk[a_(1,1)..a_(n,n),b_(1,1)..b_(n,n)]
M = genericMatrix(R,a_(1,1),n,n)
N = genericMatrix(R,b_(1,1),n,n)
J = ideal (M * N - N * M);

J = ideal J_*;
elapsedTime gens gb (J,Algorithm => LinearAlgebra);

J = ideal J_*;
elapsedTime groebnerBasis (J,Strategy => "F4", "MGBOptions" => hashTable {"Threads" => 1,"Log" => "F4"});

J = ideal J_*;
elapsedTime groebnerBasis (J,Strategy => "F4", "MGBOptions" => hashTable {"Threads" => 8,"Log" => "F4"});

restart
R = ZZ/32003[a..F]
I = ideal(
   -j*o+i*p-v*A+u*B-x*C+w*D,
   b*o-d*o-a*p+c*p-l*A+k*B-n*C+m*D,
   p*q-o*r+b*A-f*A-a*B+e*B-z*C+y*D,
   p*s-o*t+b*C-h*C-a*D+g*D+B*E-A*F,
   -b*i+d*i+a*j-c*j+r*u-q*v+t*w-s*x,
   j*o-i*p-l*q+k*r-n*s+m*t,
   d*q-f*q-c*r+e*r+t*y-s*z+j*A-i*B,
   d*s-h*s-c*t+g*t+j*C-i*D+r*E-q*F,
   -j*k+i*l-b*u+f*u+a*v-e*v-x*E+w*F,
   -d*k+f*k+c*l-e*l-p*u+o*v-n*E+m*F,
   l*q-k*r+v*A-u*B-z*E+y*F,
   l*s-k*t+v*C-u*D+f*E-h*E-e*F+g*F,
   -j*m+i*n-b*w+h*w+a*x-g*x-v*y+u*z,
   -d*m+h*m+c*n-g*n-p*w+o*x-l*y+k*z,
   n*q-m*r-f*y+h*y+e*z-g*z+x*A-w*B,
   n*s-m*t+x*C-w*D+z*E-y*F);
time gens gb I;
I = ideal I_*;
gbTrace = 1
time gens gb(I, Algorithm=>Homogeneous2, Strategy=>LongPolynomial);
