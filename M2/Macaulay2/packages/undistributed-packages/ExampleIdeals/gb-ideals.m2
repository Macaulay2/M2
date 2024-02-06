----------------------------------------------
--yang1: a GB suggested by Yang-Hui He related to string theory
kk = ZZ/101;
R1=kk[vars(0..47), MonomialSize=>8];
J1=ideal"dgjm-chjm-dfkm+bhkm+cflm-bglm-dgin+chin+dekn-ahkn-celn+agln+dfio-bhio-dejo+ahjo+belo-aflo-cfip+bgip+cejp-agjp-bekp+afkp,
  dgjq-chjq-dfkq+bhkq+cflq-bglq-dgir+chir+dekr-ahkr-celr+aglr+dfis-bhis-dejs+ahjs+bels-afls-cfit+bgit+cejt-agjt-bekt+afkt,
  dgnq-chnq-dfoq+bhoq+cfpq-bgpq-dgmr+chmr+deor-ahor-cepr+agpr+dfms-bhms-dens+ahns+beps-afps-cfmt+bgmt+cent-agnt-beot+afot,
  dknq-clnq-djoq+bloq+cjpq-bkpq-dkmr+clmr+dior-alor-cipr+akpr+djms-blms-dins+alns+bips-ajps-cjmt+bkmt+cint-aknt-biot+ajot,
  hknq-glnq-hjoq+floq+gjpq-fkpq-hkmr+glmr+hior-elor-gipr+ekpr+hjms-flms-hins+elns+fips-ejps-gjmt+fkmt+gint-eknt-fiot+ejot,
  dgju-chju-dfku+bhku+cflu-bglu-dgiv+chiv+dekv-ahkv-celv+aglv+dfiw-bhiw-dejw+ahjw+belw-aflw-cfix+bgix+cejx-agjx-bekx+afkx,
  dgnu-chnu-dfou+bhou+cfpu-bgpu-dgmv+chmv+deov-ahov-cepv+agpv+dfmw-bhmw-denw+ahnw+bepw-afpw-cfmx+bgmx+cenx-agnx-beox+afox,
  dknu-clnu-djou+blou+cjpu-bkpu-dkmv+clmv+diov-alov-cipv+akpv+djmw-blmw-dinw+alnw+bipw-ajpw-cjmx+bkmx+cinx-aknx-biox+ajox,
  hknu-glnu-hjou+flou+gjpu-fkpu-hkmv+glmv+hiov-elov-gipv+ekpv+hjmw-flmw-hinw+elnw+fipw-ejpw-gjmx+fkmx+ginx-eknx-fiox+ejox,
  dgru-chru-dfsu+bhsu+cftu-bgtu-dgqv+chqv+desv-ahsv-cetv+agtv+dfqw-bhqw-derw+ahrw+betw-aftw-cfqx+bgqx+cerx-agrx-besx+afsx,
  dkru-clru-djsu+blsu+cjtu-bktu-dkqv+clqv+disv-alsv-citv+aktv+djqw-blqw-dirw+alrw+bitw-ajtw-cjqx+bkqx+cirx-akrx-bisx+ajsx,
  hkru-glru-hjsu+flsu+gjtu-fktu-hkqv+glqv+hisv-elsv-gitv+ektv+hjqw-flqw-hirw+elrw+fitw-ejtw-gjqx+fkqx+girx-ekrx-fisx+ejsx,
  doru-cpru-dnsu+bpsu+cntu-botu-doqv+cpqv+dmsv-apsv-cmtv+aotv+dnqw-bpqw-dmrw+aprw+bmtw-antw-cnqx+boqx+cmrx-aorx-bmsx+ansx,
  horu-gpru-hnsu+fpsu+gntu-fotu-hoqv+gpqv+hmsv-epsv-gmtv+eotv+hnqw-fpqw-hmrw+eprw+fmtw-entw-gnqx+foqx+gmrx-eorx-fmsx+ensx,
  loru-kpru-lnsu+jpsu+kntu-jotu-loqv+kpqv+lmsv-ipsv-kmtv+iotv+lnqw-jpqw-lmrw+iprw+jmtw-intw-knqx+joqx+kmrx-iorx-jmsx+insx,
  ay+bz+cA+dB,ey+fz+gA+hB,iy+jz+kA+lB,my+nz+oA+pB,qy+rz+sA+tB,uy+vz+wA+xB,
  aC+bD+cE+dF,eC+fD+gE+hF,iC+jD+kE+lF,mC+nD+oE+pF,qC+rD+sE+tF,uC+vD+wE+xF,
  aG+bH+cI+dJ,eG+fH+gI+hJ,iG+jH+kI+lJ,mG+nH+oI+pJ,qG+rH+sI+tJ,uG+vH+wI+xJ,
  aK+bL+cM+dN,eK+fL+gM+hN,iK+jL+kM+lN,mK+nL+oM+pN,qK+rL+sM+tN,uK+vL+wM+xN,
  BEHK-AFHK-BDIK+zFIK+ADJK-zEJK-BEGL+AFGL+BCIL-yFIL-ACJL+yEJL+BDGM-zFGM-BCHM+yFHM+zCJM-yDJM-ADGN+zEGN+ACHN-yEHN-zCIN+yDIN,
  aO+bP+cQ+dR,eO+fP+gQ+hR,iO+jP+kQ+lR,mO+nP+oQ+pR,qO+rP+sQ+tR,uO+vP+wQ+xR,
  BEHO-AFHO-BDIO+zFIO+ADJO-zEJO-BEGP+AFGP+BCIP-yFIP-ACJP+yEJP+BDGQ-zFGQ-BCHQ+yFHQ+zCJQ-yDJQ-ADGR+zEGR+ACHR-yEHR-zCIR+yDIR,
  BELO-AFLO-BDMO+zFMO+ADNO-zENO-BEKP+AFKP+BCMP-yFMP-ACNP+yENP+BDKQ-zFKQ-BCLQ+yFLQ+zCNQ-yDNQ-ADKR+zEKR+ACLR-yELR-zCMR+yDMR,
  BILO-AJLO-BHMO+zJMO+AHNO-zINO-BIKP+AJKP+BGMP-yJMP-AGNP+yINP+BHKQ-zJKQ-BGLQ+yJLQ+zGNQ-yHNQ-AHKR+zIKR+AGLR-yILR-zGMR+yHMR,
  FILO-EJLO-FHMO+DJMO+EHNO-DINO-FIKP+EJKP+FGMP-CJMP-EGNP+CINP+FHKQ-DJKQ-FGLQ+CJLQ+DGNQ-CHNQ-EHKR+DIKR+EGLR-CILR-DGMR+CHMR,
  aS+bT+cU+dV,eS+fT+gU+hV,iS+jT+kU+lV,mS+nT+oU+pV,qS+rT+sU+tV,uS+vT+wU+xV,
  BEHS-AFHS-BDIS+zFIS+ADJS-zEJS-BEGT+AFGT+BCIT-yFIT-ACJT+yEJT+BDGU-zFGU-BCHU+yFHU+zCJU-yDJU-ADGV+zEGV+ACHV-yEHV-zCIV+yDIV,
  BELS-AFLS-BDMS+zFMS+ADNS-zENS-BEKT+AFKT+BCMT-yFMT-ACNT+yENT+BDKU-zFKU-BCLU+yFLU+zCNU-yDNU-ADKV+zEKV+ACLV-yELV-zCMV+yDMV,
  BILS-AJLS-BHMS+zJMS+AHNS-zINS-BIKT+AJKT+BGMT-yJMT-AGNT+yINT+BHKU-zJKU-BGLU+yJLU+zGNU-yHNU-AHKV+zIKV+AGLV-yILV-zGMV+yHMV,
  FILS-EJLS-FHMS+DJMS+EHNS-DINS-FIKT+EJKT+FGMT-CJMT-EGNT+CINT+FHKU-DJKU-FGLU+CJLU+DGNU-CHNU-EHKV+DIKV+EGLV-CILV-DGMV+CHMV,
  BEPS-AFPS-BDQS+zFQS+ADRS-zERS-BEOT+AFOT+BCQT-yFQT-ACRT+yERT+BDOU-zFOU-BCPU+yFPU+zCRU-yDRU-ADOV+zEOV+ACPV-yEPV-zCQV+yDQV,
  BIPS-AJPS-BHQS+zJQS+AHRS-zIRS-BIOT+AJOT+BGQT-yJQT-AGRT+yIRT+BHOU-zJOU-BGPU+yJPU+zGRU-yHRU-AHOV+zIOV+AGPV-yIPV-zGQV+yHQV,
  FIPS-EJPS-FHQS+DJQS+EHRS-DIRS-FIOT+EJOT+FGQT-CJQT-EGRT+CIRT+FHOU-DJOU-FGPU+CJPU+DGRU-CHRU-EHOV+DIOV+EGPV-CIPV-DGQV+CHQV,
  BMPS-ANPS-BLQS+zNQS+ALRS-zMRS-BMOT+ANOT+BKQT-yNQT-AKRT+yMRT+BLOU-zNOU-BKPU+yNPU+zKRU-yLRU-ALOV+zMOV+AKPV-yMPV-zKQV+yLQV,
  FMPS-ENPS-FLQS+DNQS+ELRS-DMRS-FMOT+ENOT+FKQT-CNQT-EKRT+CMRT+FLOU-DNOU-FKPU+CNPU+DKRU-CLRU-ELOV+DMOV+EKPV-CMPV-DKQV+CLQV,
  JMPS-INPS-JLQS+HNQS+ILRS-HMRS-JMOT+INOT+JKQT-GNQT-IKRT+GMRT+JLOU-HNOU-JKPU+GNPU+HKRU-GLRU-ILOV+HMOV+IKPV-GMPV-HKQV+GLQV";
J1
-*
  time gb(ideal J1_*, Algorithm=>LinearAlgebra); -- 69.9 sec
    -- Mike M1 max,  std::variant VectorArithmetic: 18.0 sec, 18.46 sec
    -- Mike M1 max,  virtual VectorArithmetic: 17.88 sec, 19.06 sec

  time gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- 66.0 sec
  time gb(J1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial); -- 43.8 sec
  time gb(J1, MaxReductionCount=>3000); -- 51.34 sec

  gbTrace=2  
  elapsedTime groebnerBasis(J1, Strategy => "F4"); -- 198 sec (4761 elements)
  J1 = ideal J1_*;
  elapsedTime groebnerBasis(J1, Strategy => "F4", "MGBOptions" => {"Threads" => 6});
  debug Core
  kk = ZZp(101, Strategy => "Old")
  R1=kk[vars(0..47), MonomialSize=>8];
  J1 = sub(J1, R1)
  J1 = ideal J1_*; elapsedTime(gens gb(J1, Algorithm=>LinearAlgebra)); -- 30 seconds -- 23 sec MES MBP 1/24/21
*-
----------------------------------------------
--random5556
kk = ZZ/101;
R1 = kk[a..g, MonomialSize=>8];
J1 = ideal random(R1^1, R1^{-5,-5,-5,-6});
J1
-*     
  gbTrace=1
  time gb(ideal J1_*, Algorithm=>LinearAlgebra); -- 54.1 sec (53.25 sec, 58.7 sec, MBP 7/17/09)
    -- Mike M1 max,  std::variant VectorArithmetic: 14.29 sec, 14.3  sec
    -- Mike M1 max,  virtual VectorArithmetic: 14.43 sec, 14.28  sec
  time gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- > 120 sec
  time gb(J1, MaxReductionCount=>3000);


  -- 28 Dec 2020
  debug Core
  setRandomSeed "342347827"
  kk = ZZp(101, Strategy => "Old")
  kk = ZZp(101, Strategy => "Aring")
  R1 = kk[a..g, MonomialSize=>8];
  J1 = ideal random(R1^1, R1^{-5,-5,-5,-6});

  gbTrace=2
  elapsedTime gb(J1, Algorithm=>LinearAlgebra); -- 23 sec -- 18 sec MES MBP 1/24/21
  J1 = ideal J1_*;
  elapsedTime groebnerBasis(J1, Strategy => "F4"); -- 12.5 sec (5.2 sec, using TBB)
  elapsedTime groebnerBasis(J1, Strategy => "F4", "MGBOptions" => {"Threads" => 1}); -- 12.5 sec
  J1 = ideal J1_*;
  elapsedTime groebnerBasis(J1, Strategy => "MGB"); -- 108.6 sec
*-
----------------------------------------------
--random5555
kk = ZZ/101;
R1 = kk[a..g, MonomialSize=>8];
J1 = ideal random(R1^1, R1^{-5,-5,-5,-5});
J1
-*     
  gbTrace=1
  time gb(J1, Algorithm=>LinearAlgebra); -- 42.1 sec, 3/11/10 MBP 2.4 GHz: 32.34 sec r11025
  time gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- > 
  time gb(J1, MaxReductionCount=>3000);

   -- 28 Dec 202
  gbTrace=1
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "F4"); -- 7.34 sec (3.1 sec tbb MES MBP 1/24/21)
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "MGB"); -- 59.1 sec
  J1 = ideal J1_*; elapsedTime gb(J1, Algorithm=>LinearAlgebra); -- 11.7 sec (10.0 sec MES MBP 1/24/21)
  -- Mike M1 max,  std::variant VectorArithmetic: 8.47 sec, 8.49 sec
  -- Mike M1 max,  virtual VectorArithmetic: 8.42 sec, 8.5 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1); -- much longer! > 720 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- 438 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial); -- 419 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, MaxReductionCount=>3000); -- 560 sec
*-
----------------------------------------------
--what was this one?
restart
kk = ZZ/32003
kk = QQ
debug Core
kk = ZZp(32003, Strategy => "Aring")
R1 = kk[vars(0..39),MonomialSize=>8];
J1 = ideal"b2de+bd2f+a2dg+abdh+ad2i+b2cj+bc2k+bcdl+a2cm+abcn+ac2o+acdp,
       b2dq+bd2r+a2ds+abdt+ad2u+b2cv+bc2w+bcdx+a2cy+abcz+ac2A+acdB,
       b2dC+bd2D+a2dE+abdF+ad2G+b2cH+bc2I+bcdJ+a2cK+abcL+ac2M+acdN";
J1
-*
  gbTrace=2
  time gb(J1, Algorithm=>LinearAlgebra); -- 6.78 sec -- uhoh
  time gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- 2.03 sec
  time gb(J1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial); -- 2.3 sec
  time gb(J1, MaxReductionCount=>3000); -- 62.7 sec

   -- 28 Dec 202
  debug Core
  kk = ZZp(32003, Strategy => "Old")
  R1 = kk[vars(0..39),MonomialSize=>8];
  J1 = ideal"b2de+bd2f+a2dg+abdh+ad2i+b2cj+bc2k+bcdl+a2cm+abcn+ac2o+acdp,
       b2dq+bd2r+a2ds+abdt+ad2u+b2cv+bc2w+bcdx+a2cy+abcz+ac2A+acdB,
       b2dC+bd2D+a2dE+abdF+ad2G+b2cH+bc2I+bcdJ+a2cK+abcL+ac2M+acdN";

  gbTrace=1
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "F4"); -- 3.2  sec
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "MGB"); -- 1.43  sec
  J1 = ideal J1_*; elapsedTime gb(J1, Algorithm=>LinearAlgebra); -- 2.89 sec
    -- Mike M1 max,  std::variant VectorArithmetic: 1.69 sec, 1.97 sec
    -- Mike M1 max,  virtual VectorArithmetic: 1.36 sec, 1.31  sec

  J1 = ideal J1_*; elapsedTime gens gb(J1); -- 48 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- 1.7 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial); -- 1.8 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, MaxReductionCount=>3000); -- 31 sec
*-
----------------------------------------------
--gbB148
R1 = ZZ/32003[reverse(p_(1,1,1,1,1)..p_(2,2,2,2,2)), MonomialSize=>8];
J1 = ideal(
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
J1
-*
  gbTrace=1
  time gb(J1, Algorithm=>LinearAlgebra); -- 58.6 sec, 3/11/10: 56.91 sec
  time gb(J1, MaxReductionCount=>3000); -- 18.3 sec
  time gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- 32.9 sec
  
  debug Core
  kk = ZZp(32003, Strategy => "Old")
  kk = ZZ/32003
  R1 = kk[reverse(p_(1,1,1,1,1)..p_(2,2,2,2,2)), MonomialSize=>8];
  J1 = sub(J1, R1)
  gbTrace=1
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "F4"); -- 65 sec (72 sec tbb MES MBP 1/24/21 hmmm)
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "MGB"); -- 4.0  sec (3.4 sec MES MBP 1/24/21)
  J1 = ideal J1_*; elapsedTime gb(J1, Algorithm=>LinearAlgebra); -- 27.5  sec (23.5 sec MES MBP 1/24/21)
    -- Mike M1 max,  std::variant VectorArithmetic: 18.83 sec, 18.43 sec
    -- Mike M1 max,  virtual VectorArithmetic: 17.91 sec, 18.33 sec

  J1 = ideal J1_*; elapsedTime gens gb(J1); -- 21 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- 20 sec (but printing debugging stuff too)
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial); -- 10.8 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, MaxReductionCount=>3000); -- 19.4 sec

*-
----------------------------------------------
--hilbert-kunz-2
R1 = ZZ/5[a..d,MonomialSize=>16];
J1 = ideal{a^3-2*a^2*b-a*b^2-2*b^3+a*b*c-2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d
	      -2*b^2*d-a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3, a^125, b^125,
	      c^125, d^125};
J1
-*
  gbTrace=1
  time gb(I, Algorithm=>LinearAlgebra); -- makes it to degree 232, when I get tired...
     -- at that point: #GB = 2607
*-
----------------------------------------------
--hilbert-kunz-1
R1 = ZZ/5[a..d,MonomialSize=>16];
J1 = ideal{a^3-2*a^2*b-a*b^2-2*b^3+a*b*c-2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d
	      -2*b^2*d-a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3, a^125, b^125,
	      c^125, d^25};
J1
-*
  gbTrace=3
  time gb(J1, Algorithm=>LinearAlgebra); -- 76.27 sec, 3/11/10: 74.49 sec
  time gb(J1, MaxReductionCount=>3000); -- 70.49 sec
  time gb(J1, Algorithm=>Homogeneous2); -- > 130 sec
  time gb(J1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial); -- much much faster at the initial reduction, still > 140 sec
  time gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- 27.3 seconds!

  debug Core
  kk = ZZp(5, Strategy => "Old")
  R1 = kk[a..d,MonomialSize=>16];
  J1 = sub(J1, R1)
  gbTrace=1
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "F4"); -- 1.30 sec
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "MGB"); -- 2.94 sec
  J1 = ideal J1_*; elapsedTime gb(J1, Algorithm=>LinearAlgebra); -- 33 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1); -- 86 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- 31 sec (but printing debugging stuff too)
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial); -- 602 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, MaxReductionCount=>3000); --  sec
*-
----------------------------------------------
--4by4 commuting matrices, grevlex
needsPackage "ExampleIdeals"
J1 = commuting4by4grevlex (ZZ/101)
-*
  gbTrace=1
  time gb(J1, DegreeLimit=>9, MaxReductionCount=>3000); 
  time gb(J1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial) -- 350.25 sec, 467 gens
  time gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial) -- 358.4 sec, 467 gens
*-
----------------------------------------------
--malkin CGU3
R1 = ZZ/2[x_0..x_38,MonomialSize=>8];
J1 = ideal(
x_0^3+1,
x_0^2+x_0*x_1+x_1^2,
x_0^2+x_0*x_6+x_6^2,
x_0^2+x_0*x_9+x_9^2,
x_1^2+x_1*x_2+x_2^2,
x_1^2+x_1*x_5+x_5^2,
x_1^2+x_1*x_7+x_7^2,
x_2^2+x_2*x_3+x_3^2,
x_2^2+x_2*x_6+x_6^2,
x_2^2+x_2*x_8+x_8^2,
x_3^2+x_3*x_4+x_4^2,
x_3^2+x_3*x_7+x_7^2,
x_3^2+x_3*x_9+x_9^2,
x_4^2+x_4*x_5+x_5^2,
x_4^2+x_4*x_8+x_8^2,
x_5^2+x_5*x_10+x_10^2,
x_6^2+x_6*x_10+x_10^2,
x_7^2+x_7*x_10+x_10^2,
x_8^2+x_8*x_10+x_10^2,
x_9^2+x_9*x_10+x_10^2,
x_4^2+x_4*x_19+x_19^2,
x_0^2+x_0*x_18+x_18^2,
x_18^2+x_18*x_19+x_19^2,
x_11^2+x_11*x_12+x_12^2,
x_12^2+x_12*x_13+x_13^2,
x_13^2+x_13*x_14+x_14^2,
x_15^2+x_15*x_16+x_16^2,
x_11^2+x_11*x_16+x_16^2,
x_0^2+x_0*x_11+x_11^2,
x_12^2+x_12*x_18+x_18^2,
x_13^2+x_13*x_19+x_19^2,
x_0^2+x_0*x_14+x_14^2,
x_15^2+x_15*x_18+x_18^2,
x_16^2+x_16*x_19+x_19^2,
x_11^2+x_11*x_17+x_17^2,
x_15^2+x_15*x_17+x_17^2,
x_13^2+x_13*x_17+x_17^2,
x_15^2+x_15*x_28+x_28^2,
x_14^2+x_14*x_27+x_27^2,
x_27^2+x_27*x_28+x_28^2,
x_20^2+x_20*x_21+x_21^2,
x_21^2+x_21*x_22+x_22^2,
x_22^2+x_22*x_23+x_23^2,
x_23^2+x_23*x_24+x_24^2,
x_24^2+x_24*x_25+x_25^2,
x_20^2+x_20*x_25+x_25^2,
x_21^2+x_21*x_27+x_27^2,
x_22^2+x_22*x_28+x_28^2,
x_14^2+x_14*x_23+x_23^2,
x_24^2+x_24*x_27+x_27^2,
x_25^2+x_25*x_28+x_28^2,
x_20^2+x_20*x_26+x_26^2,
x_24^2+x_24*x_26+x_26^2,
x_22^2+x_22*x_26+x_26^2,
x_20^2+x_20*x_37+x_37^2,
x_14^2+x_14*x_36+x_36^2,
x_36^2+x_36*x_37+x_37^2,
x_29^2+x_29*x_30+x_30^2,
x_30^2+x_30*x_31+x_31^2,
x_31^2+x_31*x_32+x_32^2,
x_32^2+x_32*x_33+x_33^2,
x_33^2+x_33*x_34+x_34^2,
x_29^2+x_29*x_34+x_34^2,
x_14^2+x_14*x_29+x_29^2,
x_30^2+x_30*x_36+x_36^2,
x_31^2+x_31*x_37+x_37^2,
x_14^2+x_14*x_32+x_32^2,
x_33^2+x_33*x_36+x_36^2,
x_34^2+x_34*x_37+x_37^2,
x_29^2+x_29*x_35+x_35^2,
x_33^2+x_33*x_35+x_35^2,
x_31^2+x_31*x_35+x_35^2
)

----------------------------------------------
--malkin CGU4
R = ZZ/2[x_0..x_47,MonomialOrder=>GRevLex, MonomialSize=>8];
I = ideal(
x_0^3+1,
x_0^2+x_0*x_1+x_1^2,
x_0^2+x_0*x_6+x_6^2,
x_0^2+x_0*x_9+x_9^2,
x_1^2+x_1*x_2+x_2^2,
x_1^2+x_1*x_5+x_5^2,
x_1^2+x_1*x_7+x_7^2,
x_2^2+x_2*x_3+x_3^2,
x_2^2+x_2*x_6+x_6^2,
x_2^2+x_2*x_8+x_8^2,
x_3^2+x_3*x_4+x_4^2,
x_3^2+x_3*x_7+x_7^2,
x_3^2+x_3*x_9+x_9^2,
x_4^2+x_4*x_5+x_5^2,
x_4^2+x_4*x_8+x_8^2,
x_5^2+x_5*x_10+x_10^2,
x_6^2+x_6*x_10+x_10^2,
x_7^2+x_7*x_10+x_10^2,
x_8^2+x_8*x_10+x_10^2,
x_9^2+x_9*x_10+x_10^2,
x_4^2+x_4*x_19+x_19^2,
x_0^2+x_0*x_18+x_18^2,
x_18^2+x_18*x_19+x_19^2,
x_11^2+x_11*x_12+x_12^2,
x_12^2+x_12*x_13+x_13^2,
x_13^2+x_13*x_14+x_14^2,
x_15^2+x_15*x_16+x_16^2,
x_11^2+x_11*x_16+x_16^2,
x_0^2+x_0*x_11+x_11^2,
x_12^2+x_12*x_18+x_18^2,
x_13^2+x_13*x_19+x_19^2,
x_0^2+x_0*x_14+x_14^2,
x_15^2+x_15*x_18+x_18^2,
x_16^2+x_16*x_19+x_19^2,
x_11^2+x_11*x_17+x_17^2,
x_15^2+x_15*x_17+x_17^2,
x_13^2+x_13*x_17+x_17^2,
x_15^2+x_15*x_28+x_28^2,
x_14^2+x_14*x_27+x_27^2,
x_27^2+x_27*x_28+x_28^2,
x_20^2+x_20*x_21+x_21^2,
x_21^2+x_21*x_22+x_22^2,
x_22^2+x_22*x_23+x_23^2,
x_23^2+x_23*x_24+x_24^2,
x_24^2+x_24*x_25+x_25^2,
x_20^2+x_20*x_25+x_25^2,
x_21^2+x_21*x_27+x_27^2,
x_22^2+x_22*x_28+x_28^2,
x_14^2+x_14*x_23+x_23^2,
x_24^2+x_24*x_27+x_27^2,
x_25^2+x_25*x_28+x_28^2,
x_20^2+x_20*x_26+x_26^2,
x_24^2+x_24*x_26+x_26^2,
x_22^2+x_22*x_26+x_26^2,
x_20^2+x_20*x_37+x_37^2,
x_14^2+x_14*x_36+x_36^2,
x_36^2+x_36*x_37+x_37^2,
x_29^2+x_29*x_30+x_30^2,
x_30^2+x_30*x_31+x_31^2,
x_31^2+x_31*x_32+x_32^2,
x_32^2+x_32*x_33+x_33^2,
x_29^2+x_29*x_34+x_34^2,
x_14^2+x_14*x_29+x_29^2,
x_30^2+x_30*x_36+x_36^2,
x_31^2+x_31*x_37+x_37^2,
x_14^2+x_14*x_32+x_32^2,
x_33^2+x_33*x_36+x_36^2,
x_34^2+x_34*x_37+x_37^2,
x_29^2+x_29*x_35+x_35^2,
x_33^2+x_33*x_35+x_35^2,
x_31^2+x_31*x_35+x_35^2,
x_34^2+x_34*x_46+x_46^2,
x_33^2+x_33*x_45+x_45^2,
x_45^2+x_45*x_46+x_46^2,
x_38^2+x_38*x_39+x_39^2,
x_39^2+x_39*x_40+x_40^2,
x_40^2+x_40*x_41+x_41^2,
x_41^2+x_41*x_42+x_42^2,
x_42^2+x_42*x_43+x_43^2,
x_38^2+x_38*x_43+x_43^2,
x_33^2+x_33*x_38+x_38^2,
x_39^2+x_39*x_45+x_45^2,
x_40^2+x_40*x_46+x_46^2,
x_33^2+x_33*x_41+x_41^2,
x_42^2+x_42*x_45+x_45^2,
x_43^2+x_43*x_46+x_46^2,
x_38^2+x_38*x_44+x_44^2,
x_42^2+x_42*x_44+x_44^2,
x_40^2+x_40*x_44+x_44^2
)

----------------------------------------------
--malkin CGU5
  -- other examples CGU6, CGU7, etc are also good
R1 = ZZ/2[x_0..x_56,MonomialOrder=>GRevLex, MonomialSize=>8];
J1 = ideal(
x_0^3+1,
x_0^2+x_0*x_1+x_1^2,
x_0^2+x_0*x_6+x_6^2,
x_0^2+x_0*x_9+x_9^2,
x_1^2+x_1*x_2+x_2^2,
x_1^2+x_1*x_5+x_5^2,
x_1^2+x_1*x_7+x_7^2,
x_2^2+x_2*x_3+x_3^2,
x_2^2+x_2*x_6+x_6^2,
x_2^2+x_2*x_8+x_8^2,
x_3^2+x_3*x_4+x_4^2,
x_3^2+x_3*x_7+x_7^2,
x_3^2+x_3*x_9+x_9^2,
x_4^2+x_4*x_5+x_5^2,
x_4^2+x_4*x_8+x_8^2,
x_5^2+x_5*x_10+x_10^2,
x_6^2+x_6*x_10+x_10^2,
x_7^2+x_7*x_10+x_10^2,
x_8^2+x_8*x_10+x_10^2,
x_9^2+x_9*x_10+x_10^2,
x_4^2+x_4*x_19+x_19^2,
x_0^2+x_0*x_18+x_18^2,
x_18^2+x_18*x_19+x_19^2,
x_11^2+x_11*x_12+x_12^2,
x_12^2+x_12*x_13+x_13^2,
x_13^2+x_13*x_14+x_14^2,
x_15^2+x_15*x_16+x_16^2,
x_11^2+x_11*x_16+x_16^2,
x_0^2+x_0*x_11+x_11^2,
x_12^2+x_12*x_18+x_18^2,
x_13^2+x_13*x_19+x_19^2,
x_0^2+x_0*x_14+x_14^2,
x_15^2+x_15*x_18+x_18^2,
x_16^2+x_16*x_19+x_19^2,
x_11^2+x_11*x_17+x_17^2,
x_15^2+x_15*x_17+x_17^2,
x_13^2+x_13*x_17+x_17^2,
x_15^2+x_15*x_28+x_28^2,
x_14^2+x_14*x_27+x_27^2,
x_27^2+x_27*x_28+x_28^2,
x_20^2+x_20*x_21+x_21^2,
x_21^2+x_21*x_22+x_22^2,
x_22^2+x_22*x_23+x_23^2,
x_23^2+x_23*x_24+x_24^2,
x_24^2+x_24*x_25+x_25^2,
x_20^2+x_20*x_25+x_25^2,
x_21^2+x_21*x_27+x_27^2,
x_22^2+x_22*x_28+x_28^2,
x_14^2+x_14*x_23+x_23^2,
x_25^2+x_25*x_28+x_28^2,
x_20^2+x_20*x_26+x_26^2,
x_24^2+x_24*x_26+x_26^2,
x_22^2+x_22*x_26+x_26^2,
x_20^2+x_20*x_37+x_37^2,
x_14^2+x_14*x_36+x_36^2,
x_36^2+x_36*x_37+x_37^2,
x_29^2+x_29*x_30+x_30^2,
x_30^2+x_30*x_31+x_31^2,
x_31^2+x_31*x_32+x_32^2,
x_32^2+x_32*x_33+x_33^2,
x_29^2+x_29*x_34+x_34^2,
x_14^2+x_14*x_29+x_29^2,
x_30^2+x_30*x_36+x_36^2,
x_31^2+x_31*x_37+x_37^2,
x_14^2+x_14*x_32+x_32^2,
x_33^2+x_33*x_36+x_36^2,
x_34^2+x_34*x_37+x_37^2,
x_29^2+x_29*x_35+x_35^2,
x_33^2+x_33*x_35+x_35^2,
x_31^2+x_31*x_35+x_35^2,
x_34^2+x_34*x_46+x_46^2,
x_33^2+x_33*x_45+x_45^2,
x_45^2+x_45*x_46+x_46^2,
x_38^2+x_38*x_39+x_39^2,
x_39^2+x_39*x_40+x_40^2,
x_40^2+x_40*x_41+x_41^2,
x_41^2+x_41*x_42+x_42^2,
x_42^2+x_42*x_43+x_43^2,
x_38^2+x_38*x_43+x_43^2,
x_33^2+x_33*x_38+x_38^2,
x_39^2+x_39*x_45+x_45^2,
x_40^2+x_40*x_46+x_46^2,
x_33^2+x_33*x_41+x_41^2,
x_42^2+x_42*x_45+x_45^2,
x_43^2+x_43*x_46+x_46^2,
x_38^2+x_38*x_44+x_44^2,
x_42^2+x_42*x_44+x_44^2,
x_40^2+x_40*x_44+x_44^2,
x_24^2+x_24*x_55+x_55^2,
x_27^2+x_27*x_54+x_54^2,
x_54^2+x_54*x_55+x_55^2,
x_47^2+x_47*x_48+x_48^2,
x_48^2+x_48*x_49+x_49^2,
x_49^2+x_49*x_50+x_50^2,
x_50^2+x_50*x_51+x_51^2,
x_51^2+x_51*x_52+x_52^2,
x_47^2+x_47*x_52+x_52^2,
x_27^2+x_27*x_47+x_47^2,
x_48^2+x_48*x_54+x_54^2,
x_49^2+x_49*x_55+x_55^2,
x_27^2+x_27*x_50+x_50^2,
x_51^2+x_51*x_54+x_54^2,
x_52^2+x_52*x_55+x_55^2,
x_47^2+x_47*x_53+x_53^2,
x_51^2+x_51*x_53+x_53^2,
x_49^2+x_49*x_53+x_53^2
)
----------------------------------------------
--
needsPackage "ExampleIdeals"
J1 = mayr(4,2,ZZ/101)
-*
  gbTrace=1
  time gb(J1, Algorithm=>LinearAlgebra); --  91.0 sec
  time gb(J1, MaxReductionCount=>3000); --  127.3 sec
  time gb(J1, Algorithm=>Sugarless); --  sec, start: 9:18:15
  time gb(J1, Algorithm=>Homogeneous2); -- sec

  debug Core
  kk = ZZp(101, Strategy => "Old")
  R1 = kk (monoid ring J1)
  J1 = sub(J1, R1)
  gbTrace=1
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "F4"); -- 41 sec
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "MGB"); -- 8.6  sec
  J1 = ideal J1_*; elapsedTime gb(J1, Algorithm=>LinearAlgebra); -- 45 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1); -- 84 sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Sugarless, Strategy=>LongPolynomial); --  sec (but printing debugging stuff too)
  J1 = ideal J1_*; elapsedTime gens gb(J1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial); --  sec
  J1 = ideal J1_*; elapsedTime gens gb(J1, MaxReductionCount=>3000); --  sec

*-
----------------------------------------------
-- assocModel(4,5)
-- problem: saturate with respect to the product of variables
R = ZZ/5[a,c..t,b,MonomialSize=>8]
I = ideal"bip-dfq,djm-ehn,-bim+cgn,-gmp+hkq,cnp-dkr,-bhk+cfl,anq-bks,-bhnr+cgms,-gnr+hls,-ior+jms,-bor+clt"
time gb(I, Algorithm=>LinearAlgebra); -- 3/11/10 MBP r11025: 19.68 sec
elapsedTime groebnerBasis(I, Strategy => "F4"); -- (7.6 sec tbb MES MBP 1/24/21)
I = ideal I_*; elapsedTime groebnerBasis(I, Strategy => "F4", "MGBOptions" => {"Threads" => 1});  -- about the same as with tbb.

  debug Core
  kk = ZZp(5, Strategy => "Old")
  R1 = kk (monoid ring I)
  J1 = sub(I, R1)
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "F4"); -- (7.5 sec tbb MES MBP 1/24/21)
  J1 = ideal J1_*; elapsedTime groebnerBasis(J1, Strategy => "MGB"); -- (3.9 sec MES MBP 1/24/21)
  J1 = ideal J1_*; elapsedTime gb(J1, Algorithm=>LinearAlgebra); -- -- (7.6 sec MES MBP 1/24/21)

  I = ideal I_*; elapsedTime gb I; -- (17.6 sec MES MBP 1/24/21)
----------------------------------------------
--chow-flag-7-7-over-h
(m,n) = (7,7)
R = ZZ/32003[a_1..a_m, b_1..b_n, h_1..h_(m+n), Degrees=>{1..m,1..n,1..m+n}]
S = R[x]
fx = (d, a) -> x^d + sum(1..d, i -> a_i * x^(d-i))
F = fx(m,a) * fx(n,b) - fx(m+n,h)
--F = (x^m + sum(1..m, i -> a_i * x^(m-i))) * (x^n + sum(1..n, i -> b_i * x^(n-i))) - sum(1..m+n)
I = sub(ideal last coefficients F, R)

gbTrace=3
time gb(ideal I_*, Algorithm=>LinearAlgebra); -- 3/11/10 MBP r11025: 22.52 sec, over ZZ/32003
time gens gb ideal I_*;
----------------------------------------------
--chow-flag-7-7
(m,n) = (7,7)
R = QQ[a_1..a_m, b_1..b_n, Degrees=>{1..m,1..n}]
S = R[x]
fx = (d, a) -> x^d + sum(1..d, i -> a_i * x^(d-i))
F = fx(m,a) * fx(n,b) - x^(m+n)
--F = (x^m + sum(1..m, i -> a_i * x^(m-i))) * (x^n + sum(1..n, i -> b_i * x^(n-i))) - sum(1..m+n)
I = sub(ideal last coefficients F, R)
gbTrace=3
time gens gb I;
----------------------------------------------
--chow-flag-7-9
(m,n) = (7,9)
R = QQ[a_1..a_m, b_1..b_n, Degrees=>{1..m,1..n}, MonomialSize=>8]
S = R[x]
fx = (d, a) -> x^d + sum(1..d, i -> a_i * x^(d-i))
F = fx(m,a) * fx(n,b) - x^(m+n)
--F = (x^m + sum(1..m, i -> a_i * x^(m-i))) * (x^n + sum(1..n, i -> b_i * x^(n-i))) - sum(1..m+n)
I = sub(ideal last coefficients F, R)
gbTrace=3
time gens gb(I, DegreeLimit=>m*n);
----------------------------------------------
