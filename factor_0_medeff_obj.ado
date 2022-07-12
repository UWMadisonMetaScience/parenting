program factor_0_medeff, rclass
  sem (gender_01 $dum_control-> factor1, ) (gender_01 $dum_control -> factor2, ) (gender_01 $dum_control -> factor3, )(gender_01 $dum_control -> factor4, )  (gender_01 $dum_control -> rel_npap, ) (gender_01 $dum_control -> rel_cite, ) (gender_01 $dum_control -> uni_cola, ) (factor1 $dum_control -> rel_npap, ) (factor1 $dum_control -> rel_cite, ) (factor1 $dum_control -> uni_cola, ) (factor2 $dum_control -> rel_npap, ) (factor2 $dum_control -> rel_cite, ) (factor2 $dum_control -> uni_cola, ) (factor3 $dum_control -> rel_npap, ) (factor3 $dum_control -> rel_cite, ) (factor3 $dum_control -> uni_cola, ) (factor4 $dum_control -> rel_npap, ) (factor4 $dum_control -> rel_cite, ) (factor4 $dum_control -> uni_cola, ) if  married_child == 0, vce(cluster mail_school) nocapslatent
  estat teffects

  *mat bid = r(indirect)
  mat bi = r(direct)
  *mat bt = r(total)
  return scalar f1_pub  = el(bi,1,1) * el(bi,1,41)
  return scalar f1_cite  = el(bi,1,1) * el(bi,1,55)
  return scalar f1_uni  = el(bi,1,1) * el(bi,1,69)

  return scalar f2_pub = el(bi,1,11) * el(bi,1,42)
  return scalar f2_cite = el(bi,1,11) * el(bi,1,56)
  return scalar f2_uni = el(bi,1,11) * el(bi,1,70)

  return scalar f3_pub  = el(bi,1,21) * el(bi,1,43)
  return scalar f3_cite = el(bi,1,21) * el(bi,1,57)
  return scalar f3_uni = el(bi,1,21) * el(bi,1,71)

  return scalar f4_pub  = el(bi,1,31) * el(bi,1,44)
  return scalar f4_cite = el(bi,1,31) * el(bi,1,58)
  return scalar f4_uni = el(bi,1,31) * el(bi,1,72)

end
