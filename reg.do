clear
import excel "data.xlsx", sheet("Sheet1") firstrow

encode gender_new, gen(gender_n)
replace gender_n = 0 if gender_n == 2
encode role_new, gen(role_n)
encode area, gen(area_n)
encode rank, gen(rank_n)
encode mail_school, gen(mail_school_n)
drop if (gender_new == "Non-binary")
gen child_consider=.
replace child_consider = 1 if Q11 == "Yes"
replace child_consider = 0 if Q11 == "No"
gen is_child = 1
replace is_child = 0 if child_num == 0
gen gender_01 = 0
replace gender_01 = 1 if gender_new == "Women"

global control0 "i.gender_n i.rank_n i.area_n i.is_white"
global control "i.gender_n i.rank_n i.area_n i.is_researcher i.is_white"
global dum_control "area_Med area_Nat area_Soc rank_Ear rank_Mid rank_Lat res_20 res_30 is_white"

global outcome "res_ach car_dev contrib gen_equa"
global impact "marriage_impact child_impact"
global impede "emo_pres wf_conf dec_nsup mar_diss net_cons"
global support "fin_sup emo_sup time_sup dec_sup tec_sup net_sup"
global conflict "tim_conf str_conf beh_conf"
global ch_weak "ch_weak_motiv ch_weak_efficiency ch_weak_time ch_weak_abil ch_weak_network ch_weak_oppo"
global ch_strong "ch_strong_motiv ch_strong_efficiency ch_strong_time ch_strong_abil ch_strong_network ch_strong_oppo"
global depvar "rel_npap rel_cite uni_cola"

/***************************************************
before dropping: Men are more likely to be committed than women?
who is more likely to have Children
***************************************************/

// basic stat fact

tab Q2 gender_new
tab Q2 gender_new if married_child == 2
tab Q2 gender_new if married_child == 3

tab is_researcher gender_new
tab is_researcher gender_new if married_child == 2
tab is_researcher gender_new if married_child == 3

tab marital gender_n,col m
mlogit is_married $control0, rrr vce(cluster mail_school) b(0)
replace is_married = 1 if is_married == 2
logit is_married $control0, or vce(cluster mail_school)

tab is_child
tab is_child gender_new, column
tab is_child gender_n,col
logit is_child $control0,or vce(cluster mail_school)

table gender_n if is_married ==1, stat(mean child_num)
table gender_n if is_married ==1, stat(sd child_num)
tobit child_num $control if is_married ==1, ul(6) vce(cluster mail_school)

foreach var of global outcome {
	display "******************************"
	display "****    `var'        ****"
	display "******************************"
	ologit `var' $control0,or vce(cluster mail_school)
}

tab marriage_impact gender_new, column
ologit marriage_impact $control0 ,or vce(cluster mail_school)

tab child_impact gender_new, column
ologit child_impact $control0 ,or vce(cluster mail_school)

// drop
replace married_child = married_child -2


/***************************************************
differences between married-no-child and married-have-child
***************************************************/

foreach var of global outcome {
	display "******************************"
	display "****    `var'        ****"
	display "******************************"
	ologit `var' $control if married_child == 0 | married_child == 1, or vce(cluster mail_school)
	qui ologit `var' $control if married_child == 0,or 
	est store a1
	qui ologit `var' $control if married_child == 1,or
	est store a2
	qui suest a1 a2, vce(cluster mail_school)
	test [a1_`var']3.gender_n=[a2_`var']3.gender_n
}
 
/***************************************************
work-family conflict
***************************************************/

// Is the number of children (include 0) you currently have related to your career considerations (more or less)?

logit child_consider $control if married_child == 0,or vce(cluster mail_school)
logit child_consider $control if married_child == 1,or vce(cluster mail_school)
logit child_consider $control if married_child == 0 | married_child == 1,or vce(cluster mail_school)
qui logit child_consider $control if married_child == 0,or
est store a1
qui logit child_consider $control if married_child == 1,or
est store a2
qui suest a1 a2, vce(cluster mail_school)
test [a1_child_consider]3.gender_n=[a2_child_consider]3.gender_n

// Please evaluate the overall impact of child-rearing/marriage on your career development

ologit marriage_satis $control if married_child == 0 | married_child == 1,or vce(cluster mail_school)
ologit marriage_satis $control if married_child == 0,or vce(cluster mail_school)
ologit marriage_satis $control if married_child == 1,or vce(cluster mail_school)
qui ologit marriage_satis $control if married_child == 0,or 
est store a1
qui ologit marriage_satis $control if married_child == 1,or
est store a2
qui suest a1 a2, vce(cluster mail_school)
test [a1_marriage_satis]3.gender_n=[a2_marriage_satis]3.gender_n

// marriage_impact

ologit marriage_impact $control if married_child == 0 | married_child == 1,or vce(cluster mail_school)
ologit marriage_impact $control if married_child == 0,or vce(cluster mail_school)
ologit marriage_impact $control if married_child == 1,or vce(cluster mail_school)
qui ologit marriage_impact $control if married_child == 0,or
est store a1
qui ologit marriage_impact $control if married_child == 1,or
est store a2
qui suest a1 a2, vce(cluster mail_school)
test [a1_marriage_impact]3.gender_n=[a2_marriage_impact]3.gender_n

ologit child_impact $control if married_child == 1,or vce(cluster mail_school)


// Have you experienced any of the followings that impeded your career development because of spouse/partner related reasons?

foreach var of global impede {
	display "******************************"
	display "****    `var'        ****"
	display "******************************"
	ologit `var' $control if married_child == 0 | married_child == 1,or vce(cluster mail_school)
	*ologit `var' $control if married_child == 0,or vce(cluster mail_school)
	*ologit `var' $control if married_child == 1,or vce(cluster mail_school)
	qui ologit `var' $control if married_child == 0,or
	est store a1
	qui ologit `var' $control if married_child == 1,or
	est store a2
	qui suest a1 a2, vce(cluster mail_school)
	test [a1_`var']3.gender_n=[a2_`var']3.gender_n
}

// Please rate the family-work conflicts you have experienced, if any.


foreach var of global conflict {
	display "******************************"
	display "****    `var'        ****"
	display "******************************"
	ologit `var' $control if married_child == 0 | married_child == 1,or vce(cluster mail_school)
	*ologit `var' $control if married_child == 0,or vce(cluster mail_school)
	*ologit `var' $control if married_child == 1,or vce(cluster mail_school)
	qui ologit `var' $control if married_child == 0,or
	est store a1
	qui ologit `var' $control if married_child == 1,or
	est store a2
	qui suest a1 a2, vce(cluster mail_school)
	test [a1_`var']3.gender_n=[a2_`var']3.gender_n
}


// Overall, to what extent did your current or most recent spouse/partner provide the following support to your career development?


foreach var of global support {
	display "******************************"
	display "****    `var'        ****"
	display "******************************"
	ologit `var' $control if married_child == 0 | married_child == 1,or vce(cluster mail_school)
	*ologit `var' $control if married_child == 0,or vce(cluster mail_school)
	*ologit `var' $control if married_child == 1,or vce(cluster mail_school)
	qui ologit `var' $control if married_child == 0,or
	est store a1
	qui ologit `var' $control if married_child == 1,or
	est store a2
	qui suest a1 a2, vce(cluster mail_school)
	test [a1_`var']3.gender_n=[a2_`var']3.gender_n
}

// Have you ever experienced any of the followings due to child-related reasons (more or less)? Negative

foreach var of global ch_weak {
	display "******************************"
	display "****    `var'        ****"
	display "******************************"
	table (married_child gender_n) (`var')
	*logit `var' $control if married_child == 0,or
	logit `var' $control if married_child == 1,or vce(cluster mail_school)
}


/***************************************************
work-family support
***************************************************/

// Have you ever experienced any of the followings due to child-related reasons (more or less)? Positive

foreach var of global ch_strong {
	display "******************************"
	display "****    `var'        ****"
	display "******************************"
	table (married_child gender_n) (`var')
	*ologit `var' $control if married_child == 0,or
	logit `var' $control if married_child == 1,or vce(cluster mail_school)
}

/***************************************************
Bibliometrics
***************************************************/

foreach var of global depvar {
	display "******************************"
	display "****    `var'        ****"
	display "******************************"
	*reg `var' $control if married_child == 0 | married_child == 1, vce(cluster mail_school)
	reg `var' $control if married_child == 0, vce(cluster mail_school)
	reg `var' $control if married_child == 1, vce(cluster mail_school)
	qui reg `var' $control if married_child == 0
	est store a1
	qui reg `var' $control if married_child == 1
	est store a2
	qui suest a1 a2, vce(cluster mail_school)
	test [a1_mean]3.gender_n=[a2_mean]3.gender_n
}

// loop different ranks
global ranks "rank_Tra rank_Ear rank_Mid rank_Lat"
foreach r of global ranks {
	foreach var of global depvar {
		display "******************************"
		display "****    `var'   `r'   ****"
		display "******************************"
		reg `var' $control if married_child == 0 | married_child == 1 & `r' == 1, vce(cluster mail_school)
		*reg `var' $control if married_child == 0 & `r' == 1, vce(cluster mail_school)
		*reg `var' $control if married_child == 1 & `r' == 1, vce(cluster mail_school)
		qui reg `var' $control if married_child == 0 & `r' == 1
		est store a1
		qui reg `var' $control if married_child == 1 & `r' == 1
		est store a2
		qui suest a1 a2, vce(cluster mail_school)
		test [a1_mean]3.gender_n=[a2_mean]3.gender_n
	}
}

// loop different areas
global areas "area_Med area_Nat area_Soc"
foreach r of global areas {
	foreach var of global depvar {
		display "******************************"
		display "****    `var'   `r'   ****"
		display "******************************"
		reg `var' $control if married_child == 0 & `r' == 1, vce(cluster mail_school)
		reg `var' $control if married_child == 1 & `r' == 1, vce(cluster mail_school)
		qui reg `var' $control if married_child == 0 & `r' == 1
		est store a1
		qui reg `var' $control if married_child == 1 & `r' == 1
		est store a2
		qui suest a1 a2, vce(cluster mail_school)
		test [a1_mean]3.gender_n=[a2_mean]3.gender_n
	}
}

// mediation

// dimension reduction
factor $support $conflict, pcf mineigen(.9)
estat kmo
rotate, blanks(.5)
predict factor1 factor2 factor3 factor4

alpha emo_sup time_sup dec_sup
alpha tim_conf str_conf beh_conf
alpha tec_sup net_sup

forvalues i = 0/1 {
qui sem (gender_01 $dum_control-> factor1, ) (gender_01 $dum_control -> factor2, ) (gender_01 $dum_control -> factor3, )(gender_01 $dum_control -> factor4, )  (gender_01 $dum_control -> res_ach, ) (gender_01 $dum_control -> car_dev, )(gender_01 $dum_control -> contrib, ) (factor1 $dum_control -> res_ach, ) (factor1 $dum_control -> car_dev, ) (factor1 $dum_control -> contrib, ) (factor2 $dum_control -> res_ach, ) (factor2 $dum_control -> car_dev, )  (factor2 $dum_control -> contrib, ) (factor3 $dum_control -> res_ach, ) (factor3 $dum_control -> car_dev, ) (factor3 $dum_control -> contrib, ) (factor4 $dum_control -> res_ach, ) (factor4 $dum_control -> car_dev, ) (factor4 $dum_control -> contrib, ) if  married_child == `i', vce(cluster mail_school) nocapslatent
estat teffects
matrix list r(direct)
}

set seed 54321
bootstrap r(f1_res_ach) r(f1_car_dev) r(f1_contrib) r(f2_res_ach) r(f2_car_dev) r(f2_contrib) r(f3_res_ach) r(f3_car_dev) r(f3_contrib) r(f4_res_ach) r(f4_car_dev) r(f4_contrib), reps(5000): factor_0_medeff_satis
set seed 54321
bootstrap r(f1_res_ach) r(f1_car_dev) r(f1_contrib) r(f2_res_ach) r(f2_car_dev) r(f2_contrib) r(f3_res_ach) r(f3_car_dev) r(f3_contrib) r(f4_res_ach) r(f4_car_dev) r(f4_contrib), reps(5000): factor_1_medeff_satis

forvalues i = 0/1 {
qui sem (gender_01 $dum_control-> factor1, ) (gender_01 $dum_control -> factor2, ) (gender_01 $dum_control -> factor3, )(gender_01 $dum_control -> factor4, )  (gender_01 $dum_control -> rel_npap, ) (gender_01 $dum_control -> rel_cite, ) (gender_01 $dum_control -> uni_cola, ) (factor1 $dum_control -> rel_npap, ) (factor1 $dum_control -> rel_cite, ) (factor1 $dum_control -> uni_cola, ) (factor2 $dum_control -> rel_npap, ) (factor2 $dum_control -> rel_cite, ) (factor2 $dum_control -> uni_cola, ) (factor3 $dum_control -> rel_npap, ) (factor3 $dum_control -> rel_cite, ) (factor3 $dum_control -> uni_cola, ) (factor4 $dum_control -> rel_npap, ) (factor4 $dum_control -> rel_cite, ) (factor4 $dum_control -> uni_cola, ) if  married_child == `i', vce(cluster mail_school) nocapslatent
estat teffects
matrix list r(direct) 
}

set seed 54321
bootstrap r(f1_pub) r(f1_cite) r(f1_uni) r(f2_pub) r(f2_cite) r(f2_uni) r(f3_pub) r(f3_cite) r(f3_uni) r(f4_pub) r(f4_cite) r(f4_uni), reps(5000): factor_0_medeff
set seed 54321
bootstrap r(f1_pub) r(f1_cite) r(f1_uni) r(f2_pub) r(f2_cite) r(f2_uni) r(f3_pub) r(f3_cite) r(f3_uni) r(f4_pub) r(f4_cite) r(f4_uni), reps(5000): factor_1_medeff
