/*

This do file replicates the main analyses conducted in:

Hasan, A., Jung, H., Kinnell, A., Maika, A., Nakajima, N., & Pradhan, M. (2021). 
Contrasting Experiences: Understanding the Longer-Term Impact of Improving Access 
to Pre-Primary Education in Rural Indonesia. 
Journal of Research on Educational Effectiveness, 14(1), 28-56.

(https://www.tandfonline.com/doi/abs/10.1080/19345747.2020.1839989) 

*/


use "oldercohort.dta", clear

*-----------------------------------------------------------------------------*
* 0. Sample size
*-----------------------------------------------------------------------------*

* How many in our analysis sample (i.e., panel, no classmates)?

	count if (usetobal == 1 | usetobal == 4 | usetobal == 5 | usetobal == 7) & year == 1
	gen sample = 1 if (usetobal == 1 | usetobal == 4 | usetobal == 5 | usetobal == 7)
	la var sample "Analytic sample"
	* N = 3,249
	
	gen sample_2013 = 1 if usetobal == 7
	la var sample_2013 "Analytic sample observed in 2013"
	* N = 2,990
	
	// We don't need the classmates and the batch 1 kids for this analysis
	drop if usetobal == 3
	drop if batcha == 1

* Set data as panel
	destring childid, replace
	xtset childid

*-----------------------------------------------------------------------------*
* 1. Clean Covariates (same ones as JOLE paper)
*-----------------------------------------------------------------------------*
	
* Age of child
	summ child_age_yrs

	// impute if missing
	foreach x in 1 2 3 {
		summ child_age_yrs if (sample_2013 == 1) & year == `x'
		replace child_age_yrs = r(mean) if missing(child_age_yrs) & (sample_2013 == 1) & year == `x'
	}
	
	
* Household size
	summ hhsize

	// carryforward if missing
	bysort childid: carryforward hhsize, replace
	
	
* Wealth z-score
	egen hhtagyear = tag(hhid year)
	egen hhtag = tag(hhid)
	
	cap drop score5
	pca  own_radio own_tv own_refrigerator own_bicycle own_motorcycle own_car own_hp own_chicken own_pig own_goat own_cow good_roof good_wall good_floor own_toilet electricity kitchen if hhtagyear & year==1, components(5)
	predict score5
	summ score5,d

	cap drop score6
	pca  own_radio own_tv own_refrigerator own_bicycle own_motorcycle own_car own_hp own_chicken own_pig own_goat own_cow good_roof good_wall good_floor own_toilet electricity kitchen if hhtagyear & year==2, components(5)
	predict score6
	summ score6,d

	cap drop score7
	pca  own_radio own_tv own_refrigerator own_bicycle own_motorcycle own_car own_hp own_chicken own_pig own_goat own_cow good_roof good_wall good_floor own_toilet electricity kitchen if hhtagyear & year==3, components(5)
	predict score7
	summ score7,d


	egen std_score5 = std(score5) if year==1
	egen std_score6 = std(score6) if year==2
	egen std_score7 = std(score7) if year==3

	gen zwealth = std_score5 if year==1
	replace zwealth = std_score6 if year==2
	replace zwealth = std_score7 if year==3

	la var zwealth "Wealth Z-score"
	la var std_score5 "std wealth in 2009"
	la var std_score6 "std wealth in 2010"
	la var std_score7 "std wealth in 2013"

	// impute if missing
	foreach x in 1 2 3 {
		summ zwealth if year == `x'
		replace zwealth = r(mean) if missing(zwealth) & year == `x'
	}

	gen poor = 0 if zwealth >= 0 
	replace poor = 1 if zwealth < 0
	label var poor "zwealth<0 (1=Yes)"

	
* Parenting scores Z-score
	egen parentingz = std(parenting_total_go), mean(0) std(1)
	lab var parentingz "Parenting z-score"

	// impute if missing
	foreach x in 1 2 3 {
		summ parentingz if (sample_2013 == 1) & year == `x'
		replace parentingz = r(mean) if missing(parentingz) & (sample_2013 == 1) & year == `x'
	}
	
	gen lowpar = 0 if parentingz >= 0
	replace lowpar = 1 if parentingz < 0
	label var lowpar "zparenting<0 (1=Yes)"

	
* Mother's education primary school or less
	gen mothered = 0 if mother_edlevel_max > 3 & mother_edlevel_max <.
	replace mothered = 1 if mother_edlevel_max < 4
	lab var mothered "Mother edu primary school or less (1 = Yes)"
	tab mothered

	// carryforward if missing
	bysort childid: carryforward mothered, replace
	
	// imput if missing
	foreach x in 1 2 3 {
		summ mothered if year == `x'
		replace mothered = round(r(mean)) if missing(mothered) & (sample_2013 == 1) & year == `x'
	}

	
* Child's gender
	replace gender = 0 if gender == 3
	lab var gender "Male"
	tab gender

	// carryforward if missing
	bysort childid: carryforward gender, replace
	

	summ child_age_yrs hhsize zwealth parentingz mothered gender if (sample_2013 == 1) & year == 3

	
*-----------------------------------------------------------------------------*
* 2. Create baseline measures of child development 
*-----------------------------------------------------------------------------*

* Impute 2010 scores if 2009 scores are missing/wrong
	
	// Baseline BMI:
	
	gen bmi_bl = .
	label var bmi_bl "Baseline BMI (2009 or 2010)"
	
	quietly forvalues i = 1/4 { 
		generate include = 1 if (year == 1 | year == 2) & bmi <= 30 
		egen work = min(bmi * include), by(childid) 
		replace bmi_bl = work if year == `i' 
		drop include work 
		} 
 
 
	// Baseline EDI:
	
	foreach var in short_phys short_soc short_emot short_langcog short_comgen{
		gen `var'_bl = .
		label var `var'_bl "Baseline `var' (2009 or 2010)"
	
	quietly forvalues i = 1/4 { 
		generate include = 1 if (year == 1 | year == 2)  
		egen work = min(`var' * include), by(childid) 
		replace `var'_bl = work if year == `i' 
		drop include work 
		
		} 
		}
	
	// Impute mean for missing 2009 & 2010 baseline measures
	
	summ short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl if year == 3 & sample == 1

	foreach var in short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl{
		summ `var' if sample == 1
		replace `var' = round(r(mean)) if missing(`var') & sample == 1
		}
	summ short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl if year == 1 & sample == 1

	
*-----------------------------------------------------------------------------*
* 3. Create outcome measures of child development 
*-----------------------------------------------------------------------------*
	
* Months of enrollment

	summ num_month_sd_* num_month_tk_* num_month_ra_* ///
		 num_month_kbntpk_* num_month_kbtpk_* if sample_2013 ==1 & year == 3

	foreach x in 08 09 10 11 12 {
		
		summ num_month_sd_`x' if ( sample_2013 == 1)
		replace num_month_sd_`x' = round(r(mean)) if missing(num_month_sd_`x') & ( sample_2013 == 1)
		
		summ num_month_tk_`x' if ( sample_2013 == 1)
		replace num_month_tk_`x' = round(r(mean)) if missing(num_month_tk_`x') & ( sample_2013 == 1)

		summ num_month_ra_`x' if ( sample_2013 == 1)
		replace num_month_ra_`x' = round(r(mean)) if missing(num_month_ra_`x') & ( sample_2013 == 1)

		summ num_month_kbntpk_`x' if ( sample_2013 == 1)
		replace num_month_kbntpk_`x' = round(r(mean)) if missing(num_month_kbntpk_`x') & ( sample_2013 == 1)

		summ num_month_kbtpk_`x' if ( sample_2013 == 1)
		replace num_month_kbtpk_`x' = round(r(mean)) if missing(num_month_kbtpk_`x') & ( sample_2013 == 1)

		}
		
	summ num_month_sd_* num_month_tk_* num_month_ra_* ///
		 num_month_kbntpk_* num_month_kbtpk_* if sample_2013 ==1 & year == 3		
		 
	egen month_pr_13 = rowtotal(num_month_sd_08 num_month_sd_09 num_month_sd_10 ///
		num_month_sd_11 num_month_sd_12)  
		label var month_pr_13 "Total months in primary (2008-2013)"
		
	egen month_kd_13 = rowtotal(num_month_tk_08 num_month_tk_09 num_month_tk_10 ///
		num_month_tk_11 num_month_tk_12 ///
		num_month_ra_08 num_month_ra_09 num_month_ra_10 ///
		num_month_ra_11 num_month_ra_12)
		label var month_kd_13 "Total months in kindergarten (2008-2013)"
			
	egen month_kbntpk_13 = rowtotal(num_month_kbntpk_08 num_month_kbntpk_09 num_month_kbntpk_10 ///
		num_month_kbntpk_11 num_month_kbntpk_12)
		label var month_kbntpk_13 "Total months in non-project playgroup (2008-2013)"
		
	egen month_kbtpk_13 = rowtotal(num_month_kbtpk_08 num_month_kbtpk_09 num_month_kbtpk_10 ///
		num_month_kbtpk_11 num_month_kbtpk_12)
		label var month_kbtpk_13 "Total months in project playgroup (2008-2013)"
			
	summ month_pr_* month_kd_* month_kbntpk_* month_kbtpk_* if ( sample_2013 == 1) & year == 3
	
	
* EDI

	bysort year: summ short_phys short_soc short_emot short_langcog ///
				 short_comgen short_nlowtot short_nlow short_nlow2 ///
				 if ( sample_2013 == 1) 
		
	gen std_phys = .
	gen std_soc = . 
	gen std_emot = . 
	gen std_langcog = . 
	gen std_comgen = .	
		
	foreach var in phys soc emot langcog comgen {
		egen std_`var'_3 = std(short_`var') if year == 3
		
		replace std_`var' = std_`var'_3 if year == 3
	}
	
	la var std_phys "EDI Physical (sd)"
	la var std_soc "EDI Social (sd)"
	la var std_emot "EDI Emotional (sd)"
	la var std_langcog "EDI Lang & Cog (sd)"
	la var std_comgen "EDI Comm & General (sd)"
	
	
* Test scores (see "testcheck.do" for why we decided on these)

	gen common_1 = .
	replace common_1 = score_11 + score_12 + score_14 if cov11 == 0
	replace common_1 = score_1 + score_4 + score_5 if cov11 == 1
	la var common_1 "Language - Match Picture (common items)"
	
	gen common_2 = .
	replace common_2 = score_exact_17 + score_exact_18 + score_exact_19 + score_exact_20 if cov11 == 0
	replace common_2 = score_exact_7 + score_exact_8 + score_exact_9 + score_exact_10 if cov11 == 1
	la var common_2 "Language - Mention Objects (common items)"
	
	gen common_3 = .
	replace common_3 = score_24 + score_25 + score_26 + score_27 + score_28 + score_29 + score_30 + score_31 if cov11 == 0
	replace common_3 = score_22 + score_23 + score_24 + score_25 + score_26 + score_27 + score_28 + score_29 if cov11 == 1
	la var common_3 "Math - Summation (common items)"
	
	gen common_4 = .
	replace common_4 = score_32 + score_33 + score_34 if cov11 == 0
	replace common_4 = score_30 + score_31 + score_32 if cov11 == 1
	la var common_4 "Math - Order number (common items)"
	
	gen common_5 =.
	replace common_5 = score_39 + score_40 + score_41 + score_42 + score_43 + score_44 + score_45 ///
						+ score_46 + score_47 + score_48 + score_49 + score_50 + score_51 + score_52 if cov11 == 0
	replace common_5 = score_47 + score_48 + score_49 + score_50 + score_51 + score_52 + score_53 ///
						+ score_54 + score_55 + score_56 + score_57 + score_58 + score_59 + score_60 if cov11 == 1
	la var common_5 "Raven Test (common items)"


	egen std_langmp = std(common_1)
	egen std_langmo = std(common_2)
	egen std_mathsu = std(common_3)
	egen std_mathon = std(common_4)
	la var std_langmp "SD Language - Match Picture (common items)"
	la var std_langmo "SD Language - Mention Objects (common items)"
	la var std_mathsu "SD Math - Summation (common items)"
	la var std_mathon "SD Math - Order number (common items)"
	summ std_langmp std_langmo std_mathsu std_mathon
	
		
	// equally weight the two sections of the language and math
	gen newlangscore = (common_1*3.5/3 + common_2*3.5/4)
	gen newmathscore = (common_3*5.5/8 + common_4*5.5/3)
	gen newravenscore = (common_5)/14
	
	
	egen std_newlangscore = std(newlangscore)
	la var std_newlangscore "Language test (sd)"
	egen std_newmathscore = std(newmathscore)
	la var std_newmathscore "Math test (sd)"
	egen std_newraven = std(newravenscore)
	la var std_newraven "Raven test (sd)"
	summ std_newlangscore std_newmathscore std_newraven 

*-----------------------------------------------------------------------------*
* 4. Check correlation between baseline measures & outcomes
*-----------------------------------------------------------------------------*
	
/* Check correlation of baseline measures

	 gr matrix short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl, half ms(p) name(corr_bl_4yo, replace)
	 graph export corr_bl_4yo.pdf, replace
	 
*/

*-----------------------------------------------------------------------------*
* 5. Baseline table
*-----------------------------------------------------------------------------*
	
* Create batch 3 v. 5

	gen treat = .
	replace treat = 1 if batcha == 3
	replace treat = 0 if batcha == 5
	la var treat "Treatment village (1 = Yes)"
	
	
* Baseline variables	

	// By batch
	
	estimates drop _all 

	foreach var in child_age_yrs hhsize zwealth parentingz mothered gender ///
				   bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl {
				   
		estpost tabstat `var' if sample == 1 & year == 1, by(batcha) statistics(mean sd) columns(statistics) listwise
		est store `var'
		
		}
		
	esttab child_age_yrs hhsize zwealth parentingz mothered gender ///
		bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl ///
		using "$output/table1_2009_4.csv", replace main(mean) aux(sd) wide
		
		
	// Batch 1 vs. 3
	
	mat drop _all
	
	foreach var in child_age_yrs hhsize zwealth parentingz mothered gender ///
				   bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl {
		
		reg `var' i.batcha if sample == 1 & year == 1, cluster(ea)
		est store `var'
		
		}

	xml_tab child_age_yrs hhsize zwealth parentingz mothered gender ///
		bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl, ///
		save("$output/table1diff_2009_4.xls") replace sheet(diff) drop (_cons) below
	
	
	// By treatment status
	
	estimates drop _all 

	foreach var in child_age_yrs hhsize zwealth parentingz mothered gender ///
				   bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl {
				   
		estpost tabstat `var' if sample == 1 & year == 1, by(treat) statistics(mean sd) columns(statistics) listwise
		est store `var'
		
		}
		
	esttab child_age_yrs hhsize zwealth parentingz mothered gender ///
		bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl ///
		using "$output/table1_2009final_4.csv", replace main(mean) aux(sd) wide

		
	// Treatment v. Comparison
	
	mat drop _all
	
	foreach var in child_age_yrs hhsize zwealth parentingz mothered gender ///
				   bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl {
		
		reg `var' i.treat if sample == 1 & year == 1, cluster(ea)
		est store `var'
		
		}

	xml_tab child_age_yrs hhsize zwealth parentingz mothered gender ///
		bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl, ///
		save("$output/table1diff_2009final_4.xls") replace sheet(diff) drop (_cons) below
		
		
*-----------------------------------------------------------------------------*
* 6. Final regression tables
*-----------------------------------------------------------------------------*	
	
	global base "bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl"
	global controls "child_age_yrs hhsize zwealth parentingz mothered gender"

	
* 2013 - 8 years-old
	
	// Test score
	
	estimates clear
	global condyr3 "batchactual>=3 & batchactual<=5 & year==3 & sample_2013 == 1"
	
	reg std_langmp treat $base $controls cov11 if $condyr3, robust cluster(ea)
		summ std_langmp if e(sample) & treat == 0
		outreg2 using older_final_3.xls, replace bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_langmp treat $base $controls cov11 if $condyr3 & poor == 1, robust cluster(ea)
		summ std_langmp if e(sample) & treat == 0
		outreg2 using older_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_langmp treat $base $controls cov11 if $condyr3 & poor == 0, robust cluster(ea)
		summ std_langmp if e(sample) & treat == 0
		outreg2 using older_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_langmp treat $base $controls cov11 if $condyr3 & lowpar == 1, robust cluster(ea)
		summ std_langmp if e(sample) & treat == 0
		outreg2 using older_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_langmp treat $base $controls cov11 if $condyr3 & lowpar == 0, robust cluster(ea)
		summ std_langmp if e(sample) & treat == 0
		outreg2 using older_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	
	foreach varname in std_langmo std_mathsu std_mathon std_newraven{
					
	reg `varname' treat $base $controls cov11 if $condyr3, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using older_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controls cov11 if $condyr3 & poor == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using older_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	reg `varname' treat $base $controls cov11 if $condyr3 & poor == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using older_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controls cov11 if $condyr3 & lowpar == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using older_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controls cov11 if $condyr3 & lowpar == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using older_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	}

	
*-----------------------------------------------------------------------------*
* 12. Mother education and wealth profiles by enrollment
*-----------------------------------------------------------------------------*		
	
	summ month_kd_13 month_kbntpk_13 month_kbtpk_13 if year == 3
			
	// no eced
	summ mothered zwealth if sample_2013 == 1 & year == 3 & month_kbtpk_13 == 0 & month_kd_13 == 0 & month_kbntpk_13 == 0
	
	// project playgroup only
	summ mothered zwealth if sample_2013 == 1 & year == 3 & month_kbtpk_13 > 0 & month_kd_13 < 1 & month_kbntpk_13 < 1
	
	// non-project playgroup only
	summ mothered zwealth if sample_2013 == 1 & year == 3 & month_kbntpk_13 > 0 & month_kd_13 < 1 & month_kbtpk_13 < 1
	
	// kindergarten only
	summ mothered zwealth if sample_2013 == 1 & year == 3 & month_kd_13 > 0 & month_kbtpk_13 < 1 & month_kbntpk_13 < 1  
	
	// project playgroup & other
	summ mothered zwealth if sample_2013 == 1 & year == 3 & month_kbtpk_13 > 0 & (month_kd_13 > 0 | month_kbntpk_13 > 0)
	
	// non-project playgroup & other
	summ mothered zwealth if sample_2013 == 1 & year == 3 & month_kbntpk_13 > 0 & (month_kd_13 > 0 | month_kbtpk_13 > 0)
	
	
*-------------------------------------------------------------------------------------------------------*
* Save data with key vars for appending with younger panel
*-------------------------------------------------------------------------------------------------------*	

keep bmi_bl short_phys_bl short_soc_bl short_emot_bl short_langcog_bl short_comgen_bl ///
     child_age_months child_age_months_alt child_age_years child_age_yrs ///
	 hhsize zwealth parentingz mothered gender ///
	 treat batch* year sample* ///
	 hhid childid studentid ea ///
	 std_phys std_soc std_emot std_langcog std_comgen
	 
save "Older_Panel_Child_Outcomes_2009-2013_v2_forappend.dta", replace
	 
	 
