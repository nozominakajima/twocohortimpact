/*

This do file replicates the main analyses conducted in:

Hasan, A., Jung, H., Kinnell, A., Maika, A., Nakajima, N., & Pradhan, M. (2021). 
Contrasting Experiences: Understanding the Longer-Term Impact of Improving Access 
to Pre-Primary Education in Rural Indonesia. 
Journal of Research on Educational Effectiveness, 14(1), 28-56.

(https://www.tandfonline.com/doi/abs/10.1080/19345747.2020.1839989) 

*/


use "youngercohort.dta", clear

*-----------------------------------------------------------------------------*
* 0. Sample size
*-----------------------------------------------------------------------------*

* How many in our analysis sample (i.e., panel, no classmates)?

	count if usetobal < 9 & year == 1
	gen sample = 1 if usetobal < 9
	la var sample "Analytic sample"
	* N = 3,089
		
	
	gen sample_2016 = 1 if usetobal == 4| usetobal == 6| usetobal == 7| usetobal == 8
	la var sample_2016 "Analytic sample observed in 2016"
	* N = 2,894
	
	gen sample_2013 = 1 if usetobal == 3| usetobal == 5| usetobal == 7| usetobal == 8
	la var sample_2013 "Analytic sample observed in 2013"
	* N = 2,778
	
	
* Drop the classmates collected only in 2013
	drop if usetobalance == 9
	

* Set data as panel
	destring childid, replace
	tsset childid year

*-----------------------------------------------------------------------------*
* 1. Clean Covariates (same ones as JOLE paper)
*-----------------------------------------------------------------------------*
	
* Age of child
	summ child_age_yrs

	// impute if missing
	foreach x in 1 2 3 4 {
		summ child_age_yrs if (sample_2016 == 1 | sample_2013 == 1) & year == `x'
		replace child_age_yrs = r(mean) if missing(child_age_yrs) & (sample_2016 == 1 | sample_2013 == 1) & year == `x'
	}
	
	
* Household size
	summ hhsize

	// carryforward if missing
	bysort childid: carryforward hhsize, replace
	
	
* Wealth z-score
	egen hhtagyear = tag(hhid year)
	egen hhtag = tag(hhid)
	
	cap drop score5
	pca  own_radio own_tv own_refrigerator own_bicycle own_motorcycle own_car own_hp own_chicken own_pig own_goat own_cow good_roof good_wall good_floor own_toilet electricity kitchen own_lots_fl own_lots_hb if hhtagyear & year==1, components(5)
	predict score5
	summ score5,d

	cap drop score6
	pca  own_radio own_tv own_refrigerator own_bicycle own_motorcycle own_car own_hp own_chicken own_pig own_goat own_cow good_roof good_wall good_floor own_toilet electricity kitchen own_lots_fl own_lots_hb if hhtagyear & year==2, components(5)
	predict score6
	summ score6,d

	cap drop score7
	pca  own_radio own_tv own_refrigerator own_bicycle own_motorcycle own_car own_hp own_chicken own_pig own_goat own_cow good_roof good_wall good_floor own_toilet electricity kitchen own_lots_fl own_lots_hb if hhtagyear & year==3, components(5)
	predict score7
	summ score7,d

	cap drop score8
	pca  own_radio own_tv own_refrigerator own_bicycle own_motorcycle own_car own_hp own_chicken own_pig own_goat own_cow good_roof good_wall good_floor own_toilet electricity kitchen own_lots_fl own_lots_hb if hhtagyear & year==4, components(5)
	predict score8
	summ score8,d

	egen std_score5 = std(score5) if year==1
	egen std_score6 = std(score6) if year==2
	egen std_score7 = std(score7) if year==3
	egen std_score8 = std(score8) if year==4

	gen zwealth = std_score5 if year==1
	replace zwealth = std_score6 if year==2
	replace zwealth = std_score7 if year==3
	replace zwealth = std_score8 if year==4

	la var zwealth "Wealth Z-score"
	la var std_score5 "std wealth in 2009"
	la var std_score6 "std wealth in 2010"
	la var std_score7 "std wealth in 2013"
	la var std_score8 "std wealth in 2016"

	// impute if missing
	foreach x in 1 2 3 4 {
		summ zwealth if (sample_2016 == 1 | sample_2013 == 1) & year == `x'
		replace zwealth = r(mean) if missing(zwealth) & (sample_2016 == 1 | sample_2013 == 1) & year == `x'
	}
	
	gen poor = 0 if zwealth >= 0
	replace poor = 1 if zwealth < 0
	label var poor "zwealth<0 (1=Yes)"
	
	xtile w = zwealth, nquantiles(4)	
	bysort w: summ zwealth
	gen wealth = 0  if w == 4
	replace wealth = 1 if w == 1
	bysort wealth: summ zwealth

	
* Parenting scores Z-score
	
	egen parenting1 = std(parenting_TOTAL_GO) if year==1
	egen parenting2 = std(parenting_TOTAL_GO) if year==2
	egen parenting3 = std(parenting_TOTAL_GO) if year==3
	egen parenting4 = std(parenting_TOTAL_GO) if year==4

	gen parentingz = parenting1 if year==1
	replace parentingz = parenting2 if year==2
	replace parentingz = parenting3 if year==3
	replace parentingz = parenting4 if year==4
	drop parenting1 parenting2 parenting3 parenting4
	lab var parentingz "Parenting z-score"

	// impute if missing
	foreach x in 1 2 3 4 {
		summ parentingz if (sample_2016 == 1 | sample_2013 == 1) & year == `x'
		replace parentingz = r(mean) if missing(parentingz) & (sample_2016 == 1 | sample_2013 == 1) & year == `x'
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

	
* Child's gender
	replace gender = 0 if gender == 3
	lab var gender "Male"
	tab gender

	// carryforward if missing
	bysort childid: carryforward gender, replace
	

	summ child_age_yrs hhsize zwealth parentingz mothered gender if (sample_2016 == 1 | sample_2013 == 1) & year == 4

	
*-----------------------------------------------------------------------------*
* 2. Create baseline measures of child development 
*-----------------------------------------------------------------------------*
	
/* Child baseline measures

	foreach var of varlist bmi ctask_cog_1yr ctask_fm_1yr ctask_gm_1yr ctask_lang_1yr ctask_socemot_1yr {
		
		local l`v' : variable label `var'
		qui: summ `var' if year == 1
		
		histogram `var' if year == 1, freq ytitle(`l`v'') ///
		note(Measured in 2009. Sample size: `r(N)') ///
		name(`var', replace)
		
		}
	
	graph combine bmi ctask_cog_1yr ctask_fm_1yr ctask_gm_1yr ctask_lang_1yr ctask_socemot_1yr	
	graph export baseline_2009.pdf, replace 
	
	
	foreach var of varlist bmi ctask_cog_1yr ctask_fm_1yr ctask_gm_1yr ctask_lang_1yr ctask_socemot_1yr {
		
		local l`v' : variable label `var'
		qui: summ `var' if year == 2
		
		histogram `var' if year == 2, freq ytitle(`l`v'') ///
		note(Measured in 2010. Sample size: `r(N)') ///
		name(`var', replace)
		
		}
	
	graph combine bmi ctask_cog_1yr ctask_fm_1yr ctask_gm_1yr ctask_lang_1yr ctask_socemot_1yr	
	graph export baseline_2010.pdf, replace 
*/	
	
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
 
 
	// Baseline child tasks:
	
	foreach var in cog fm gm lang socemot{
		gen `var'_bl = .
		label var `var'_bl "Baseline `var' (2009 or 2010)"
	
	quietly forvalues i = 1/4 { 
		generate include = 1 if (year == 1 | year == 2)  
		egen work = min(ctask_`var'_1yr * include), by(childid) 
		replace `var'_bl = work if year == `i' 
		drop include work 
		
		} 
		}
	
	// Impute mean for missing 2009 & 2010 baseline measures
	
	summ bmi_bl cog_bl fm_bl gm_bl lang_bl socemot_bl if year == 4 & sample == 1

	foreach var in bmi_bl cog_bl fm_bl gm_bl lang_bl socemot_bl{
		summ `var' if sample == 1
		replace `var' = round(r(mean)) if missing(`var') & sample == 1
		}
	summ bmi_bl cog_bl fm_bl gm_bl lang_bl socemot_bl if year == 1 & sample == 1

/*	
	// Final baseline child measures
	
	foreach var of varlist bmi_bl cog_bl fm_bl gm_bl lang_bl socemot_bl {
		
		local l`v' : variable label `var'
		qui: summ `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1)
		
		histogram `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1), freq ytitle(`l`v'') ///
		note(Sample size: `r(N)') ///
		name(`var', replace)
		
		}
	
	graph combine bmi_bl cog_bl fm_bl gm_bl lang_bl socemot_bl	
	graph export baseline_final.pdf, replace 
*/	


	foreach var in cog_bl fm_bl gm_bl lang_bl {
		egen std_`var' = std(`var') if year == 1
	}
	
	la var std_cog_bl "Baseline cognitive skills (sd)"
	la var std_fm_bl "Baseline fine motor skills (sd)"
	la var std_gm_bl "Baseline gross motor skills (sd)"
	la var std_lang_bl "Baseline language skills (sd)"
	
*-----------------------------------------------------------------------------*
* 3. Create outcome measures of child development 
*-----------------------------------------------------------------------------*
	
* Months of enrollment

	summ num_month_sd_* num_month_tk_* num_month_ra_* ///
		 num_month_kbntpk_* num_month_kbtpk_* if sample_2016 ==1 & year == 4

	foreach x in 08 09 10 11 12 13 14 15 {
		
		summ num_month_sd_`x' if (sample_2016 == 1 | sample_2013 == 1)
		replace num_month_sd_`x' = round(r(mean)) if missing(num_month_sd_`x') & (sample_2016 == 1 | sample_2013 == 1)
		
		summ num_month_tk_`x' if (sample_2016 == 1 | sample_2013 == 1)
		replace num_month_tk_`x' = round(r(mean)) if missing(num_month_tk_`x') & (sample_2016 == 1 | sample_2013 == 1)

		summ num_month_ra_`x' if (sample_2016 == 1 | sample_2013 == 1)
		replace num_month_ra_`x' = round(r(mean)) if missing(num_month_ra_`x') & (sample_2016 == 1 | sample_2013 == 1)

		summ num_month_kbntpk_`x' if (sample_2016 == 1 | sample_2013 == 1)
		replace num_month_kbntpk_`x' = round(r(mean)) if missing(num_month_kbntpk_`x') & (sample_2016 == 1 | sample_2013 == 1)

		summ num_month_kbtpk_`x' if (sample_2016 == 1 | sample_2013 == 1)
		replace num_month_kbtpk_`x' = round(r(mean)) if missing(num_month_kbtpk_`x') & (sample_2016 == 1 | sample_2013 == 1)

		}
		
	summ num_month_sd_* num_month_tk_* num_month_ra_* ///
		 num_month_kbntpk_* num_month_kbtpk_* if sample_2016 ==1 & year == 4		
		 
	egen month_pr_13 = rowtotal(num_month_sd_08 num_month_sd_09 num_month_sd_10 ///
		num_month_sd_11 num_month_sd_12)  
		label var month_pr_13 "Total months in primary (2008-2013)"
		
	egen month_pr_16 = rowtotal(num_month_sd_08 num_month_sd_09 num_month_sd_10 ///
		num_month_sd_11 num_month_sd_12 num_month_sd_13 ///
		num_month_sd_14 num_month_sd_15)  
		label var month_pr_16 "Total months in primary (2008-2016)"
		
	egen month_kd_13 = rowtotal(num_month_tk_08 num_month_tk_09 num_month_tk_10 ///
		num_month_tk_11 num_month_tk_12 ///
		num_month_ra_08 num_month_ra_09 num_month_ra_10 ///
		num_month_ra_11 num_month_ra_12)
		label var month_kd_13 "Total months in kindergarten (2008-2013)"
		
	egen month_kd_16 = rowtotal(num_month_tk_08 num_month_tk_09 num_month_tk_10 ///
		num_month_tk_11 num_month_tk_12 num_month_tk_13 ///
		num_month_tk_14 num_month_tk_15 ///
		num_month_ra_08 num_month_ra_09 num_month_ra_10 ///
		num_month_ra_11 num_month_ra_12 num_month_ra_13 ///
		num_month_ra_14 num_month_ra_15)
		label var month_kd_16 "Total months in kindergarten (2008-2016)"
	
	egen month_kbntpk_13 = rowtotal(num_month_kbntpk_08 num_month_kbntpk_09 num_month_kbntpk_10 ///
		num_month_kbntpk_11 num_month_kbntpk_12)
		label var month_kbntpk_13 "Total months in non-project playgroup (2008-2013)"
		
	egen month_kbntpk_16 = rowtotal(num_month_kbntpk_08 num_month_kbntpk_09 num_month_kbntpk_10 ///
		num_month_kbntpk_11 num_month_kbntpk_12 num_month_kbntpk_13 ///
		num_month_kbntpk_14 num_month_kbntpk_15)
		label var month_kbntpk_16 "Total months in non-project playgroup (2008-2016)"
	
	egen month_kbtpk_13 = rowtotal(num_month_kbtpk_08 num_month_kbtpk_09 num_month_kbtpk_10 ///
		num_month_kbtpk_11 num_month_kbtpk_12)
		label var month_kbtpk_13 "Total months in project playgroup (2008-2013)"
		
	egen month_kbtpk_16 = rowtotal(num_month_kbtpk_08 num_month_kbtpk_09 num_month_kbtpk_10 ///
		num_month_kbtpk_11 num_month_kbtpk_12 num_month_kbtpk_13 ///
		num_month_kbtpk_14 num_month_kbtpk_15)
		label var month_kbtpk_16 "Total months in project playgroup (2008-2016)"
	
	summ month_pr_* month_kd_* month_kbntpk_* month_kbtpk_* if (sample_2016 == 1 | sample_2013 == 1) & year == 4
	
	
	
* Ever enrolled

	gen ever_kd_13 = 0 if month_kd_13 == 0
	  replace ever_kd_13 = 1 if month_kd_13 > 0 & month_kd_13 < .
	  label var ever_kd_13 "Ever enrolled in kindergarten (2008-2013)"
	
	gen ever_kd_16 = 0 if month_kd_16 == 0
	  replace ever_kd_16 = 1 if month_kd_16 > 0 & month_kd_16 < .
	  label var ever_kd_16 "Ever enrolled in kindergarten (2008-2016)"
	
	gen ever_kbntpk_13 = 0 if month_kbntpk_13 == 0
	  replace ever_kbntpk_13 = 1 if month_kbntpk_13 > 0 & month_kbntpk_13 < .
	  label var ever_kbntpk_13 "Ever enrolled in non-project playgroup (2008-2013)"
	
	gen ever_kbntpk_16 = 0 if month_kbntpk_16 == 0
	  replace ever_kbntpk_16 = 1 if month_kbntpk_16 > 0 & month_kbntpk_16 < .
	  label var ever_kbntpk_16 "Ever enrolled in non-project playgroup (2008-2016)"
	
	gen ever_kbtpk_13 = 0 if month_kbtpk_13 == 0
	  replace ever_kbtpk_13 = 1 if month_kbtpk_13 > 0 & month_kbtpk_13 < .
	  label var ever_kbtpk_13 "Ever enrolled in project playgroup (2008-2013)"
	
	gen ever_kbtpk_16 = 0 if month_kbtpk_16 == 0
	  replace ever_kbtpk_16 = 1 if month_kbtpk_16 > 0 & month_kbtpk_16 < .
	  label var ever_kbtpk_16 "Ever enrolled in project playgroup (2008-2016)"


	
	
* EDI

	bysort year: summ short_phys short_soc short_emot short_langcog ///
				 short_comgen short_nlowtot short_nlow short_nlow2 ///
				 long_phys_LCG long_soc_LCG long_emot_LCG long_langcog_LCG long_comgen_LCG ///
				 long_nlow_LCG long_nlow2_LCG ///
				 if (sample_2016 == 1 | sample_2013 == 1) 
		
	gen std_phys = .
	gen std_soc = . 
	gen std_emot = . 
	gen std_langcog = . 
	gen std_comgen = .	
	
	gen stdl_phys = .
	gen stdl_soc = . 
	gen stdl_emot = . 
	gen stdl_langcog = . 
	gen stdl_comgen = .	
		
	foreach var in phys soc emot langcog comgen {
		egen std_`var'_3 = std(short_`var') if year == 3
		egen std_`var'_4 = std(short_`var') if year == 4
		
		replace std_`var' = std_`var'_3 if year == 3
		replace std_`var' = std_`var'_4 if year == 4
		
		
		egen stdl_`var'_3 = std(long_`var'_LCG) if year == 3
		egen stdl_`var'_4 = std(long_`var'_LCG) if year == 4
		
		replace stdl_`var' = stdl_`var'_3 if year == 3
		replace stdl_`var' = stdl_`var'_4 if year == 4
	}
	
	la var std_phys "EDI Physical (sd)"
	la var std_soc "EDI Social (sd)"
	la var std_emot "EDI Emotional (sd)"
	la var std_langcog "EDI Lang & Cog (sd)"
	la var std_comgen "EDI Comm & General (sd)"
	
	la var stdl_phys "Long EDI Physical (sd)"
	la var stdl_soc "Long EDI Social (sd)"
	la var stdl_emot "Long EDI Emotional (sd)"
	la var stdl_langcog "Long EDI Lang & Cog (sd)"
	la var stdl_comgen "Long EDI Comm & General (sd)"

	
	
* Test scores (see "testcheck.do" for why we decided on these)

	gen common_1 = .
	replace common_1 = score_11 + score_12 + score_14 if cov11 == 0
	replace common_1 = score_1 + score_4 + score_5 if cov11 == 1
	la var common_1 "Language - Match Picture"
	
	gen common_2 = .
	replace common_2 = score_exact_17 + score_exact_18 + score_exact_19 + score_exact_20 if cov11 == 0
	replace common_2 = score_exact_7 + score_exact_8 + score_exact_9 + score_exact_10 if cov11 == 1
	la var common_2 "Language - Mention Objects"
	
	gen common_3 = .
	replace common_3 = score_24 + score_25 + score_26 + score_27 + score_28 + score_29 + score_30 + score_31 if cov11 == 0
	replace common_3 = score_22 + score_23 + score_24 + score_25 + score_26 + score_27 + score_28 + score_29 if cov11 == 1
	la var common_3 "Math - Summation"
	
	gen common_4 = .
	replace common_4 = score_32 + score_33 + score_34 if cov11 == 0
	replace common_4 = score_30 + score_31 + score_32 if cov11 == 1
	la var common_4 "Math - Order number"
	
	gen common_5 =.
	replace common_5 = score_39 + score_40 + score_41 + score_42 + score_43 + score_44 + score_45 ///
						+ score_46 + score_47 + score_48 + score_49 + score_50 + score_51 + score_52 if cov11 == 0
	replace common_5 = score_47 + score_48 + score_49 + score_50 + score_51 + score_52 + score_53 ///
						+ score_54 + score_55 + score_56 + score_57 + score_58 + score_59 + score_60 if cov11 == 1
	la var common_5 "Abstract reasoning"


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

	 gr matrix bmi_bl cog_bl fm_bl gm_bl lang_bl socemot_bl, half ms(p) name(corr, replace)
	 graph export corr.pdf, replace
	 
	 corr bmi_bl cog_bl fm_bl gm_bl lang_bl socemot_bl
*/

*-----------------------------------------------------------------------------*
* 5. Baseline table
*-----------------------------------------------------------------------------*

* Create batch dummies	
 
	tab batchactual, gen(batchd) 
	rename batchd3 batchd5
	rename batchd2 batchd3
	la var batchd1 "Village is Batch 1 (1 = Yes)"
	la var batchd3 "Village is Batch 3 (1 = Yes)"
	la var batchd5 "Village is Batch 5 (1 = Yes)"
	
* Create batch 1 & 3 v. 5

	gen treat = .
	replace treat = 1 if batcha == 1 | batcha == 3
	replace treat = 0 if batcha == 5
	la var treat "Treatment village (1 = Yes)"
	
	
* Baseline variables	

	// By batch
	
	estimates drop _all 

	foreach var in child_age_yrs hhsize zwealth parentingz mothered gender ///
				   bmi_bl std_cog_bl std_fm_bl std_gm_bl std_lang_bl {
				   
		estpost tabstat `var' if sample == 1 & year == 1, by(batcha) statistics(mean sd) columns(statistics) listwise
		est store `var'
		
		}
		
	esttab child_age_yrs hhsize zwealth parentingz mothered gender ///
		bmi_bl std_cog_bl std_fm_bl std_gm_bl std_lang_bl ///
		using "$output/table1_2009.csv", replace main(mean) aux(sd) wide
		
		
	// Batch 1 vs. 3
	
	mat drop _all
	
	foreach var in child_age_yrs hhsize zwealth parentingz mothered gender ///
				   bmi_bl std_cog_bl std_fm_bl std_gm_bl std_lang_bl {
		
		reg `var' i.batcha if sample == 1 & year == 1, cluster(ea)
		est store `var'
		
		}

	xml_tab child_age_yrs hhsize zwealth parentingz mothered gender ///
		bmi_bl std_cog_bl std_fm_bl std_gm_bl std_lang_bl, ///
		save("$output/table1diff_2009.xls") replace sheet(diff) drop (_cons) below star(0.05 0.01 0.001)
	
	
	// By treatment status
	
	estimates drop _all 

	foreach var in child_age_yrs hhsize zwealth parentingz mothered gender ///
				   bmi_bl std_cog_bl std_fm_bl std_gm_bl std_lang_bl {
				   
		estpost tabstat `var' if sample == 1 & year == 1, by(treat) statistics(mean sd) columns(statistics) listwise
		est store `var'
		
		}
		
	esttab child_age_yrs hhsize zwealth parentingz mothered gender ///
		bmi_bl std_cog_bl std_fm_bl std_gm_bl std_lang_bl ///
		using "$output/table1_2009final.csv", replace main(mean) aux(sd) wide

		
	// Treatment v. Comparison
	
	mat drop _all
	
	foreach var in child_age_yrs hhsize zwealth parentingz mothered gender ///
				   bmi_bl std_cog_bl std_fm_bl std_gm_bl std_lang_bl {
		
		reg `var' i.treat if sample == 1 & year == 1, cluster(ea)
		est store `var'
		
		}

	xml_tab child_age_yrs hhsize zwealth parentingz mothered gender ///
		bmi_bl std_cog_bl std_fm_bl std_gm_bl std_lang_bl, ///
		save("$output/table1diff_2009final.xls") replace sheet(diff) drop (_cons) below star(0.05 0.01 0.001)
		
		
	// Number of observations at baseline
	
	tab sample batcha if year == 1
	
*-----------------------------------------------------------------------------*
* 6. Regressions
*-----------------------------------------------------------------------------*

* Using the following specifications:
	 * i.	Batch dummies only
	 *ii.	Batch dummies + baseline outcomes
     *iii.	Batch dummies + baseline outcomes + controls

	 
	global base "bmi_bl cog_bl fm_bl gm_bl lang_bl"
	global controls "child_age_yrs hhsize zwealth parentingz mothered gender"
	global controlswealth "child_age_yrs hhsize parentingz mothered gender"
	global controlsparent "child_age_yrs hhsize zwealth mothered gender"
	
* 2013 Estimation 
	
	// Batch 1 v. 3 v. 5
	estimates clear
		
	global condyr3 "batchactual>=1 & batchactual<=5 & year==3 & sample_2013 == 1"

	foreach varname in std_phys std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
					 stdl_phys stdl_soc stdl_emot stdl_langcog stdl_comgen ///	
					 ever_kbtpk_13 ever_kbntpk_13 ever_kd_13 ///
					 month_kbtpk_13 month_kbntpk_13 month_kd_13 month_pr_13 {

	reg `varname' batchd1 batchd3 if $condyr3, robust cluster(ea)
	estimates store `varname'13_batch

	reg `varname' batchd1 batchd3 $base if $condyr3, robust cluster(ea)
	estimates store `varname'13_base
	
	reg `varname' batchd1 batchd3 $base $controls if $condyr3, robust cluster(ea)
	estimates store `varname'13_all

	}

	xml_tab std_phys13_* std_soc13_* std_emot13_* std_langcog13_* std_comgen13_* ///
		short_nlowtot13_* short_nlow13_* short_nlow213_* ///
		stdl_phys13_* stdl_soc13_* stdl_emot13_* stdl_langcog13_* stdl_comgen13_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS13.xls") replace stats(r2_a N) below title("2013 EDI") 
		
	xml_tab ever_kbtpk_1313_* ever_kbntpk_1313_* ever_kd_1313_* ///
	    month_kbtpk_1313_* month_kbntpk_1313_* month_kd_1313_* month_pr_1313_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS13.xls") append stats(r2_a N) below title("2013 Enrollment")

		

	// Batch 1 & 3 v. 5
	estimates clear
		
	global condyr3 "batchactual>=1 & batchactual<=5 & year==3 & sample_2013 == 1"

	foreach varname in std_phys std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
					stdl_phys stdl_soc stdl_emot stdl_langcog stdl_comgen ///	
					ever_kbtpk_13 ever_kbntpk_13 ever_kd_13 ///
					month_kbtpk_13 month_kbntpk_13 month_kd_13 month_pr_13{

	reg `varname' treat if $condyr3, robust cluster(ea)
	estimates store `varname'13_batch

	reg `varname' treat $base if $condyr3, robust cluster(ea)
	estimates store `varname'13_base
	
	reg `varname' treat $base $controls if $condyr3, robust cluster(ea)
	estimates store `varname'13_all

	}

	xml_tab std_phys13_* std_soc13_* std_emot13_* std_langcog13_* std_comgen13_* ///
		short_nlowtot13_* short_nlow13_* short_nlow213_* ///
		stdl_phys13_* stdl_soc13_* stdl_emot13_* stdl_langcog13_* stdl_comgen13_* , ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS13_final.xls") replace stats(r2_a N) below title("2013 EDI") 
		
	xml_tab ever_kbtpk_1313_* ever_kbntpk_1313_* ever_kd_1313_* ///
	    month_kbtpk_1313_* month_kbntpk_1313_* month_kd_1313_* month_pr_1313_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS13_final.xls") append stats(r2_a N) below title("2013 Enrollment")

		
		
		
* 2016 estimation 
	
	// Batch 1 v. 3 v. 5
	estimates clear
	global condyr4 "batchactual>=1 & batchactual<=5 & year==4 & sample_2016 == 1"
	
	foreach varname in std_phys std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
				    stdl_phys stdl_soc stdl_emot stdl_langcog stdl_comgen ///
					ever_kbtpk_16 ever_kbntpk_16 ever_kd_16 ///
					month_kbtpk_16 month_kbntpk_16 month_kd_16 month_pr_16 ///
					std_newlangscore std_newmathscore std_newraven std_langmp std_langmo std_mathsu std_mathon{

	reg `varname' batchd1 batchd3 if $condyr4, robust cluster(ea)
	estimates store `varname'16_batch

	reg `varname' batchd1 batchd3 $base cov11 if $condyr4, robust cluster(ea)
	estimates store `varname'16_base
	
	reg `varname' batchd1 batchd3 $base $controls cov11 if $condyr4, robust cluster(ea)
	estimates store `varname'16_all

	}

	xml_tab std_phys16_* std_soc16_* std_emot16_* std_langcog16_* std_comgen16_* ///
		short_nlowtot16_* short_nlow16_* short_nlow216_* ///
		stdl_phys16_* stdl_soc16_* stdl_emot16_* stdl_langcog16_* stdl_comgen16_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS16.xls") replace stats(r2_a N) below title("2016 EDI")
		
	xml_tab ever_kbtpk_1616_* ever_kbntpk_1616_* ever_kd_1616_* ///
	    month_kbtpk_1616_* month_kbntpk_1616_* month_kd_1616_* month_pr_1616_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS16.xls") append stats(r2_a N) below title("2016 Enrollment")

	xml_tab std_newlangscore16_* std_newmathscore16_* std_newraven16_* std_langmp16_* std_langmo16_* std_mathsu16_* std_mathon16_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS16.xls") append stats(r2_a N) below title("2016 Test")
	
	
	// Batch 1 & 3 v. 5
	estimates clear
	global condyr4 "batchactual>=1 & batchactual<=5 & year==4 & sample_2016 == 1"
	
	foreach varname in std_phys std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
				    stdl_phys stdl_soc stdl_emot stdl_langcog stdl_comgen ///
					ever_kbtpk_16 ever_kbntpk_16 ever_kd_16 ///
					month_kbtpk_16 month_kbntpk_16 month_kd_16 month_pr_16 ///
					std_newlangscore std_newmathscore std_newraven std_langmp std_langmo std_mathsu std_mathon{

	reg `varname' treat if $condyr4, robust cluster(ea)
	estimates store `varname'16_batch

	reg `varname' treat $base cov11 if $condyr4, robust cluster(ea)
	estimates store `varname'16_base
	
	reg `varname' treat $base $controls cov11 if $condyr4, robust cluster(ea)
	estimates store `varname'16_all

	}


	xml_tab std_phys16_* std_soc16_* std_emot16_* std_langcog16_* std_comgen16_* ///
	    stdl_phys16_* stdl_soc16_* stdl_emot16_* stdl_langcog16_* stdl_comgen16_* ///
		short_nlowtot16_* short_nlow16_* short_nlow216_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS16_final.xls") replace stats(r2_a N) below title("2016 EDI")
		
	xml_tab ever_kbtpk_1616_* ever_kbntpk_1616_* ever_kd_1616_* ///
	month_kbtpk_1616_* month_kbntpk_1616_* month_kd_1616_* month_pr_1616_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS16_final.xls") append stats(r2_a N) below title("2016 Enrollment")

	xml_tab std_newlangscore16_* std_newmathscore16_* std_newraven16_* std_langmp16_* std_langmo16_* std_mathsu16_* std_mathon16_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/youngerRHS16_final.xls") append stats(r2_a N) below title("2016 Test")
	
	


*-----------------------------------------------------------------------------*
* 7. Check if there is evidence of ceiling effects in EDI & test scores
*-----------------------------------------------------------------------------*
lab var short_phys "EDI Physical"
lab var short_soc "EDI Social"
lab var short_emot "EDI Emotional"
lab var short_langcog "EDI Language"
lab var short_comgen "EDI Communication"

** Histograms **

	foreach var of varlist short_phys short_soc short_emot short_langcog short_comgen{
		
		local l`var' : variable label `var'
		summ `var' if year == 3 & sample_2013 == 1
		global `var'_mean: di %6.3f r(mean)
		
		histogram `var' if year == 3 & sample_2013 == 1, ///
		color(ltblue%70) ///
		freq xtitle(`l`var'') ///
		xline(`r(mean)', lcolor(black) lpattern(dash) lwidth(vthin)) ///
		note(Sample size = `r(N)'. Mean = ${`var'_mean}) ///
		name(`var', replace)
		}

	graph combine short_phys short_soc short_emot short_langcog short_comgen 	
	graph export rawEDI_2013.pdf, replace 

	
	foreach var of varlist short_phys short_soc short_emot short_langcog short_comgen{
		
		local l`v' : variable label `var'
		qui: summ `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1)
		global `var'_mean: di %6.3f r(mean)
		
		histogram `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1), ///
		color(ltblue%70) ///
		freq xtitle(`l`var'') ///
		xline(`r(mean)', lcolor(black) lpattern(dash) lwidth(vthin)) ///
		note(Sample size = `r(N)'. Mean = ${`var'_mean}) ///
		name(`var', replace)
		}
	
	graph combine short_phys short_soc short_emot short_langcog short_comgen 	
	graph export rawEDI_2016.pdf, replace 
	
	
	foreach var of varlist common_1 common_2 common_3 common_4 common_5 {
		
		local l`v' : variable label `var'
		qui: summ `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1)
		global `var'_mean: di %6.3f r(mean)
		
		histogram `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1), ///
		color(ltblue%70) ///
		freq xtitle(`l`var'') ///
		xline(`r(mean)', lcolor(black) lpattern(dash) lwidth(vthin)) ///
		note(Sample size = `r(N)'. Mean = ${`var'_mean}) ///
		name(`var', replace)
		}
	
	graph combine common_1 common_2 common_3 common_4 common_5 	
	graph export rawtest_2016.pdf, replace 
	
	
** Density of variables **

	foreach var of varlist short_phys short_soc short_emot short_langcog short_comgen {
		
		local l`var' : variable label `var'
		summ `var' if year == 3 & sample_2013 == 1
		global `var'_mean: di %6.3f r(mean)
		
		twoway kdensity `var' if year == 3 & sample_2013 == 1, bwidth(1.5) ///
		xtitle(`l`var'') ytitle(Density) ///
		xline(`r(mean)', lcolor(black) lpattern(dash) lwidth(vthin)) ///
		note(Sample size = `r(N)'. Mean = ${`var'_mean}) ///
		name(`var', replace)
		}

	graph combine short_phys short_soc short_emot short_langcog short_comgen
	graph export kdensityEDI_2013.png, replace 

	
	foreach var of varlist short_phys short_soc short_emot short_langcog short_comgen {
		
		local l`var' : variable label `var'
		qui: summ `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1)
		global `var'_mean: di %6.3f r(mean)
		
		twoway kdensity `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1), bwidth(1.5) ///
		xtitle(`l`var'') ytitle(Density) ///
		xline(`r(mean)', lcolor(black) lpattern(dash) lwidth(vthin)) ///
		note(Sample size = `r(N)'. Mean = ${`var'_mean}) ///
		name(`var', replace)
		}
	
	graph combine short_phys short_soc short_emot short_langcog short_comgen 	
	graph export kdensityEDI_2016.png, replace 	

	
	foreach var of varlist common_1 common_2 common_3 common_4 common_5 {
		
		local l`var' : variable label `var'
		qui: summ `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1)
		global `var'_mean: di %6.3f r(mean)
		
		twoway kdensity `var' if year == 4 & (sample_2016 == 1 | sample_2013 == 1), bwidth(1.5) ///
		xtitle(`l`var'') ytitle(Density) ///
		xline(`r(mean)', lcolor(black) lpattern(dash) lwidth(vthin)) ///
		note(Sample size = `r(N)'. Mean = ${`var'_mean}) ///
		name(`var', replace)
		}
	
	graph combine common_1 common_2 common_3 common_4 common_5 	
	graph export kdensitytest_2016.png, replace 	
	
*-----------------------------------------------------------------------------*
* 8. Final regression tables
*-----------------------------------------------------------------------------*	

* 2013 - 5 years-old

	// EDI & Enrollment
	
	estimates clear
		
	global condyr3 "batchactual>=1 & batchactual<=5 & year==3 & sample_2013 == 1"

	
	reg std_phys treat $base $controls if $condyr3, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, replace bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlswealth if $condyr3 & poor == 1, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlswealth if $condyr3 & poor == 0, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlsparent if $condyr3 & lowpar == 1, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlsparent if $condyr3 & lowpar == 0, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	
	foreach varname in std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
					stdl_phys stdl_soc stdl_emot stdl_langcog stdl_comgen ///	
					ever_kbtpk_13 ever_kbntpk_13 ever_kd_13 ///
					month_kbtpk_13 month_kbntpk_13 month_kd_13 {

	reg `varname' treat $base $controls if $condyr3, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlswealth if $condyr3 & poor == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	reg `varname' treat $base $controlswealth if $condyr3 & poor == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlsparent if $condyr3 & lowpar == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlsparent if $condyr3 & lowpar == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	}
		
* 2016 - 8 years-old
	
	// EDI & Enrollment & Test
	
	estimates clear
	global condyr4 "batchactual>=1 & batchactual<=5 & year==4 & sample_2016 == 1"
	
	reg std_phys treat $base $controls if $condyr4, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, replace bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlswealth if $condyr4 & poor == 1, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlswealth if $condyr4 & poor == 0, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlsparent if $condyr4 & lowpar == 1, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlsparent if $condyr4 & lowpar == 0, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	
	foreach varname in std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
					stdl_phys stdl_soc stdl_emot stdl_langcog stdl_comgen ///	
					ever_kbtpk_16 ever_kbntpk_16 ever_kd_16 ///
					month_kbtpk_16 month_kbntpk_16 month_kd_16 month_pr_16 ///
					std_langmp std_langmo std_mathsu std_mathon std_newraven{
					
	reg `varname' treat $base $controls if $condyr4, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlswealth if $condyr4 & poor == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	reg `varname' treat $base $controlswealth if $condyr4 & poor == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlsparent if $condyr4 & lowpar == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlsparent if $condyr4 & lowpar == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_final_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	}
		
*-----------------------------------------------------------------------------*
* 9. Appendix - By batches (asked by SIEF)
*-----------------------------------------------------------------------------*	

* 2013 - 5 years-old

	// EDI & Enrollment
	
	estimates clear
		
	global condyr3 "batchactual>=1 & batchactual<=5 & year==3 & sample_2013 == 1"

	foreach varname in std_phys std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
					ever_kbtpk_13 ever_kbntpk_13 ever_kd_13 ///
					month_kbtpk_13 month_kbntpk_13 month_kd_13{

	reg `varname' batchd1 batchd3 $base $controls if $condyr3, robust cluster(ea)
	estimates store `varname'13_all
	
	reg `varname' batchd1 batchd3 $base $controlswealth if $condyr3 & poor == 1, robust cluster(ea)
	estimates store `varname'13_poor
	
	reg `varname' batchd1 batchd3 $base $controlswealth if $condyr3 & poor == 0, robust cluster(ea)
	estimates store `varname'13_nonpoor
	
	reg `varname' batchd1 batchd3 $base $controlsparent if $condyr3 & lowpar == 1, robust cluster(ea)
	estimates store `varname'13_lowp
	
	reg `varname' batchd1 batchd3 $base $controlsparent if $condyr3 & lowpar == 0, robust cluster(ea)
	estimates store `varname'13_highp

	}

	xml_tab std_phys13_* std_soc13_* std_emot13_* std_langcog13_* std_comgen13_* ///
		short_nlowtot13_* short_nlow13_* short_nlow213_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/younger_appendix.xls") replace stats(r2_a N) below title("2013 EDI") 
		
	xml_tab ever_kbtpk_1313_* ever_kbntpk_1313_* ever_kd_1313_* ///
	    month_kbtpk_1313_* month_kbntpk_1313_* month_kd_1313_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/younger_appendix.xls") append stats(r2_a N) below title("2013 Enrollment")
		
* 2016 - 8 years-old
	
	// EDI & Enrollment & Test
	
	estimates clear
	global condyr4 "batchactual>=1 & batchactual<=5 & year==4 & sample_2016 == 1"
	
	foreach varname in std_phys std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
					ever_kbtpk_16 ever_kbntpk_16 ever_kd_16 ///
					month_kbtpk_16 month_kbntpk_16 month_kd_16 ///
					std_langmp std_langmo std_mathsu std_mathon std_newraven{

	reg `varname' batchd1 batchd3 $base $controls cov11 if $condyr4, robust cluster(ea)
	estimates store `varname'16_all

	reg `varname' batchd1 batchd3 $base $controlswealth cov11 if $condyr4 & poor == 1, robust cluster(ea)
	estimates store `varname'16_poor
	
	reg `varname' batchd1 batchd3 $base $controlswealth cov11 if $condyr4 & poor == 0, robust cluster(ea)
	estimates store `varname'16_nonpoor
	
	reg `varname' batchd1 batchd3 $base $controlsparent cov11 if $condyr4 & lowpar == 1, robust cluster(ea)
	estimates store `varname'16_lowp
	
	reg `varname' batchd1 batchd3 $base $controlsparent cov11 if $condyr4 & lowpar == 0, robust cluster(ea)
	estimates store `varname'16_highp
	}


	xml_tab std_phys16_* std_soc16_* std_emot16_* std_langcog16_* std_comgen16_* ///
		short_nlowtot16_* short_nlow16_* short_nlow216_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/younger_appendix.xls") append stats(r2_a N) below title("2016 EDI")
		
	xml_tab ever_kbtpk_1616_* ever_kbntpk_1616_* ever_kd_1616_* ///
	    month_kbtpk_1616_* month_kbntpk_1616_* month_kd_1616_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/younger_appendix.xls") append stats(r2_a N) below title("2016 Enrollment")

	xml_tab std_langmp16_* std_langmo16_* std_mathsu16_* std_mathon16_* std_newraven16_*, ///
		star(0.05 0.01 0.001) ///
		save("$output/younger_appendix.xls") append stats(r2_a N) below title("2016 Test")

*-------------------------------------------------------------------------------------------------------*
* 10. Appendix - Interactions by poor, by mother's education (asked by SIEF) and by poor quantile (R&R)
*-------------------------------------------------------------------------------------------------------*	

* 2013 - 5 years-old

	// EDI & Enrollment
	
	estimates clear
		
	global condyr3 "batchactual>=1 & batchactual<=5 & year==3 & sample_2013 == 1"

	
	reg std_phys treat $base $controls if $condyr3, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, replace bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys i.treat##i.poor $base $controlswealth if $condyr3, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys i.treat##i.lowpar $base $controlsparent if $condyr3, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
		
	reg std_phys i.treat##i.wealth $base $controlsparent if $condyr3, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
		

	foreach varname in std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
					ever_kbtpk_13 ever_kbntpk_13 ever_kd_13  ///
					month_kbtpk_13 month_kbntpk_13 month_kd_13 {

	reg `varname' treat $base $controls if $condyr3, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' i.treat##i.poor $base $controlswealth if $condyr3, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' i.treat##i.lowpar $base $controlsparent if $condyr3, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
		
	reg `varname' i.treat##i.wealth $base $controlsparent if $condyr3, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
		
	}
		
* 2016 - 8 years-old
	
	// EDI & Enrollment & Test
	
	estimates clear
	global condyr4 "batchactual>=1 & batchactual<=5 & year==4 & sample_2016 == 1"
	
	reg std_phys treat $base $controls if $condyr4, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys i.treat##i.poor $base $controlswealth if $condyr4, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
		
	reg std_phys i.treat##i.lowpar $base $controlsparent if $condyr4, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys i.treat##i.wealth $base $controlsparent if $condyr4, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	foreach varname in std_soc std_emot std_langcog std_comgen short_nlowtot short_nlow short_nlow2 ///
					ever_kbtpk_16 ever_kbntpk_16 ever_kd_16 ///
					month_kbtpk_16 month_kbntpk_16 month_kd_16 ///
					std_langmp std_langmo std_mathsu std_mathon std_newraven{
					
	reg `varname' treat $base $controls if $condyr4, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' i.treat##i.poor $base $controlswealth if $condyr4, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
		
	reg `varname' i.treat##i.lowpar $base $controlsparent if $condyr4, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' i.treat##i.wealth $base $controlsparent if $condyr4, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix2.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	}

		
*-----------------------------------------------------------------------------*
* 11. Attrition
*-----------------------------------------------------------------------------*		

	gen attrition16 = .
	replace attrition16 = 0 if sample == 1
	replace attrition16 = 1 if sample == 1 & sample_2016 != 1
	bysort batcha: tab attrition16 if year == 4
	la var attrition16 "Attrited in 2016"

	gen attrition13 = .
	replace attrition13 = 0 if sample == 1
	replace attrition13 = 1 if sample == 1 & sample_2013 != 1
	bysort batcha: tab attrition13 if year == 3
	la var attrition13 "Attrited in 2013"
		
	reg attrition16 batchd1 batchd3 $base $controls if sample == 1 & year == 1, robust cluster(ea)
	outreg2 using attrition.xls, replace bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) 
	
	reg attrition16 treat $base $controls if sample == 1 & year == 1, robust cluster(ea)
	outreg2 using attrition.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05)
	
	reg attrition16 treat $base $controls 1.treat#c.bmi_bl 1.treat#c.cog_bl 1.treat#c.fm_bl 1.treat#c.gm_bl 1.treat#c.lang_bl 1.treat#c.child_age_yrs 1.treat#c.hhsize 1.treat#c.zwealth 1.treat#c.parentingz 1.treat#1.mothered 1.treat#1.gender if sample == 1 & year == 1, robust cluster(ea)
	outreg2 using attrition.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05)

	
*-----------------------------------------------------------------------------*
* 12. Stunting outcomes
*-----------------------------------------------------------------------------*	

	tab stunted_cat 
	
	gen stuntingany = 1 if stunted_cat > 1
	replace stuntingany = 0 if stunted_cat == 1
	la var stuntingany "Stunted (moderate/severe)"
	
	gen stuntingmod = 1 if stunted_cat == 2
	replace stuntingmod = 0 if stunted_cat == 1 | stunted_cat == 3
	la var stuntingmod "Stunted (moderate)"
	
	gen stuntingsev = 1 if stunted_cat == 3
	replace stuntingsev  = 0 if stunted_cat == 1 | stunted_cat == 2
	la var stuntingsev "Stunted (severe)"
	
	

	* 2013 - 5 years-old
	estimates clear	
	global condyr3 "batchactual>=1 & batchactual<=5 & year==3 & sample_2013 == 1"

	reg stuntingany treat $base $controls if $condyr3, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, replace bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg stuntingany treat $base $controlswealth if $condyr3 & poor == 1, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg stuntingany treat $base $controlswealth if $condyr3 & poor == 0, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg stuntingany treat $base $controlsparent if $condyr3 & lowpar == 1, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg stuntingany treat $base $controlsparent if $condyr3 & lowpar == 0, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	
	foreach varname in stuntingmod stuntingsev {

	reg `varname' treat $base $controls if $condyr3, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlswealth if $condyr3 & poor == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	reg `varname' treat $base $controlswealth if $condyr3 & poor == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlsparent if $condyr3 & lowpar == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlsparent if $condyr3 & lowpar == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	}
		
	* 2016 - 8 years-old
	estimates clear
	global condyr4 "batchactual>=1 & batchactual<=5 & year==4 & sample_2016 == 1"
	
	reg stuntingany treat $base $controls if $condyr4, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, replace bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlswealth if $condyr4 & poor == 1, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlswealth if $condyr4 & poor == 0, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlsparent if $condyr4 & lowpar == 1, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg std_phys treat $base $controlsparent if $condyr4 & lowpar == 0, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	
	foreach varname in stuntingmod stuntingsev {
					
	reg `varname' treat $base $controls if $condyr4, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlswealth if $condyr4 & poor == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

	reg `varname' treat $base $controlswealth if $condyr4 & poor == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlsparent if $condyr4 & lowpar == 1, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	
	reg `varname' treat $base $controlsparent if $condyr4 & lowpar == 0, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_stunting_4.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	}
	
*-----------------------------------------------------------------------------*
* 13. Mother education and wealth profiles by enrollment
*-----------------------------------------------------------------------------*		
	
	summ month_pr_16 month_kd_16 month_kbntpk_16 month_kbtpk_16 if sample_2016 == 1 & year == 4
	
	// no eced
	summ mothered zwealth if sample_2016 == 1 & year == 4 & month_kbtpk_16 == 0 & month_kd_16 == 0 & month_kbntpk_16 == 0
	
	// project playgroup only
	summ mothered zwealth if sample_2016 == 1 & year == 4 & month_kbtpk_16 > 0 & month_kd_16 < 1 & month_kbntpk_16 < 1
	
	// non-project playgroup only
	summ mothered zwealth if sample_2016 == 1 & year == 4 & month_kbntpk_16 > 0 & month_kd_16 < 1 & month_kbtpk_16 < 1
	
	// kindergarten only
	summ mothered zwealth if sample_2016 == 1 & year == 4 & month_kd_16 > 0 & month_kbtpk_16 < 1 & month_kbntpk_16 < 1  
	
	// project playgroup & other
	summ mothered zwealth if sample_2016 == 1 & year == 4 & month_kbtpk_16 > 0 & (month_kd_16 > 0 | month_kbntpk_16 > 0)
	
	// non-project playgroup & other
	summ mothered zwealth if sample_2016 == 1 & year == 4 & month_kbntpk_16 > 0 & (month_kd_16 > 0 | month_kbtpk_16 > 0)

*-------------------------------------------------------------------------------------------------------*
* 14. Variation by exposure to playgroup
*-------------------------------------------------------------------------------------------------------*	

* generate "appropriate" exposure to playgroup
summ child_age_yrs if year==3 & sample_2013 == 1
summ child_age_yrs if year==4 & sample_2016 == 1

gen x = (num_month_kbtpk_08 + num_month_kbtpk_09) if year==3 & sample_2013 == 1 & child_age_yrs < .	
replace x = (num_month_kbtpk_09 + num_month_kbtpk_10) if year==3 & sample_2013 == 1 & child_age_yrs < 8	
replace x = (num_month_kbtpk_10 + num_month_kbtpk_11) if year==3 & sample_2013 == 1 & child_age_yrs < 7
replace x = (num_month_kbtpk_11 + num_month_kbtpk_12) if year==3 & sample_2013 == 1 & child_age_yrs < 6

replace x = (num_month_kbtpk_08 + num_month_kbtpk_09) if year==4 & sample_2016 == 1 & child_age_yrs < .
replace x = (num_month_kbtpk_09 + num_month_kbtpk_10) if year==4 & sample_2016 == 1 & child_age_yrs < 11	
replace x = (num_month_kbtpk_10 + num_month_kbtpk_11) if year==4 & sample_2016 == 1 & child_age_yrs < 10
replace x = (num_month_kbtpk_11 + num_month_kbtpk_12) if year==4 & sample_2016 == 1 & child_age_yrs < 9
	
	
gen expo = 0 if x <= 9
replace expo = 1 if x > 9 & x <= 20
lab def expo 0 "Less than 1 yr of playgroup at 3/4" 1 "At least 1 yr of playgroup at 3/4", replace
lab val expo expo

	
* 2013 - 5 years-old

	// EDI 
	
	global base "bmi_bl cog_bl fm_bl gm_bl lang_bl"
	global controls "child_age_yrs hhsize zwealth parentingz mothered gender"
	global controlswealth "child_age_yrs hhsize parentingz mothered gender"
	global controlsparent "child_age_yrs hhsize zwealth mothered gender"
	global controlsenr "child_age_yrs hhsize parentingz mothered gender"
	
	estimates clear
		
	global condyr3 "batchactual>=1 & batchactual<=5 & year==3 & sample_2013 == 1"

	
	reg std_phys i.treat##i.expo $base $controls if $condyr3, robust cluster(ea)
		summ std_phys if e(sample) & treat == 0
		outreg2 using younger_appendix3.xls, replace bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))

		
	foreach varname in std_soc std_emot std_langcog std_comgen {
	
	reg `varname' i.treat##i.expo $base $controls if $condyr3, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
	}
		
* 2016 - 8 years-old
	
	// EDI & Test
	
	estimates clear
	global condyr4 "batchactual>=1 & batchactual<=5 & year==4 & sample_2016 == 1"
		
	foreach varname in std_phys std_soc std_emot std_langcog std_comgen {

	reg `varname' i.treat##i.expo $base $controls if $condyr4, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
			
	}

	
	foreach varname in std_langmp std_langmo std_mathsu std_mathon std_newraven {

	reg `varname' i.treat##i.expo $base $controlsenr if $condyr4, robust cluster(ea)
		summ `varname' if e(sample) & treat == 0
		outreg2 using younger_appendix3.xls, append bdec(3) sdec(3) se alpha(0.001, 0.01, 0.05) addstat(Mean, r(mean))
			
	}
	

*-------------------------------------------------------------------------------------------------------*
* Save data with key vars for appending with older panel
*-------------------------------------------------------------------------------------------------------*	

keep bmi_bl cog_bl fm_bl gm_bl lang_bl ///
	 child_age_months child_age_months_alt child_age_years child_age_yrs ///
	 hhsize zwealth parentingz mothered gender ///
	 treat batch* year sample* ///
	 hhid childid studentid ea ///
	 std_phys std_soc std_emot std_langcog std_comgen 
	 
save "Younger_Panel_Child_Outcomes_2009-2016_v2_forappend.dta",replace
