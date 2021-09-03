/*

This do file replicates the main analyses conducted in:

Hasan, A., Jung, H., Kinnell, A., Maika, A., Nakajima, N., & Pradhan, M. (2021). 
Contrasting Experiences: Understanding the Longer-Term Impact of Improving Access 
to Pre-Primary Education in Rural Indonesia. 
Journal of Research on Educational Effectiveness, 14(1), 28-56.

(https://www.tandfonline.com/doi/abs/10.1080/19345747.2020.1839989) 

*/


tempfile a b

* Load data for younger panel

use "Younger_Panel_Child_Outcomes_2009-2016_v2_forappend.dta", clear

gen co = "younger"

save `a', replace


* Load data for older panel

use "Older_Panel_Child_Outcomes_2009-2013_v2_forappend.dta", clear

gen co = "older"

save `b', replace


* Append younger and older panels

use `a', clear
append using `b'

gen cohort = 0 if co == "younger"
replace cohort = 1 if co == "older"


* For older cohort, EDI at age 5 is coded as "baseline"

	foreach var in phys soc emot langcog comgen {
		
		// standardize the baseline EDI from raw scale
		egen std_`var'_bl = std(short_`var'_bl) if cohort == 1 & year == 2
		
		// replace with existing so can be ran as one regression
		replace std_`var' = std_`var'_bl if cohort == 1 & year == 2
	}
	


* Check "parallel trends" of comparison group (batch 5) for cohorts at age 5 and age 8

bysort year: summ child_age_yrs if cohort == 0 & batcha == 5
bysort year: summ child_age_yrs if cohort == 1 & batcha == 5


grstyle set ci

foreach var in phys soc emot langcog comgen {

graph twoway (lpolyci std_`var' child_age_yrs if cohort == 0 & batcha == 5 & child_age_yrs >= 4, ///
			  clcolor(navy)) ///
		     (lpolyci std_`var' child_age_yrs if cohort == 1 & batcha == 5 & child_age_yrs >= 4, ///
			  clcolor(brown) astyle(ci2) ///
			 xtitle(Age of child) ytitle(S.D.) title(EDI - `var') ///
			 legend(order(2 "Younger cohort" 4 "Older cohort")) ), name(`var', replace)
}	
grc1leg phys soc emot langcog comgen
graph export paralleltrendsedi_all.pdf, replace

foreach var in phys soc emot langcog comgen {

graph twoway (lpolyci std_`var' child_age_yrs if cohort == 0 & batcha == 5 & child_age_yrs >= 4 & (year == 3), ///
			  clcolor(navy)) ///
		     (lpolyci std_`var' child_age_yrs if cohort == 1 & batcha == 5 & child_age_yrs >= 4 & (year == 2), ///
			  clcolor(brown) astyle(ci2) ///
			 xtitle(Age of child) ytitle(S.D.) title(EDI - `var') ///
			 xlabel(4(1)8) ///
			 legend(order(2 "Younger cohort" 4 "Older cohort")) ), name(`var', replace)
}	
grc1leg phys soc emot langcog comgen		 		 
graph export paralleltrendsedi_only5.pdf, replace
			 
keep if !missing(sample) 		 
foreach var in phys soc emot langcog comgen {

reg std_`var' child_age_yrs cohort if batcha == 5 & child_age_yrs >= 4 , robust cluster(ea)

}


preserve
* keep only at age 5 data collection
keep if (cohort == 0 & year == 3) | (cohort == 1 & year == 2)
reg std_phys child_age_yrs cohort if batcha == 5 & child_age_yrs >= 4 , robust cluster(ea)
restore
	
       (histogram child_age_yrs if cohort == 1 & child_age_yrs >= 4, color(green%30)), ///   
       legend(order(1 "Younger cohort" 2 "Older cohort" ))	
	
