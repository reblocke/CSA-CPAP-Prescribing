* Do file for CSA analysis

set scheme cleanplots
graph set window fontface "Helvetica"
* Regression on likelihood of initial prescription
clear
*log using cpap_rx_analysis
cd "/Users/reblocke/Box Sync/Residency Personal Files/Scholarly Work/CSA/Results/Mar 21 Update"

program define datetime 
end
capture mkdir "Results and Figures"
capture mkdir "Results and Figures/$S_DATE/" //make new folder for figure output if needed
capture mkdir "Results and Figures/$S_DATE/Logs/" //new folder for stata logs
local a1=substr(c(current_time),1,2)
local a2=substr(c(current_time),4,2)
local a3=substr(c(current_time),7,2)
local b = "CSA regressions.do" // do file name
copy "`b'" "Results and Figures/$S_DATE/Logs/(`a1'_`a2'_`a3')`b'"

import excel "coded_output.xlsx", sheet("Sheet1") firstrow case(lower)
label variable age "Age per decade"
label variable bmi "BMI per 5 kg/m^2"
label variable sex "Male"
recode sex 1=0 0=1, gen(female)
label variable female "Female"
label variable elevation "Elevation per 1000 ft"
label variable ahi "AHI per 10 events/hour"
label variable studytype "Diagnosed by PSG"
recode studytype 1=0 0=1, gen(hsat)
label variable hsat "Diagnosed by HSAT"
label define hsat_label 0 "PSG" 1 "HSAT"
label values hsat hsat_label
label variable inittx_cpap "Initial CPAP trial?"
label define inittx_cpap_lab 0 "No CPAP trial" 1 "Initial CPAP trial"
label values inittx_cpap inittx_cpap_lab
label variable has_cns "Neurologic Causes"
label variable has_cv "Cardiac Causes"
label variable has_opiate "Opiate Etiology"
label variable has_primary "Primary CSA"
label variable has_tecsa "Treatment-Emergent"
label variable has_osacsa "CA-OSA"
label variable perc_osa_ord "Portion Central Apneas"
label define perc_osa_ord_label_abbrev 0 "<10%" 1 "10-49%" 2 "50-89%" 3 "{&ge}90%"
label values perc_osa_ord perc_osa_ord_label_abbrev
label variable smoking "Smoking Status"
label define smoking_label 0 "Never Smoker" 1 "Prior Smoker" 2 "Current Smoker"
label values smoking smoking_label
ladder elevation
label variable log_elevation "Log of Elevation"
label define init_cpap_label 0 "No CPAP Trial" 1 "CPAP Prescribed"
label values inittx_cpap init_cpap_label
gen no_cpap = 1 if inittx_cpap == 0
replace no_cpap = 0 if inittx_cpap == 1
label define no_cpap_label 0  "CPAP Prescribed" 1 "No CPAP Trial" //inverse outcome
label values no_cpap no_cpap_label
gen age_n = age * 10
label variable age_n "Age (years)"
gen bmi_n = bmi * 5
label variable bmi_n "BMI (kg/m^2)"
gen elevation_n = elevation * 1000
label variable elevation_n "Elevation (ft)"
gen ahi_n = ahi * 10
label variable ahi_n "AHI (events/hour)"

//Neurologic, Cardiac, Medication, Primary, TECSA, OSACSA, multi
gen etio_cat = 6 if has_osacsa == 1
replace etio_cat = 5 if has_tecsa == 1
replace etio_cat = 4 if has_primary == 1
replace etio_cat = 1 if has_cns == 1
replace etio_cat = 2 if has_cv == 1
replace etio_cat = 3 if has_opiate == 1
replace etio_cat = 0 if has_cns + has_cv + has_opiate + has_primary + has_tecsa + has_osacsa > 1 //captures that if there's more than 1, replace with multiple -> thus order doesn't matter. 
label define etio_cat_label 0 "Multiple" 1 "Neurologic" 2 "Cardiac" 3 "Opiate" 4 "Primary" 5 "TECSA" 6 "CA-OSA"
label values etio_cat etio_cat_label

*multinomial version on the initial dataset: with outcomes: not prescribed cpap, resolve cpap, not resolve cpap
generate outcome = 2*inittx_cpap - outcome_resolvedwcpap 
label variable outcome "Results"
label define outcome_label 0 "No CPAP Trial" 1 "Adequate CPAP Trial" 2 "Unsuccessful CPAP Trial" // add outcome_nonadherence
label values outcome outcome_label

recode outcome (0=.) (1=0) (2=1), gen(outcome_w_cpap)
label define outcome_cpap_label 0 "Adequate CPAP Trial" 1 "Unsuccessful CPAP Trial"
label values outcome_w_cpap outcome_cpap_label

recode outcome_w_cpap (0=1) (1=0), gen(success_w_cpap)
label define success_cpap_label 0 "Unsuccessful CPAP Trial" 1 "Adequate CPAP Trial"
label values success_w_cpap success_cpap_label 
tab success_w_cpap outcome_w_cpap

heatplot inittx_cpap i.perc_osa_ord i.etio_cat, values(format(%3.2f) size(3)) aspectratio(0.7) xlabel(,angle(vertical) labsize(3)) ylabel(,labsize(3)) ytitle("Percentage of Central Apneas", size(4)) xtitle("Etiology of Central Apneas", size(4)) color(RdYlGn) ramp(right format(%3.2f) space(18) subtitle("Portion" "Precribed" "CPAP Trial", size(medsmall) justification(center)) label(0(0.1)1, labsize(3))) p(lcolor(black%10) lwidth(*0.15)) xsize(3.5) ysize(2.5)

//more in depth labels for later figures
label define perc_osa_ord_label 0 "<10% Central Apneas" 1 "10-49.9% Central Apneas" 2 "50-89.9% Central Apneas" 3 "{&ge}90% Central Apneas"
label values perc_osa_ord perc_osa_ord_label

nmissing

*'Table 1' for CPAP Rx 
table1_mc, by(inittx_cpap) ///
vars( ///
age_n conts %4.0f \ ///
female bin %4.0f \ ///
bmi_n conts %4.1f \ ///
smoking cat %4.0f \ ///
elevation_n conts %4.1f \ ///
ahi_n conts %4.0f \ ///
hsat bin %4.0f \ ///
has_cns bin %4.0f \ ///
has_cv bin %4.0f \ ///
has_opiate bin %4.0f \ ///
has_primary bin %4.1f \ ///
has_tecsa bin %4.0f \ ///
has_osacsa bin %4.0f \ ///
perc_osa_ord cat %4.0f \ ///
) ///
nospace percent_n onecol total(before) ///
saving("cpap rx table1.xlsx", replace)

* Supplementary 'Table 1' by dx via HSAT or not
table1_mc, by(hsat) ///
vars( ///
age_n conts %4.0f \ ///
female bin %4.0f \ ///
bmi_n conts %4.1f \ ///
smoking cat %4.0f \ ///
elevation_n conts %4.1f \ ///
ahi_n conts %4.0f \ ///
has_cns bin %4.0f \ ///
has_cv bin %4.0f \ ///
has_opiate bin %4.0f \ ///
has_primary bin %4.1f \ ///
has_tecsa bin %4.0f \ ///
has_osacsa bin %4.0f \ ///
perc_osa_ord cat %4.0f \ ///
outcome cat %4.0f \ ///
) ///
nospace percent_n onecol total(before) ///
saving("hsat supp table1.xlsx", replace)

/* Regression used in supplement; section 4.2 */ 
mlogit outcome age bmi female ib0.smoking ahi hsat has_cns has_cv has_opiate has_osacsa has_tecsa has_primary ib2.perc_osa_ord, baseoutcome(0) rrr 
outreg2 using multinomial_outcomes, word replace stats(coef ci pval) label 
fitstat
adjrr age
adjrr female
adjrr bmi
adjrr smoking , x0(1) x1(0)
adjrr smoking , x0(1) x1(2)
adjrr ahi
adjrr hsat
adjrr has_cns
adjrr has_cv
adjrr has_opiate
adjrr has_primary
adjrr has_tecsa
adjrr has_osacsa
adjrr perc_osa_ord , x0(2) x1(0)
adjrr perc_osa_ord , x0(2) x1(1)
//adjrr perc_osa_ord , x0(2) x1(3) //doesn't converge


/* Check for interactions */ 
logistic inittx_cpap c.age##i.female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord
testparm c.age#i.female //not quite signficant, leave interaction out
logistic inittx_cpap c.age##ib2.perc_osa_ord female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa 
testparm c.age#ib2.perc_osa_ord //not signficant

*ORs - full model
logistic inittx_cpap age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord //changed from predict foregoing the CPAP trial and thus be consistent through manuscript
estimates store rx

// Untransformed. Full model; for unequal-variance t-test 
logit inittx_cpap age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord

//How good is this model? 
lroc  //auroc 0.71
estat gof, group(10) table // H-L goodness of fit test p=0.11

* Multi-colinearity testing
vif, unc 

//Average Marginal effects of each predictor: 
margins, dydx(age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord) post  ///this is the new output for the text
//Regression for the figure
estimates store rx_margins
outreg2 using logistic_result_margins, word replace ctitle(Prescribe CPAP) dec(2) sdec(2) stat(coef ci pval) title(Factors association w foregoing CPAP trial and suboptimal outcome) label 	
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter) 

// Generate propensity score, and then inverse probability of treatment weight
//= propensity to be treated w CPAP
logistic inittx_cpap age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord
predict ps 
xtile tx_pr_quintile = ps, nq(5)
tab outcome tx_pr_quintile //quintile of treatment probability aka propensity score.

gen ipw = 1/ps if inittx_cpap==1
replace ipw = 1/(1-ps) if inittx_cpap==0 // now ps has inverse probability weights. 
hist ipw, by(outcome_w_cpap)

//Presentation of elevation spectrum
hist elevation_n, frequency width(500) ytitle("Patients in elevation range", size(4.5)) xtitle("Elevation (500 ft bins)", size(4)) ylabel(, labsize(4)) xlabel(0(5000)12000, labsize(4.5)) xsize(3.5) ysize(2.5)

save csa_regressions, replace
* End of data processing

*log close

* -------
* Regression on likelihood of resolution once prescribed, regression on likelihood of noncompliance once prescribed

clear
use csa_regressions

drop if no_cpap == 1 
label values outcome_resolvedwcpap outcome_cpap_label
gen cpap_inadequate = 1 if outcome_resolvedwcpap == 0
replace cpap_inadequate = 0 if outcome_resolvedwcpap == 1
label define cpap_inadequate_label 0 "CPAP Adequate" 1 "CPAP Unsuccessful"
label values cpap_inadequate cpap_inadequate_label
label define outcome_noncompliant_label 0 "Adherent to CPAP" 1 "Not adherent to CPAP"
label values outcome_noncompliant outcome_noncompliant_label

mdesc

heatplot success_w_cpap i.perc_osa_ord i.etio_cat, values(format(%3.2f) size(3)) aspectratio(0.7) xlabel(,angle(vertical) labsize(3)) ylabel(,labsize(3)) ytitle("Percentage of Central Apneas", size(4)) xtitle("Etiology of Central Apneas", size(4)) color(RdYlGn) ramp(right format(%3.2f) space(18) subtitle("Portion" "Adequate" "CPAP Trial", size(medsmall) justification(center)) label(0(0.1)1, labsize(3))) p(lcolor(black%10) lwidth(*0.15)) xsize(3.5) ysize(2.5)

tab success_w_cpap outcome_w_cpap

*'Table 1' for Resolution with CPAP 
table1_mc, by(success_w_cpap) ///
vars( ///
age_n conts %4.0f \ ///
female bin %4.0f \ ///
bmi_n conts %4.1f \ ///
smoking cat %4.0f \ ///
elevation_n conts %4.1f \ ///
ahi_n conts %4.0f \ ///
hsat bin %4.0f \ ///
has_cns bin %4.0f \ ///
has_cv bin %4.0f \ ///
has_opiate bin %4.0f \ ///
has_primary bin %4.1f \ ///
has_tecsa bin %4.0f \ ///
has_osacsa bin %4.0f \ ///
perc_osa_ord cat %4.0f \ ///
) ///
nospace percent_n onecol total(before) ///
saving("cpap outcome table1.xlsx", replace)

recode perc_osa_ord 0/1=0 2/3=1, gen(perc_osa_bin)
tab perc_osa_ord perc_osa_bin
tab perc_osa_bin outcome_resolvedwcpap, row
tab3way outcome_resolvedwcpap perc_osa_bin has_opiate
bysort has_opiate: tab perc_osa_bin outcome_resolvedwcpap , row chi2

/* Logistic Regressions to Evaluate likelihood of suboptimal outcome*/

//Check for Interactions
logistic outcome_resolvedwcpap c.age##i.female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord
testparm c.age#i.female //not signficant, leave interaction out
logistic outcome_resolvedwcpap c.age##ib2.perc_osa_ord female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa 
testparm c.age#ib2.perc_osa_ord //not signficant
logistic outcome_resolvedwcpap c.ahi##ib2.perc_osa_ord c.age i.female bmi i.smoking elevation hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa 
testparm c.ahi##ib2.perc_osa_ord //none of these are signficant

*Version to calculate ORs
logistic outcome_resolvedwcpap age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord

//How good is this model? 
lroc  //auroc 0.688
estat gof, group(10) table // H-L goodness of fit test p=0.84

outreg2 using logistic_results, word append ctitle(Inadequate CPAP Outcome) dec(2) sdec(2) stat(coef ci pval) title(Factors association with foregoing CPAP trial and suboptimal outcome if CPAP trialed) label 	
estimates store res

// Untransformed. Full model; for unequal-variance t-test 
//Test of differences between Rx and Res estimates for reach
//d=E1-E2
//SE(d)=√[SE(E1)2 + SE(E2)2]
//z=d/SE(d)
//95CI: d-1.96SE(d) to d+1.96SE(d).
logit outcome_resolvedwcpap age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord

//Average Marginal effects of each predictor: 
margins, dydx(age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord) post
estimates store res_margins
outreg2 using logistic_result_margins, word append ctitle(Adequate CPAP trial) dec(2) sdec(2) stat(coef ci pval) title(Factors associated w prescription for CPAP trial and adequate outcome) label 
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)


* Multi-colinearity testing
vif, unc



//OR Version, used for manuscript
coefplot (rx, ciopts(recast(rcap) lpattern("-"))) (res, ciopts(recast(rcap) lpattern("l"))), drop(_cons) legend(pos(10) ring(0) size(2.3)) eform xscale(log) xline(1) xlabel(0.03 0.06 0.13 0.25 0.5 1 2 4 8 16 32) xscale(extend) yscale(extend)  headings(age = "{bf:Patient Characteristics}" ahi = "{bf:Disease Characteristics}" 0.perc_osa_ord = "{bf:Proportion Central Apneas}" has_cns = "{bf:Etiology}") xtitle("OR of initial CPAP trial prescription & adequate CPAP response when trialed" , size(small)) plotlabels("CPAP Trial Prescribed?" "CPAP Adequate When Trialed?") text(13 0.11 "Favors No CPAP Trial &" "Unsuccessful CPAP Trial" 13 9.9 "Favors CPAP Trial &" "Adequate Response to CPAP", size(small) color(gs5)) xsize(6.85) ysize(5) baselevels scheme(plotplain)


/* -----
PROPENSITY SCORE ANALYSIS; inverse probability of treatment weighting.  
*/ 

//Inverse probability of treatment weighted.
logistic outcome_resolvedwcpap age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord [pweight=ipw]
estimates store res_ipw

//Average Marginal effects of each predictor: 
margins, dydx(age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord) post
estimates store res_ipw_margins
outreg2 using logistic_result_margins, word append ctitle(Inadequate CPAP result) dec(2) sdec(2) stat(coef ci pval) title(Factors association w foregoing CPAP trial and suboptimal outcome) label 
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)


heatplot outcome_resolvedwcpap i.perc_osa_ord i.etio_cat [pweight=ipw], values(format(%3.2f) size(3)) aspectratio(0.7) xlabel(,angle(vertical) labsize(3)) ylabel(,labsize(3)) ytitle("Percentage of Central Events", size(4)) xtitle("Etiology of Central Events", size(4)) color(RdYlGn) ramp(right format(%3.2f) space(18) subtitle("Portion" "Adequate" "CPAP Trial", size(medsmall) justification(center)) label(0(0.1)1, labsize(3))) p(lcolor(black%10) lwidth(*0.15)) xsize(3.5) ysize(2.5)

//IPTW OR Version
coefplot rx res_ipw, drop(_cons) legend(pos(2) ring(0) size(2.5)) eform xscale(log) xline(1) xlabel(0.03 0.06 0.13 0.25 0.5 1 2 4 8 16 32) xscale(extend) yscale(extend)  headings(age = "{bf:Patient Characteristics}" ahi = "{bf:Disease Characteristics}" 0.perc_osa_ord = "{bf:Proportion Central Events}" has_cns = "{bf:Etiology}") xtitle("Odds Ratio of receviing an initial CPAP trial, or adequate CPAP response when trialed" , size(small)) plotlabels("CPAP Trial Prescribed?" "CPAP Adequate When Trialed? (IPTW)") ciopts(recast(rcap)) text(13 0.11 "Favors No CPAP Trial &" "Unsuccessful CPAP Trial" 13 9.9 "Favors CPAP Trial &" "Adequate Response to CPAP", size(small) color(gs9)) xsize(7) ysize(5) baselevels

//Change in OR with IPTW weighting
coefplot res res_ipw, drop(_cons) legend(pos(2) ring(0) size(2.5)) eform xscale(log) xline(1) xlabel(0.125 0.25 0.5 1 2 4 8 16 32) xscale(extend) yscale(extend)  headings(age = "{bf:Patient Characteristics}" ahi = "{bf:Disease Characteristics}" 0.perc_osa_ord = "{bf:Proportion Central Events}" has_cns = "{bf:Etiology}") xtitle("Odds Ratio of adequate CPAP response when trialed" , size(small)) plotlabels("CPAP Adequate When Trialed?" "CPAP Adequate When Trialed? (IPTW)") ciopts(recast(rcap)) baselevels scheme(white_tableau)

/* Logistic Regressions to Evaluate likelihood of noncompliance as a particular reason for suboptimal outcome*/
table1_mc, by(outcome_noncompliant) ///
vars( ///
age_n conts %4.0f \ ///
female bin %4.0f \ ///
bmi_n conts %4.1f \ ///
smoking cat %4.0f \ ///
elevation_n conts %4.1f \ ///
ahi_n conts %4.0f \ ///
hsat bin %4.0f \ ///
has_cns bin %4.0f \ ///
has_cv bin %4.0f \ ///
has_opiate bin %4.0f \ ///
has_primary bin %4.0f \ ///
has_tecsa bin %4.1f \ ///
has_osacsa bin %4.0f \ ///
perc_osa_ord cat %4.0f \ ///
) ///
nospace percent_n onecol total(before) ///
saving("cpap compliant table1.xlsx", replace)

//Noncompliance 
logistic outcome_noncompliant age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord 
outreg2 using logistic_results, word append ctitle(CPAP Nonadherence) dec(2) sdec(2) stat(coef ci pval) title(Factors association with foregoing CPAP trial and suboptimal outcome if CPAP trialed) label 	
* Multi-colinearity testing
vif, unc

//Average Marginal effects of each predictor: 
margins, dydx(age female bmi i.smoking elevation ahi hsat has_cns has_cv has_opiate has_primary has_tecsa has_osacsa ib2.perc_osa_ord) post
estimates store noncomp_margins
outreg2 using logistic_result_margins, word append ctitle(CPAP Noncompliance) dec(2) sdec(2) stat(coef ci pval ) title(Average Marginal Effects of with respect to likelihood of foregoing CPAP trial and suboptimal outcome if CPAP trialed) label 
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

log close
