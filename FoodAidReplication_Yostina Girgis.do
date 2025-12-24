clear all
capture log close
set more off

cd "\Users\yosti\OneDrive - Hunter - CUNY\FALL 2025\Eco 726 Policy & Program Evaluation\Project"
* Change file path to your device.

log using "FoodAidRep.log", replace

use "FAid_Final.dta", clear

**********************************
*** TABLE 1: Summary Statistics ***
**********************************

tsset obs year

* Convert to thousands of tonnes (makes coefficients readable)
replace wheat_aid = wheat_aid/1000
replace US_wheat_production = US_wheat_production/1000
replace recipient_cereals_prod = recipient_cereals_prod/1000
replace recipient_wheat_prod = recipient_wheat_prod/1000

* Create the instrumental variable
gen instrument = l.US_wheat_production*fadum_avg

* Create control variable interactions with year dummies
gen USA_ln_income = ln(USA_rgdpch)
bysort risocode: egen ln_rgdpch_avg = mean(ln_rgdpch) if year>=1971 & year<=2006

gen oil_fadum_avg = oil_price_2011_USD*fadum_avg
gen US_income_fadum_avg = USA_ln_income*fadum_avg
gen US_democ_pres_fadum_avg = US_president_democ*fadum_avg

* Generate year dummies and their interactions
tab year, gen(ydum)

* Create interactions of country characteristics with year dummies
forval x=1/36 {
    gen gdp_y`x' = ln_rgdpch_avg*ydum`x'
    gen usmil_y`x' = real_usmilaid_avg*ydum`x'
    gen usec_y`x' = real_us_nonfoodaid_ecaid_avg*ydum`x'
    gen rcereal_y`x' = recipient_pc_cereals_prod_avg*ydum`x'
    gen rimport_y`x' = cereal_pc_import_quantity_avg*ydum`x'
}

* Define baseline controls (this is the full set used in main regressions)
local baseline_controls "oil_fadum_avg US_income_fadum_avg US_democ_pres_fadum_avg gdp_y2-gdp_y36 usmil_y2-usmil_y36 usec_y2-usec_y36 rcereal_y2-rcereal_y36 rimport_y2-rimport_y36 all_Precip_jan-all_Precip_dec all_Temp_jan-all_Temp_dec all_Precip_jan_faavg-all_Precip_dec_faavg all_Temp_jan_faavg-all_Temp_dec_faavg"

* Restrict to sample years
keep if year>=1971 & year<=2006

* Create in-sample indicator (ensures consistent sample across all specs)
quietly: xi: ivreg2 intra_state (wheat_aid=instrument) `baseline_controls' i.risocode i.year*i.wb_region, cluster(risocode)
gen in_sample = 1 if e(sample)==1

* Generate summary statistics
summarize any_war intra_state inter_state wheat_aid fadum_avg instrument recipient_cereals_prod recipient_wheat_prod if in_sample==1

**********************************
*** TABLE 2: Baseline OLS & IV ***
**********************************

* Load data
use "FAid_Final.dta", clear
tsset obs year

* Convert to thousands of tonnes
replace wheat_aid = wheat_aid/1000
replace US_wheat_production = US_wheat_production/1000

* Create instrument
gen instrument = l.US_wheat_production*fadum_avg

* Create control variables
gen USA_ln_income = ln(USA_rgdpch)
bysort risocode: egen ln_rgdpch_avg = mean(ln_rgdpch) if year>=1971 & year<=2006

gen oil_fadum_avg = oil_price_2011_USD*fadum_avg
gen US_income_fadum_avg = USA_ln_income*fadum_avg
gen US_democ_pres_fadum_avg = US_president_democ*fadum_avg

* Generate year dummies
tab year, gen(ydum)

* Create interactions with year dummies
forval x=1/36 {
    gen gdp_y`x' = ln_rgdpch_avg*ydum`x'
    gen usmil_y`x' = real_usmilaid_avg*ydum`x'
    gen usec_y`x' = real_us_nonfoodaid_ecaid_avg*ydum`x'
    gen rcereal_y`x' = recipient_pc_cereals_prod_avg*ydum`x'
    gen rimport_y`x' = cereal_pc_import_quantity_avg*ydum`x'
}

* Define control groups (building up gradually)
local US_controls "oil_fadum_avg US_income_fadum_avg US_democ_pres_fadum_avg"
local weather_controls "all_Precip_jan-all_Precip_dec all_Temp_jan-all_Temp_dec all_Precip_jan_faavg-all_Precip_dec_faavg all_Temp_jan_faavg-all_Temp_dec_faavg"
local country_chars_controls "gdp_y2-gdp_y36 usmil_y2-usmil_y36 usec_y2-usec_y36"
local cereals_controls "rcereal_y2-rcereal_y36 rimport_y2-rimport_y36"
local baseline_controls "`US_controls' `weather_controls' `country_chars_controls' `cereals_controls'"

* Restrict sample
keep if year>=1971 & year<=2006

* Create in-sample indicator (ensures all specs use same observations)
quietly: xi: ivreg2 intra_state (wheat_aid=instrument) `baseline_controls' i.risocode i.year*i.wb_region, cluster(risocode)
gen in_sample = 1 if e(sample)==1

*****************************
*** Panel A: OLS Estimates ***
*****************************

* Column 1: No controls except fixed effects
xi: reg any_war wheat_aid i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 2: Add US-level controls
xi: reg any_war wheat_aid `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 3: Add weather controls
xi: reg any_war wheat_aid `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 4: Add country characteristics × year FE
xi: reg any_war wheat_aid `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 5: Add cereal controls (full baseline specification) - any war
xi: reg any_war wheat_aid `cereals_controls' `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 6: Full controls - intra-state conflict
xi: reg intra_state wheat_aid `cereals_controls' `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 7: Full controls - inter-state conflict
xi: reg inter_state wheat_aid `cereals_controls' `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

*****************************
*** Panel B: Reduced Form ***
*****************************

* Scale outcomes by 1000 to make coefficients easier to read
* (Reduced form shows effect of instrument on outcome directly)
preserve
foreach x of varlist any_war intra_state inter_state {
    replace `x' = `x'*1000
}

* Column 1: No controls
xi: reg any_war instrument i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 2: Add US controls
xi: reg any_war instrument `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 3: Add weather controls
xi: reg any_war instrument `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 4: Add country characteristics
xi: reg any_war instrument `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 5: Full controls - any war
xi: reg any_war instrument `cereals_controls' `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 6: Full controls - intra-state
xi: reg intra_state instrument `cereals_controls' `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

* Column 7: Full controls - inter-state
xi: reg inter_state instrument `cereals_controls' `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode)

restore

******************************************
*** Panel C: Second Stage of IV (2SLS) ***
******************************************

* Column 1: No controls
xi: ivreg2 any_war (wheat_aid=instrument) i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode) ffirst

* Column 2: Add US controls
xi: ivreg2 any_war (wheat_aid=instrument) `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode) ffirst

* Column 3: Add weather controls
xi: ivreg2 any_war (wheat_aid=instrument) `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode) ffirst

* Column 4: Add country characteristics
xi: ivreg2 any_war (wheat_aid=instrument) `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode) ffirst

* Column 5: Full controls - any war
xi: ivreg2 any_war (wheat_aid=instrument) `cereals_controls' `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode) ffirst

* Column 6: Full controls - intra-state conflict
xi: ivreg2 intra_state (wheat_aid=instrument) `cereals_controls' `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode) ffirst

* Column 7: Full controls - inter-state conflict
xi: ivreg2 inter_state (wheat_aid=instrument) `cereals_controls' `country_chars_controls' `weather_controls' `US_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode) ffirst

*********************************
*** TABLE 7: Onset & Duration ***
*********************************

* Load fresh data
use "FAid_Final.dta", clear
tsset obs year

* Convert to thousands
replace wheat_aid = wheat_aid/1000
replace US_wheat_production = US_wheat_production/1000

* Create instrument
gen instrument = l.US_wheat_production*fadum_avg

* Restrict sample
keep if year>=1971 & year<=2006

* Create control variables
gen USA_ln_income = ln(USA_rgdpch)
bysort risocode: egen ln_rgdpch_avg = mean(ln_rgdpch) if year>=1971 & year<=2006

gen oil_fadum_avg = oil_price_2011_USD*fadum_avg
gen US_income_fadum_avg = USA_ln_income*fadum_avg
gen US_democ_pres_fadum_avg = US_president_democ*fadum_avg

* Precipitation and temperature interactions
* foreach x of varlist all_Precip_jan-all_Precip_dec all_Temp_jan-all_Temp_dec {
*    gen `x'_faavg = `x'*fadum_avg
* }

* Year dummies and interactions
tab year, gen(ydum)

forval x=1/36 {
    gen gdp_y`x' = ln_rgdpch_avg*ydum`x'
    gen usmil_y`x' = real_usmilaid_avg*ydum`x'
    gen usec_y`x' = real_us_nonfoodaid_ecaid_avg*ydum`x'
    gen rcereal_y`x' = recipient_pc_cereals_prod_avg*ydum`x'
    gen rimport_y`x' = cereal_pc_import_quantity_avg*ydum`x'
}

* Define baseline controls
local baseline_controls "oil_fadum_avg US_income_fadum_avg US_democ_pres_fadum_avg gdp_y2-gdp_y36 usmil_y2-usmil_y36 usec_y2-usec_y36 rcereal_y2-rcereal_y36 rimport_y2-rimport_y36 all_Precip_jan-all_Precip_dec all_Temp_jan-all_Temp_dec all_Precip_jan_faavg-all_Precip_dec_faavg all_Temp_jan_faavg-all_Temp_dec_faavg"

* Create in-sample indicator
quietly: xi: ivreg2 intra_state (wheat_aid=instrument) `baseline_controls' i.risocode i.year*i.wb_region, cluster(risocode)
gen in_sample = 1 if e(sample)==1

*************************************************
*** Column 1: Collier-Hoeffler Onset Definition ***
*************************************************
* Only includes first year of conflict (drops observations where country was already in conflict)

preserve
tsset obs year  // Re-declare panel structure
drop if l.intra_state==1  // Only keep observations where country was at peace last year

xi: ivreg2 intra_state_onset (wheat_aid=instrument) `baseline_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode) ffirst

* Summary statistics for this specification
summarize intra_state_onset if e(sample)==1

restore

***********************************************
*** Column 2: Fearon-Laitin Onset Definition ***
***********************************************
* Includes all observations and controls for lagged conflict

* Re-declare panel structure
tsset obs year

* Create lagged conflict variable
gen l_intra_state = l.intra_state

xi: ivreg2 intra_state_onset l_intra_state (wheat_aid=instrument) `baseline_controls' i.risocode i.year*i.wb_region if in_sample==1, cluster(risocode) ffirst

* Summary statistics for this specification
summarize intra_state_onset if e(sample)==1

***************************************************************
*** Columns 3-5: Duration Models for Onset (Peace → War) ***
***************************************************************
* Uses logistic discrete time hazard model with control function approach

* Define shorter control list for duration models
local dur_controls "real_usmilaid_avg real_us_nonfoodaid_ecaid_avg recipient_pc_cereals_prod_avg cereal_pc_import_quantity_avg ln_rgdpch_avg"

*** Column 3: Duration polynomial only ***
preserve
tsset obs year
drop if l.intra_state==1  // Only at risk of onset if at peace last year

* Create polynomial terms for peace duration
gen peace_dur2 = peace_dur*peace_dur
gen peace_dur3 = peace_dur2*peace_dur

* Step 1: First stage regression (generate control function residual)
xi: reg wheat_aid instrument peace_dur peace_dur2 peace_dur3 if !missing(intra_state_onset), cluster(risocode)
test instrument  // Test instrument strength - gives F-statistic
predict aid_resid if e(sample)==1, resid

* Step 2: Logit with control function residual
xi: logit intra_state_onset wheat_aid aid_resid peace_dur peace_dur2 peace_dur3, cluster(risocode)

* Step 3: Calculate marginal effects at means
margins, dydx(wheat_aid) atmeans

restore

*** Column 4: Add time-invariant country controls ***
preserve
tsset obs year
drop if l.intra_state==1

gen peace_dur2 = peace_dur*peace_dur
gen peace_dur3 = peace_dur2*peace_dur

* First stage
xi: reg wheat_aid instrument peace_dur peace_dur2 peace_dur3 `dur_controls' if !missing(intra_state_onset), cluster(risocode)
test instrument
predict aid_resid if e(sample)==1, resid

* Logit with control function
xi: logit intra_state_onset wheat_aid aid_resid peace_dur peace_dur2 peace_dur3 `dur_controls', cluster(risocode)

* Marginal effects
margins, dydx(wheat_aid) atmeans

restore

*** Column 5: Add region fixed effects ***
preserve
tsset obs year
drop if l.intra_state==1

gen peace_dur2 = peace_dur*peace_dur
gen peace_dur3 = peace_dur2*peace_dur

* First stage
xi: reg wheat_aid instrument peace_dur peace_dur2 peace_dur3 `dur_controls' i.wb_region if !missing(intra_state_onset), cluster(risocode)
test instrument
predict aid_resid if e(sample)==1, resid

* Logit with control function
xi: logit intra_state_onset wheat_aid aid_resid peace_dur peace_dur2 peace_dur3 `dur_controls' i.wb_region, cluster(risocode)

* Marginal effects
margins, dydx(wheat_aid) atmeans

* Summary statistics
summarize intra_state_onset if e(sample)==1

restore

***************************************************************
*** Columns 6-8: Duration Models for Offset (War → Peace) ***
***************************************************************
* Uses logistic discrete time hazard model with control function approach

*** Column 6: Duration polynomial only ***
preserve
tsset obs year
drop if l.intra_state==0  // Only at risk of offset if at war last year

* Create polynomial terms for conflict duration
gen intra_state_dur2 = intra_state_dur*intra_state_dur
gen intra_state_dur3 = intra_state_dur2*intra_state_dur

* Step 1: First stage regression
xi: reg wheat_aid instrument intra_state_dur intra_state_dur2 intra_state_dur3 if !missing(intra_state_offset), cluster(risocode)
test instrument
predict aid_resid if e(sample)==1, resid

* Step 2: Logit with control function
xi: logit intra_state_offset wheat_aid aid_resid intra_state_dur intra_state_dur2 intra_state_dur3, cluster(risocode)

* Step 3: Marginal effects
margins, dydx(wheat_aid) atmeans

restore

*** Column 7: Add time-invariant country controls ***
preserve
tsset obs year
drop if l.intra_state==0

gen intra_state_dur2 = intra_state_dur*intra_state_dur
gen intra_state_dur3 = intra_state_dur2*intra_state_dur

* First stage
xi: reg wheat_aid instrument intra_state_dur intra_state_dur2 intra_state_dur3 `dur_controls' if !missing(intra_state_offset), cluster(risocode)
test instrument
predict aid_resid if e(sample)==1, resid

* Logit with control function
xi: logit intra_state_offset wheat_aid aid_resid intra_state_dur intra_state_dur2 intra_state_dur3 `dur_controls', cluster(risocode)

* Marginal effects
margins, dydx(wheat_aid) atmeans

restore

*** Column 8: Add region fixed effects ***
preserve
tsset obs year
drop if l.intra_state==0

gen intra_state_dur2 = intra_state_dur*intra_state_dur
gen intra_state_dur3 = intra_state_dur2*intra_state_dur

* First stage
xi: reg wheat_aid instrument intra_state_dur intra_state_dur2 intra_state_dur3 `dur_controls' i.wb_region if !missing(intra_state_offset), cluster(risocode)
test instrument
predict aid_resid if e(sample)==1, resid

* Logit with control function
xi: logit intra_state_offset wheat_aid aid_resid intra_state_dur intra_state_dur2 intra_state_dur3 `dur_controls' i.wb_region, cluster(risocode)

* Marginal effects
margins, dydx(wheat_aid) atmeans

* Summary statistics
summarize intra_state_offset if e(sample)==1

restore

log close