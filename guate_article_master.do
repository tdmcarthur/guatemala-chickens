version 15.1
clear all
cap log close
set more off, perm

/*******************************************************************************
Running this .do file executes the code that de-identifies the raw follow-up
data, drops pilot households not in the evaluation, merges baseline and 
follow-up data, takes other steps to prepare the data for later analysis,
estimates all average ITTs shown in the paper, creates all tables and 
figures for average ITTs shown in the paper, and generates all results
given in the appendix. 
*******************************************************************************/

/*******************************************************************************
TO DO LIST:
- edit do file one to create pre post Marek table with FCR intervals

-Table with power calcs: MDEs in own units and standard deviations (table for
appedix)

-Change map to give community locations instead of hhs


.DO FILE DIRECTORY FOR DATA PREP FILES
import_guatemala_fomin_ronda_2: converts raw data to .dta format

guatemala_basic_data_prep_1: Converts baseline data from Windows-1252 encoding
to UTF-8. 

guatemala_basic_data_prep_2: matches communities in follow-up data to their 
original treatment assignments, clusters, and strata. 

guatemala_basic_data_prep_3: household roster, both rounds.

guatemala_basic_data_prep_4: prepares anthro data

guatemala_basic_data_prep_5: prepares food consumption data, but does not 
add up expenditure, calories, etc. 

guatemala_basic_data_prep_6a: round 2 household and individual dietary diversity,
food expenditure

guatemala_basic_data_prep_7: round 1 food consumption, dietary diversity
and food expenditure, calories, protein

guatemala_basic_data_prep_8a: HFIAS 

guatemala_basic_data_prep_9: round 2 non-food expenditure. Also, calculates per
capita consumption. 

guatemala_basic_data_prep_10: dwelling

guatemala_basic_data_prep_11: egg production and egg income, livestock

guatemala_basic_data_prep_12: consumer durables. Poverty line indicators.

guatemala_basic_data_prep_13: value of ag tools and installations. HH members
who contributed to purchase of ag implements in round 2. Amount invested in
ag tools in round 2. 

guatemala_basic_data_prep_14: hh savings

guatemala_basic_data_prep_15: transfers, money sent to individuals
outside of household, PINPEP transfers, mibono seguro, baseline credit.

guatemala_basic_data_prep_16: land

guatemala_basic_data_prep_17: maize and bean production.

guatemala_basic_data_prep_18: brings in Mancomunidad admin data. Drops stratum
21 (contaminated, treatments reversed).

guatemala_basic_data_prep_19: Matches each hh to nearest weather station.

guatemala_basic_data_prep_20: Creates data set of daily temp, rain, and wind, 
by station. Brings in data from baseline ch 14 on control over maize and bean 
production/revenue, share of planted area receiving chemical and organic
inputs, unit value and quantity of seed. Matches each household to climate 
variables for closest station. Finds nearest neighbors for each hh.

guatemala_basic_data_prep_21: gathers baseline women's empowerment data

guatemala_basic_data_prep_22: creates additional covariates

.DO FILE DIRECTORY FOR ANALYSIS FILES
guatemala_analysis_1: create figures showing differences in indicators pre and 
post Marek vaccine

guatemala_analysis_2: balance tests

guatemala_analysis_3: HH level ITT effects in tables

guatemala_analysis_4: HH level ITT effects in a figure

guatemala_analysis_5: Child level ITT effects in tables

guatemala_analysis_6: Child level ITT effects in a figure

guatemala_analysis_7: Spillovers, household level

guatemala_analysis_8: Spillovers, child level

guatemala_analysis_9: Preps data for estimation of heterogeneous effects in R.

guatemala_analysis_10: Creates tables and figures from R output.

guatemala_analysis_11: Lee bounds, hh-level outcomes

guatemala_analysis_12: Checking attrition bias for children.

guatemala_analysis_13: Tables with hh level ITTs, controlling for imbalanced
baseline characteristics. 

guatemala_analysis_14: Tables with child-level ITTs, controlling for imbalanced
baseline characteristics. 

guatemala_analysis_15: Differences in illness among children by gender. Impacts
on transfers and gift giving.

guatemala_analysis_16: Impacts on children controlling for baseline household
means in outcomes

guatemala_analysis_17: Power calculations, hh-level outcomes

guatemala_analysis_18: Power calculations, child-level outcomes
*******************************************************************************/
gl d = "C:/Users/Conner/Google Drive"
*gl d = "D:/Mullally,Conner/Documents/google drive"

gl dir1 = "C:/Users/Conner/Google Drive/Guatemala ronda 2 2017"
*gl dir1 = "D:/Mullally,Conner/Documents/google drive/Guatemala ronda 2 2017"

cd "$dir1"

cap mkdir "$dir1/stata files/article stata work/logs"
cap mkdir "$dir1/stata files/article stata work/stata data files"
cap mkdir "$dir1/stata files/article stata work/graphs"
cap mkdir "$dir1/stata files/article stata work/tables"

* Packages
cap ssc install geodist /* geo commands used to calculate number of nearest
						neighbors for spillover tests */
cap ssc install geonear
cap ssc install tabstatmat // For creating matrix of summary stats
cap net install st0489.pkg // Randomization inference
cap ssc install coefplot // Impact graphs
cap net install sg97_5.pkg // Some of the tables
cap ssc install mmat2tex // Used to create most of the tables
cap net install sg97_5, from(http://www.stata-journal.com/software/sj12-4)
cap ssc install moremata
* above command installs frmttable
* must download edfreg from Alwyn Young's web page and place in ado folder.

* Global macro ensuring the contained strata are dropped
global strata "drop if stratum == 21 | stratum == 32" 

/* Set this global macro to 1 to run the analysis on the balanced panel. Set it 
to zero to run on the full sample (including replacements). */
gl panel 1

* Reps for randomization inference
gl ri_reps 999

* Reps for power calculations
gl mde_reps 1000

* Random number seed
gl rngseed 38

* Converts SurveyCTO .csv files from follow-up data to .dta files.
do "$dir1/stata files/article stata work/do files/import_guatemala_fomin_ronda_2"

* Data prep
foreach i in 1 2 3 4 5 6a 7 8a 9 10 11 12 13 14 15 16 17 18a 19 20 21a 22 {
	do "$dir1/stata files/article stata work/do files/guatemala_basic_data_prep_`i'.do"
}

* Analysis
foreach i in 1 2 3 4 5 6 7 8 9 {
	do "$dir1/stata files/article stata work/do files/guatemala_analysis_`i'.do"
}
* Run R code before running last set of .do files
foreach i in /* 10 */ 11 12 13 14 15 16 17 18 {
	do "$dir1/stata files/article stata work/do files/guatemala_analysis_`i'.do"
}
