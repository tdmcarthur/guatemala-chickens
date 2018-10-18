clear
clear matrix
clear mata
clear results
clear programs

cap log close
set maxvar 32767
set seed 13489

log using ///
"$dir1/stata files/impact analysis revision 1/logs/scratch.smcl", replace

use "$dir1/stata files/impact analysis/data files/hhroster.dta", clear

* set to 1 to just use panel households
local pan 1

* reps for randomization inference
local reps = 999

* HH level ITT
gl outcomes_hh gastos_perAME foodcons_perc pobreza pobreza_extrema calorias_percap ASFproteina_percap egg_freq chicken_num egg_production sold_eggs 
foreach v of varlist gastos_perAME foodcons_perc calorias_percap  {
	local l`v' : var lab `v'
	lab var `v' "`l`v'' (log)"
	replace `v' = ln(`v')
}

foreach v of varlist ASFproteina_percap chicken_num egg_production {
	local l`v' : var lab `v'
	lab var `v' "`l`v'' (log)"
	replace `v' = ln(`v' + (`v'*`v' + 1)^.5)
}

foreach v of varlist * {
	local l`v' : variable label `v'
	cap local ll`v': val lab `v'
	if `"`l`v''"' == "" {
	local l`v' "`v'"
	}
}
collapse (firstnm) treat $outcomes_hh cluster stratum comunidad_text dose, by(ronda sbjnum)

foreach v of varlist * {
	cap label var `v' "`l`v''"
	cap label val `v' "`ll`v''"
}

* Drop contaminated strata
$strata

* Set to panel or not
if `pan' == 1 {
	bysort sbjnum: egen a = max(ronda)
	bysort sbjnum: egen b = min(ronda)
	drop if a != 2 | b != 1
	drop a b
}
drop if ronda == 1
* Stratum dummies
qui tab stratum, g(dstrat)
drop dstrat1

edfreg foodcons_perc treat dstrat*, cluster(cluster)

* Now try to replicate this.
*sort cluster, stable // not necessary

qui tab cluster, g(dclus) // Cluster dummies

putmata x = (treat dstrat*) G = (dclus*), replace

mata
x = x, J(rows(x), 1, 1) // add a constant
w = (1\J(cols(x)-1, 1, 0)) // Change sumthe w vector to test different coefficients
// for example, to test whether coefficients one and zero are equal, use w = 1\-1\J(cols(x)-2,1,0)
z = x*invsym(x'x)*w // see equation 8 in Young (2016)
block_z = (G :* z)*(G :* z)' // block diagonal matrix
M = I(rows(x)) - x*invsym(x'x)*x' // "residual maker"
c = cols(G)/(cols(G) - 1)*(rows(x) - 1)/(rows(x) - cols(x)) // finite sample 
// correction, same as Stata CRVE
Bx = c*M*block_z*M/(z'z) // see equation 9 in Young (2016)
Mu = trace(Bx) // This is the proportional bias of the CRVE. We divide Stata's
// default CRVE by this to obtain bias-reduced variance. In other words, 
// Stata default cluster-robust standard error for the hypothesis being tested 
// gets divided by the square root of Mu.
v = 2*trace(Bx*Bx)
edf = 2*Mu*Mu/v // degrees of freedom for hypothesis being tested given by edf
end

log close
