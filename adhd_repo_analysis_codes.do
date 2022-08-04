/*----------------------------------

Analysis codes for:
Using quantitative trait in adults with ADHD to test predictions of dual-process theory
E Persson, M Heilig, G Tinghög, A J Capusan


Emil Persson (emil.persson@liu.se) with Stata v14.2

------------------------------------ */



****** Supplementary Table S2 - realized sample size at each stage of the study for our main dependent variables
use adhd_repo_data.dta, clear
tabstat GmeanRiskans LmeanRiskans meanAltruans meanmoralAccept meanDiscSoon CRT_nCorr, stat(n) by(adhd) /* n begun (see table note) */


****** CRT & Jellybean task
use adhd_repo_data.dta, clear
tabstat CRT_nCorr, stat(mean sd n) by(adhd) /* summary stat reported in ms text */
regr CRT_nCorr adhd age female, r cformat(%4.3f) /* result reported in ms text */
tab jellybean adhd, col chi /* result reported in ms text */


****** Section 4.1 Altruistic behavior
use adhd_repo_data.dta, clear
tabstat meanAltruans, stat(mean sd n) by(adhd) /* summary stat reported in ms text */
regr meanAltruans adhd age female, r cformat(%4.3f) /* main result reported in ms text & Table S3 */

* Fig 1A (histogram)
use adhd_repo_data.dta, clear
byhist meanAltruans, by(adhd) dens disc ///
	tw1(fcolor(maroon) lcolor(black) lwidth(medthick)) ///
	tw2(fcolor(navy) lcolor(black) lwidth(medthick)) ///
	tw(legend(order(1 "Control" 2 "ADHD" )cols(1) region(lc(gs16)) symxsize(7) ring(0) bplacement(neast) textfirst) ///
	xlab(0(.25)1) xtitle("Altruistic choices [prop.]", size(vlarge) height(8)) subtitle(" ", fcolor(white) lcolor(white)) ///
	yscale(range(0(0.5)2.5)) ylab(0(0.5)2.5, nogrid angle(horizontal)) ytitle("Density", size(vlarge) height(5)) ///
	graphregion(color(gs16)) text(2.5 -0.05 "A", placement(seast) size(vlarge)))
	
	
****** Section 4.2 Moral judgment
use adhd_repo_data.dta, clear
tabstat meanmoralAccept, stat(mean sd n) by(adhd) /* summary stat reported in ms text */
regr meanmoralAccept adhd age female, r cformat(%4.3f) /* main result reported in ms text & Table S3 */

* Fig 2A (histogram)
use adhd_repo_data.dta, clear
byhist meanmoralAccept, by(adhd) dens disc ///
	tw1(fcolor(maroon) lcolor(black) lwidth(medthick)) ///
	tw2(fcolor(navy) lcolor(black) lwidth(medthick)) ///
	tw(legend(order(1 "Control" 2 "ADHD" )cols(1) region(lc(gs16)) symxsize(7) ring(0) bplacement(neast) textfirst) ///
	xlab(0(.20)1) xtitle("Utilitarian choices [prop.]", size(vlarge) height(8)) subtitle(" ", fcolor(white) lcolor(white)) ///
	yscale(range(0(0.5)2)) ylab(0(0.5)2, nogrid angle(horizontal)) ytitle("Density", size(vlarge) height(5)) ///
	graphregion(color(gs16)) text(2 -0.05 "A", placement(seast) size(vlarge)))


****** Section 4.3 Decisions involving risks
use adhd_repo_data.dta, clear

* Gains
tabstat risk_g_35, stat(mean sd n) by(adhd) /* summary stat reported in ms text. Note that n is slightly lower because main dep (GmeanRiskans) is the prop. of chocies where risky option was chosen calc for each subj but some subjects dropped out during this stage and thus n is slightly lower for some of the separate gambles (here risk_g_35) */
tabstat GmeanRiskans, stat(mean sd n) by(adhd) /* summary stat reported in ms text for gain domain */
regr GmeanRiskans adhd age female, r cformat(%4.3f) /* main result reported in ms text & Table S3 */

* Losses
tabstat LmeanRiskans, stat(mean sd n) by(adhd) /* summary stat reported in ms text for loss domain */
regr LmeanRiskans adhd age female, r cformat(%4.3f) /* main result reported in ms text & Table S3 */

* Gains vs losses
gen Rdiff = GmeanRiskans-LmeanRiskans /* for each subj: difference in prop of chocies where risky option was chosen between gain and loss domains */
tabstat Rdiff, stat(mean sd n) by(adhd) /* summary stat reported in ms text for domain sensitivity */
regr Rdiff adhd age female, r cformat(%4.3f) /* main result reported in ms text */

* Figs 3A-B
use adhd_repo_data.dta, clear
rename risk_g_35 r_1
rename risk_g_40 r_2
rename risk_g_45 r_3
rename risk_g_50 r_4
rename risk_l_35 rl_1
rename risk_l_40 rl_2
rename risk_l_45 rl_3
rename risk_l_50 rl_4
collapse (mean) r_1-rl_4, by(adhd) /* Gets us mean values (over subjects) for each trial (corresponds to one point on the x-axis) */
reshape long r_ rl_, i(adhd)
reshape wide r_ rl_, i(_j) j(adhd)
tw conn r_0 r_1 _j, ///
	xtitle("Trial", size(vlarge) height(8)) ytitle("Risky choices [prop.]", size(vlarge) height(5)) ///
	xscale(range(0.5(1)4.5)) xlab(1 `" "35 vs" "100/0" "' 2 `" "40 vs" "100/0" "' 3 `" "45 vs" "100/0" "' 4 `" "50 vs" "100/0" "') ///
	subtitle(" ", fcolor(white) lcolor(white)) ///
	msymb(s s) mc(maroon navy) msize(large large) mlcolor(maroon navy) lc(maroon navy) lw(thick thick) lp(solid longdash) ///
	legend(order(2 "ADHD" 1 "Control" )cols(1) region(lc(gs16)) symxsize(15) ring(0) bplacement(seast) textfirst) ///
	yscale(range(0(.25)1)) ylab(0(.25)1, nogrid angle(horizontal)) ///
	title("Gain domain", color(gs1) size(vlarge)) graphregion(color(gs16)) plotregion(lcolor(gs1)) ///
	text(1 0.6 "A", placement(seast) size(vlarge))
tw conn rl_0 rl_1 _j, ///
	xtitle("Trial", size(vlarge) height(8)) ytitle("Risky choices [prop.]", size(vlarge) height(5)) ///
	xscale(range(0.5(1)4.5)) xlab(1 `" "-35 vs" "-100/0" "' 2 `" "-40 vs" "-100/0" "' 3 `" "-45 vs" "-100/0" "' 4 `" "-50 vs" "-100/0" "') ///
	subtitle(" ", fcolor(white) lcolor(white)) ///
	msymb(s s) mc(maroon navy) msize(large large) mlcolor(maroon navy) lc(maroon navy) lw(thick thick) lp(solid longdash) ///
	legend(order(2 "ADHD" 1 "Control" )cols(1) region(lc(gs16)) symxsize(15) ring(0) bplacement(seast) textfirst) ///
	yscale(range(0(.25)1)) ylab(0(.25)1, nogrid angle(horizontal)) ///
	title("Loss domain", color(gs1) size(vlarge)) graphregion(color(gs16)) plotregion(lcolor(gs1)) ///
	text(1 0.6 "B", placement(seast) size(vlarge))


****** Section 4.4 Intertemporal choices
use adhd_repo_data.dta, clear
tabstat disc_1_2_soon, stat(mean sd n) by(adhd) /* summary stat reported in ms text. See comment under risk-summary-stat above for expl why n is slightly lower (one obs) here vs main dep var (meanDiscSoon) */
tabstat meanDiscSoon, stat(mean sd n) by(adhd) /* summary stat reported in ms text */
regr meanDiscSoon adhd age female, r cformat(%4.3f) /* main result reported in ms text & Table S3 */

* Figs 4A-B
use adhd_repo_data.dta, clear
rename disc_0_1_soon ds_1
rename disc_1_2_soon ds_2
rename disc_10_11_soon ds_3
rename disc_20_21_soon ds_4
rename disc_0_5_soon dl_1
rename disc_1_5_soon dl_2
rename disc_10_15_soon dl_3
rename disc_20_25_soon dl_4
collapse (mean) ds_1-dl_4, by(adhd)
reshape long ds_ dl_, i(adhd)
reshape wide ds_ dl_, i(_j) j(adhd)
tw conn ds_0 ds_1 _j, ///
	xtitle("Front-end delay", size(vlarge) height(8)) ytitle("Impatient choices [prop.]", size(vlarge) height(5)) ///
	xscale(range(0.5(1)4.5)) xlab(1 "0 days" 2 "1 day" 3 "10 days" 4 "20 days") ///
	subtitle(" ", fcolor(white) lcolor(white)) ///
	msymb(s s) mc(maroon navy) msize(large large) mlcolor(maroon navy) lc(maroon navy) lw(thick thick) lp(solid longdash) ///
	legend(order(2 "ADHD" 1 "Control" )cols(1) region(lc(gs16)) symxsize(15) ring(0) bplacement(neast) textfirst) ///
	yscale(range(0(.25)1)) ylab(0(.25)1, nogrid angle(horizontal)) ///
	title("Short horizon [1 day]", color(gs1) size(vlarge)) graphregion(color(gs16)) plotregion(lcolor(gs1)) ///
	text(1 0.6 "A", placement(seast) size(vlarge))
tw conn dl_0 dl_1 _j, ///
	xtitle("Front-end delay", size(vlarge) height(8)) ytitle("Impatient choices [prop.]", size(vlarge) height(5)) ///
	xscale(range(0.5(1)4.5)) xlab(1 "0 days" 2 "1 day" 3 "10 days" 4 "20 days") ///
	subtitle(" ", fcolor(white) lcolor(white)) ///
	msymb(s s) mc(maroon navy) msize(large large) mlcolor(maroon navy) lc(maroon navy) lw(thick thick) lp(solid longdash) ///
	legend(order(2 "ADHD" 1 "Control" )cols(1) region(lc(gs16)) symxsize(15) ring(0) bplacement(neast) textfirst) ///
	yscale(range(0(.25)1)) ylab(0(.25)1, nogrid angle(horizontal)) ///
	title("Long horizon [4-5 days]", color(gs1) size(vlarge)) graphregion(color(gs16)) plotregion(lcolor(gs1)) ///
	text(1 0.6 "B", placement(seast) size(vlarge))


* Supplementary Table S5 - model each choice (regression w clusters)
use adhd_repo_data.dta, clear
gen ID=_n /* generate identifier for individuals */
rename disc_0_1_soon Disc_1
rename disc_0_5_soon Disc_2
rename disc_1_2_soon Disc_3
rename disc_1_5_soon Disc_4
rename disc_10_11_soon Disc_5
rename disc_10_15_soon Disc_6
rename disc_20_21_soon Disc_7
rename disc_20_25_soon Disc_8
reshape long Disc_, i(ID)
label var Disc_ "1=sbj chose sooner reward, 0=later"
gen horizon = (_j==2|_j==4|_j==6|_j==8)
label var horizon "1=long (4d/5d), 0=short (1d)"
gen front_end_d = 0 if _j==1|_j==2
replace front_end_d = 1 if _j==3|_j==4
replace front_end_d = 10 if _j==5|_j==6
replace front_end_d = 20 if _j==7|_j==8
label var front_end_d "# days to sooner reward"
regr Disc_ 1.adhd age female, cl(ID) cformat(%4.3f) /* supp Table S5 column 1*/
regr Disc_ 1.adhd i.horizon age female, cl(ID) cformat(%4.3f) /* supp Table S5 column 2*/
regr Disc_ 1.adhd i.horizon ib0.front_end_d age female, cl(ID) cformat(%4.3f) /* supp Table S5 column 3*/

