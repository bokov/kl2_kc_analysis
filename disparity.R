#' ---
#' title: "Attempting to Replicate Disparity Results from Old Data"
#' author:
#' - "Alex F. Bokov^[UT Health San Antonio]"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' tags: ["data characterization", "preliminary", "NAACCR", "urology", "cancer"]
#' link-citations: true
#' css: production.css
#' bibliography: kidneycancer.bib
#' csl: nature.csl
#' reference-section-title: Bibliography
#' fig_caption: yes
#' linkReferences: true
#' nameInLink: true
#' tblLabels: "roman"
#' tblPrefix: ["table","tables"]
#' output:
#'  html_document:
#'   keep_md: true
#'   pandoc_args: ["--filter", "pandoc-crossref"]
#'  word_document:
#'   reference_docx: 'nt_styletemplate.docx'
#'   keep_md: true
#'   pandoc_args: ["--filter", "pandoc-crossref"]
#'  pdf_document:
#'   keep_md: true
#'   pandoc_args: ["--filter", "pandoc-crossref"]
#' ---
#'
#+ set_vars, echo=FALSE, message=FALSE, results='hide'
knitr::opts_chunk$set(echo=FALSE);
# Add the names of packages (enclosed in quotes) you need to this vector
.projpackages <- c('pander','dplyr','survival','ggfortify');
# If you want to reuse calculations done by other scripts, add them to `.deps`
.deps <- c( 'data.R' );
#+ load_deps, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
source('functions.R');
library(tidbits);
source('scripts/functions.R');
load_deps2(.deps,render = FALSE);
.origfiles <- ls();
# Edit the next line only if you copy or rename this file (make the new name the argument to `current_scriptname()`)
.currentscript <- current_scriptname('disparity.R');
instrequire(.projpackages);
#' # Previous Analysis
#'
#' ![Analysis from February 9, 2018](images/old_table.png)
#'
#' ![Curves from February 9, 2018](images/old_survcurve.png)
#'
#' # Replicating Previous Analysis
#'
#+ subset_dat1disp, echo=FALSE, message=FALSE
# dat1hsp ----
dat1hsp <- subset(dat1,between(a_emr_tdiag,0,2723) & a_emr_crecur < 2) %>%
  summarise_all(function(xx) last(na.omit(xx)));
#+ cox_diag_recur_emr, echo=FALSE
cox_diag_recur_emr <- coxph(Surv(a_emr_tdiag,a_emr_crecur==1) ~
                              e_hisp + e_rdwrbc + e_opd_anlgscs
                            ,subset(dat1hsp,!is.na(e_rdwrbc)));
#' ## Cox Proportional Hazard
#'
#' Full model from last time
pander(cox_diag_recur_emr);
#' This version only uses `e_hisp` as a predictor
pander(cox_diag_recur_emr_hsp <- update(cox_diag_recur_emr,.~e_hisp));
#' This version omits `e_hisp`
pander(update(cox_diag_recur_emr_nohsp <- cox_diag_recur_emr,.~.-e_hisp));
#'
#' ## KM Plot Using Linear Predictor from Cox Proportional Hazard Models
#'
#' Full model
survfit(Surv(a_emr_tdiag,a_emr_crecur==1)~predict(cox_diag_recur_emr)>0
        ,subset(dat1hsp,!is.na(e_rdwrbc))) %>% autoplot();
#'
#' Ethnicity-only
#'
survfit(Surv(a_emr_tdiag,a_emr_crecur==1)~predict(cox_diag_recur_emr_hsp)>0
        ,subset(dat1hsp,!is.na(e_rdwrbc))) %>% autoplot();
#'
#'
#' No Ethnicity
#'
survfit(Surv(a_emr_tdiag,a_emr_crecur==1)~predict(cox_diag_recur_emr_nohsp)>0
        ,subset(dat1hsp,!is.na(e_rdwrbc))) %>% autoplot();
#'
#' # Next milestones:
#'
#' ## ~~Re-run the original analysis that showed the disparity in our data  (UT Med + UHS data)~~
#'
#' The following variables were used by the earlier analysis:
#'
#' * `v029_Hspnc_or_Ltn` is the Hispanic ethnicity indicator
#'     * corresponds to `e_hisp` for EMR/i2b2
#'     * corresponds to `a_hisp_naaccr` for our NAACCR registry.
#' * `v050_RDW_RBC_At_Rt_GENERIC_KUH_COMPONENT_ID_5629_numnona`, the red bloodcell count.
#'     * Corresponds to `v113_rdw_rbc_at_rt_788_0_num` in EMR/i2b2
#'     * **NO EQUIVALENT AVAILABLE IN NAACCR**
#' * `v037_CN_ANLGSCS`, analgesics,
#'     * Split between `v110_opd_anlgscs` (opioid) and `v123_nn_opd_anlgscs` (non-opioid) in EMR/i2b2
#'     * **NO EQUIVALENT AVAILABLE IN NAACCR**
#' * `a_cens_1` is the event indicator variable, which in this case is the earliest occurrence of a
#'   secondary neoplasm as represented by any of these variables: `v003_Scndr_nrndcrn`, `v004_mlgnt_unspcfd`, `v005_rsprtr_dgstv`,
#'   and `v006_unspcfd_mlgnt`.
#'     * In the EMR/i2b2 data they are respectively: `v008_scndr_nrndcrn`, `v008_scndr_nrndcrn_inactive`,
#'       `v009_mlgnt_unspcfd`, `v009_mlgnt_unspcfd_inactive`, `v010_rsprtr_dgstv`, `v010_rsprtr_dgstv_inactive`, `v011_unspcfd_mlgnt`,
#'       `v011_unspcfd_mlgnt_inactive`, `v012_unspcfd_mlgnt`, `v012_unspcfd_mlgnt_inactive`, `v013_rsprtr_dgstv`, `v013_rsprtr_dgstv_inactive`,
#'       `v014_mlgnt_spcfd`, `v014_mlgnt_spcfd_inactive`.
#'     * In the NAACCR data this information can be obtained from the `a_n_recur` variable.
#' * `a_dxage3` was obtained by getting the patient's age at the earliest of: their first secondary neoplasm
#'   (i.e. `a_cens_1` is equal to `1`), their last followup, or 2723 days (in the old data, the last recurrence to be observed) and then that
#'   patient's age at initial diagnosis was subtracted from this quantity, so that `a_dxage3` is (supposed to be) the number of days from initial
#'   diagnosis to first recurrence.
#'     * As I showed previously, deriving initial diagnosis from EMR data is problematic because it disagrees with the NAACCR records for the same
#'       patients. Nevertheless, the `a_e_kc` is the analytic variable in the EMR/i2b2 data that has this information.
#'     * The time of diagnosis for NAACCR is encoded by `a_tdiag`
#'
#' So, the following model should reproduce the old results:
#'
#' `Surv(a_emr_tdiag,a_emr_crecur == 1) ~ e_hisp + v113_rdw_rbc_at_rt_788_0_num + v110_opd_anlgscs`
#'
#' The following model just shows the effect of Hispanic ethnicity:
#'
#' `Surv(a_emr_tdiag,a_emr_crecur == 1) ~ e_hisp`
#'
#' TODO:
#'
#' * ~~Create analytic variables to fill in the `TIME` and `EVENT` placeholders in the above models using `a_e_kc` and the secondary tumor codes in `data.R`~~
#' * ~~Run analysis and survival plots in this script~~
#' * Use the automatic variable formatting used in `exploration.R` in this script as well so that it's readable
#'
#' Time: 2-3 days
#'
#' ## Run the same analysis in the local (UT Med) TX Cancer Registry data
#'
#' TODO:
#'
#' Only the second model can be run -- `Surv(TIME,EVENT) ~ a_hisp_naaccr`.
#'
#' Time: 1 day
#'
#' ## Run the same analysis in UT Med data enhanced with the TX Cancer Registry data
#'
#' TODO:
#'
#' One source should be treated as the authoritative one and back-filled from the other source when missing/invalid. What standards should we use for
#' missing or invalid?
#'
#' Time: 1 day _after_ above two are done and this question is answered.
#'
#' ## Request the TX Cancer Registry data and run the analysis there (depending on time the request takes, this may not be possible for this meeting but you should have the data by then)
#'
#' TODO:
#'
#' * Re-read online instructions for requesting de-id data from TCR
#' * Make list of all relevant non-id variables and then fill out the request
#' * Submit request
#'
#' Time: 1-2 days
#
#' The results are saved and available for use by other scriports if you
#' place `r sprintf("\x60'%s'\x60",.currentscript)` among the values in their
#' `.deps` variables.
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
#+ echo=FALSE, results='hide'
c()
