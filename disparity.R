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
# packages ----
# Add the names of packages (enclosed in quotes) you need to this vector
.projpackages <- c('pander','dplyr','survival','ggfortify','forcats','broom'
                   ,'gridExtra','MASS');
# If you want to reuse calculations done by other scripts, add them to `.deps`
.deps <- c( 'data.R','dictionary.R' );
#+ load_deps, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
source('functions.R');
library(tidbits,quietly = TRUE);
source('scripts/functions.R');
# return both colname and varname
formals(v)$retcol <- c('colname','varname');
load_deps2(.deps,render = FALSE);
.origfiles <- ls();
# Edit the next line only if you copy or rename this file (make the new name the argument to `current_scriptname()`)
.currentscript <- current_scriptname('disparity.R');
# instrequire ----
instrequire(.projpackages);
# set options ----
options(tb.retcol='colname');
panderOptions('p.copula', ', and ');
panderOptions('p.wrap', '`');
#' # Previous Analysis
#'
#' ![Analysis from February 9, 2018](images/old_table.png)
#'
#' ![Curves from February 9, 2018](images/old_survcurve.png)
#'
#' # Replicating Previous Analysis
#'
#+ subset_dat1hisp, echo=FALSE, message=FALSE
# trainsample ----
dat1 <- subset(dat1,patient_num %in% pat_samples$train);
# dat1hsp ----
dat1hsp <- .dat1hsp <- subset(dat1,between(a_emr_tdiag,0,2723) &
                                a_emr_crecur < 2) %>%
  group_modify(function(xx,...) as_tibble(sapply(names(xx),function(ii){
    if(ii %in% v(c_eunivar)) first(na.omit(xx[[ii]])) else {
      last(na.omit(xx[[ii]]))}
    },simplify = F))) %>%
  # excluding those with secondary tumors within 30 days or lost to followup
  # within 30 days to avoid the sharp early dropoff making the predictors
  # look better than they really are
  subset(a_emr_tdiag>30);
# Recode the missing values so they are included as a valid level
dat1hsp[,v(c_valflag,.dat1hsp)] <-
  sapply(.dat1hsp[,v(c_valflag,.dat1hsp)],function(xx){
    ifelse(is.na(xx),'Other',xx) %>% factor(levels=c('Normal','Other'
                                                     ,'Lo','Hi')) %>%
      fct_lump_min(15,other_level = 'Other') %>% fct_relevel('Other')}
    ,simplify = F);

#' # Univariate Predictors
#'
#+ univar
# univar ----
cox_null <- coxph(Surv(a_emr_tdiag,a_emr_crecur==1) ~1,data=dat1hsp);
srv_null <- survfit(Surv(a_emr_tdiag,a_emr_crecur==1)~1,data=dat1hsp);
# create univariate models
cox_univar <- list();
#cox_univar <- sapply(v(c_eunivar,dat1hsp),function(xx){
for(xx in v(c_eunivar,dat1hsp)){
  xxcoxph <- update(cox_null,paste0('.~',xx));
  xxsurvfit <- update(srv_null,paste0('.~',xx));
  if(is.numeric(dat1hsp[[xx]])){
    xxsurvfit <- update(xxsurvfit,sprintf('.~%1$s>median(%1$s,na.rm=TRUE)',xx))};
  cox_univar[[xx]] <- list(cox=xxcoxph,srv=xxsurvfit);
}
#,simplify=FALSE);

padj_cox_univar <- sapply(cox_univar,function(xx){
  broom::glance(xx$cox)$p.value.wald}) %>% sort %>% p.adjust(method = 'fdr');
v_univarshortlist <- names(padj_cox_univar);
# comment the below out to see all the plots... or change the cutoff below
v_univarshortlist <- names(padj_cox_univar)[padj_cox_univar<.2];

#' ## KM plots and Cox PH tables for the univariate predictors under consideration
#'
#' The 'short list' of univariate predictors is: `r p(v_univarshortlist)`. These
#' were selected on the basis of having an FDR <=
#' `r round(max(padj_cox_univar[v_univarshortlist]),3) ` .
#' `
#+ univar_results,results='asis'
# univar_results ----
# https://stackoverflow.com/a/31627333/945039
panderOptions('knitr.auto.asis', FALSE);
for(ii in v_univarshortlist){
  cat('\n\n***\n\n')
  autoplot(cox_univar[[ii]]$srv,main=ii) %>% print();
  #print(pander_return(cox_univar[[ii]]$cox,justify='right'));
  cat('\n\n\n');
  cat(pander(cox_univar[[ii]]$cox,justify='right', split.tables = Inf));
  cat('\n\n\n');
}
#+ cox_diag_recur_emr, echo=FALSE

cox_diag_recur_emr <- coxph(Surv(a_emr_tdiag,a_emr_crecur==1) ~
                              e_hisp + e_rdwrbc + e_opd_anlgscs
                            ,subset(dat1hsp,!is.na(e_rdwrbc)));
panderOptions('knitr.auto.asis', TRUE);
#'
#' ## Multivariate Model Selection
#'
#+ cox_mv_aic, message=FALSE, warning=FALSE
# cox_mv_aic ----
cox_multivar <- sapply(names(padj_cox_univar),function(xx){
  mean(!is.na(dat1hsp[[xx]]))}) %>%
  # steps on below line amount to "select the variables with no missing values"
  `[`(.==1) %>% names %>% paste(collapse ='+') %>%
  # turn them into a right-sided formula
  sprintf('~(%s)^3',.) %>% formula %>%
  # put it into a list and use as the 'scope' argument for 'stepAIC()'
  list(lower=~1,upper=.) %>% MASS::stepAIC(cox_null,.,trace=0);
cox_mv_lp <- predict(cox_multivar);
srv_mv <- update(srv_null,.~factor(cox_mv_lp>median(cox_mv_lp)
                                   ,levels=c(FALSE,TRUE)
                                   ,labels=c('Low','High')));
autoplot(srv_mv);
cat('\n\n\n')
pander(cox_multivar,justify='right', split.tables = Inf);
#'
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
