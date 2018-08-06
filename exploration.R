#' ---
#' title: "Kidney Cancer Data Exploration"
#' author: "Alex F. Bokov"
#' date: "08/04/2018"
#' ---
#' 
#+ echo=FALSE, inlcude=FALSE, message=FALSE
# if running in test-mode, uncomment the line below
options(gitstamp_prod=F);
.junk<-capture.output(source('global.R',echo=F));
.depends <- 'data.R';
.depdata <- paste0(.depends,'.rdata');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
project_seed <- 20180803;
if(!file.exists(.depdata)) system(sprintf('R -e "source(\'%s\')"',.depends));
.loadedobjects <- tload(.depdata);

#' How well does sex match up between the EMRs and NAACCR?
with(dat2,table(sex_cd,v011_sx,useNA = 'always')) %>% addmargins() %>% pander();

#' How well does race match up between the EMRs and NAACCR?
with(dat2,table(race_cd,v005_rc,useNA = 'always')) %>% addmargins() %>% pander();

#' How well does Hispanic ethnicity match up between the EMRs and NAACCR?
with(dat2,table(v044_hspnc_or_ltn,v010_spnsh_hspnc,useNA = 'always')) %>% 
  addmargins() %>% pander();

#' ### Next steps
#' 
#' * TODO: Create time-since-first-diagnosis variable
#' * TODO: Create TTE variable for death (several raw variables)
#' * TODO: Create TTE variable for recurrence
# example
# baz <- group_by(dat0,patient_num) %>% mutate(a_tterecur = tte(age_at_visit_days,!is.na(v001_rcrnc_dt_st)),n_after=sum(a_tterecur<0),n_before=sum(a_tterecur>0))
#' * TODO: Create TTE variable for surgery date
#' * TODO: Create censoring variable for surgery/death
#' * TODO: Create censoring variable for recurrence/death
#' * TODO: Create unified Hispanic indicator
#' * TODO: Map cancer status variable
#' * TODO: Create unified comorbidity variable for:
#'     * Diabetes
#'     * Others?
#' * TODO: Mappings for other numcode variables
#' * TODO: Follow up re additional patient linkages, more recent NAACCR data
#' * TODO: Re-run query with additional variables:
#'     * EMR codes for metastasis
#'     * median household income, 2016 and 2013
#'     * HbA1c
#'     * Family history of diabetes and cancer
#' * TODO: Clean up TNM variables, in consultation with domain expert (Peter?)
#' 
#' ### Audit trail
walktrail()[,-5] %>% pander(split.tables=600);
