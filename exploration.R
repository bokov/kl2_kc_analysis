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

#' ### Consistency-Checks
#' 
#' How well does sex match up between the EMRs and NAACCR?
with(dat2,table(sex_cd,n_sex,useNA = 'always')) %>% addmargins() %>% pander(split.tables=600);

#' How well does race match up between the EMRs and NAACCR?
with(dat2,table(race_cd,a_n_race,useNA = 'always')) %>% addmargins() %>% pander(split.tables=600);

#' How well does Hispanic ethnicity match up between the EMRs and NAACCR?
with(dat2,table(n_hisp,e_hisp,useNA = 'always')) %>% 
  addmargins() %>% pander();

#' How many patients are in NAACCR, the EMR, both, neither, or have a diagnosis
#' prior to first available record?
# rbind(consort_table
#       ,summarise(consort_table
#                  ,NAACCR='',EMR='',PreExisting='',N=sum(N))) %>% pander;
consort_table[with(consort_table,order(PreExisting,decreasing = T)),] %>% 
  mutate(`N Cumulative`=rev(cumsum(rev(N)))) %>% pander;

#' What is the overall response range for the lag from diagnosis to surgery?
subset(dat1,eval(subs_criteria$diag_surg)) %>% 
  summarise_all(function(xx) last(na.omit(xx))) %>%
  survfit(Surv(a_tdiag,a_csurg)~1,.) %>% 
  autoplot(mark.time=T,xlab='Days Since Diagnosis',ylab='% Not Undergone Surgery');

#' What is the overall response range recurrence-free survival after surgery?
subset(dat1,eval(subs_criteria$surg_drecur)) %>% 
  summarise_all(function(xx) last(na.omit(xx))) %>%
  survfit(Surv(a_tsurg,pmax(a_csurg,a_cdeath,na.rm=T))~1,.) %>% 
  autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving in Remission');

#' What is the overall response range for survival after surgery?
subset(dat1,eval(subs_criteria$surg_death)) %>% 
  summarise_all(function(xx) last(na.omit(xx))) %>%
  survfit(Surv(a_tsurg,a_cdeath)~1,.) %>% 
  autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving');

#' There are `r length(setdiff(kcpatients.emr,kcpatients.naaccr))` patients with
#' active kidney cancer diagnoses in the EMR that are not in NAACCR, 
#' `r length(setdiff(kcpatients.naaccr,kcpatients.emr))` patients that are in
#' NAACCR that do not have active kidney cancer diagnoses in the EMR, 
#' `r length(intersect(kcpatients.emr,kcpatients.naaccr))` that are in both, and
#' `r length(setdiff(dat1$patient_num,union(kcpatients.emr,kcpatients.naaccr)))`
#' are not in either, for a total of `r length(unique(dat1$patient_num))` patients
#' in the dataset.

#' 
#' ### Next steps
#' 
#' * TODO: tableOne
#' * DONE: Create time-since-first-diagnosis variable
#' * DONE: Create TTE variable for death (several raw variables)
#' * DONE: Create TTE variable for recurrence
#' * DONE: Create TTE variable for surgery date
#' * TODO: Plot time from diagnosis to surgery, hisp vs non
#' * DONE: Create censoring variable for surgery
#' * DONE: Create censoring variable for recurrence/death
#' * TODO: Create unified Hispanic indicator
#' * DONE: Map cancer status variable (didn't turn out to be useful)
#' * DONE: Create unified comorbidity variable for:
#'     * DONE Diabetes
#'     * Others?
#' * DONE: Mappings for other numcode variables
#' * TODO: Follow up re additional patient linkages, more recent NAACCR data
#' * TODO: Re-run query with additional variables:
#'     * EMR codes for secondary tumors
#'     * median household income, 2016 and 2013
#'     * HbA1c
#'     * Family history of diabetes and cancer
#' * TODO: Clean up TNM variables, in consultation with domain expert (Peter?)
#' 
#' ### Audit trail
walktrail()[,-5] %>% pander(split.tables=600);
