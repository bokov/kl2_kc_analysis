#' ---
#' title: "Kidney Cancer Data Exploration"
#' subtitle: "KL2 Aim 2"
#' author: 
#' - "Alex F. Bokov^[UT Health San Antonio]"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' tags: ["data characterization", "preliminary", "NAACCR", "urology", "cancer"]
#' thanks: ["Dr. Shawn Murphy", "Dr. Ronald Rodriguez", "Dr. Amelie Ramirez", "Dr. Joel Michalek"]
#' abstract: |
#'   Minimal necessary NAACCR variables chosen and 
#'   process documented for preparing them for analysis, as well as 
#'   supplementing some of them with additional data from EMR if available.
#'   Ready to proceed to chart review of existing data, acquisition of 
#'   independent NAACCR data, development of additional variables, and working
#'   on Aim 1.
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
#+ init, echo=FALSE, include=FALSE, message=FALSE
# init -------------------------------------------------------------------------
# if running in test-mode, uncomment the line below
#options(gitstamp_prod=F);
.junk<-capture.output(source('global.R',echo=F));

default_font <- 'Times New Roman';

.depends <- c('dictionary.R','data.R');
.depdata <- paste0(.depends,'.rdata');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- knitr::current_input();
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
project_seed <- 20180803;
.loadedobjects <- c();
for(ii in seq_along(.depends)) {
  if(!file.exists(.depdata[ii])) system(sprintf('R -e "source(\'%s\')"'
                                                ,.depends[ii]));
  .loadedobjects <- union(.loadedobjects,tload(.depdata[ii]));
}
knitr::opts_chunk$set(echo = F,warning = F,message=F,fig.scap=NA,fig.lp=''
                      ,dev.args=list(family=default_font));

# if a text string named FOO is created prior to a named chunk also named FOO
# then specifying opts.label='fig_opts' in the options for that chunk will use
# that string as the caption
knitr::opts_template$set(
  fig_opts=alist(fig.cap=get0(knitr::opts_current$get("label"))
                 ,results='asis'));

# Set default arguments for some functions
panderOptions('table.split.table',Inf);
panderOptions('missing','-');
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','left');
.args_default_v <- formals(v);

# default arguments for getting lists of column names
formals(v)[c('dat','retcol')]<-alist(dat1,c('colname','varname'));

# defaults for 'fancy span' string transformation of variable names 
# IN THE MAIN SECTION ONLY!! The retfun should be return for inline use and cat
# for use generating asis chunks.
.args_default_fs <- formals(fs);
formals(fs)[c('url','fs_reg','retfun')] <- alist(str,'fs_reg',return);
formals(fs)$template <- fstmplts$link_colnamelong;

# We don't yet explicitly reference patient_num outside the news block, so I'm 
# priming the fs_reg option with it here manually
options(fs_reg='patient_num');

# defaults for graphical parameters as used by the eventplot() function
.par_eventplot <- .par_default <- par(no.readonly = T);
.par_eventplot$family = 'Times New Roman';

# markdown snippets of boilerplate
md <- list(
  pbreak=cm('\n\n\n::::: {.pbreak custom-style="pbreak"}\n&nbsp;\n:::::\n\n\n','

Creates a page break, depends on custom styling at output end to either create a
page break and/or hide this code

')
  ,mainvars = v(c_main_naaccr_vars) %>% sapply(fs) %>% 
    (knitr::combine_words)
  );

# note_toc ---------------------------------------------------------------------
#' ###### TOC {-}
#+ news_toc,results='asis'
.news <- c("
**Note:** This is not (yet) a manuscript. We are still at the data 
cleaning/alignment stage and it is far too early to draw conclusions. Rather, 
this is a regularly updated report that I am sharing with you to keep you in the 
loop on my work and/or because you are also working on NAACCR, i2b2, Epic, or 
Sunrise because I value your perspective and perhaps my results might be useful 
to your own work.\\
\\
Only de-identified data has been used to generate these results any dates or 
[`patient_num`](#patient%5Fnum) values you see here are also de-identified (with 
size of time intervals preserved).\\
\\
This portion of the study is under Dr. Michalek's exempt project IRB number 
HSC20170563N. If you are a researcher who would like a copy of the data, please 
email me and I will get back to you with further instructions and any additional
information needed for our records.\\
\\
Verbatim names of files, variables, or values are displayed in
a special style, `like this`. Variable names are in addition linked to a 
glossary at the end of this document. This is where any relevant cleaning or 
tranformation steps will in the future be described for the respective variables. 
Tables, figures, and sections are also linked from text that references them. To 
follow a link in Word, please hold down the 'control' key and click on that 
link. [Yellow highlights]{.note2self custom-style='note2self'} are items with 
which I know I need to deal soon."
);

.toc <- rep_len(NA,length(.news));
.toc[1] <- "
* [-@sec:overview Overview]
* [-@sec:dataprep Data preparation]
* [-@sec:descplots Plots of test data]
* [4 Cohort characterization](#sec:cohorchar)
* [5 Conclusion & next steps](#sec:nextsteps)
* Appendices
____* [A1. Stage/grade export sample](#sec:stage)
____* [A2. TODO list](#sec:todo)
____* [A3. Supplementary results](#sec:supp)
____* [A4. Variable descriptions](#sec:vars)
____* [A5. Audit trail](#sec:audit)
";
.temp0 <- cbind(.news,.toc) %>% unname;
pander(.temp0,style='grid',keep.line.breaks=T,justify='left'
               ,split.cells=c(30,Inf),missing='')[1] %>% 
  gsub('_',' ',.) %>% cat;
# overview ---------------------------------------------------------------------
#' 
#' # Overview {#sec:overview}
#' 
#' A recent study of state death records [@PinheiroHighcancermortality2017]
#' reports that among US-born Texans of Hispanic ancestry (7.3 million, 27% of
#' the State's population), annual age-adjusted mortality rates for kidney
#' cancer  are 1.5-fold and 1.4-fold those of non-Hispanic whites for males and
#' females respectively. My goal is to determine whether these findings can be
#' replicated at UT Health (Aim 2) and Massachusetts General Hospital (Aim 3).
#' If there is evidence for an ethnic disparity, I will look for possible
#' _mediators_ of this disparity among socioeconomic, lifestyle, and family
#' history variables (Aim 2a). Otherwise the focus will shift to determining
#' which of these same variables are the best _predictors_ of mortality and
#' recurrence.
#' 
#' At the Clinical Informatics Research Division (CIRD) we operate an i2b2
#' [@MurphyInstrumentinghealthcare2009] data warehouse containing deidentified
#' data for over 1.3 million patients from the electronic medical record (EMR)
#' systems of the UT Health faculty practice and the University Health System
#' (UHS) county hospital. We use the HERON [@AdagarlaSEINEMethodsElectronic2015]
#' extract transform load (ETL) process to link data from multiple sources
#' including copies of monthly reports that the Mays Cancer Center sends to the
#' Texas Cancer Registry with detailed information on cancer cases including
#' dates of diagnosis, surgery, and recurrence along with stage and grade at
#' presentation. My first-pass eligibility query returned `r nrow(dat2a)`
#' patients having one or more of the following in their records: an ICD9 code
#' of 189.0 or any ICD10 code starting with C64; the NAACCR item 
#' [`0400 Primary Site`](`r paste0(urls$dict_naaccr,'#400')`) having a value 
#' starting with C64 (`r fs('n_kcancer')`); or the SEER Primary Site having a 
#' value of `r fs('n_seer_kcancer')`.
#' 
#' My second pass criteria aimed at finding among these patients those for which
#' NAACCR records also exist, defined as having a non-missing `r fs('n_ddiag')`
#' and at least one of `r fs('n_kcancer')` or `r fs('n_seer_kcancer')`. As can
#' be seen from [@tbl:cohortrectype] only `r length(kcpatients.naaccr)` of the
#' patient-set met these criteria and `r nrow(dat2a) - length(kcpatients.naaccr)` 
#' did not. In [-@sec:diag; -@sec:surg; -@sec:recur] I identified additional 
#' exclusion criteria which I will implement in the next major revision of 
#' this document.
#' 
#' In [@sec:linkagever] I summarize the evidence that NAACCR and EMR records are
#' correctly matched with each other. In [@sec:reqelmnts] I summarize the 
#' minimum set of NAACCR data elements that is sufficient to replicate my 
#' analysis in an independent NAACCR data set. In [@sec:merging] I report the
#' extent to which the completeness of NAACCR records can be improved by using
#' EMR records of the same patients. In [@sec:descplots] is a technical 
#' demonstration of the data analysis scripts (on a small random sample). In 
#' [@sec:cohorchar] there is a characterization of the full (N=`r nrow(dat2a)`)
#' patient cohort. Finally, in [@sec:nextsteps] I present my plans for 
#' overcoming the data issues I found, replicating the analysis on independent 
#' data, preparing additional variables, and starting work on Aim 1.
#' 
# dataprep ---------------------------------------------------------------------
#' # Data preparation {#sec:dataprep}
#' 
#' ## Verifying correct patient linkage {#sec:linkagever}
#' 
#' Since this is the first study at our site to make such extensive use of 
#' combined EMR and NAACCR data, it is important to first validate the data 
#' linkage done by our ETL.
#'
#' The following data elements exist in both NAACCR and the EMR, respectively:
#' date of birth (`r fs('n_dob')` and `r fs('birth_date')`), marital status 
#' (`r fs('n_marital')` and `r fs('e_marital')`), sex (`r fs('n_sex')` and 
#' `r fs('sex_cd')`), race (`r fs('a_n_race')` and `r fs('race_cd')`), and
#' Hispanic ethnicity (`r fs('n_hisp')` and `r fs('e_hisp')`). The agreement 
#' between NAACCR and the EMR is never going to be 100% for these variables
#' because of errors in the original source data as well as variation in how a 
#' patient chooses to self-report from one visit to another (race, Hispanic 
#' ancestry, and marital status are expected to be especially variable).
#' Nonetheless, if patient counts for NAACCR and EMR are tabulated against each
#' variable, then _most_ of the values should agree.
#' 
#' I have confirmed that this _is_ the case for marital status
#' ([@tbl:xc_marital]), sex ([@tbl:xc_sex]), race ([@tbl:xc_race]), and
#' Hispanic ancestry ([@tbl:xc_hisp0]). Furthermore, there are 
#' `r nrow(subset(dat3,is.na(n_dob)))` eligible patients lacking a 
#' `r fs('n_dob')` and only `r length(kcpatients.naaccr_bad_dob)` with a 
#' mismatch between `r fs('n_dob')` and `r fs('birth_date')`. Independent 
#' evidence for correct linkage is that EMR ICD9/10 codes for primary kidney 
#' cancer rarely precede `r fs('n_ddiag')` ([@fig:diag_plot]), EMR surgical 
#' history of nephrectomy and ICD9/10 codes for acquired absence of a kidney 
#' rarely precede `r fs('n_dsurg')` or `r fs('n_rx3170')` ([@fig:surg0_plot0]), 
#' and death dates from non-NAACCR sources (`r fs('e_death')`, `r fs('s_death')`
#' , and `r fs('e_dscdeath')`) rarely precede `r fs('n_vtstat')` 
#' ([@fig:death_plot]).
#' 
#' ## Required NAACCR data elements. {#sec:reqelmnts}
#' 
#' The primary outcome variables I need are date of initial diagnosis, date of 
#' surgery (if any), date of recurrence (if any), and date of death (if any). 
#' The primary predictor variable is whether or not a patient is  Hispanic. 
#' There are many covariates of interest, but these five values are the 
#' scaffolding on which the rest of the analysis will be built.
#' 
#' **I found the following NAACCR fields sufficient for deriving all the above 
#' analytic variables: `r fs('n_hisp')`, `r md$mainvars`.** More details about 
#' how these were selected can be found in the 
#' ["Which EMR and NAACCR variables are reliable event indicators?"]
#' section. In addition the following will almost certainly be needed for the
#' covariates: `r fs('n_sex')`, `r fs('n_dob')`, `r fs('n_marital')`
#' , `r fs('n_brthplc')` and any field whose name contains `Race`
#' , `Comorbid/Complication`, `Derived AJCC`, or `TNM`. For crosschecking
#' purposes it may also be useful to have `r fs('n_mets')`, `r fs('n_fc')`
#' , and `r fs('n_mult')`. Additional items are likely to be needed as this
#' project evolves, but **I believe the elements listed so far will be 
#' sufficient to replicate my analysis de-identified State or National NAACCR 
#' data**.
#' 
#' ## Merging NAACCR and EMR variables {#sec:merging}
#' 
#' EMR records can not only enrich the data with additional elements not 
#' available in NAACCR alone, but might also make it possible to fill in missing 
#' `r fs('n_ddiag')`,  `r fs('n_rx3170')` / `r fs('n_dsurg')`, 
#' `r fs('n_drecur')`, and `r fs('n_lc')`  values. It may even be possible to 
#' reconstruct entire records for the `r nrow(dat2a)-length(kcpatients.naaccr)` 
#' kidney cancer patients in the EMR lacking NAACCR records. This depends on how 
#' much the EMR and NAACCR versions of a variable agree when neither is 
#' missing.
#' 
# merging EMR and NAACCR -------------------------------------------------------
#' 
#' **Data elements representing date of death and Hispanic ethnicity are in 
#' sufficient agreement ( [@tbl:xc_hisp0] and [-@sec:death] ) to justify merging
#' information from the EMR and NAACCR.** The process for combining them is
#' described in the `r fs('a_tdeath')`, `r fs('a_hsp_strict')`, and 
#' `r fs('a_hsp_broad')` sections of [-@sec:vars] respectively. At this time I 
#' cannot merge diagnosis, surgery, or recurrence-- where data from both sources 
#' is available I found that EMR dates lag considerably behind NAACCR dates 
#' ( [-@sec:diag; -@sec:surg; -@sec:recur] ) and their variability is probably
#' larger than the effect size. The surgery and recurrence lags might be 
#' because those actual visits are not yet available in the data 
#' warehouse and I am only seeing them as reflected in the patient history 
#' at visits long after the fact. The diagnosis lag may be due to the
#' decision to proceed with surgery often being made based on imaging data,
#' [@pcRodriguez2018] with definitive pathology results only available after 
#' surgery ([-@sec:surg]). Attempting to merge these elements would bias the data 
#' and obscure the actual differences. However there are several ways forward 
#' that I will discuss in [@sec:nextsteps] below.
#' 
#' EMR data can still be used to flag records for exclusion or verification via 
#' chart review if, despite the lag, EMR codes for kidney cancer or secondary 
#' tumors precede `r fs('a_tdiag')` or `r fs('a_trecur')`  respectively. This 
#' can also apply to nephrectomy EMR codes and `r fs('a_tsurg')` but I will need 
#' to distinguish between the prior nephrectomy being due cancer versus other 
#' indications.
#' 
#' For now I am analyzing the data as if I only have access to NAACCR except
#' mortality where I do it both with ( [@fig:naaccrdeath_survfit] ) and 
#' without ( [@fig:alldeath_survfit] ) the EMR.
#' 
# descriptive plots ------------------------------------------------------------
#' # Plots of test data {#sec:descplots}
#' 
.testsamp<-table(subset(dat2a,patient_num %in% 
                          intersect(pat_samples$train
                                    ,kcpatients.naaccr))$a_hsp_naaccr);
#' The point of this section is **solely** to confirm that my scripts succeeded in 
#' turning the data into a time-to-event (TTE) format to which Kaplan-Meier 
#' curves can be fit without numeric errors or grossly implausible results.
#' All the results below are from a small random subset of the data-- 
#' N=`r sum(.testsamp[1:2])`, `r .testsamp['Hispanic']` Hispanic and 
#' `r .testsamp['non-Hispanic white']` non-Hispanic white. There were 
#' `r .testsamp['Unknown']` excluded. This is further reduced in some
#' cases as described in the figure captions. These sample sizes doe not have
#' sufficient power to detect clinically significant differences and **this is 
#' not the goal yet**. Again, the intent is only to insure that my software 
#' performs correctly without viewing the hold-out data on which the hypothesis 
#' testing will ultimately be done.
#' 
#' Furthermore, these survival curves are not yet adjusted for covariates such 
#' as age and stage at diagnosis. Finally, there are upcoming refinements 
#' to `r fs('a_hsp_naaccr')` and the exclusion criteria which I discuss below in 
#' [@sec:nextsteps].
#' 
#' In all the plots below, the time is expressed in weeks and `+` signs denote
#' censored events (the last follow-up of patients for whom the respective 
#' outcomes were never observed). The lightly-shaded regions around each line 
#' are 95% confidence intervals. In all cases a 3-year follow-up period is 
#' represented on the x-axis meaning patients for whom the outcomes 
#' occurred beyond that are censored at 3 years. The vertical
#' scales for the plots vary and are automatically determined by the data.
#' 
#' ###### blank
#' 
#' ::::: {#fig:surg_survfit custom-style="Image Caption"}
#+ surv_surg,results='asis',fig.dim=c(3.1,3),fig.align='center'
#,fig.align='right',fig.dim=c(5,3)
(.survfit_plot0 <- survfit_wrapper(dat2a,'a_tsurg',censrvars = c()
                                   ,startvars = 'a_tdiag'
                                   ,predvars = 'a_hsp_naaccr'
                                   ,default.censrvars = 'n_lc'
                                   # Reduce(intersect,list(...)) is how one can
                                   # do intersect with >2 vectors
                                   ,subs = a_tdiag <= n_lc & patient_num %in% 
                                     Reduce(intersect,list(kcpatients.naaccr
                                                           ,pat_samples$train))
                                   ,followup = 365.25*3,scale=7,unit='Weeks'
                                   ,main='Time from diagnosis to surgery\n'
                                   ,ylab='Fraction not undergone surgery'
                                   ,xlab='Weeks since diagnosis'
                                   ,plotargs=list(mark.time=T,conf.int.alpha=0.1
                                                  ,surv.size=1.5)
                                   ,plotadd = list(
                                     guides(colour=guide_legend('')
                                            ,fill=guide_legend(''))
                                     ,theme_light()
                                     ,theme(legend.position = 'top')))
)$plot;
cat('

Number of weeks elapsed from ',fs('a_tdiag'),' (time 0) to ',fs('a_tsurg')
,' for ',.survfit_plot0$fit$n[1],' Hispanic and ',.survfit_plot0$fit$n[2]
,' non-Hispanic white patients');
#' :::::
#' 
#' ###### blank
#' 
#' ::::: {#fig:recur_survfit custom-style="Image Caption"}
#+ surv_recur,results='asis',fig.dim=c(3.1,3),fig.align='center'
(.survfit_plot1 <- update(.survfit_plot0,eventvars='a_trecur'
                          ,startvars='a_tsurg'
                          # turns out there needs to be a requirement that
                          # the startvars be no larger than the censrvars
                          # (their respective pmins actually, but right now 
                          # there) is just one of each.
                          ,subs = a_tsurg<=n_lc & patient_num %in% 
                            Reduce(intersect,list(kcpatients.naaccr
                                                  ,kcpatients.surg
                                                  ,pat_samples$train))
                          ,main='Time from surgery to recurrence\n'
                          ,ylab='Fraction recurrence-free'
                          ,xlab='Weeks since surgery'))$plot;
cat('

Number of weeks elapsed from ',fs('a_tsurg'),' (time 0) to ',fs('a_trecur')
,' for ',.survfit_plot1$fit$n[1],' Hispanic  and ',.survfit_plot1$fit$n[2]
,' non-Hispanic white patients. The numbers are lower than for 
[@fig:surg_survfit]  because patients not undergoing surgery are excluded');
#' :::::
#' 
#' 
#' ###### blank
#' 
#' ::::: {#fig:naaccrdeath_survfit custom-style="Image Caption"}
#+ naaccrdeath_survfit,results='asis',fig.dim=c(3.1,3),fig.align='center'
(.survfit_plot2 <- update(.survfit_plot1,eventvars='n_vtstat'
                          ,main='Time from surgery to death\n'
                          ,ylab='Fraction alive'))$plot;
cat('

Like [@fig:recur_survfit] except now the outcome is ',fs('n_vtstat')
,' for ',.survfit_plot2$fit$n[1],' Hispanic  and ',.survfit_plot2$fit$n[2]
,' non-Hispanic white patients');
#' :::::
#' 
#' 
#' ###### blank
#' 
#' ::::: {#fig:alldeath_survfit custom-style="Image Caption"}
#+ alldeath_survfit,results='asis',fig.dim=c(3.1,3),fig.align='center'
(.survfit_plot2a <- update(
  .survfit_plot2,eventvars='a_tdeath'
  ,predvars='a_hsp_broad'
  ,default.censrvars='age_at_visit_days'
  ,main='Time from surgery to death\nsupplemented with EMR data'))$plot
cat('

Like [@fig:naaccrdeath_survfit] but now supplemented EMR information to see how
much of a difference it makes. For the predictor ',fs('a_hsp_broad'),' is used 
instead of ',fs('a_hsp_naaccr'),' and for the outcome ',fs('a_tdeath'),' is used
instead of ',fs('n_vtstat'),'. There were ',.survfit_plot2a$fit$n[1]
,' Hispanic  and ',.survfit_plot2a$fit$n[2],' non-Hispanic white patients. There 
were ',sum(.survfit_plot2$fit$n.censor)-sum(.survfit_plot2a$fit$n.censor)
,' fewer censored events than in [@fig:naaccrdeath_survfit] which may improve 
sensitivity in the actual analysis');
#' :::::
#' 
#' 
#' ###### blank
#' 
# tableone ---------------------------------------------------------------------
#' # Cohort Characterization {#sec:cohorchar}
#'
#' The below variables are subject to change as the data validation and 
#' preparation processes evolve.
#' 
#+ TableOne, cache=FALSE
.tc <- paste0('
Summary of all the variables in the combined i2b2/NAACCR set broken up by '
,fs('a_n_recur'),'. `Disease-free` and `Never disease-free` have the same 
meanings as codes 00 and 70 in the [NAACCR definition]('
,paste0(urls$dict_naaccr,'#1880'),') for ',fs('n_rectype'),". `Recurred` is any 
code other than (00, 70, or 99), and `Unknown if recurred or was ever gone` is 
99. `Not in NAACCR` means there is an EMR diagnosis of kidney cancer and there 
may in some cases also be a _record_ for that patient in NAACCR but it does not 
indicate kidney as the site {#tbl:cohortrectype}");

dat2a[,unique(c('patient_num',v(c_analytic),'n_cstatus','e_death'
        ,'a_n_race','a_n_dm','a_e_dm','a_e_kc','n_kcancer','a_n_recur'
        ,'a_hsp_naaccr'))] %>% 
  mutate(
    a_n_recur=ifelse(!patient_num %in% kcpatients.naaccr | a_n_recur==''
                     ,'NONE',as.character(a_n_recur)) %>% 
      # changing the order of the levels so the NONE ends up on the right side
      factor(.,levels=c(setdiff(sort(unique(.)),'NONE'),'NONE')) %>% 
      recode(NONE='Not in NAACCR')
    # n_cstatus=ifelse(!patient_num%in%kcpatients.naaccr
    #                  ,'No KC in NAACCR',as.character(n_cstatus)) %>%
    #   factor(levels=c(levels(n_cstatus),'No KC in NAACCR')),
    ,n_vtstat=n_vtstat<=age_at_visit_days
    ,s_death=s_death<=age_at_visit_days
    ,e_death=e_death<=age_at_visit_days
    ,a_tdeath=a_tdeath<=age_at_visit_days
    ,a_tdiag=a_tdiag<=age_at_visit_days
    ,a_trecur=a_trecur<=age_at_visit_days
    ,a_tsurg=a_tsurg<=age_at_visit_days
    ,age_at_visit_days=age_at_visit_days/365.25
    #,n_kcancer=n_kcancer>=0
    ) %>% assign('.t1input',.,envir=.GlobalEnv) %>%
  rename(`Age at Last Contact, combined`=age_at_visit_days
         ,`Sex, i2b2`=sex_cd
         ,`Sex, Registry`=n_sex
         ,`Language, i2b2`=language_cd
         ,`Hispanic, i2b2`=e_hisp
         ,`Hispanic, Registry`=n_hisp
         ,`Race, i2b2`=race_cd
         ,`Race, Registry`=a_n_race
         ,`Marital Status, Registry`=n_marital
         ,`Deceased, Registry`=n_vtstat
         ,`Deceased, SSN`=s_death
         ,`Deceased, EMR`=e_death
         ,`Insurance, Registry`=n_payer
         ,`Diabetes, Registry`=a_n_dm
         ,`Diabetes, i2b2`=a_e_dm
         ,`Kidney Cancer, Registry`=n_kcancer
         ,`Kidney Cancer, i2b2`=a_e_kc
         ,BMI=e_bmi) %>% select(-patient_num) %>%
  select(sort(names(.))) %>% 
  CreateTableOne(vars = setdiff(names(.),'a_n_recur'),strata='a_n_recur'
                 ,data = .,includeNA = T,test = F) %>% 
  print(printToggle=F) %>% 
  set_rownames(gsub('^([A-Za-z].*)$','**\\1**'
                    ,gsub('   ','&nbsp;&nbsp;',rownames(.)))) %>%
  set_rownames(gsub('[ ]?=[ ]?|[ ]?TRUE[ ]?',' ',rownames(.))) %>%
  gsub('0[ ]?\\([ ]+0\\.0\\)','0',.) %>% 
  pander(emphasize.rownames=F,caption=.tc);
# With above just a matter of finding a place to put the code below and then
# cleaning it up a little (including restricting it only to predictor vars that
# come from NAACCR)
# .t1input %>% CreateTableOne(vars=setdiff(names(.)
#                                          ,c('a_hsp_naaccr','patient_num'))
#                             ,strata='a_hsp_naaccr',data=.,includeNA = T ) %>%
#   print(printToggle=F,missing=T) %>% `[`(-5) %>% 
#   set_rownames(gsub('^([A-Za-z].*)$','**\\1**'
#                     ,gsub('   ','&nbsp;&nbsp;',rownames(.)))) %>%
#   set_rownames(gsub('[ ]?=[ ]?|[ ]?TRUE[ ]?',' ',rownames(.))) %>%
#   gsub('0[ ]?\\([ ]+0\\.0\\)','0',.) %>% 
#   pander(emphasize.rownames=F);
#
# conclusions ---------------------------------------------------------
#' # Conclusion and next steps {#sec:nextsteps}
#' 
#' This detailed investigation of the available data elements and development 
#' of analysis scripts opens the following directions: more data, 
#' _external_ data, more covariates, and improved pre-processing at the i2b2 
#' end (Aim 1).
#' 
#' More data can be acquired by reclaiming values that are currently
#' inconsistent or missing. There are various ad-hoc consistency checks 
#' described in [-@sec:xchecks; -@sec:diag; -@sec:surg]
#' [I need to gather these checks in one place and 
#' systematically run them on every patient to get a total count of records that
#' need manual chart review (Dr. Rodriguez's protocol) and for each record a 
#' list of issues to resolve]{.note2self custom-style='note2self'}. 
#' 
#' To reclaim missing values I will need to solve the problem of lag and 
#' disagreement between the EMR and NAACCR ([@sec:merging]). [I will meet with 
#' the MCC NAACCR registrar and learn where exactly in the EMR and other sources 
#' she looks to abstract `r md$mainvars`. I will also meet with personnel 
#' experienced in Urology chart review to learn their methods.]{.note2self custom-style='note2self'} 
#' This may lead to  improvements in the CIRD ETL process. As per Dr. Rodriguez 
#' I also plan on adding all ICD codes for 'renal mass' to my i2b2 query 
#' ([-@sec:diag]). Meanwhile, in response to researcher questions including my 
#' own, CIRD staff have identified thousands of NAACCR entries and surgery 
#' billing records that got excluded from i2b2 because they are not associated 
#' with visits to UT Health clinics and corrected the problem. After the next 
#' i2b2 refresh we expect an increased number of patients and better agreement 
#' of surgery dates between EMR and NAACCR.
#' 
#' [For external data I will request non-aggregated limited/deidentified records 
#' from the Texas Cancer Registry. I will also look at the NCDB dataset obtained 
#' by Urology]{.note2self custom-style='note2self'} to see if it has the 
#' elements listed in [@sec:reqelmnts].
#' 
#' In the remainder of Aim 2 and Aim 3 I will need the following additional
#' variables: (NAACCR only) stage and grade; (EMR only) analgesics, smoking and
#' alcohol, family history of cancer or diabetes, lab results, vital signs,
#' Miperamine (as per Dr. Michalek), frequency of lab and image orders,
#' frequency and duration of visits, and participation in adjuvant trials;
#' (both) birthplace, language, and diabetes; and (census data in i2b2) income
#' and education. [Each of these will require a workup similar to that reported
#' in [@sec:dataprep] and [-@sec:supp]. I can work independently on many of these 
#' but I will need guidance from experts in Urology on interpreting the stage 
#' and grade data.]{.note2self custom-style='note2self'} If genomic data from 
#' the Urology biorepository becomes available for these patients in the course 
#' of this study it also will become an important variable for Aim 2.
#' 
#' The use of TCR or NCDB data is *not* a substitute for UT Health and MGH i2b2
#' data. The registries allow me to test the replicability of high-level
#' findings to State and National populations but they will not have the
#' detailed additional variables I will need to investigate the causes of
#' disparate patient outcomes.
#' 
#' Nor are the R scripts I wrote for this project a substitute for DataFinisher
#' [@bokov_denormalize_2016] and i2b2. On the contrary, the reason I was able to 
#' make this much progress
#' in one month is that the data linkage and de-identification was done by the 
#' CIRD i2b2 ETL, the data selection was simplified by the i2b2 web client, and
#' an enormous amount of post-processing was done by my DataFinisher app which
#' is integrated into our local i2b2. In writing my scripts I have identified a
#' number of additional post-processing steps that generalize to other studies
#' and [I will integrate those into DataFinisher so that the data it outputs is
#' even more analysis-ready.]{.note2self custom-style='note2self'} This will, in 
#' turn, will simplify the logistics of Aim 3. 
#' 
#' [While I am incorporating the new methods into DataFinisher, I will also 
#' reorganize and document the code so I can present it to Dr. Murphy and his 
#' informatics team for review and input.]{.note2self custom-style='note2self'}
#'
#' # References
#' 
#' ::::: {#refs}
#' &nbsp;
#' :::::
#'
# A1 stage/grade ---------------------------------------------------------------
#' `r md$pbreak`
#' # : Example of stage/grade data {#sec:stage label="Appendix 1"}
#' 
.tc <- paste0('
This is proof of feasibility for extracting stage and grade at diagnosis for 
each NAACCR patient for import into the EMR system (e.g. Epic/Beacon). Clinical
and pathology stage descriptors are also available in NAACCR. Here the '
,fs('patient_num'),' are de-identified but with proper authorization they can 
be mapped to MRNs or internal database index keys. {#tbl:stage}');
subset(dat2a[,c('patient_num',v(c_tnm,NA))],patient_num %in% kcpatients.naaccr) %>% 
  na.omit() %>% 
  setNames(c('patient_num'
             ,submulti(v(c_tnm,NA)
                       ,cbind(v(c_tnm,NA),v(c_tnm,NA,retcol = 'colname_long'))))) %>% 
  # head(5) %>% 
  # show a sampling of rows and columns that fits on the page and remove the
  # the extra quotation marks
  `[`(1:15,1:8) %>% apply(2,function(xx) gsub('["]','',xx)) %>% 
  pander(caption=.tc);
#' 
# A2 next steps ---------------------------------------------------------------
formals(fs)$template <- fstmplts$link_colnamelong;
formals(fs)$retfun <- as.name('return');
#' `r md$pbreak`
#' # : Next steps {#sec:todo label="Appendix 2"}
#' 
#' * TODO: Update and clean up the plots and tables, including labels.
#'     * ~~[Consistency-Checks]~~
#'         * ~~Marital status, sex, race, hispanic(2):** shorten text and move to 
#'           captions.~~
#'         * ~~Write motivation and summary.~~
#'     * ~~[Testing/Interpreting Variables](#which-emr-and-naaccr-variables-are-reliable-event-indicators)~~
#'         * ~~Write motivation, intro, summary. Incorporate edits.~~
#'         * [Initial diagnosis], [Surgery], [Re-occurrence], [Death]
#'             * Move plots to the top of each
#'             * Shorten text and move to captions.
#'             * For each plot state what the conclusions are.
#'             * [Surgery]: turn the outline at the beginning into a more 
#'               concise paragraph.
#'     * [Hispanic variable recoding](#whether-or-not-the-patient-is-hispanic):
#'       turn into paragraphs, think about moving to variable glossary.
#'     * [Descriptive Plots (Preliminary)]
#'         * ~~Move them to right after the overview.~~
#'         * ~~Write intro mentioning that these are the relationships of interest
#'           among the four main variables.~~
#'         * ~~Expand why there are two versions of the survival plot~~
#' * DONE: ~~Update and streamline the narrative.~~
#'     * ~~Intro~~
#'     * ~~Motivation~~
#'     * ~~Summary of results~~
#'     * ~~Summary of next steps~~
#'     * ~~Move questions to after the [Descriptive Plots (Preliminary)] but 
#'       before the [Consistency-Checks], and place the answered questions at
#'       the bottom. [Domain expert questions](#questions-for-mentors-and-other-domain-experts)~~ 
#'       still go ahead of [empirical questions](#questions-to-answer-empirically).
#' * TODO: Remove the crossed-off stuff in [Appendix III: Supplementary tables] '
#'         but note someplace what was removed and why.
#' * TODO: Organize the inclusion/exclusion criteria into a single named list
#' * TODO: Overhaul the existing TableOne in [Cohort Characterization] -- use 
#'         data dictionary for renaming instead of _ad-hoc_ .
#' * TODO: Migrate everything that uses ~~`dat2`~~ and `dat3` to using `dat2a`.
#' * TODO: Create a TableOne for `r fs('a_hsp_naaccr')` (that specific one 
#'         because then the conclusions can be directly applied to TCR data) to 
#'         find possible confounding variables. Age, perhaps? Income? 
#' * TODO: Fill in more of the variable descriptions in 
#'         [Appendix IV: Variable descriptions]
#' * TODO: Prior to doing the above `tte()` put in a safeguard to make
#'         sure all the `c_tte` variables are `TRUE/FALSE` only. They
#'         are right now as it happens, but nothing enforces that.
#' * TODO: Clean up TNM variables, in consultation with domain expert (Peter?)
#' * TODO: Create access/quality variables including: number of visits per year, 
#'         number of lab tests and imaging orders per visit, time spent with 
#'         provider per visit
#' * TODO: Resume effort to link Mays Center historic trial records from IDEAS 
#'         to get information about enrollment in adjuvant trials
#' * TODO: Start validating and using additional 2a variables already in current 
#'         data
#'     * `[CN101] OPIOID ANALGESICS` (EMR)
#'     * `[CN103] NON-OPIOID ANALGESICS` (EMR)
#'     * `0250 Birthplace` (NAACCR possibly EMR)
#'     * Language (NAACCR and EMR)
#'     * smoking and alcohol (EMR)
#'     * Diabetes (NAACCR and EMR)
#'     * Family history (EMR)
#'     * Labs (EMR) including:  hemoglobin A1c, HDL, VLDL
#'     * Vitals (EMR) including: systolic and diastolic blood pressure, BMI
#'     * income (Census)
#'     * Miperamine, other anti-depressants
#'     * DONE: ~~Should use [`0580 Date of 1st Contact`](http://datadictionary.naaccr.org/default.aspx?c=10#580)
#'       as the diagnosis date if earlier than `r fs('n_ddiag')`!~~ _Actually, evidence 
#'       that it's neither a diagnosis date nor a first contact. Not known what
#'       it is._
#'     * DONE: ~~Surgery fields:~~
#'         * ~~[`1260 Date of Initial RX--SEER`](http://datadictionary.naaccr.org/default.aspx?c=10#1260)~~
#'         * ~~[`1270 Date of 1st Crs RX--CoC`](http://datadictionary.naaccr.org/default.aspx?c=10#1270)~~
#'         * ~~[`3170 RX Date--Most Defin Surg`](http://datadictionary.naaccr.org/default.aspx?c=10#3170)~~
#'     * DONE: ~~Recurrence: [`1880 Recurrence Type--1st`](http://datadictionary.naaccr.org/default.aspx?c=10#1880)~~
#' * TODO: In a future re-run of query...
#'     * Follow up re additional patient linkages, more recent NAACCR data
#'     * education (Census, not ready, ETL needs fixing)
#' * TODO: Separate script-level calls to `instrequire()` to reduce the number 
#'         of libraries that get loaded unnecessarily.
#' * TODO: Create a light version of `data.R.rdata` that has only the minimal 
#'         necessary stuff for, e.g. `exploration.R`
#' * DONE: ~~Create combined (if applicable) variables for each of the following:~~
#'     * ~~Initial diagnosis~~ `r fs('a_tdiag')`, `r fs('a_cdiag')`
#'     * ~~Surgery~~ `r fs('a_tsurg')`, `r fs('a_csurg')`
#'     * ~~Re-ocurrence~~ `r fs('a_trecur')`, `r fs('a_crecur')`
#'     * ~~_Last follow-up ?_~~ 
#'     * ~~Death~~ `r fs('a_tdeath')`, `r fs('a_cdeath')`
#'     * ~~Strict Hispanic designator~~ `r fs('a_hsp_strict')`
#'     * ~~Lenient Hispanic designator~~ `r fs('a_hsp_broad')`
#'     * ~~NAACCR-only Hispanic designator~~ `r fs('a_hsp_naaccr')`
#' * DONE: ~~Verify that the [ETL](http://www.hostedredmine.com/issues/719444#note-11) 
#'         gets `r fs('start_date')` for `1770 Cancer Status` from 
#'         [`1772 Date of Last Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1770)~~
#'         _in NAACCR v16 it cannot/doesn't need to_
#' * DONE: ~~tableOne~~
#' * DONE: ~~Create time-since-first-diagnosis variable~~
#' * DONE: ~~Create a special TTE variable from the main i2b2 age at death~~
#' * DONE: ~~Matrices of pairwise differences between all TTE variables~~
#' * DONE: ~~Create TTE variable for death (several raw variables)~~
#' * DONE: ~~Create TTE variable for recurrence~~
#' * DONE: ~~Create TTE variable for surgery date~~
#' * DONE: ~~Plot time from diagnosis to surgery, hisp vs non~~
#'     * ~~First need to confirm interpretation of outcome variable~~
#' * DONE: ~~Apply the `tte()` function to all variable in `c_tte`~~
#' * DONE: ~~Create censoring variable for surgery~~
#' * DONE: ~~Create censoring variable for recurrence/death~~
#' * DONE: ~~Map cancer status variable (didn't turn out to be useful)~~
#' * DONE: ~~Create unified comorbidity variable for:~~
#'     * DONE ~~Diabetes~~
#' * DONE: ~~Mappings for other numcode variables~~
#' * DONE: ~~Re-run query with additional variables (_query completed_):~~
#'     * ~~EMR codes for secondary tumors~~
#'     * ~~median household income, 2016 and 2013~~
#'     * ~~HbA1c~~
#'     * ~~Family history of diabetes and cancer~~
#' 
# A3 supplementary results ------------------------------------------------------
#' `r md$pbreak`
#' # Supplementary results {#sec:supp label="Appendix 3"}
#' 
#' ## Consistency checks {#sec:xchecks}
#' 
#' In this section are patient counts for all `r nrow(dat2a)` patients in the 
#' overall set, broken down by various NAACCR variables (rows) and equivalent 
#' EMR variables (columns). The **bold** values are counts of patients for
#' whom NAACCR and EMR are in agreement. Patients in the `NA` are the ones with
#' only EMR and no NAACCR records, so there is no information about whether 
#' NAACCR and the EMR would have been in agreement for those patients. 
#' 
#+ marital_status
with(dat2a,table(n_marital,e_marital,useNA='if')) %>% addmargins %>%
  set_colnames(.,gsub('@','',colnames(.))) %>% (function(xx){
    assign('.tbtmp',xx,envir=.GlobalEnv);xx}) %>%
  pander(emphasize.strong.cells=cbind(1:7,c(2:4,6:9))
         ,caption='Marital status has good agreement between NAACCR and EMR. {#tbl:xc_marital}');
#' 
#+ xc_sex
with(dat2a,table(n_sex,sex_cd,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=cbind(1:2,1:2)
         ,caption='Sex has good agreement between NAACCR and EMR. {#tbl:xc_sex}');
#+ xc_race
with(dat2a,table(a_n_race,race_cd,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=cbind(1:6,1:6)
         ,caption='Race has good agreement between NAACCR and EMR. {#tbl:xc_race}');

#+ xc_hisp0
.tc <- paste0('Hispanic designation has good agreement between NAACCR and EMR.
Here the ',fs('n_hisp'),' variable was simplified by binning into `Hispanic` and
`non-Hispanic`. {#tbl:xc_hisp0}');
with(dat2a,table(recode_factor(n_hisp,'Non_Hispanic'='Non_Hispanic'
                                ,'Unknown'='Non_Hispanic'
                                ,.default='Hispanic')
                 ,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>% 
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(emphasize.strong.cells=cbind(1:2,1:2),caption=.tc);
#+ xc_hisp1
.tc <- paste0('As [@tbl:xc_hisp0] but with all the different levels of '
,fs('n_hisp'),' shown. {#tbl:xc_hisp1}');
with(dat2a,table(n_hisp,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>%
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(emphasize.strong.cells=cbind(1:7,c(1,2,2,2,2,2,2))
                          ,caption=.tc);
#+ xc_dob
.tcap <- paste0(
  "Below is a summary of ",fs('birth_date')," - ",fs('n_dob')
," (in years) for the patients with non-matching dates of birth mentioned in 
[@sec:linkagever]. Though there are only ",length(kcpatients.naaccr_bad_dob)
," of them those few deviate by multiple years from the EMR 
records. {#tbl:xc_dob}");

dat0[!is.na(dat0[[cstatic_n_dob]]) & 
       dat0$patient_num%in%kcpatients.naaccr_bad_dob
     ,c('birth_date',cstatic_n_dob)] %>% 
  apply(2,as.Date) %>% apply(1,diff) %>% `/`(365.25) %>% summary %>% 
  pander(caption=.tcap);
#' 
#' **Divergent birth dates may become an exclusion criterion for analyses where 
#' age is a covariate**. However there is no evidence for increased disagreement 
#' between NAACCR and EMR for other variables among the patients from 
#' [@tbl:xc_dob] according to  
#' [@tbl:xc_dob_marital; @tbl:xc_dob_sex; @tbl:xc_dob_race; @tbl:xc_dob_hisp] 
#' which are like 
#' [@tbl:xc_marital; @tbl:xc_sex; @tbl:xc_race; @tbl:xc_hisp0] but tallied up 
#' for only those 
#' `r length(kcpatients.naaccr_bad_dob)` patients.
#' 
#+ dat2_bad_dob, cache=TRUE
dat2_bad_dob <- subset(dat2a,patient_num %in% kcpatients.bad_dob);
#+ xc_dob_marital
with(dat2_bad_dob,table(n_marital,e_marital,useNA = 'ifany')) %>%
  set_colnames(.,gsub('@','',colnames(.))) %>%
  pander(emphasize.strong.cells=cbind(1:7,c(2:4,6:9))
         ,caption='Marital status, for DOB-mismatched patients. {#tbl:xc_dob_marital}');
#+ xc_dob_sex
with(dat2_bad_dob,table(n_sex,sex_cd,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=as.matrix(expand.grid(1:2,1:2))
         ,caption='Sex, for DOB-mismatched patients {#tbl:xc_dob_sex}');
#+ xc_dob_race
with(dat2_bad_dob,table(a_n_race,race_cd,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=as.matrix(expand.grid(1:4,1:4))
         ,caption='Race, for DOB-mismatched patients {#tbl:xc_dob_race}');
#+ xc_dob_hisp
with(dat2_bad_dob,table(recode_factor(n_hisp,'Non_Hispanic'='Non_Hispanic'
                                      ,'Unknown'='Non_Hispanic'
                                      ,.default='Hispanic')
                        ,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>% 
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(emphasize.strong.cells=as.matrix(expand.grid(1:2,1:2))
                          ,caption='Hispanic ethnicity, for DOB-mismatched patients {#tbl:xc_dob_hisp}');
#+ xc_dob_surg
dat3_bad_dob <- subset(dat3,patient_num %in% kcpatients.bad_dob);
mutate_all(dat3_bad_dob[,v(c_nephx)]
           # break each column 
           ,function(xx) cut(xx-dat3_bad_dob$n_ddiag
                             ,breaks=c(-Inf,-.00001,.00001,Inf)
                             ,lab=c('before','same-day','after'),include=T)) %>%
  sapply(table,useNA='always') %>% t %>% 
  pander(caption='There is also no increase in EMR nephrectomy dates preceding 
diagnosis among the DOB-mismatched patients (see [@tbl:neph_b4_diag]). This 
suggests that other dates associated with these patients are not systematically
wrong. {#tbl:xc_dob_surg}');
#'
#  event indicators -------------------------------------------------------------
#' ## Which EMR and NAACCR variables are reliable event indicators? {#sec:vartrn}
#' 
#' For each of the main analytic variables `r fs('a_tdiag')`, `r fs('a_tsurg')`,
#' `r fs('a_trecur')`, and `r fs('a_tdeath')` / `r fs('n_vtstat')` there were 
#' multiple candidate data elements in the raw EMR and NAACCR data. If such a 
#' family of temporal values is in good agreement with each other overall, 
#' then individual missing dates can be filled in by taking the earliest 
#' non-missing dates from equivalent data elements (except for mortality where 
#' the latest non-missing date would be the logical choice). In addition
#' to the qualitative agreement I needed to establish for demographic variables 
#' in [@sec:linkagever] and [@sec:xchecks] here I needed to determine how often 
#' they lag or lead each other and by how much. The plots in this section use 
#' the y-axis to represent time for patient records arranged along the x-axis. 
#' They are arranged in an order that varies from one plot to another, chosen 
#' for visual interpretability. Each vertical slice of a plot represents one 
#' patient's history, with different colors representing events as documented by
#' different data elements. The goal is to see the frequency, magnitude, and 
#' direction of divergence for several related variables at the same time.
#' 
#  diagnosis ====================================================================
#' ### Initial diagnosis {#sec:diag}
#' 
#' At this time only `r fs('n_ddiag')` is usable for calculating 
#' `r fs('a_tdiag')`. Initially `r fs('n_fc')` was considered as an additional
#' NAACCR source along with the earliest EMR records of `r fs('e_kc_i9')` and 
#' `r fs('e_kc_i10')`. 
#' [`0443 Date Conclusive DX`](http://datadictionary.naaccr.org/default.aspx?c=10#443) 
#' is never used by our NAACCR. All other NAACCR data elements containing the 
#' word 'date' seem to be retired or related to events after initial diagnosis.
#' `r fs('n_fc')` was disqualified because it never precedes `r fs('n_ddiag')` 
#' but often trails behind `r fs('n_dsurg')`, see [@fig:diag2lc_eventplot]. 
#' [I will need to consult with a NAACCR registrar about what `r fs('n_fc')` actually means]{.note2self custom-style="note2self"} 
#' but it does not appear to be a first visit nor first diagnosis. As can be 
#' seen in [@fig:diag_plot] and [@tbl:diag_lag], the first ICD9 or ICD10 code 
#' most often occurs after initial diagnosis, sometimes before the date of 
#' diagnosis, and coinciding with the date of diagnosis rarest of all. Several 
#' of the ICD9/10 first observed dates lead or trail the `r fs('n_ddiag')` by 
#' multiple years.
#' 
#' ::::: {#fig:diag_plot custom-style="Image Caption"}
#+ diag_plot,results='asis'
# .diag_plot, opts.label='fig_opts'
par(xaxt='n');
.ev_diag_plot <- event_plot(
  dat3,'e_kc_i10',tunit='months',type='s'
  ,ylab='Months since NAACCR Date of Diagnosis'
  ,xlab='Patients, sorted by time to first ICD10 code\n\n\n'
  ,main='Time from Diagnosis to First ICD9/10 Code');
abline(h=c(-3,0,3),lty=c(2,1,2),col='blue');
points(.ev_diag_plot$e_kc_i9,col='#ff000050',pch='-');
#cat('{#fig:diag_plot}');
cat('\n\nHere is a plot centered on ',fs('n_ddiag')
    ,"(blue horizontal line at 0) with black lines indicating ICD10 codes for 
    primary kidney cancer from the EMR and dashed red lines indicating ICD9 codes. 
    The dashed horizontal blue lines indicate +- 3 months from ",fs('n_ddiag'));
#' :::::
#' 
#' ###### blank
#'
#+ .diag_plot_summ
.eplot_diag_summ <- summary(with(.ev_diag_plot
                                 ,cut(pmin(e_kc_i10,e_kc_i9,na.rm=T)
                                      ,c(-Inf,-3,-.001,.001,3,Inf))));
#'
#+ dat3_icdtimes,cache=TRUE
.tc <- paste0('
For patients with NAACCR records, how often do ICD9 or ICD10 codes for kidney 
cancer in the EMR lead or trail ',fs('n_ddiag'),' and by how 
much? {#tbl:diag_lag}');

# select the diagnosis-related variables
dat3[,v(c_kcdiag)] %>% 
  # subtract from each column the 'n_ddiag' value if present
  # divide by one year and remove patients with missing 'n_ddiag'
  `-`(.,with(.,n_ddiag)) %>% `/`(365.25) %>% subset(!is.na(n_ddiag)) %>% 
  with({
    # in the scope of the resulting data frame create a throwaway function to 
    # break up each variable into before, roughly coincident with, or after 
    # 'n_ddiag'
    ff<-function(xx) cut(xx,breaks=c(-Inf,-7/365.25,7/365.25,Inf)
                         ,labels=c('before','+/- 2 weeks','after'),include = T); 
    # use this function to make our table
    table(ICD9=ff(e_kc_i9),ICD10=ff(e_kc_i10),useNA = 'if')}) %>% addmargins() %>% 
  # format for viewing
  pander(caption=.tc);
#' 
#' 
#' For most patients (`r sum(.eplot_diag_summ[2:4])`), the first EMR code is 
#' recorded within 3 months of first diagnosis as recorded by NAACCR. Of those 
#' with a larger time difference, the majority (`r .eplot_diag_summ[5]`) have 
#' their first EMR code _after_ first `r fs('n_ddiag')`. Only 
#' `r .eplot_diag_summ[1]` patients have ICD9/10 diagnoses that precede their
#' `r fs('n_ddiag')` by more than 3 months. And additional `r .eplot_diag_summ[2]`
#' patients have first EMR diagnoses that precede `r fs('n_ddiag')` by less than
#' three months. **These might need to be eliminated from the sample on the 
#' grounds of not being first occurrences of kidney cancer.** However, we cannot 
#' back-fill missing NAACCR records or NAACCR records lacking a diagnosis date 
#' because there is too frequently disagreement between the the two sources, and 
#' the EMR records are currently biased toward later dates.
#' 
#' [I will need to meet with the MCC NAACCR registrar to see how she obtains 
#' her dates of initial diagnosis and I will need to do a chart review of a 
#' sample of NAACCR patients to understand what information visible in Epic sets 
#' them apart from kidney cancer patients without NAACCR records. I will also 
#' need to do a chart review of the patients with ICD9/10 codes for kidney 
#' cancer that seemingly pre-date their `r fs('n_ddiag')`. There are 
#' `r length(kcpatients.naaccr_dupe)` patients with multiple NAACCR records. I 
#' will need to learn how NAACCR distinguishes their first occurrences and see 
#' if **restricting the NAACCR data to just first occurrences will diminish the
#' number of EMR diagnoses preceding those in NAACCR.** It will also be helpful
#' to learn whether there is anything in the EMR other than lack of previous
#' which distinguishes first kidney cancer occurrences]{.note2self custom-style='note2self'}
#  surgery ======================================================================
#' ### Surgery {#sec:surg}
#' 
#' * NAACCR: 
#'      * In addition to `r fs('n_dsurg')`) the following possibly relevant 
#'        fields are available in our local NAACCR:
#'          * `r fs('n_rx1260')`
#'          * `r fs('n_rx1270')`
#'          * `r fs('n_rx3170')`
#'      * Here are the questions raised:
#'          * Do they agree with `r fs('n_dsurg')` sufficiently that missing 
#'            `r fs('n_dsurg')` can be backfilled from some or all of them?
#'          * Under what circumstances can they be interpreted as surgery dates 
#'            rather dates for something else?
#'          * How accurate is `r fs('n_surgreason')` in distinguishing surgical 
#'            cases from non-surgical cases as per EMR records?
#' * EMR: First occurrence of any ICD9/10 code for acquired absence of 
#'   kidney; or first occurence of surgical history of nephrectomy. How much do 
#'   they agree with NAACCR?
#' * Question: Where in the chart would one positively establish the date of the 
#'   patient's first nephrectomy...
#'     * ...in Epic?
#'     * ...in Sunrise?
#' * Question: What is the typical time that elapses between diagnosis and 
#'   surgery?
#'     * Answer (RR): 2-4 weeks, try to avoid more than 4
#' * Question: Is it possible for surgery to happen on the same day as the 
#'   diagnosis? How common is that?
#'     * Answer (RR): Fairly common, if NAACCR diagnosis based on pathology 
#'       rather than clinical examination, which is usually technically a renal 
#'       mass, not a cancer. Might want to use imaging result date as the date 
#'       of diagnosis if it isn't already being used as such.
#' * Question: What would be the threshold on the lag to surgery until we must 
#'   conclude that there is an error in that record? E.g. is four years too 
#'   long?
#'     * Answer (RR): No, there are a few local cases that took over a decade to 
#'       get to surgery for various reasons (e.g. indolent tumor, or contact 
#'       lost with patient).
#' * Question: What fraction of KC patients do not undergo surgery?
#'     * Answer (RR): Around 15%
#'     
#+ dat3_surg, cache=TRUE
# make each date of surgery proxy relative to date of diagnosis
dat3_surg <- (dat3[,c(v(c_nephx),'n_drecur')] - dat3$n_ddiag) %>% 
  # keep only the ones that have a date of diagnosis and sort by NAACCR 
  # surgery date, then convert to weeks.
  subset(!is.na(dat3$n_ddiag)) %>% arrange(n_dsurg) %>% '/'(7);
# make a summary table for the 'c_nephx' candidate surgery proxy variables
dat3_surg_summary <- summary(dat3_surg) %>% 
  # extract the rownames from the arcane way that summary() returns them
  # getting rid of extra whitespace and then strip them out from the values
  set_rownames(.,trimws(gsub(':.*$','',`[`(.,,1)))) %>% gsub('^.*:','',.) %>% 
  # convert to numeric transposing as a (good) side-effect and set the rownames
  apply(1,as.numeric) %>% set_rownames(colnames(dat3_surg)) %>% 
  # convert to data.frame without 'fixing' the column names.
  data.frame(check.names = F);
# list of variables that can first occur before 'n_ddiag'
#t_priorcond <- paste(rownames(subset(dat3_surg_summary,Min.<0)),collapse=', ');
t_priorcond <- subset(dat3_surg_summary,Min.<0) %>% rownames %>% 
  sapply(fs,template=fstmplts$link_varname,retfun=return) %>% 
  knitr::combine_words();
#' As can be seen in the table below, the variables `r t_priorcond` _sometimes_ 
#' precede `r fs('n_ddiag')` by many weeks. However, they _usually_ follow 
#' `r fs('n_ddiag')` by more weeks than the two NAACCR variables 
#' `r fs('n_dsdisc')` and `r fs('n_dsurg')`. Those two NAACCR variables never
#' occur before `r fs('n_ddiag')` and usually occur within 2-8 weeks after it.
#' 
#' As can be seen from the `NA's` column, the inactive ICD9/10 V/Z codes for
#' acquired absence of kidney are disqualified because they are very rare in 
#' addition to being even more divergent from the `r fs('n_ddiag')` than the 
#' non-inactive codes.
pander(dat3_surg_summary);
#' It's understandable if the Epic EMR lags behind NAACCR (because as an 
#' outpatient system, it's probably recording just the visits after the original
#' surgery, and perhaps we are not yet importing the actual surgery events from 
#' Sunrise EMR). But for the V or Z or surgical history codes that precede 
#' `r fs('n_ddiag')`, it could mean that those NAACCR cases are not first-time 
#' occurrences. How big of a problem is this?
#' 
#+ before_sameday_after_00,cache=TRUE
mutate_all(dat3[,v(c_nephx)]
           # break each column 
           ,function(xx) cut(xx-dat3$n_ddiag,breaks=c(-Inf,-.00001,.00001,Inf)
                             ,lab=c('before','same-day','after'),include=T)) %>%
  sapply(table,useNA='always') %>% t %>% 
  pander(caption='
How often do ICD9/10 or surgical history codes for nephrectomy precede diagnosis
and by how much? {#tbl:neph_b4_diag}');

#' Not too bad. Though we cannot trust the ICD9/10 codes as replacements for
#' missing surgery dates, there are few enough of them preceding diagnosis that
#' we can remove them as source data errors without ruining the sample size.
#' 
# 
# Here is a more general table, comparing every possible recurrence event or 
# surgery event to the NAACCR surgery variable, to clean up and uncomment later
# mutate_all(dat3[,c(v(c_nephx),v(c_recur))]
#            # break each column 
#            ,function(xx) cut(xx-dat3$n_dsurg,breaks=c(-Inf,-.00001,.00001,Inf)
#                              ,lab=c('before','same-day','after'),include=T)) %>% 
#   cbind(.,TOTAL=apply(.,1,function(xx) factor(
#     ifelse(any(xx=='before',na.rm=T),'before'
#            ,ifelse(any(xx=='same-day',na.rm=T),'same-day'
#                    ,ifelse(any(xx=='after',na.rm=T),'after',NA)))
#     ,levels=c('before','same-day','after')))) %>% 
#   sapply(table,useNA='always') %>% t %>% pander();
#' 
# Now, as far as the two NAACCR variables go, does `n_dsdisc` (date of 
# discharge) contribute anything more than `r fs('n_dsurg')`? There are 
# `r nrow(subset(dat3,is.na(n_dsurg)&!is.na(n_dsdisc)))` non-missing values 
# of `n_dsdisc` when `r fs('n_dsurg')` is missing. As can be seen from the plot below
# where `n_dsdisc` are the red dashed lines and `r fs('n_dsurg')` are the black lines,
# both relative to date of diagnosis, `n_dsdisc` either coincides with `r fs('n_dsurg')`
# or lags by multiple weeks, as might be expected of a discharge date (what is 
# the plausible threshold on time from surgery to discharge?).
#' 
#' 
#' ::::: {#fig:surg0_plot0 custom-style="Image Caption"}
#+ surg0_plot0,results='asis'
par(xaxt='n');
.eplot_surg0 <- subset(dat3,patient_num %in% 
                         kcpatients_surgreason$`Surgery Performed`) %>% 
  mutate(nrx=pmin(n_rx3170,n_rx1270,n_rx1260)) %>% 
  event_plot('n_dsurg','n_rx3170',tunit='months',type='s',ltys = c(1,1)
             ,main='Time from Diagnosis to Surgery'
             ,ylab='Months Since Diagnosis'
             ,xlab='Patients, sorted by time to surgery\n\n\n'
             ,xlim=c(0,length(kcpatients_surgreason$`Surgery Performed`))
             ,ylim=c(-10,60));
abline(h=0,col='blue');
.eplot_surg0$icd <- apply(.eplot_surg0[,c('e_i9neph','e_i10neph','e_hstneph')]
                          ,1,min,na.rm=T);
.eplot_surg0$icd[is.infinite(.eplot_surg0$icd)]<-NA;
lines(.eplot_surg0$icd,type='s',col='#FF00FF50');
with(.eplot_surg0,abline(v=which(icd<n_dsurg),col='#FFFF0030',lwd=4));

cat("\n\nAbove is a plot of all patients sorted by "
,fs('n_dsurg')," (black line).  On the same axis is ",fs('n_rx3170')
," (red line) which is almost  identical to ",fs('n_dsurg')," except for a small
number of cases where it occurs later than ",fs('n_dsurg'),". It never occurs
earlier. The purple lines indicate for each patient the earliest EMR code
implying that a surgery had taken place (acquired absence of kidney ICD V/Z 
codes or surgical history of nephrectomy)");
#' :::::
#' 
#' 
#' In [@fig:surg0_plot0] the `r sum(with(.eplot_surg0,icd<n_dsurg),na.rm=T)` 
#' patients for which one or more EMR codes are recorded prior to 
#' `r fs('n_dsurg')` are highlighted in yellow.
#' 
#' ::::: {#fig:surg0_plot1 custom-style="Image Caption"}
#+ .surg0_plot1,results='asis'
par(xaxt='n');
.eplot_surg0 <- subset(dat3,patient_num %in% 
                         kcpatients_surgreason$`Surgery Performed`) %>% 
  mutate(nrx=pmin(n_rx3170,n_rx1270,n_rx1260)) %>% 
  event_plot('n_dsurg','n_rx3170',tunit='months',type='s',ltys = c(1,1)
             ,main='Time from Diagnosis to Surgery'
             ,ylab='Months Since Diagnosis'
             ,xlab='Patients, sorted by time to surgery\n\n\n'
             ,xlim=c(0,length(kcpatients_surgreason$`Surgery Performed`))
             ,ylim=c(-10,60));
abline(h=0,col='blue');
lines(.eplot_surg0$n_rx1270,col='#00FF0060',type='s');
lines(.eplot_surg0$n_rx1260,col='#00FFFF60',type='s');

cat("\n\nIn the above plot the ",fs('n_rx1270')," (green) and "
,fs('n_rx1260')," (cyan) events are superimposed on time till ",fs('n_dsurg')
," like in [@fig:surg0_plot0] (but purple lines for nephrectomy EMR codes are 
omitted for readability). The ",fs('n_rx1270')," and ",fs('n_rx1260')
," variables trend earlier than ",fs('n_dsurg'));
#' :::::
#' 
#' ###### blank
#' 
#' ::::: {#fig:surg1_plot custom-style="Image Caption"}
#+ .surg1_plot,results='asis'
par(xaxt='n');
.eplot_surg1 <- subset(dat3,!patient_num %in% 
                         kcpatients_surgreason$`Surgery Performed`) %>% 
  mutate(nrx=pmin(n_rx3170,n_rx1270,n_rx1260)) %>% 
  event_plot('n_dsurg','n_rx3170',tunit='months',type='s',ltys = c(1,1)
             ,main='Time from Diagnosis to Surgery'
             ,ylab='Months Since Diagnosis'
             ,xlab='Patients, sorted by time to surgery\n\n\n'
             ,xlim=c(0,length(kcpatients_surgreason$`Surgery Performed`))
             ,ylim=c(-10,60));
abline(h=0,col='blue');
.eplot_surg1$icd <- apply(.eplot_surg1[,v(c_nephx,dat3)[1:5]],1,min,na.rm=T);
.eplot_surg1$icd[is.infinite(.eplot_surg1$icd)]<-NA;
lines(.eplot_surg1$icd,type='s',col='#FF00FF50');
with(.eplot_surg1,abline(v=which(icd<n_dsurg),col='#FFFF0030',lwd=4));
lines(.eplot_surg1$n_rx1270,col='#00FF0060',type='s');
lines(.eplot_surg1$n_rx1260,col='#00FFFF60',type='s');
cat("

Above is a plot equivalent to [@fig:surg0_plot1] but for patients who do _not_
have a ",fs('n_surgreason')," code equal to `Surgery Performed`. There are many "
,fs('n_rx1270')," and ",fs('n_rx1260')," events but only a small number of "
,fs('n_dsurg')," (black) and ",fs('n_rx3170')," (red). The ",fs('n_dsurg')
," and ",fs('n_rx3170')," that do occur track each other perfectly. Together 
with NAACCR data dictionary's description this suggests that ",fs('n_rx3170')
," is the correct principal surgery date in close agreement with "
,fs('n_dsurg'),", so perhaps missing ",fs('n_rx3170')," values can be filled in
from ",fs('n_dsurg'),". However ",fs('n_rx1270')," and ",fs('n_rx1260')
," seem like non-primary surgeries or other events"); 
#' :::::
#' 
#' ###### blank
#' 
.tc <- paste0("Table of every NAACCR surgery event variable versus "
              ,fs('n_surgreason')," {#tbl:srgvars}");
lapply(v(c_nephx,dat2a)[6:9],function(ii) 
  table(dat2a$n_surgreason,dat2a[[ii]]<=dat2a$age_at_visit_days) %>% 
    set_colnames(.,paste0(ii,' = ',colnames(.)))) %>% do.call(cbind,.) %>% 
  pander(caption=.tc);

#' ##### Surgery Conclusion
#' 
#' As of now the sole variables on which I can rely for date of surgery are 
#' `r fs('n_rx3170')` supplemented by `r fs('n_dsurg')`, and the small number of cases where EMR 
#' codes imply surgery prior to diagnosis will be excluded. For the purposes of
#' determining whether there is a difference in the time from diagnosis to 
#' surgery I could also create an alternative 'naive' variable that is simply
#' the earliest of all possible surgery events for each patient. For the time
#' elapsed from surgery to death or recurrence, I will use the first (`r fs('n_rx3170')`
#' and `r fs('n_dsurg')`) variable as above with the additional criterion that only 
#' cases where the `r fs('n_surgreason')` is `Surgery Performed` be included.
#' 
#  re-occurrence ================================================================
#' ### Re-occurrence {#sec:recur}
#' 
#' The current available variables are: `r fs('n_cstatus')`, `r fs('n_rectype')`
#' and `r fs('n_drecur')`. Our site is on NAACCR v16, not v18, and we do not 
#' have [`1772 Date of Last Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1772).
#' According to the v16 standard, instead `r fs('n_lc')` should be used.
#' Now we can reconcile `r fs('n_cstatus')` and `r fs('n_rectype')`. 
#' 
#' ###### blank
#' 
#+ rectype_cstatus
.tc <- paste0('Almost all ',fs('n_cstatus'),' `Tumor_Free` patients also have a 
`Disease-free` in their ',fs('n_rectype'),' column, the `Tumor` ones have a 
variety of values, and the `Unknown` ones are mostly `Unknown if recurred or was 
ever gone`. {#tbl:rectype_cstatus}');

subset(dat2a,!patient_num %in% kcpatients.naaccr_dupe) %>% droplevels() %>%
  with(table(n_rectype,n_cstatus)) %>% pander(caption=.tc);
#' 
#' ###### blank
#' 
#' This suggests the following rules for binning them:
#' 
#' * `r fs('n_rectype')` is `Disease-free` (disease free)
#' * `r fs('n_rectype')` is `Never disease-free` (never disease free)
#' * `r fs('n_rectype')` raw code includes 70 then assume never diease free
#' * `r fs('n_rectype')` is `Unknown if recurred or was ever gone` (unknown)
#' * Otherwise, (recurred)
#' 
#' ###### blank
#
t_recur_drecur <- with(dat2a
                       ,table(a_n_recur
                              ,`Has recurrence date`=n_drecur<=age_at_visit_days
                              ,useNA='if'));
#' Here is the condensed version after having followed the above rules. Looks 
#' like the only ones who have a `r fs('n_drecur')` (recurrence date) are the
#' ones which also have a `Recurred` status for `r fs('a_n_recur')` (with `r t_recur_drecur['Recurred','FALSE']`
#' missing an `r fs('n_drecur')`). The only exception is `r t_recur_drecur['Never disease-free','TRUE']`
#' `Never diease-free` patient that had an `r fs('n_drecur')`.
t_recur_drecur %>% set_colnames(.,paste0('Recur Date=',colnames(.))) %>% 
  pander(emphasize.strong.cells=cbind(2:5,c(1,1,2,1)));
#' This explains why  `r fs('n_drecur')` values are relatively rare in the data-- they 
#' are specific to actual recurrences which are not a majority of the cases. 
#' This is a good from the standpoint of data consistency. Now we need to see to 
#' what extent the EMR codes agree with this. 
#' 
#' 
#' ::::: {#fig:recur_plot custom-style="Image Caption"}
#+ recur_plot,results='asis'
par(xaxt='n');
.eplot_recur0 <-subset(dat3,patient_num %in% 
                         setdiff(kcpatients_surgreason$`Surgery Performed`
                                 ,kcpatients.naaccr_dupe)) %>% 
  mutate(.,rec=na_if(apply((.)[,v(c_recur)[-15]],1,min,na.rm=T),Inf)) %>% 
  event_plot('rec',start_event = 'n_dsurg',type='s',ltys=c(1,1)
             ,main='Time from Surgery to Recurrence'
             ,ylab='Months Since Surgery'
             ,xlab='Patients, sorted by time to first mets according to EMR\n\n\n'
             ,tunit = 'month');
abline(h=c(-3,0,3),lty=c(3,1,3),col='blue');
# Highlight patients with recurrence
with(.eplot_recur0,abline(v=which(patient_num %in% kcpatients_rectype$Recurred)
                          ,col='#FF000020',lwd=2));
# ...never disease-free
with(.eplot_recur0,abline(v=which(patient_num %in% 
                                    kcpatients_rectype$`Never disease-free`)
                          ,col='#FFFF0060',lwd=2));
# ... and disease-free
with(.eplot_recur0,abline(v=which(patient_num %in% 
                                    kcpatients_rectype$`Disease-free`)
                          ,col='#00FF0020',lwd=2));
points(.eplot_recur0$n_drecur,col='red',pch='-',cex=2);
cat("

In the above plot, the black line represents months elapsed between surgery and
the first occurence of an EMR code for secondary tumors, if any. The horizontal
red line segments indicate individual ",fs('n_drecur'),". The blue horizontal
line is the date of surgery. Patients whose status ",fs('n_rectype')," is 
`Disease-free` are highlighted in green, `Never disease-free` in yellow, and
`Recurred` in red");
#' :::::
#' 
#' 
#' The green highlights are _mostly_ where one would expect, but why are there
#' `r nrow(subset(.eplot_recur0,patient_num %in% kcpatients_rectype[['Disease-free']] & !is.na(rec)))`
#' patients on the left side of the plot that have EMR codes for secondary 
#' tumors? Also, there are `r nrow(subset(.eplot_recur0,rec<0))` patients with 
#' metastatic tumor codes earlier than `r fs('n_dsurg')` and of those 
#' `r nrow(subset(.eplot_recur0,rec< -3))` occur more than 3 months prior to 
#' `r fs('n_dsurg')`. Did they present with secondary tumors to begin with but
#' remained disease free after surgery? Removing the `_inactive` versions of the 
#' secondary tumor codes does not make the left-side green patients go away.
#' 
#' 
# Not ready for production code to plot ranked events...
# baz<- subset(dat3,!patient_num %in% kcpatients.naaccr_dupe)[,c('patient_num',v(c_kcdiag,dat3),v(c_nephx,dat3),v(c_recur,dat3),v(c_death,dat3),'n_lc')]
# baz[,-1] <- t(apply( baz[,-1],1,rank,na.last='keep'))
# baz<-arrange(baz,n_ddiag)
# plot(baz$n_ddiag,type='l',ylim=c(0,21))
# lines(baz$n_fc,col='red')
# lines(baz$n_lc,col='black')
# lines(baz$n_dsurg,col='green',lty=2)
# lines(baz$n_dsurg,col='green',lwd=2)
# lines(baz$n_drecur,col='blue',lwd=2)
# for(ii in v(c_death,baz)) lines(baz[[ii]],col='orange',lwd=2)
# for(ii in v(c_kcdiag,baz)[1:2]) lines(baz[[ii]],lty=3,col='pink')
# for(ii in v(c_recur,baz)[-15]) lines(baz[[ii]],col='cyan')
# for(ii in v(c_death,baz)) lines(baz[[ii]],col='orange',lwd=2)
#'  
#  death ========================================================================
#' ### Death {#sec:death}
#' 
#' When more than one source has a death date, they are in agreement. To be 
#' fair, the agreement between `r fs('e_death')`, `r fs('e_dscdeath')`, and `r fs('s_death')` is 
#' probably due to our i2b2 ETL already merging `r fs('e_dscdeath')` and `r fs('s_death')` into 
#' `r fs('e_death')`. But it is also encouraging that none of them seem (by visual 
#' inspection) to occur prior to the date of last contact in NAACCR. That 
#' suggests I can simply take the mininum of available death dates to fill in 
#' data for patients that NAACCR is not aware are deceased. It also means that 
#' the ETL's coverage of vital status can be further improved by using the 
#' NAACCR vital status and last contact variables in combination.
#'
#'
#' ::::: {#fig:death_plot custom-style="Image Caption"}
#+ .death_plot,results='asis'
par(xaxt='n');
.eplot_death <- event_plot(dat3,'n_lc',start_event = 'n_ddiag'
                           ,main='Time from Diagnosis to Death (if any)'
                           ,ylab='Months Since Diagnosis'
                           ,xlab='Patients, sorted by last contact date\n\n\n'
                           ,tunit = 'mon',ltys = 0,type='s');
points(.eplot_death$e_death,pch=2,col='#FF00FF70',lwd=2); # \triangle
points(.eplot_death$s_death,pch=6,col='#00999970',lwd=3); # \nabla (not making this up!)
points(.eplot_death$e_dscdeath,pch=3,col='#00FF0090',lwd=2); # +
points(.eplot_death$e_disdeath,pch=3,col='#00FF0090',lwd=2);
points(.eplot_death$n_vtstat,col='brown',cex=1.5,lwd=0.5); # \bigcirc
abline(h=0,col='blue');
.xch_vtstat_lc<-subset(dat2a
                       ,n_vtstat==n_lc&n_lc!=age_at_visit_days+1)$patient_num;
.xch_vtstat_lc_death = intersect(kcpatients.naaccr_death,.xch_vtstat_lc);
if(length(.xch_vtstat_lc_death)!=length(.xch_vtstat_lc)){
  stop('.xch_vtstat_lc check failed')};
cat("

Above are plotted times of death (for patients that have them) relative to "
,fs('n_ddiag')," (horizontal blue line). The four data sources are "
,fs('e_death')," (![](resources/pinktriangle.png){width=10}), ",fs('s_death')
," (![](resources/blueinvtriangle.png){width=10}), ",fs('e_dscdeath')
," (![](resources/greencross.png){width=10}), and ",fs('n_vtstat')
," (![](resources/browncircle.png){width=10})");
#' :::::
#' 
#' 
#' Overall there are `r length(kcpatients.naaccr_death)` patients that according
#' to `r fs('n_vtstat')` are deceased. For all `r length(.xch_vtstat_lc)` of these
#' patients, _and only for them_, the condition also holds that `r fs('n_vtstat')` 
#' is equal to `r fs('n_lc')` but `r fs('n_lc')` happens before or on `r fs('age_at_visit_days')`.
#' If something is coded as happening _after_ `r fs('age_at_visit_days')` then 
#' because of how the data is summarized in the `dat2` section of the `data.R` 
#' script it means that the event never happened. If `r fs('n_lc')` never 
#' happened it means that patient has evidence of kidney cancer in the EMR data 
#' but no accompanying NAACCR record. In short, we are filtering for existence 
#' of NAACCR records. That, in turn, means that `r fs('n_vtstat')` `<=` `r fs('n_lc')`
#' is a valid censoring criteria (censored if false) provided that the input
#' data is filtered to include only patients with NAACCR records (for other 
#' patients, both `r fs('n_vtstat')` and `r fs('age_at_visit_days')` should be 
#' interpreted as missing).
#  hispanic ethnicity ===========================================================
#' ### Whether or not the patient is Hispanic
#' 
#' A similar process needs to be done for Hispanic ethnicity, but as an ordinary 
#' static variable rather than time-to-event. I think I'll do two variables: one 
#' that is true if we are very sure the patient is Hispanic, and the other one 
#' that is true if we aren't certain the patient is _not_ Hispanic. In both 
#' cases, there will also be `Unknown` bins for where all variables are 
#' unanimous on the patient's Hispanic status being unknown.
#' 
#' Basically two variables because there are the two ends of the spectrum for
#' resolving disagreement about a binary variable between multiple sources.
#' 
#' Here are the variables to process:
#' 
#' * `r fs('language_cd')` is an i2b2 PATIENT_DIMENSION variable that is simplified by 
#'   `data.R` and `levels_map.csv`
#'     * Hispanic : `Spanish`
#'     * non-Hispanic: `Other`
#'     * Unknown: `English` or `Unknown` or NA
#' * `r fs('e_lng')` is an i2b2 OBSERVATION_FACT variable currently in the raw form that
#'   DataFinisher uses for complex variables lacking a specific rule. Below are 
#'   regexp patterns for a non case-sensitive match.
#'     * Hispanic: ` ^.*spanish.*$` ELSE
#'     * Unknown: ` ^.*(english|sign language|unknown).*$` or NA ELSE
#'     * non-Hispanic: anything not caught by the above two filters
#' * `r fs('n_hisp')` is the [`0190 Spanish/Hispanic Origin`](http://datadictionary.naaccr.org/default.aspx?c=10#190)
#'   variable from NAACCR. Slightly processed by `data.R` and `levels_map.csv`
#'     * non-Hispanic: `Non_Hispanic`
#'     * Unknown: `Unknown`
#'     * Hispanic: any other value
#' * `r fs('e_hisp')` is the indicator variable for Hispanic ethnicity from i2b2 
#'   OBSERVATION_FACT.
#'     * Hispanic: `TRUE`
#'     * Unknown: `FALSE`
#' * `r fs('e_eth')` is the whole ethnicity variable from i2b2 OBSERVATION_FACT and
#'   suprprisingly it is not in full agreement with `r fs('e_hisp')`
#'     * Hispanic: `hispanic`
#'     * Unknown: `other`,`unknown`,`unknown/othe`,`i choose not`,`@`
#'     * non-Hispanic: `arab-amer`,`non-hispanic`
#'     
#' The strict Hispanic variable.
#' 
#' * Hispanic if ALL non-missing values of `r fs('n_hisp')`, `r fs('e_hisp')`, and `r fs('e_eth')` are 
#'   unanimous for `Hispanic`
#' * non-Hispanic if ALL non-missing values of `r fs('n_hisp')` and `r fs('e_eth')` are 
#'   unanimous for `non-Hispanic` (note that `r fs('e_hisp')` not included here) and
#'   neither `r fs('e_lng')` nor `r fs('language_cd')` vote for `Hispanic`
#' * Unknown if any other result.
#' 
#' The lenient Hispanic variable.
#' 
#' * Hispanic if ANY non-missing values of `r fs('language_cd')`, `r fs('e_lng')`, `r fs('n_hisp')`,
#'   `r fs('e_hisp')`, and `r fs('e_eth')` have value `Hispanic`
#' * Unknown if ALL non-missing values of `r fs('language_cd')`, `r fs('e_lng')`, `r fs('n_hisp')`,
#'   `r fs('e_hisp')`, and `r fs('e_eth')` are unanimous for `Unknown` 
#' * non-Hispanic if any other result
#' 
#' 
#' 
# Now plot two different scenarios on the same axes, the original and enhanced 
# by EMR variables. Strict
# plot(.survfit_plot0$fit,col=c('red','blue'),mark.time = T,xlim=c(-1,150)
#      ,lwd=2,main='Time from Diagnosis to Surgery',ylab='Not Undergone Surgery'
#      ,xlab='Weeks Since Diagnosis');
# lines(update(.survfit_plot0,default.censrvars=c('a_tdeath','age_at_visit_days')
#              ,predvars='a_hsp_strict')$fit
#       ,col=c('#0000ff40','#00ff0040','#ff000040'),lwd=4,mark.time=T);
# lines(update(.survfit_plot0,default.censrvars=c('a_tdeath','age_at_visit_days')
#              ,predvars='a_hsp_broad')$fit
#       ,col=c('#ff000040','#0000ff40'),lwd=4,lty=2,mark.time=T);
#' 
#' What is the risk of relapse for patients after nephrectomy?
#' 
#' 
# plot(.survfit_plot1$fit,col=c('red','blue'),mark.time = T,xlim=c(-1,150)
#      ,lwd=2,main='Time from surgery to recurrence',ylab='Fraction recurrence-free'
#      ,xlab='Weeks since surgery',ylim=c(.7,1));
# lines(update(.survfit_plot1,default.censrvars=c('a_tdeath','age_at_visit_days')
#              ,predvars='a_hsp_strict')$fit
#       ,col=c('#ff000040','#00ff0040','#0000ff40'),lwd=4,mark.time=T);
# lines(update(.survfit_plot1,default.censrvars=c('a_tdeath','age_at_visit_days')
#              ,predvars='a_hsp_broad')$fit
#       ,col=c('#ff000040','#0000ff40'),lwd=4,lty=2,mark.time=T);
#' 
#' `r md$pbreak`
#' 
#  validrecords_old -------------------------------------------------------------
#' ## What is the coverage of valid records in each data source.
#' 
#' How many patients are in NAACCR, the EMR, both, neither, or have a diagnosis
#' prior to first available record?
consort_table[with(consort_table,order(PreExisting,decreasing = T)),] %>% 
  mutate(`N Cumulative`=rev(cumsum(rev(N)))) %>% pander();
#' _This has been temporarily moved from the main section pending finalization
#' of the recurrence variables. For now, the only ones we can be sure of 
#' [as indicators of a pre-existing condition](#surgery-conclusion) as exclusion
#' criteria for possibly invalid records are `r t_priorcond` if they occur 
#' prior to `r fs('n_ddiag')` and those will exclude far fewer records than suggested 
#' by this table_ .
#' 
#  firscontact ------------------------------------------------------------------
#' ## What is going on with the first contact variable?
#' 
#' 
#' ::::: {#fig:diag2lc_eventplot custom-style="Image Caption"}
#+ diag2lc_eventplot,results='asis'
.eplot_fc <-event_plot(subset(dat3,!patient_num %in% kcpatients.naaccr_dupe)
                       ,'n_lc','n_fc',start_event = 'n_ddiag'
                       ,main='Time from Diagnosis to Last Contact'
                       ,ylab='Months Since Diagnosis'
                       ,xlab='Patients, sorted by Last Contact'
                       ,tunit = 'mon'
                       ,ltys = c(1,1));
abline(h=0,col='blue');
cat("

Wierd observation-- ",fs('n_fc')," (red) is almost always between ",fs('n_lc')
,"(black) and ",fs('n_ddiag')," (blue) though diagnosis is usually on a biopsy
sample and that's why it's dated as during or after surgery we thought. If first contact is some kind of event after first diagnosis, what is it?");
#' :::::
#' 
#' 
#' Surgery `r fs('n_dsurg')` seems to happen in significant amounts both before 
#' and after first contact `r fs('n_fc')`.
#' 
#  heatmap ----------------------------------------------------------------------
#' ## Which variables are near-synonymous?
#' 
#' Some variables will, despite what they sound like will be clearly unrelated 
#' to each other. Others will be in high pairwise agreement when both are
#' non-missing. The ones in between need to be investigated further to determine
#' whether they are more informative than no information at all, whether they
#' can be cleaned up, and whether there is a bias (i.e. one variable will 
#' consistently lag another variable). 
#' 
#' In the `data.R` script we will convert all the event variables to a time to
#' event (tte) form. The above variables plus a few that are dates which aren't 
#' currently known to correlate with any of the events of interest, but doesn't 
#' hurt to check. The overall approach will be:
#'  
#' 1. Take for each patient the first visit where the variable is TRUE, 
#'    non-missing, or in some cases meets some other criteria.
#' 2. Center the `r fs('age_at_visit_days')` variable on that visit, so for that 
#'    patient it is `0` on the visit, a negative integer prior to the visit, and 
#'    a positive integer after. It will be seen later that this will help make
#'    survival analysis easier when we get to it. For patients where an event is
#'    never observed, these numbers will be shifted to that the value at the
#'    last visit is `-1`, _not_ `0`. This is so that we can easily distinguish 
#'    patients where the event never occurred.
#' 
#' Then we will be ready to probe the degree of agreement and size of lags 
#' between these variables.
#' 
#' We will then obtain diagonal matrices of various pairwise comparisons of
#' the timing of events. Not only the ones believed to reflect the same event, 
#' but all of them. This is so that we can do an overall sanity check on the 
#' relationships between  groups of variables. For example, if the supposed 
#' dates of surgery are in good agreement with each other, but they often happen 
#' after the supposed date of reoccurence, then that would be a problem we need 
#' to resolve before proceeding further. The below heatmap indicates the 
#' fraction of the column events that occurred before or at the same time as the
#' row events.
#+ medians_heatmap,cache=TRUE,fig.width=10,fig.height=10
dat3.gteq<-outer(dat3[,-1],dat3[,-1],FUN = function(xx,yy)
  mapply(function(aa,bb) mean(aa>bb,na.rm = T),xx,yy));
dat3.meds<-outer(dat3[,-1],dat3[,-1],FUN = function(xx,yy)
  mapply(function(aa,bb) quantile(aa-bb,.5,na.rm = T),xx,yy));
dat3.mabs<-outer(dat3[,-1],dat3[,-1],FUN = function(xx,yy)
  mapply(function(aa,bb) quantile(abs(aa-bb),.5,na.rm = T),xx,yy));
dat3.mabx<-outer(dat3[,-1],dat3[,-1],FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-max(abs(aa-bb),na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));
dat3.maxs<-outer(dat3[,-1],dat3[,-1],FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-max(aa-bb,na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));
dat3.mins<-outer(dat3[,-1],dat3[,-1],FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-min(aa-bb,na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));

# We need to exclude the 'n_dob' variable because it otherwise screws up the
# scaling. Also excluding all variables that have fewer than 10 non-null 
# observations
.dat3.keep <- !colnames(dat3.gteq) %in% 
  c(names(dat3)[colSums(!is.na(dat3))<10],'n_dob');
# This is to distinguish missing values from 0 values in a heatmap! No other way
# to do that!!
#layout(matrix(1,nrow=2,ncol=2));
par(bg='gray'); #,mfrow=1:2,mfcol=1:2);
heatmap(dat3.gteq[.dat3.keep,.dat3.keep],symm = T,na.rm = T,margins=c(10,10)
        ,col=colorRampPalette(c('pink','red','darkred'))(2000));
# ,col=color.palette(c('darkred','red','pink','white','lightblue','blue'
#                      ,'darkblue'),n.steps=c(3,200,2,2,200,3))(2000));
#' 
#' A lot to unpack here! We can already see that some variables are in close
#' agreement. Another early conclusion from this is that it isn't looking good 
#' for EMR events lining up with NAACCR events... they seem to lag behind NAACCR 
#' dates, especially diagnoses and surgical history. Might need to see if there
#' is something in the EMR that captures date of surgery (especially in Sunrise)
#' and chart review to see why the KC diagnosis codes lag behind NAACCR
#' diagnosis date.
#' 
#' Closer visualization of individual groups of variables can be accomplished by 
#' subsetting from this master table.
#' 
#' In addition to medians, we might also generate tables of the 5th and 95th 
#' percentiles of the differences as well as medians of the absolute values of
#' the differences. The former are for identifying directional trends and the
#' latter are to distinguish variables that track each other from variables that
#' are uncorrelated but their difference is unbiased in one direction versus 
#' another.
#' 
#' However, most of this shotgun approach is now superseded by the more focused 
#' investigation in the [initial diagnosis](#initial-diagnosis) and 
#' [surgery](#surgery-conclusion) sections in the main document above. This is
#' just for historic reference.
#' 
#' 
#' ::::: {.pbreak custom-style="pbreak"}
#' &nbsp;
#' :::::
#' 
#' 
# A4 variables -----------------------------------------------------------------
#'
#' # Variable descriptions {#sec:vars label="Appendix 4"}
#' 
#+ progfootnotes, results='asis'
# set new template for creating the internal link VALUES
# formals(fs)[c('url','template','retfun')] <- alist(paste0('#',str)
#                                           ,'[%1$s]: %2$s "%4$s"\n',cat);
formals(fs)[c('url','retfun')] <- alist(paste0('#',str),cat);
formals(fs)$template <- fstmplts$linkref;
.junk <- subset(dct0,varname %in% getOption('fs_reg')
                ,select = c('varname','colname_long')) %>% 
  apply(1,function(xx) fs(xx[1],tooltip=xx[2]));
#' Here are descriptions of the variables referenced in this document.
#+ readablefootnotes, results='asis'
# set new template for creating the internal link TARGETS
cat('***\n');
.junk <- subset(dct0,varname %in% getOption('fs_reg')
                ,select = c('varname','colname_long','chartname','comment'
                            ,'col_url')) %>% 
  apply(1,function(xx) cat(
    '######',xx[1],'\n\n',na.omit(xx[2:1])[1],':\n\n  ~ '
    ,ifelse(length(na.omit(xx[2:4]))>0
            ,iconv(paste(na.omit(xx[2:4]),collapse='; '),to='UTF-8',sub=''),'')
    ,ifelse(is.na(xx[5]),'',paste('\n\n  ~ Link:',xx[5])),'\n\n***\n'));
#' 
#' 
#' ::::: {.pbreak custom-style="pbreak"}
#' &nbsp;
#' :::::
#' 
# A5 audit ---------------------------------------------------------------------
#' # Audit trail {#sec:audit label="Appendix 5"}
walktrail()[,-5] %>% pander(split.tables=600,,justify='left');
#+ results='hide'
c()
