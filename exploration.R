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
options(gitstamp_prod=F);
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
.args_default_fs <- formals(fs);
# this is a copy of fs with different arguments, for note2self
n2s <- fs;
formals(n2s)[c('fs_reg','retfun')] <- alist(NULL,return);
formals(n2s)$template <- fstmplts$n2s;
# IN THE MAIN SECTION ONLY!! The retfun should be return for inline use and cat
# for use generating asis chunks.
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

# Place to create tables that will get used throughout script
dat2tte <- transmute_all(dat2a[,v(c_istte)],function(xx){
  ifelse(xx>dat2a$age_at_visit_days,NA,xx)});
dat2tte$patient_num <- dat2a$patient_num;
dat2tte$`Earliest Death` <- do.call(pmin,c(dat2tte[,v(c_death)],na.rm=T));
dat2tte$`Latest Death` <- do.call(pmax,c(dat2tte[,v(c_death)],na.rm=T));

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
[Yellow highlights]{.note2self custom-style='note2self'} are items with 
which I know I need to deal soon. Verbatim names of files, variables/elements,
or values are displayed in a special style, `like this`. Data element names are 
in addition linked to a glossary at the end of this document, e.g. 
[`Surgical Oncology`](#e%5Fsurgonc). This is where any relevant cleaning or 
tranformation steps will be described (in progress). Data elements from 
NAACCR usually have a NAACCR ID preceding them, e.g. 
[`1780 Quality of Survival`](#n%5Fqsurv). I try to use the word 'data element' to
describe data in its raw state and 'variable' to refer to analysis-ready data
that I have already processed. Often one variable incorporates information from
multiple data elements. Tables, figures, and sections are also linked from text 
that references them. If you have a Word version of this document, to follow a 
link, please hold down the 'control' key and click on it. The most current
version of this document can be found online at 
https://rpubs.com/bokov/kidneycancer and it has a built-in chat session."
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
# Why the heck was it these two not forming links until I pre-initialized them
# here? Can't find a difference from the others.
.junk <- fs(c('e_surgonc','n_dsdisc',v(c_tnm)));
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
#' presentation. My first-pass eligibility query returns `r nrow(dat2a)`
#' patients having one or more of the following in their records: an ICD9 code
#' of 189.0 or any ICD10 code starting with C64; the NAACCR item 
#' [`0400 Primary Site`](`r paste0(urls$dict_naaccr,'#400')`) having a value 
#' starting with C64 (`r fs('n_kcancer')`); or the SEER Primary Site having a 
#' value of `r fs('n_seer_kcancer')`.
#' 
#' My second pass criteria narrow the initial cohort to patients that have
#' NAACCR, defined as having a non-missing `r fs('n_ddiag')` and one or both of
#' `r fs('n_kcancer')` or `r fs('n_seer_kcancer')`. As can
#' be seen from [@tbl:cohortrectype] only `r length(kcpatients.naaccr)` of the
#' patient-set met these criteria and `r nrow(dat2a) - length(kcpatients.naaccr)` 
#' did not. Actually a total of `r sum(!is.na(dat2tte$n_fc))` patients had 
#' NAACCR records but `r sum(!is.na(dat2tte$n_fc)) - length(kcpatients.naaccr)` 
#' of them had kidney cancer documented _only_ in the EMR, but neither 
#' `r fs('n_kcancer')` or `r fs('n_seer_kcancer')` in NAACCR. [Next time I 
#' re-run my i2b2 query I will include all site of occurrence information from
#' NAACCR not just kidney. This will allow me to find out what types of cancer
#' these patients do in fact have.]`r n2s('6')` In 
#' [-@sec:diag; -@sec:surg; -@sec:recur] I identified additional 
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
#' between NAACCR and the EMR is never going to be 100%  with race, Hispanic 
#' ancestry, and marital status expected to be especially variable.
#' Nonetheless, if record linkage is correct, when patient counts for NAACCR and 
#' EMR are tabulated against each of the above variables, then _most_ of the 
#' values should agree.
#' 
#' I confirmed that this _is_ the case for marital status
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
#' **I found the following NAACCR elements sufficient for deriving all the above 
#' analytic variables: `r fs('n_hisp')`, 
#' `r fs(v(c_main_naaccr_vars),retfun=knitr::combine_words)`.** More details 
#' about how these were selected can be found in [-@sec:vartrn]. In addition the 
#' following will almost certainly be needed for covariates or mediators: 
#' `r knitr::combine_words(fs(c('n_sex','n_dob','n_marital','n_brthplc')),and='')`,
#' and any field whose name contains `Race`, `Comorbid/Complication`, 
#' `AJCC`, or `TNM`. For crosschecking it will also be useful to have 
#' `r fs('n_mets')`, `r fs('n_fc')`, and `r fs('n_mult')`. Additional items 
#' are likely to be needed as this project evolves, but **the elements listed so
#' far should be sufficient to replicate my analysis on de-identified State or
#' National NAACCR data**.
#' 
#' ## Merging NAACCR and EMR variables {#sec:merging}
#' 
#' EMR records can not only enrich the data with additional elements unavailable 
#' in NAACCR alone, but might also make it possible to fill in missing 
#' `r fs('n_ddiag')`,  `r fs('n_rx3170')` / `r fs('n_dsurg')`, 
#' `r fs('n_drecur')`, and `r fs('n_lc')`  values. It may even be possible to 
#' reconstruct entire records for the `r nrow(dat2a)-length(kcpatients.naaccr)` 
#' kidney cancer patients in the EMR lacking NAACCR records. However, this 
#' depends on how much the EMR and NAACCR versions of a variable agree when 
#' neither is missing.
#' 
# merging EMR and NAACCR -------------------------------------------------------
#' 
#' **Data elements representing date of death and Hispanic ethnicity are in 
#' sufficient agreement ( [@tbl:xc_hisp0] and [-@sec:death] ) to justify merging
#' information from the EMR and NAACCR.** The process for combining them is
#' described in the `r fs('a_tdeath')`, `r fs('a_hsp_strict')`, and 
#' `r fs('a_hsp_broad')` sections of [-@sec:vars] respectively. At this time I 
#' cannot merge diagnosis, surgery, or recurrence-- where data from both sources 
#' is available, EMR dates lag considerably behind NAACCR dates 
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
#' EMR data can still be used to flag records for exclusion pending verification
#' by chart review in cases where EMR codes for kidney cancer or secondary 
#' tumors precede `r fs('a_tdiag')` or `r fs('a_trecur')` respectively. [This 
#' can also apply to nephrectomy EMR codes and `r fs('a_tsurg')` but I will need 
#' to distinguish between the prior nephrectomy being due to cancer versus other 
#' indications.]`r n2s(4.0)`
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
#' The point of this section is **solely** to test whether my scripts succeeded 
#' in turning the raw data elements into a time-to-event (TTE) variables to 
#' which Kaplan-Meier curves can be fit without numeric errors or grossly 
#' implausible results. All the plots below are from a small random sample of 
#' the data-- N=`r sum(.testsamp[1:2])`, `r .testsamp['Hispanic']` Hispanic and 
#' `r .testsamp['non-Hispanic white']` non-Hispanic white, 
#' `r .testsamp['Unknown']` unknown excluded. This is further reduced in some
#' cases as described in the figure captions. These sample sizes are not 
#' sufficient  to detect clinically significant differences and, again, **this 
#' is not the goal yet**. The intent is only to insure that my software 
#' performs correctly while keeping myself blinded to the hold-out data on 
#' which the hypothesis testing will ultimately be done.
#' 
#' Furthermore, these survival curves are not yet adjusted for covariates such 
#' as age or stage at diagnosis. There are also refinements planned to the
#'  exclusion criteria which I discuss below in [@sec:nextsteps].
#' 
#' In all the plots below, the time is expressed in weeks and `+` signs denote
#' censored events (the last follow-up of patients for whom the respective 
#' outcomes were never observed). The lightly-shaded regions around each line 
#' are 95% confidence intervals. 
#' 
#+ .survfit_prep,results='hide'
# THIS PART JUST MAKES THE PLOT, NOT SHOWS IT YET 
.survfit_plot0 <- survfit_wrapper(dat2a,'a_tsurg',censrvars = c()
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
                                     ,coord_cartesian(ylim=0:1)
                                     ,theme_light()
                                     ,theme(legend.position = 'top')));
c();
#' 
#' Typically 2-4 weeks elapse diagnosis from surgery and providers try to 
#' not exceed 4 weeks. Nevertheless years may sometimes elapse due to factors 
#' such as an indolent tumors or loss of contact with the patient. About 15% of 
#' patients never undergo surgery [@pcRodriguez2018]. [@Fig:surg_survfit] is in
#' agreement with this. It can also be seen in [@fig:surg_survfit] that 
#' `r sum(with(.survfit_plot0$fit,n.event[time==0]))` surgeries seem to happen 
#' on the day of diagnosis. This is plausible if NAACCR diagnosis is based on 
#' pathology rather than clinical examination where a positive result is usually 
#' coded as a renal mass, not a cancer. [In my next data update I intend to 
#' also include all ICD9/10 codes for renal mass at which point I will revisit
#' the question of using EMR data to fill in missing diagnosis 
#' dates]`r n2s('6_0')` (see [@sec:nextsteps]).
#' 
#' ###### blank
#' 
#' ::::: {#fig:surg_survfit custom-style="Image Caption"}
#+ surv_surg,results='asis',fig.dim=c(3.1,3),fig.align='center'
.survfit_plot0$plot;
cat('

Number of weeks elapsed from ',fs('a_tdiag'),' (time 0) to ',fs('a_tsurg')
,' for ',.survfit_plot0$fit$n[1],' Hispanic and ',.survfit_plot0$fit$n[2]
,' non-Hispanic white patients with a 3-year follow-up period (any surgeries 
occurring more than 3 years post-diagnosis are treated as censored)');
#' :::::
#' 
#' ::::: {#fig:recur_survfit custom-style="Image Caption"}
#+ surv_recur,results='asis',fig.dim=c(3.1,3),fig.align='center'
(.survfit_plot1 <- update(.survfit_plot0,eventvars='a_trecur'
                          ,startvars='a_tsurg'
                          ,followup=365.25*6
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
[@fig:surg_survfit]  because patients not undergoing surgery are excluded. Here
the follow-up period is six years');
#' :::::
#' 
#' 
#' ###### blank
#'
#' ::::: {#fig:naaccrdeath_survfit custom-style="Image Caption"}
#+ naaccrdeath_survfit,results='asis',fig.dim=c(3.1,3),fig.align='center'
(.survfit_plot2 <- update(.survfit_plot1,eventvars='n_vtstat'
                          # ,plotadd = list(
                          #   guides(colour=guide_legend('')
                          #          ,fill=guide_legend(''))
                          #   ,coord_cartesian(ylim=c(.5,1))
                          #   ,theme_light()
                          #   ,theme(legend.position = 'top'))
                          ,main='Time from surgery to death\n'
                          ,ylab='Fraction alive'))$plot;
cat('

Like [@fig:recur_survfit] except now the outcome is ',fs('n_vtstat')
,' for ',.survfit_plot2$fit$n[1],' Hispanic  and ',.survfit_plot2$fit$n[2]
,' non-Hispanic white patients. Six-year follow-up');
#' :::::
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
indicate kidney as the principal site {#tbl:cohortrectype}");

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
# Hispanic table
# 
# .t1input %>% subset(patient_num %in% kcpatients.naaccr) %>% droplevels %>%
#   CreateTableOne(vars=setdiff(names(.),c('a_hsp_naaccr','patient_num'))
#                  ,strata='a_hsp_naaccr',data=.,includeNA = T ) %>%
#   print(printToggle=F,missing=T) %>% `[`(,-6) %>%
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
#' of analysis scripts opens four priority directions: more data, 
#' _external_ data, more covariates, and improved pre-processing at the i2b2 
#' end (Aim 1).
#' 
#' More data can be acquired by reclaiming values that are currently
#' inconsistent or missing. There are various ad-hoc consistency checks 
#' described in [-@sec:xchecks; -@sec:diag; -@sec:surg]
#' [I need to gather these checks in one place and 
#' systematically run them on every patient to get a total count of records that
#' need manual chart review (Dr. Rodriguez's protocol) and for each record a 
#' list of issues to resolve]`r n2s(21)`. 
#' 
#' To reclaim missing values I will need to solve the problem of lag and 
#' disagreement between the EMR and NAACCR ([@sec:merging]). [I will meet with 
#' the MCC NAACCR registrar and learn where exactly in the EMR and other sources 
#' she looks to abstract 
#' `r fs(v(c_main_naaccr_vars),retfun=knitr::combine_words)`. I will also meet 
#' with personnel experienced in Urology chart review to learn their 
#' methods.]`r n2s(4.1)`. This may lead to  improvements in the CIRD ETL 
#' process. I also plan on adding all ICD codes for 'renal mass' 
#' [@pcRodriguez2018] to my i2b2 query ([-@sec:diag]). Meanwhile, in response to
#' researcher questions including my own, CIRD staff have identified thousands 
#' of NAACCR entries and surgery billing records that got excluded from i2b2 
#' because they are not associated with visits to UT Health clinics. After the 
#' next i2b2 refresh we expect an increased number of patients and possible 
#' improved agreement of event dates between EMR and NAACCR.
#' 
#' [For external data I will request non-aggregated limited/deidentified records 
#' from the Texas Cancer Registry. I will also look at the NCDB dataset obtained 
#' by Urology]`r n2s('69')` to see if it has the 
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
#' and grade data.]`r n2s(10)` If genomic data from 
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
#' [@bokov_denormalize_2016] development planned for Aim 1. On the contrary, the
#'  reason I was able to make this much progress
#' in one month is that the data linkage and de-identification was done by the 
#' CIRD i2b2 ETL, the data selection was simplified by the i2b2 web client, and
#' an enormous amount of post-processing was done by my DataFinisher app that
#' is integrated into our local i2b2. During the work I present here I found
#' several additional post-processing steps that generalize to other studies
#' and [I will integrate those into DataFinisher so that the data it outputs is
#' even more analysis-ready.]`r n2s('58')` This will, in 
#' turn, will simplify the logistics of Aim 3. 
#' 
#' [While I am incorporating the new methods into DataFinisher, I will also 
#' reorganize and document the code so I can present it to Dr. Murphy and his 
#' informatics team for review and input.]`r n2s(59)`
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
#' [Need to tabulate the frequencies of various combinations of TNM 
#' values]`r n2s(10.0)`
#' 
#' ## Observations about NAACCR staging
#' 
.astages<-sprintf('n_a%d%s',rep(6:7,3),c('t','n','m')) %>% sort %>% rev;
# OMG what a hack. Pasting the corresponding AJCC6/7 desriptor fields onto the 
# left side of all AJCC6/7 TNM fields. The idea here is to build an argument for 
# mutate dynamically, without a lot of copy-paste code nor without yet another 
# c_ group in the data dictionary if this turns out to not be needed
.mutate_stages <- .astages %>% setNames(sprintf(
  'ifelse(is.na(%1$s)|is.na(%1$s),NA,paste0(%1$sd,%1$s))',.),.) %>% 
  lapply(function(xx) parse(text=xx)[[1]]);
# now create a data.frame on which to do the various ftables below
dat1tnmcounts <- subset(dat1,a_n_visit) %>% as.data.frame %>% 
  select(c(v(c_tnm),paste0(.astages,'d'))) %>% 
  mutate_all(function(xx) gsub('"','',xx)) %>% list %>% c(.mutate_stages) %>% 
  do.call(mutate,.);
# note that here the pipeline is nested within the sapply statement... the 
# inline function is one big pipe. This is for viewing the counts of various 
# TNM/stage values
tnmtabs <- sapply(c('c_stagegrp','c_staget','c_stagen','c_stagem')
                  ,function(xx) eval(substitute(v(xx),list(xx=xx))) %>% 
                    select(dat1tnmcounts,.) %>% table(useNA='if') %>% 
                    as.data.frame(stringsAsFactors=F) %>% subset(Freq>0) %>% 
                    arrange(desc(Freq)),simplify = F);
#' `r knitr::combine_words(fs(.astages))` are missing if and only if 
#' `r knitr::combine_words(fs(paste0(.astages,'d')))` are also missing, 
#' respectively. For the tables in this section, the counts are by visit rather
#' than by unique patient since the question of interest is how often do the 
#' stages assigned to the same case agree with each other. Each of the tables 
#' shows the 20 most common combinations of values.
#' 
#+ stagegrp
.tc <- paste0('Frequency of various combinations of '
              ,knitr::combine_words(fs(v(c_stagegrp))),' {#tbl:stagegrp}');

pander(head(tnmtabs$c_stagegrp,20),col.names=c(fs(colnames(tnmtabs$c_stagegrp)[1:4])
                                             ,'N'),caption=.tc);
#' 
#+ staget
.tc <- paste0('Frequency of various combinations of '
,knitr::combine_words(fs(v(c_staget))),' {#tbl:staget}');

pander(head(tnmtabs$c_staget,20),col.names=c(fs(colnames(tnmtabs$c_staget)[1:4])
                                             ,'N'),caption=.tc);
#' 
#+ stagen
.tc <- paste0('Frequency of various combinations of '
              ,knitr::combine_words(fs(v(c_stagen))),' {#tbl:stagen}');

pander(head(tnmtabs$c_stagen,20),col.names=c(fs(colnames(tnmtabs$c_stagen)[1:4])
                                             ,'N'),caption=.tc);
#' 
#+ stagem
.tc <- paste0('Frequency of various combinations of '
              ,knitr::combine_words(fs(v(c_stagem))),' {#tbl:stagem}');

pander(head(tnmtabs$c_stagem,20),col.names=c(fs(colnames(tnmtabs$c_stagem)[1:4])
                                             ,'N'),caption=.tc);
#' 
#+ tnm_agree_miss
.tnmmissvals <- c('N-',NA,'UNK','-');
.tnmagree<-lapply(tnmtabs,function(xx) {
  dd<-xx[!xx[,1]%in%.tnmmissvals&!xx[,2]%in%.tnmmissvals,];
  sum(dd[dd[,1]==dd[,2],'Freq'])/sum(dd$Freq)*100}) %>% sprintf('%3.1f%%',.);
.tnmnoa7 <- lapply(tnmtabs,function(xx) {
  dd <- xx[-1,];
  sum(dd[dd[,1]%in%.tnmmissvals,'Freq'])/sum(dd$Freq)*100}) %>% 
  sprintf('%3.1f%%',.);
.tnmrescueable <- lapply(tnmtabs,function(xx) {
  #dd<-xx[!is.na(xx[,1])|!is.na(xx[,2]),];
  dd <- xx[-1,];
  sum(dd[dd[,1]%in%.tnmmissvals & !dd[,2]%in%.tnmmissvals,'Freq'])/
    sum(dd$Freq)*100}) %>% sprintf('%3.1f%%',.);
#' In [@tbl:stagegrp; @tbl:staget; @tbl:stagen; @tbl:stagem], when both the 
#' AJCC-7 and AJCC-6 values are non-missing they agree with each other 
#' `r knitr::combine_words(.tnmagree)` of the time for T, N, and M respectively.
#' There are `r knitr::combine_words(.tnmnoa7)` AJCC-7 values missing but
#' `r knitr::combine_words(.tnmrescueable)` can be filled in from AJCC-6 for overall stage, T, 
#' N, and M respectively.
#' 
.tc <- paste0('
This is proof of feasibility for extracting stage and grade at diagnosis for 
each NAACCR patient for import into the EMR system (e.g. Epic/Beacon). Clinical
and pathology stage descriptors are also available in NAACCR. Here the '
,fs('patient_num'),' and ',fs('start_date'),' are de-identified but with proper 
authorization they can be mapped to MRNs or internal database index 
keys. {#tbl:stageraw}');
dat1[,c('patient_num','start_date','n_a7t','n_a7n','n_a7m','n_a7sg')] %>% 
  subset(patient_num %in% kcpatients.naaccr) %>% na.omit %>% head(20) %>% 
  pander(.,col.names=fs(colnames(.)),caption=.tc);
# subset(dat2a[,c('patient_num',v(c_tnm))],patient_num %in% kcpatients.naaccr) %>% 
#   na.omit() %>% 
#   setNames(fs(c('patient_num',v(c_tnm)))) %>%
#   # show a sampling of rows and columns that fits on the page and remove the
#   # the extra quotation marks
#   `[`(1:15,1:8) %>% apply(2,function(xx) gsub('["]','',xx)) %>% 
#   pander(caption=.tc);
#' 
# A2 next steps ---------------------------------------------------------------
#formals(fs)$template <- fstmplts$link_colnamelong;
#formals(fs)$retfun <- as.name('return');
#' 
#' `r md$pbreak`
#' # : Next steps {#sec:todo label="Appendix 2"}
#' All the TODO items are now [tracked on to GitHub](`r urls$git_tix`) as well
#' as linked from their respective yellow-highlighted text throughout the 
#' document.
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
#' only EMR and no NAACCR records, so they count as missing rather than 
#' discrepant.
#' 
#+ xc_mrtl
(xc_mrtl <- with(dat2a,table(n_marital,e_marital,useNA='if')) %>% addmargins %>%
  set_colnames(.,gsub('@','',colnames(.)))  %>% 
   with_attrs(.,list(xc=cbind(1:7,c(2:4,6:9))
                     ,rws=setdiff(rownames(.),c(NA,'Sum','','u'))
                     ,cls=setdiff(colnames(.),c(NA,'Sum','','u'))))) %>% 
  pander(.,emphasize.strong.cells=attr(.,'xc')
         ,caption='Marital status has good agreement between NAACCR and EMR. {#tbl:xc_marital}');
#' 
#+ xc_sex
(xc_sex <- with(dat2a,table(n_sex,sex_cd,useNA = 'ifany')) %>% addmargins() %>% 
  with_attrs(.,list(xc=cbind(1:2,1:2)
                    ,rws=setdiff(rownames(.),c(NA,'Sum','','u'))
                    ,cls=setdiff(colnames(.),c(NA,'Sum','','u'))))) %>% 
  pander(.,emphasize.strong.cells=attr(.,'xc')
         ,caption='Sex has good agreement between NAACCR and EMR. {#tbl:xc_sex}');
#+ xc_race
(xc_race <- with(dat2a,table(a_n_race,race_cd,useNA = 'ifany')) %>% addmargins() %>% 
  with_attrs(.,list(xc=cbind(1:6,1:6)
                    ,rws=setdiff(rownames(.),c(NA,'Sum',''))
                    ,cls=setdiff(colnames(.),c(NA,'Sum',''))))) %>% 
  
  pander(.,memphasize.strong.cells=attr(.,'xc')
         ,caption='Race has good agreement between NAACCR and EMR. {#tbl:xc_race}');

#+ xc_hisp0
.tc <- paste0('Hispanic designation has good agreement between NAACCR and EMR.
Here the ',fs('n_hisp'),' variable was simplified by binning into `Hispanic` and
`non-Hispanic`. {#tbl:xc_hisp0}');
(xc_hisp<-with(dat2a,table(recode_factor(n_hisp,'Non_Hispanic'='Non_Hispanic'
                                ,'Unknown'='Non_Hispanic'
                                ,.default='Hispanic')
                 ,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>% 
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% 
  with_attrs(.,list(xc=cbind(1:2,1:2)
                    ,rws=setdiff(rownames(.),c(NA,'Sum',''))
                    ,cls=setdiff(colnames(.),c(NA,'Sum',''))))) %>% 
  pander(.,emphasize.strong.cells=attr(.,'xc'),caption=.tc);
#+ xc_hisp1
.tc <- paste0('As [@tbl:xc_hisp0] but with all the different levels of '
,fs('n_hisp'),' shown. {#tbl:xc_hisp1}');
with(dat2a,table(n_hisp,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>%
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(emphasize.strong.cells=cbind(1:7,c(1,2,2,2,2,2,2))
                          ,caption=.tc);
# xc_dob -----------------------------------------------------------------------
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
#' _The tables of patients with discrpant birthdates have been removed because 
#' the only apply to 15 patients, and are mostly empty. They can still be 
#' viewed in the 181009 archival version of this document for 
#' `r knitr::combine_words(mprintf(paste0('[%1$s](',urls$exp_raw_181009,'#tbl:xc_dob_%1$s)'),c('marital','sex','race','hisp','surg')))`_
#' 
#' ###### blank
#' 
#  event indicators -------------------------------------------------------------
#' ## Which EMR and NAACCR variables are reliable event indicators? {#sec:vartrn}
#' 
#' For each of the main event variables `r fs('a_tdiag')`, `r fs('a_tsurg')`,
#' `r fs('a_trecur')`, and `r fs('a_tdeath')` / `r fs('n_vtstat')` there were 
#' multiple candidate data elements in the raw data. If such a 
#' family of elements is in good agreement overall then individual missing 
#' dates can be filled in with the earliest non-missing dates from other data
#' elements in that family (except for mortality where the _latest_ non-missing
#' date would make more sense). But to do this I needed not only to establish
#' qualitative agreement as I did for demographic variables in [@sec:linkagever]
#' and [-@sec:xchecks] but also determine how often these dates lag or lead each
#' other and by how much. The plots in this section use the y-axis to represent
#' time for patient records arranged along the x-axis. They are arranged in an
#' order that varies from one plot to another, chosen for visual 
#' interpretability. Each vertical slice of a plot represents one patient's 
#' history, with different colors representing events as documented by different
#' data elements. The goal is to see the frequency, magnitude, and direction of
#' divergence for several variables at the same time.
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
#' [I will need to consult with a NAACCR registrar about what `r fs('n_fc')` 
#' actually means]`r n2s(4.2)` but it does not appear to be a first visit nor 
#' first diagnosis. As can be seen in [@fig:diag_plot] and [@tbl:diag_lag], the 
#' first ICD9 or ICD10 code most often occurs after initial diagnosis, sometimes
#' before the date of diagnosis, and coinciding with the date of diagnosis 
#' rarest of all. Several of the ICD9/10 first observed dates lead or trail the 
#' `r fs('n_ddiag')` by multiple years.
#' 
#' ::::: {#fig:diag_plot custom-style="Image Caption"}
#+ diag_plot,results='asis', fig.dim=c(4.5,3)
# .diag_plot, opts.label='fig_opts'
par(xaxt='n');
.ev_diag_plot <- event_plot(
  dat3,'e_kc_i10',tunit='months',type='s'
  ,ylab='Months since Diagnosis'
  ,xlab='Patients, sorted by time to first ICD10 code\n\n\n'
  ,main='Time from Diagnosis to First ICD9/10 Code');
abline(h=c(-3,0,3),lty=c(2,1,2),col='blue');
#abline(v=seq_len(nrow(.ev_diag_plot)),lwd=0.1);
points(.ev_diag_plot$e_kc_i9,col='#ff000070',pch='-');
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
#' `r fs('n_ddiag')` by more than 3 months. An additional `r .eplot_diag_summ[2]`
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
#' to learn whether there is anything in the EMR distinguishes first kidney 
#' cancer occurrences besides lack of previous 
#' diagnosis.]`r n2s(4.3)`
#  surgery ======================================================================
#' ### Surgery {#sec:surg}
#' 
#' To construct the `r fs('a_tsurg')` analytic variable I considered 
#' `r fs('n_dsurg')`, `r fs('n_rx1260')`, `r fs('n_rx1270')`, and 
#' `r fs('n_rx3170')` from NAACCR as well as earliest occurrences of 
#' `r fs('e_i9neph')`, `r fs('e_i10neph')`, or `r fs('e_hstneph')` from the EMR. 
#' In the plots and tables below I show why I decided to use `r fs('n_rx3170')` 
#' as the surgery date and when that is unavailable, to fall back on 
#' `r fs('n_dsurg')`. The other data elements are not used **except to flag 
#' potentially incorrect records if they occur earlier than the date of 
#' diagnosis**.
#' 
#' ###### blank
#' 
#' ::::: {#fig:surg0_plot0 custom-style="Image Caption"}
#+ surg0_plot0,results='asis',fig.dim=c(4.5,3)
par(xaxt='n');
.eplot_surg0 <- mutate(dat3,nrx=pmin(n_dsurg,n_rx3170,n_rx1270,n_rx1260)) %>% 
  event_plot('n_dsurg','n_rx3170',tunit='months',type='s',ltys = c(1,1)
             ,main='Time from Diagnosis to Surgery'
             ,ylab='Months Since Diagnosis'
             ,xlab='Patients, sorted by time to surgery\n\n\n'
             ,subset=bquote(patient_num %in% intersect(
               kcpatients_surgreason$`Surgery Performed`
               ,kcpatients.naaccr))
             ,xlim=c(0,length(kcpatients_surgreason$`Surgery Performed`))
             ,ylim=c(-10,60));
abline(h=c(-3,0,3),col='blue',lty=c(2,1,2));
.eplot_surg0$icd <- apply(.eplot_surg0[,c('e_i9neph','e_i10neph','e_hstneph')]
                          ,1,min,na.rm=T);
.eplot_surg0$icd[is.infinite(.eplot_surg0$icd)]<-NA;
lines(.eplot_surg0$icd,type='s',col='#FF00FF70');
with(.eplot_surg0,abline(v=which(icd<nrx),col='#FFFF0030',lwd=4));

cat("\n\nAbove is a plot of all patients sorted by "
    ,fs('n_dsurg')," (black line).  On the same axis is ",fs('n_rx3170')
    ," (red line) which is almost  identical to ",fs('n_dsurg')," except for a small
number of cases where it occurs later than ",fs('n_dsurg'),". It never occurs
earlier. The violet lines indicate for each patient the earliest EMR code
implying that a surgery had taken place (acquired absence of kidney ICD V/Z 
codes or surgical history of nephrectomy). The blue horizontal line is "
,fs('n_ddiag')," with the dashed lines representing a 3-month window in both
directions.");
#' :::::
#' 
#' ::::: {#fig:surg0_plot1 custom-style="Image Caption"}
#+ .surg0_plot1,results='asis',fig.dim=c(4.5,3)
par(xaxt='n');
.eplot_surg0 <- mutate(dat3,nrx=pmin(n_dsurg,n_rx3170,n_rx1270,n_rx1260)) %>% 
  event_plot('n_dsurg','n_rx3170',tunit='months',type='s',ltys = c(1,1)
             ,main='Time from Diagnosis to Surgery'
             ,ylab='Months Since Diagnosis'
             ,xlab='Patients, sorted by time to surgery\n\n\n'
             ,subset=bquote(patient_num %in% intersect(
               kcpatients_surgreason$`Surgery Performed`
               ,kcpatients.naaccr))
             ,xlim=c(0,length(kcpatients_surgreason$`Surgery Performed`))
             ,ylim=c(-10,60));
abline(h=c(-3,0,3),col='blue',lty=c(2,1,2));
.eplot_surg0$icd <- apply(.eplot_surg0[,c('e_i9neph','e_i10neph','e_hstneph')]
                          ,1,min,na.rm=T);
.eplot_surg0$icd[is.infinite(.eplot_surg0$icd)]<-NA;
lines(.eplot_surg0$n_rx1270,col='#00FF0060',type='s');
lines(.eplot_surg0$n_rx1260,col='#00FFFF60',type='s');

cat("\n\nIn the above plot the ",fs('n_rx1270')," (green) and "
    ,fs('n_rx1260')," (cyan) events are superimposed on time till ",fs('n_dsurg')
    ," like in [@fig:surg0_plot0] (but violet lines for nephrectomy EMR codes are 
omitted for readability). The ",fs('n_rx1270')," and ",fs('n_rx1260')
    ," variables trend earlier than ",fs('n_dsurg'));
#' :::::
#' 
#' ###### blank
#' 
#' In [@fig:surg0_plot0] the `r sum(with(.eplot_surg0,icd<nrx),na.rm=T)` 
#' patients for which the earliest EMR nephrectomy code occurs before the 
#' earliest NAACCR possible record of surgery are highlighted in yellow. Among 
#' the remaining `r with(.eplot_surg0,sum(icd>=nrx,na.rm=T))` patients who have 
#' an EMR code for nephrectomy, there are 
#' `r .surg0thresh<-3; with(.eplot_surg0,sum(icd>(n_dsurg+.surg0thresh),na.rm=T))` 
#' for whom it happens more than `r .surg0thresh` months after `r fs('n_dsurg')` 
#' and those lags have a median of 
#' `r with(.eplot_surg0,round(median(icd[icd>(n_dsurg+.surg0thresh)],na.rm=T),1))`
#' months. This level of discrepancy disqualifies `r fs('e_i9neph')`, 
#' `r fs('e_i10neph')`, and `r fs('e_hstneph')` from being used to fill in 
#' missing NAACCR dates. [This may change after the next i2b2 update
#' in which the fix to the "visit-less patient" problem will be 
#' implemented]`r n2s(16)` ([@sec:nextsteps])
#' 
#' ###### blank
#' 
#' ::::: {#fig:surg1_plot custom-style="Image Caption"}
#+ .surg1_plot,results='asis',fig.dim=c(4.5,3)
par(xaxt='n');
.eplot_surg1 <- mutate(dat3,nrx=pmin(n_dsurg,n_rx3170,n_rx1270,n_rx1260)) %>% 
  event_plot('n_dsurg','n_rx3170',tunit='months',type='s',ltys = c(1,1)
             ,main='Time from Diagnosis to Surgery'
             ,ylab='Months Since Diagnosis'
             ,xlab='Patients, sorted by time to surgery\n\n\n'
             ,subset=bquote(patient_num %in% setdiff(
               kcpatients.naaccr,kcpatients_surgreason$`Surgery Performed`))
             ,xlim=c(0,length(kcpatients_surgreason$`Surgery Performed`))
             ,ylim=c(-10,60));
abline(h=0,col='blue');
.eplot_surg1$icd <- apply(.eplot_surg1[,v(c_nephx,dat3)[1:5]],1,min,na.rm=T);
.eplot_surg1$icd[is.infinite(.eplot_surg1$icd)]<-NA;
lines(.eplot_surg1$icd,type='s',col='#FF00FF50');
with(.eplot_surg1,abline(v=which(icd<nrx),col='#FFFF0030',lwd=4));
lines(.eplot_surg1$n_rx1270,col='#00FF0060',type='s');
lines(.eplot_surg1$n_rx1260,col='#00FFFF60',type='s');
cat("

Above is a plot equivalent to [@fig:surg0_plot1] but for patients who do **not**
have a ",fs('n_surgreason')," code equal to `Surgery Performed`. There are many"
,fs('n_rx1270')," and ",fs('n_rx1260')," events but only a small number of "
,fs('n_dsurg')," (black) and ",fs('n_rx3170')," (red). The ",fs('n_dsurg')
," and ",fs('n_rx3170')," that do occur track each other perfectly. Together 
with NAACCR data dictionary's description this suggests that ",fs('n_rx3170')
," is the correct principal surgery date in close agreement with "
,fs('n_dsurg'),", so perhaps missing ",fs('n_rx3170')," values can be filled 
from ",fs('n_dsurg'),". However ",fs('n_rx1270')," and ",fs('n_rx1260')
," seem like non-primary surgeries or other events and cannot be used to fill in
missing values"); 
#' :::::
#' 
#' ###### blank
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
  sapply(fs,retfun=return) %>% (knitr::combine_words);
.tc <- paste0("As can be seen in the table below, the variables ",t_priorcond
," _sometimes_ precede ",fs('n_ddiag')," by many weeks but they _usually_ 
follow ",fs('n_ddiag')," by more weeks than do ",fs('n_dsdisc')," and "
,fs('n_dsurg'),". Those two NAACCR variables never occur before "
,fs('n_ddiag')," and usually occur within 2-8 weeks after it. This is 
another way of summarizing how much the EMR variables lag behind NAACCR 
variables. {#tbl:priordiag}");
pander(dat3_surg_summary,caption=.tc,row.names=fs(rownames(dat3_surg_summary)));
#' It makes sense that the Epic EMR lags behind NAACCR. As an outpatient system, 
#' it's probably recording visits after the original surgery, and perhaps we are 
#' not yet importing the right elements from Sunrise EMR. In [@sec:nextsteps] I
#' outline possible remedies to that. For now, `r t_priorcond` can still be used 
#' to exclude cases as not first-time occurrences if it precedes diagnosis. 
#' Would I lose a lot of cases to such a criterion? 
#' 
#+ before_sameday_after_00,cache=TRUE

# This table predates the e_table() function and I need to migrate it to that
# function when time permits.
e_table_neph <- mutate_all(dat3[,v(c_nephx)]
                           # break each column 
                           ,function(xx) {
                             cut(xx-dat3$n_ddiag
                                 ,breaks=c(-Inf,-.00001,.00001,Inf)
                                 ,lab=c('before','same-day','after')
                                 ,include=T)}) %>%
  sapply(table,useNA='always') %>% t;
  pander(e_table_neph,caption='
How often ICD9/10 or surgical history codes for nephrectomy precede diagnosis
and by how much {#tbl:neph_b4_diag}',row.names=fs(rownames(e_table_neph)));

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
# discharge) contribute anything more than `r fs('n_dsurg')`? There are `r
# nrow(subset(dat3,is.na(n_dsurg)&!is.na(n_dsdisc)))` non-missing values of
# `n_dsdisc` when `r fs('n_dsurg')` is missing. As can be seen from the plot
# below where `n_dsdisc` are the red dashed lines and `r fs('n_dsurg')` are the
# black lines, both relative to date of diagnosis, `n_dsdisc` either coincides
# with `r fs('n_dsurg')` or lags by multiple weeks, as might be expected of a
# discharge date (what is the plausible threshold on time from surgery to
# discharge?).
#' 
#' Only a small number of cases would be disqualified. Another important 
#' question is the level of agreement between `r fs('n_surgreason')` and the 
#' NAACCR data elements that are candidates for comprising the surgery variable.
#' 
.tc <- paste0("Every NAACCR candidate data element (columns) tabulated against "
,fs('n_surgreason')," (rows). The bold cells are ones consistent with their 
respective data elements indicating the primary surgery. The second row is 
italicized because surgery may still occur as a non-primary course of treatment.
Nevertheless the counts in the `FALSE` columns should be greater than the counts 
in the `TRUE` columns for every row except the first. ",fs('n_rx3170')
," and ",fs('n_dsurg')," are in close agreement with each other and have the 
fewest deviations from expected behavior of a primary surgery data 
element {#tbl:srgvars}");
lapply(v(c_nephx_naaccr),function(ii){
  table(dat2a$n_surgreason,dat2a[[ii]]<=dat2a$age_at_visit_days) %>% 
    set_colnames(.,paste0(ii,' = ',colnames(.)))
  }) %>% do.call(cbind,.) %>% 
  pander(emphasize.strong.cells=cbind(1:6,rep(c(0,2,4,6),each=6)+c(2,rep(1,5)))
         ,emphasize.italics.rows=2
         ,caption=.tc);

#' 
#' In summary, based on [@fig:surg0_plot0] and [@tbl:diag_lag] 
#' `r t_priorcond` can only be used to disqualify patients for having erroneous
#' records or previous history of kidney cancer but cannot fill in missing 
#' diagnosis dates. Based on 
#' [@fig:surg0_plot1; @fig:surg1_plot], and [@tbl:rectype_cstatus] 
#' `r fs('n_rx1270')` and `r fs('n_rx1260')` are not necessarily always surgery 
#' events. This leaves `r fs('n_rx3170')` with `r fs('n_ddiag')` as a fallback. 
#' [When I meet with  the NAACCR regisrar I will seek their feedback about this 
#' approach and I will ask them about the most reliable way to identify the 
#' first kidney cancer occurrence for a patient if they have several 
#' (overlapping?) NAACCR entries. I also need to ask a chart abstraction expert 
#' about the best way to find in Epic and in Sunrise the date of a patient's 
#' first nephrectomy]`r n2s(4.4)`
#' 
#  re-occurrence ================================================================
#' ### Re-occurrence {#sec:recur}
#' 
.emr_recur <- lapply(v(c_recur)[-1],fs) %>% (knitr::combine_words);
#' Candidate  data elements for constructing the `r fs('a_trecur')` variable 
#' were `r fs('n_cstatus')`, `r fs('n_rectype')`, and `r fs('n_drecur')` from
#' NAACCR. Our site is on NAACCR v16, not v18, so we do not have 
#' [`1772 Date of Last Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1772).
#' According to the v16 standard, `r fs('n_lc')` should be used instead. From
#' the EMR the candidates were `r length(v(c_recur))-1` ICD9/10 codes for 
#' secondary tumors. In [@tbl:rectype_cstatus] I reconcile `r fs('n_cstatus')` and 
#' `r fs('n_rectype')`. 
#' 
#' ###### blank
#' 
#+ rectype_cstatus
.tc <- paste0(fs('n_cstatus'),' is in good agreement with ',fs('n_rectype')
,'. Almost all ',fs('n_cstatus'),' `Tumor_Free` patients also have
`Disease-free` in their ',fs('n_rectype'),' column, the `Tumor` ones have a 
variety of values, and the `Unknown` ones are mostly `Unknown if recurred or was 
ever gone`. {#tbl:rectype_cstatus}');

subset(dat2a,!patient_num %in% kcpatients.naaccr_dupe) %>% droplevels() %>%
  with(table(n_rectype,n_cstatus)) %>% pander(caption=.tc);
#' 
#' ###### blank
#' 
#' `r fs('n_rectype')` can be simplified by leaving values of `Disease-free` 
#' (0), `Never disease-free` (70), and `Unknown if recurred or was ever gone` 
#' (99) as they are; if there were multiple values for the same case
#' and one of those values was 70 then defaulting to `Never disease-free`; and 
#' recoding all other values as simply `Recurred`. I named this analytic 
#' variable `r fs('a_n_recur')`.
#' 
#' ###### blank
#
t_recur_drecur <- with(dat2a
                       ,table(a_n_recur
                              ,`Has recurrence date`=n_drecur<=age_at_visit_days
                              ,useNA='if'));
# We encountered a situation where there are no patients with recurrence dates. 
# This code is to check for that and put in a dummy column with 0's to fill in
# the table so that code further down does not fail.
if(length(.t_missing_col <- setdiff(c('TRUE','FALSE')
                                    ,colnames(t_recur_drecur)))>0){
  .t_new_colnames <- c(colnames(t_recur_drecur),.t_missing_col);
  t_recur_drecur <- cbind(t_recur_drecur,0) %>% as.table %>% 
    set_colnames(.t_new_colnames);
}

.tc <- paste0('Here is the condensed version after having followed the above 
rules. Looks like the only ones who have a ',fs('n_drecur')," are the ones which 
also have a `Recurred` status for ",fs('a_n_recur')," (with "
,t_recur_drecur['Recurred','FALSE']," missing an ",fs('n_drecur'),").");
# Check if there are in fact any exceptions to the pattern and only run the 
# below if there are (i.e. .n_recur_except is an integer rather than an error)
# TODO: We need a better, more generic way of handling conditional text!
.n_recur_except <- try(t_recur_drecur['Never disease-free','TRUE']);
if(is.numeric(.n_recur_except) && .n_recur_except>0) .tc <- paste0(.tc
," The only exception ",if(.n_recur_except>1) 'are ' else 'is '
,.n_recur_except," `Never diease-free` 
patient",if(.n_recur_except>1) 's'," with a ",fs('n_drecur')
," {#tbl:rectype_drecur}");

t_recur_drecur %>% set_colnames(.,paste0('Recur Date=',colnames(.))) %>% 
  pander(emphasize.strong.cells=cbind(2:5,c(1,1,2,1)),caption=.tc);
#' 
#' This explains why  `r fs('n_drecur')` values are relatively rare in the 
#' data-- they are specific to actual recurrences which are not a majority of 
#' the cases. This is a good from the standpoint of data consistency. Now we 
#' need to see to what extent the EMR codes agree with this. 
#' 
#' 
#' ::::: {#fig:recur_plot custom-style="Image Caption"}
#+ recur_plot,results='asis',fig.dim=c(4.5,3)
par(xaxt='n');
.eplot_recur0 <-subset(dat3,patient_num %in% 
                         setdiff(kcpatients_surgreason$`Surgery Performed`
                                 ,kcpatients.naaccr_dupe)) %>% 
  mutate(.,rec=na_if(apply((.)[,v(c_recur)[-15]],1,min,na.rm=T),Inf)) %>% 
  event_plot('rec',start_event = 'n_dsurg',type='s',ltys=c(1,1)
             ,main='Time from Surgery to Recurrence'
             ,ylab='Months Since Surgery'
             ,xlab='Patients, sorted by time to first mets\naccording to EMR\n\n'
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
points(.eplot_recur0$n_drecur,col='red',pch='-',cex=1);
with(.eplot_recur0
     ,abline(v=which(patient_num %in% kcpatients_rectype$Recurred & 
                       is.na(n_drecur)),col='red',lty=3));
cat("

In the above plot, the black line represents months elapsed between surgery and
the first occurence of an EMR code for secondary tumors, if any. The horizontal
red line segments indicate individual ",fs('n_drecur'),". The dotted vertical
red lines denote `Recurred` patients who are missing a ",fs('n_drecur')
,". The blue horizontal line is the date of surgery and the 
dotted horizontal lines above and below it are +- 3 months. Patients whose "
,fs('n_rectype')," is `Disease-free` are highlighted in green, 
`Never disease-free` in yellow, and `Recurred` in red. There are "
,length(kcpatients.naaccr_dupe)," patients with multiple NAACCR records, 
and all records for these patients have been excluded from this plot");
#' :::::
#' 
#' ###### blank
#' 
#' The green highlights in [@fig:recur_plot] are _mostly_ where one would 
#' expect, but why are there
#' `r nrow(subset(.eplot_recur0,patient_num %in% kcpatients_rectype[['Disease-free']] & !is.na(rec)))`
#' patients on the left side of the plot labeled `Disease-free` that have EMR 
#' codes for secondary tumors? Also, there are 
#' `r nrow(subset(.eplot_recur0,rec<0))` patients with metastatic tumor codes 
#' earlier than `r fs('n_dsurg')` and of those 
#' `r nrow(subset(.eplot_recur0,rec< -3))` occur more than 3 months prior to 
#' `r fs('n_dsurg')`. Did they present with secondary tumors to begin with but
#' remained disease free after surgery? [These are questions to ask the NAACCR
#' registrar]`r n2s(4.5)`. The EMR codes are in better
#' agreement with `r fs('n_drecur')` than the data elements in [-@sec:diag] and
#' [-@sec:surg] so it might make sense to back-fill the few `r fs('n_drecur')`
#' that are missing but first I want to make sure I [understand how to reliably
#' distinguish on the EMR side genuine recurrences from secondary tumors that
#' existed at presentation]`r n2s(4.6)`. The small 
#' number of cases affected either way lowers the priority of this isuse.
#' For now I will rely only on `r fs('n_drecur')` in constructing the analytical
#' variable `r fs('a_trecur')`. 
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
#' Unlike diagnosis ([-@sec:diag]), surgery ([-@sec:surg]), and recurrence 
#' ([-@sec:recur]) death dates exhibit good agreement between various 
#' sources and can be used to supplement the data available from NAACCR.
#'
#'
#' ::::: {#fig:death_plot custom-style="Image Caption"}
#+ .death_plot,results='asis',fig.dim=c(4.5,3)
par(xaxt='n');
.eplot_death <- event_plot(dat3,'n_lc',start_event = 'n_ddiag'
                           ,ylim=c(0,300)
                           ,main='Time from Diagnosis to Death'
                           ,ylab='Months Since Diagnosis'
                           ,xlab='Patients, sorted by last contact date\n\n\n'
                           ,tunit = 'mon',ltys = 0,type='s');
points(.eplot_death$e_death,pch=2,cex=0.5,col='#FF00FF70',lwd=2); # \triangle
points(.eplot_death$s_death,pch=6,cex=0.5,col='#00999970',lwd=3); # inv triangle
points(.eplot_death$e_dscdeath,pch=3,cex=0.5,col='#00FF0090',lwd=2); # +
points(.eplot_death$e_disdeath,pch=3,cex=0.5,col='#00FF0090',lwd=2);
points(.eplot_death$n_vtstat,col='brown',cex=1.5,lwd=0.5); # \bigcirc
abline(h=0,col='blue');
.xch_vtstat_lc<-subset(dat2a
                       ,n_vtstat==n_lc&n_lc!=age_at_visit_days+1)$patient_num;
.xch_vtstat_lc_death = intersect(kcpatients.naaccr_death,.xch_vtstat_lc);
if(length(.xch_vtstat_lc_death)!=length(.xch_vtstat_lc)){
  stop('.xch_vtstat_lc check failed')};
cat("

Above are plotted times of death (if any) relative to "
,fs('n_ddiag')," (horizontal blue line). The four data sources are "
,fs('e_death')," (![](resources/pinktriangle.png){width=10}), ",fs('s_death')
," (![](resources/blueinvtriangle.png){width=10}), ",fs('e_dscdeath')
," (![](resources/greencross.png){width=10}), and ",fs('n_vtstat')
," (![](resources/browncircle.png){width=10})");
#' :::::
#' 
#' ###### blank
#' 
#+ etabledeath, results='asis'
e_table_death <- subset(dat2tte,patient_num %in% kcpatients.naaccr) %>% 
  e_table('n_vtstat',c(setdiff(v(c_death),'n_vtstat')
                       ,'Earliest Death','Latest Death'),breaks=c(-30,30));
.tc <- paste0('Date associated with ',fs('n_vtstat')
,' compared to death dates from each source (rows). The first five columns 
represent the number of patients falling into each of the time-bins (in days) 
relative to ',fs('n_vtstat'),". The last four columns indicate the number of 
patients for each possible combination of missing values (`Left` means
the variable indicated in the row name is missing and `Right` means "
,fs('n_vtstat')," is missing). The parenthesized values below the counts are
percentages (of the total number of patients with both variables non-missing 
for the first five columns and of the total number of patients for the last four 
columns). Where available, the median difference in days is shown below the 
count and percentage. This table has only the ",length(kcpatients.naaccr)
," patients having a kidney cancer diagnosis in NAACCR. The last two rows represent the earliest and latest documentation of death, 
respectively, from ",knitr::combine_words(fs(rownames(e_table_death)))
," {#tbl:etabledeath}");
pander(e_table_death,caption=.tc,missing='&nbsp;',justify='left'
       ,fmt='%3s\\\n%s\\\n%s',row.names=fs(rownames(e_table_death))); 
#' 
.etd_tail <- tail(e_table_death);
#' In [@tbl:etabledeath] the sum of the `Neither missing` and `Left missing` is
#' always `r unique(rowSums(e_table_death$count[,6:7]))` which is the number of
#' deceased patients according to NAACCR records alone. The `Right missing` 
#' column is the number of patients whose deceased status is recorded in the 
#' external source but not in NAACCR. For the last two rows `Right missing` 
#' means the total number of deceased patients not recorded in NAACCR but which 
#' can be filled in from one or more of the other sources. There are
#' `r unique(.etd_tail$count[,'Right\\\nmissing'])` such 
#' patients. Finally the last column, `Both missing`, is the number of 
#' patients presumed to be alive because none of the sources have any evidence 
#' for being deceased. The 
#' `Left missing` column indicates how many patients are reported deceased in
#' NAACCR but _not_ the other source. Though there are some missing for each
#' individual data source, NAACCR is never the only source reporting them 
#' deceased-- the values in the bottom two rows are both
#' `r unique(.etd_tail$count[,'Left\\\nmissing'])`. 
#' 
#' The left-side columns of [@tbl:etabledeath] show the prevalence and magnitude
#' of discrepancies in death dates of the
#' `r unique(rowSums(.etd_tail$count[,6:7]))` patients that NAACCR and at 
#' least one other source agree are deceased. There are at most 
#' `r sum(.etd_tail$count[,c(1:2,4:5)])` such patients and for 
#' `r sum(.etd_tail$count[,c(2,4)])` of them the discrepancy is less than one
#' month, with a median difference ranging from 
#' `r paste0(range(.etd_tail$stat[,c(2,4)],na.rm=T),collapse=' to ')` days. 
#' **The small number of discrepancies and the small magnitude of the ones that 
#' do occur justify filling in missing NAACCR death dates from the other 
#' sources.**
#' 
#' 
#  hispanic ethnicity ===========================================================
#' ### Whether or not the patient is Hispanic {#sec:hispanic}
#' 
#' Despite the overall agreement between `r fs('n_hisp')` and `r fs('e_hisp')`
#' there needs to be some way to adjudicate the minority of cases where the 
#' sources disagree. The following additional data elements can provide 
#' relevant information to form a final consensus variable for analysis: 
#' `r fs(c('language_cd','e_lng','e_eth','race_cd','a_n_race'),retfun=knitr::combine_words)`
#' First, each of these variables is re-coded to `Hispanic`, `non-Hispanic`, and
#' `Unknown`.
#' 
#' `r fs('language_cd')` and `r fs('e_lng')` are interpreted as being evidence
#' in favor of `Hispanic` ethnicity if the language includes Spanish. English,
#' ASL, and unknown values are all treated as `Unknown` ethnicity.
#' However, a language _other_ than the above (e.g. German) is interpreted as
#' evidence for being `non-Hispanic`.
#' 
#' `r fs('n_hisp')` already have explicit designations of `non-Hispanic` and 
#' `Unknown` and all other values are interpreted as `Hispanic`. 
#' `r fs('e_hisp')` is interpreted as `Hispanic` if `TRUE` and `Unknown` if 
#' `FALSE` (in contrast with most of the other elements, there is no way to
#' distinguish a genuinely `FALSE` value of `r fs('e_hisp')` from a missing one).
#' 
#' `r fs('e_eth')` is the whole ethnicity variable from i2b2 OBSERVATION_FACT 
#' and suprprisingly it sometimes disagrees with `r fs('e_hisp')`. A value of
#' `hispanic` is interpreted directly. The values `other`,`unknown`,
#' `unknown/othe`,`i choose not`, and `@` are all interpeted as `Unknown` and 
#' any other value (at our site, `arab-amer` and `non-hispanic`) is interpreted
#' as `non-Hispanic`. Rules are then applied to create unified variables from 
#' all these data elements. I have three such variables-- 
#' `r knitr::combine_words(fs(c('a_hsp_naaccr','a_hsp_broad','a_hsp_strict')))`
#' 
#' `r fs('a_hsp_naaccr')` only uses information from NAACCR. 
#' 
#' `r fs('a_hsp_broad')` errs on
#' the side of assigning `Hispanic` ethnicity if there is any evidence for it at
#' all, then `non-Hispanic`, and `Unknown` only if there is truly no information
#' from any source about the patient's ethnicity. In particular, `Hispanic` is 
#' assigned if _any_ non-missing values of 
#' `r knitr::combine_words(fs(c('language_cd','e_lng','n_hisp','e_hisp','e_eth')))` 
#' have a value of `Hispanic`; `Unknown` if _all_ non-missing values of 
#' `r knitr::combine_words(fs(c('language_cd','e_lng','n_hisp','e_hisp','e_eth')))` 
#' are unanimous for `Unknown` ; and `non-Hispanic` otherwise.
#' 
#' Finally, 
#' `r fs('a_hsp_strict')` only assigns `Hispanic` if _all_ non-missing values of 
#' `r fs('n_hisp')`, `r fs('e_hisp')`, and `r fs('e_eth')` are unanimous for
#' `Hispanic`. `non-Hispanic` is assigned if _all_ non-missing values of
#' `r fs('n_hisp')` and `r fs('e_eth')` are unanimous for `non-Hispanic` (the 
#' `r fs('e_hisp')` element is not used for the reasons explained 
#' above) _and_ neither `r fs('e_lng')` nor `r fs('language_cd')` vote for
#' `Hispanic`. If neither of these conditions are met, `Unknown` is assigned.
#' 
#' There is an additional step for patients coded as `non-Hispanic` where they 
#' are further classified into `non-Hispanic white` and `Other`. For 
#' `r fs('a_hsp_naaccr')` this is determined by whether or `r fs('a_n_race')` is
#' `White`. For `r fs('a_hsp_broad')` the criterion is whether _at least one_ of
#' `r fs('a_n_race')` or `r fs('race_cd')` is `White`. For 
#' `r fs('a_hsp_strict')` it's whether _both_ `r fs('a_n_race')` and 
#' `r fs('race_cd')` are `White`.
#' 
#' In the end, 
#' `r fs(c('a_hsp_naaccr','a_hsp_broad','a_hsp_strict'),retfun=knitr::combine_words)`
#' all have the same levels, but differ in the proportion of patients assigned 
#' to each.
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
#' 
#+ tbl_ahsp
tbl_ahsp <- with(dat2a
                 ,table(a_hsp_naaccr,a_hsp_broad,a_hsp_strict,useNA='if')) %>%
  data.frame %>% subset(Freq>0) %>% arrange(a_hsp_naaccr,desc(Freq));
.tc <- paste0('The agreement and disagreement between '
,fs(colnames(tbl_ahsp)[1:3],retfun=knitr::combine_words),' The bottom '
,sum(is.na(tbl_ahsp[,'a_hsp_naaccr'])),' rows represent the kidney cancer 
patients currently without NAACCR records, so for them ',fs('a_hsp_naaccr')
,' does not exist. {#tbl:hspcounts}');
pander(tbl_ahsp,caption=.tc
       ,col.names=c(fs(colnames(tbl_ahsp)[1:3]),'N Patients'));
#' 
#' Of the `r sum(subset(tbl_ahsp,!is.na(a_hsp_naaccr))$Freq)` with NAACCR 
#' records (all, not just the `r length(kcpatients.naaccr)` meeting the 
#' current criteria, see [@sec:overview]) only 
#' `r sum(subset(tbl_ahsp,as.character(a_hsp_naaccr)!=as.character(a_hsp_broad))$Freq)`
#' have differences between `r fs('a_hsp_naaccr')` and `r fs('a_hsp_broad')` but 
#' `r sum(subset(tbl_ahsp,as.character(a_hsp_naaccr)!=as.character(a_hsp_strict))$Freq)` 
#' have differences between `r fs('a_hsp_naaccr')` and `r fs('a_hsp_strict')`. 
#+ pct_ahsp
pct_ahsp <- sprintf('%4.1f%%'
                    ,sapply(alist(a_hsp_naaccr,a_hsp_broad,a_hsp_strict)
                            ,function(xx) {
                              with(subset(tbl_ahsp,!is.na(a_hsp_naaccr))
                                   ,sum(Freq[eval(xx)=='Hispanic'])/sum(Freq))}
                            )*100);
# pct_ahsp <- sapply(c('a_hsp_naaccr','a_hsp_broad','a_hsp_strict')
#                    ,function(xx){
#                      with(subset(tbl_ahsp,!is.na(a_hsp_naaccr))
#                           ,sum(Freq[tbl_ahsp[[xx]]=='Hispanic'],na.rm=T)
#                           /sum(Freq)*100)}) %>% sprintf('%4.1f%%',.);
#' According to 
#' `r knitr::combine_words(fs(c('a_hsp_naaccr','a_hsp_broad','a_hsp_strict')))`
#' respectively, `r knitr::combine_words(pct_ahsp)` of the NAACCR patients are
#' Hispanic. At `r pct_ahsp[2]` `r fs('a_hsp_broad')` comes the closest to the 
#' [2016 Census estimates for San Antonio](https://www.census.gov/quickfacts/fact/table/sanantoniocitytexas/HSD410216). Also, anecdotal evidence suggests that 
#' Hispanic ethnicity is under-reported. This argues for using 
#' `r fs('a_hsp_broad')` when possible, but I will keep `r fs('a_hsp_strict')`
#' available for sensitivity analysis.
#' 
# TODO: I wonder if there is reasearch on under-reporting ethnicity. I should 
# look
#' `r md$pbreak`
#' 
#' 
#  firscontact ------------------------------------------------------------------
#' ## What is going on with the first contact variable?
#' 
#' ::::: {#fig:diag2lc_eventplot custom-style="Image Caption"}
#+ diag2lc_eventplot,results='asis',fig.dim=c(4.5,3)
par(xaxt='n');
.eplot_fc <-event_plot(subset(dat3,!patient_num %in% kcpatients.naaccr_dupe)
                       ,'n_lc','n_fc',start_event = 'n_ddiag'
                       ,main='Time from Diagnosis to Last Contact'
                       ,ylab='Months Since Diagnosis'
                       ,xlab='Patients, sorted by Last Contact\n'
                       ,tunit = 'mon'
                       ,ltys = c(1,1));
abline(h=0,col='blue');
cat("

Wierd observation-- ",fs('n_fc')," (red) is almost always between ",fs('n_lc')
    ,"(black) and ",fs('n_ddiag')," (blue) though diagnosis is usually on a biopsy
sample and that's why it's dated as during or after surgery we thought. If first contact is some kind of event after first diagnosis, what is it?");
#' :::::
#' 
#' ###### blank
#' 
#' Surgery `r fs('n_dsurg')` seems to happen in significant amounts both before 
#' and after first contact `r fs('n_fc')`.
#' 
#' ## What is the coverage of valid records in each data source.
#' 
#' _This section is no longer relevant but is still available for reference in 
#' the [kidneycancer_181009 snapshot of this document](`r paste0(urls$exp_raw_181009,'#what-is-the-coverage-of-valid-records-in-each-data-source')`)_
#' 
#' ## Which variables are near-synonymous?
#' 
#' _This section is no longer relevant but is still available for reference in 
#' the [kidneycancer_181009 snapshot of this document](`r paste0(urls$exp_raw_181009,'#which-variables-are-near-synonymous')`)_
#' 
#' 
#' `r md$pbreak`
#' 
#' 
# A4 variables -----------------------------------------------------------------
#'
#' # Variable descriptions {#sec:vars label="Appendix 4"}
#' 
if(!'e_surgonc' %in% (debug00 <- getOption('fs_reg'))){
  message('e_surgonc not found before tooltips');
  save(debug00,file='debug00.rdata');
}
#' 
#+ progfootnotes, results='asis'
# set new template for creating the internal link VALUES
# formals(fs)[c('url','template','retfun')] <- alist(paste0('#',str)
#                                           ,'[%1$s]: %2$s "%4$s"\n',cat);
#formals(fs)[c('url','retfun')] <- alist(paste0('#',str),cat);
#formals(fs)$template <- fstmplts$linkref;
# .junk <- subset(dct0,varname %in% getOption('fs_reg')
#                 ,select = c('varname','colname_long')) %>% 
#   apply(1,function(xx) fs(xx[1],url=paste0('#',xx[1]),tooltip=xx[2],retfun=cat
#                           ,template=fstmplts$linkref));
fs(getOption('fs_reg'),url=paste0('#',getOption('fs_reg'))
   ,template=fstmplts$linkref,retfun=cat);
#' 
if(!'e_surgonc' %in% (debug00 <- getOption('fs_reg'))){
  message('e_surgonc not found after tooltips before targets');
  save(debug00,file='debug00.rdata');
}
#' Here are descriptions of the variables referenced in this document.
#+ readablefootnotes, results='asis'
# This is brittle. Really ought to make fs() flexible enough to do this itself.
# TODO: stop using those silly H6 headers, use a fenced div
# 
# Here is a mockup of how it can be done (before even editing fs() to be able to
# combine different columns for the text value)
#
# # This part opens the div, writes the link target, and writes the 
# # human-readable variable name
# {fs('foo','bar','baz','bat'
# ,template='\n\n\n::::: {#%1$s .vardef custom-style=\"vardef\"}\n\n %4$s :\n\n  ~ ');
# # This part constructs the body of the variable definition, can't be done in
# # fs() yet because 'blah blah' has to be pasted together from the non-NA 
# # values of several columns.
# cat('blah blah','\n\n',if(is.na(3)) '' else c(' ~ Link: ','THEURL','\n\n'),':::::\n\n')}
cat('***\n');
.junk <- dct0[match(getOption('fs_reg')
                    ,do.call(coalesce,dct0[,c('varname','colname')]))
              ,c('varname','colname_long','chartname'
                 ,'comment','col_url','colname')] %>%
  # subset(dct0,varname %in% getOption('fs_reg')
  #               ,select = c('varname','colname_long','chartname','comment'
  #                           ,'col_url')) %>% 
  apply(1,function(xx) {
    # TODO: the hardcoded offsets make this brittle. Fina better way.
    cat('######',na.omit(xx[c(1,6)])[1],'\n\n',na.omit(xx[2:1])[1],':\n\n  ~ '
        ,ifelse(length(na.omit(xx[2:4]))>0
                ,iconv(paste(na.omit(xx[2:4]),collapse='; '),to='UTF-8',sub='')
                ,'')
        ,ifelse(is.na(xx[5]),'',paste('\n\n  ~ Link:',xx[5])),'\n\n***\n')});
#' 
#' `r md$pbreak`
#' 
#' ##### v055_tnm_cln_dscrptr
#' 
#' Test section
#' 
# A5 audit ---------------------------------------------------------------------
#' # Audit trail {#sec:audit label="Appendix 5"}
walktrail()[,-5] %>% pander(split.tables=600,,justify='left');
#+ results='hide'
c()
