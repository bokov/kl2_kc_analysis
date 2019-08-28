#' ---
#' title: "Kidney Cancer Data Dictionary Init"
#' author: 
#' - "Alex F. Bokov^[UT Health, Department of Epidemiology and Biostatistics]"
#' date: "09/14/2018"
#' ---
#' 
# init -------------------------------------------------------------------------
source('global.R');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
project_seed <- 20180803;
debug <- 1;
#' Default args
formals(v)$dat <- as.name('dat1');
#' Saving original file-list so we don't keep exporting functions and 
#' environment variables to other scripts
.origfiles <- ls();
# read dat0 --------------------------------------------------------------------
#' Initialize the column specification for parsing the input data
dat0spec <- tread(inputdata,spec_csv,na=c('(null)',''),guess_max=10000);
#' Force the `patient_num` column to be numeric rather than integer, to avoid
#' missing values due to `patient_num` being too large
dat0spec$cols[['patient_num']] <- col_number();
#' Read the data 
dat0 <- tread(inputdata,read_csv,na=c('(null)',''),col_type=dat0spec);
colnames(dat0) <- tolower(colnames(dat0));

# make data dictionary ---------------------------------------------------------
#' Create the data dictionary
dct0 <- rebuild_dct(dat0,dctfile_raw,dctfile_tpl,tread_fun = read_csv,na=''
                    ,searchrep=globalsearchrep);
#' 
# a few dat0 hacks -------------------------------------------------------------
#' A workaround for the fact that in `dat1` columns get transformed and we need
#' an original value from `dat0`, but in `dat0` the column names are not yet 
#' renamed to stable values.
cstatic_n_dob <- subset(dct0,varname=='n_dob')$colname;
#' We use this value to create a list of `patient_num` s that have mismatched
#' dates of birth between NAACCR and EMR
kcpatients.bad_dob <- dat0[as.character(dat0$birth_date)!=
                             as.character(dat0[[cstatic_n_dob]]) & 
                             !is.na(dat0[[cstatic_n_dob]]),'patient_num'] %>% 
  unlist %>% unname;

#' A similar pattern for finding NAACCR dates in dat0, before we start 
#' transforming it in dat1.
cstatic_n_tpoints <- subset(dct0,varname %in% c('n_ddiag','n_fc','n_dsurg'
                                                ,'n_drecur','n_lc'))$colname;
#' And then recording the `patient_num`s for patients with multiple NAACCR 
#' rows (until I can ascertain that values from the same row in NAACCR are
#' bound together in some way recoverable from i2b2 such as instance numbers).
#' Until then, might have to drop such cases.
kcpatients.naaccr_dupe <- group_by(dat0,patient_num)[
  ,c('patient_num',cstatic_n_tpoints)] %>% 
  summarize_all(function(xx) sum(!is.na(xx))) %>% 
  apply(1,function(xx) c(xx[1],max(xx[-1]))) %>% t %>% data.frame %>% 
  subset(V2>1) %>% `$`(patient_num);

# save out ---------------------------------------------------------------------
#' ## Save all the processed data to an rdata file 
#' 
#' ...which includes the audit trail
tsave(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
