#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' Please read this file through before trying to run it. The comments tell
#' you what you need to edit in order to proceed.
#' 
source('global.R');
.depends <- 'data.R';
.depdata <- paste0(.depends,'.rdata');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
project_seed <- 20180803;
if(!file.exists(.depdata)) system(sprintf('R -e "source(\'%s\')"',.depends));
tload(.depdata);

#' How well does sex match up between the EMRs and NAACCR?
with(dat2,table(sex_cd,v011_sx,useNA = 'always')) %>% addmargins() %>% pander();

#' How well does race match up between the EMRs and NAACCR?
with(dat2,table(race_cd,v005_rc,useNA = 'always')) %>% addmargins() %>% pander();

#' How well does race match up between the EMRs and NAACCR?
with(dat2,table(race_cd,v005_rc,useNA = 'always')) %>% addmargins() %>% pander();

#' How well does Hispanic ethnicity match up between the EMRs and NAACCR?
with(dat2,table(v044_hspnc_or_ltn,v010_spnsh_hspnc,useNA = 'always')) %>% 
  addmargins() %>% pander();

