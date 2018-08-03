#' ---
#' title: "Generic R Project Configuration File"
#' author: "Alex F. Bokov, Ph.D."
#' date: "08/03/2018"
#' ---
#' 
#' Please copy this file to `config.R`, edit that copy, and copy it over to
#' the working directory whenever you check out this project. This is just an
#' example of what computer-specific variables should be set. This does not 
#' actually get called by `run.R`. A file needs to be called `config.R` in order
#' to be used as a source of configuration information by our scripts
#' 
#' ## Used by all
#' Replace the following with the full path to the actual directory that 
#' resulted after you ran `git clone git@github.com:UTHSCSA-CIRD/THISREPO`
#' Here is mine...
.workdir <- '/tmp/THISREPO/';
#' ## Used by data.R. Please put a path to your nsqip CSV file here.
inputdata <- 'local/in/RAWDATA.tsv';
#' 
#' For some projects we might need to create additional input variables named 
#' following the pattern `inputdata_FOO`
#' ## Cached previous results (WIP)
#incache_run <- 'local/in/THISREPO_SCRIPTNAME_MD5HASHSTRING.rdata';
#outcache_path <- 'local/in/'
#' 
#' ## Password for creating dashboard
.shinypw <- 'SHINYPASSWORD';
#'
#' ## For the crosscheck_old_new.R script...
.inputenv_old <- 'local/THISREPO_YYMMDD.rdata';
