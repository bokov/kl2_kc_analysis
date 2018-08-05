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
.depends <- 'data';
.depdata <- paste0(.depends,'.rdata'); .depscript <- paste0(.depends,'.R');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
if(!file.exists(.depdata)) system(sprintf('R -e "%s"',.depscript));
tload(.depdata);
