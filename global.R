#' ---
#' title: "Generic Global R Settings"
#' author: "Alex F. Bokov"
#' date: "08/03/2018"
#' ---
#' 
#' This is a script that loads (and if needed, installs)
#' libraries needed for this project, and sets a few global
#' variables that will be needed by many different scripts.
#' Keep this script minimalistic and *no number crunching here*
#' 
# local_fns --------------------------------------------------------------------
#' ## Local functions
#+ warning=FALSE, message=FALSE
source('./functions.R');
source('./trailR.R');

# libs -------------------------------------------------------------------------
#' Libraries
#' 
#' load and if necessary install needed libraries
#+ warning=FALSE, message=FALSE
instrequire(
  c(# just-in-time compilation
    # 'compiler'
    
    # propensity scores and glm residuals
    #,'MatchIt','DHARMa'
    
    # zero-inflated poisson, sigh
    #,'pscl'
    
    # various analysis methods
    'survival' #,'MASS','Hmisc','zoo','coin'
    
    # evaluating predictive power
    #,'survAUC','survivalROC','pROC'
    
    # for pd matrices needed by faker()
    #,'Matrix'
    
    # data manipulation & piping
    ,'readr','dplyr','stringr','magrittr','tools'
    #,'lubridate'
    
    # plotting
    ,'ggfortify'
    #,'ggplot2','grid','GGally','heatmap3','survminer','gridExtra','scales'
    
    # string manipulation
    ,'stringi'
    
    # table formatting
    ,'pander','tableone'
    #,'knitr','htmltab','stargazer','broom','janitor'
    
    # Web
    ,'RCurl','XML'
));

#' Turn JIT to max: pre-compile all closures, `for`, `while`, and `repeat` loops
#enableJIT(3);
# config -----------------------------------------------------------------------
#' ## Load local config file
#' 
source('./config.R');

# vars -------------------------------------------------------------------------
#' ## Set generic variables
#' 
#' That is to say, variables which can be set without reference to the data and
#' do not take a lot of time to do.
#' 
#' data dictionary produced by datafinisher, should physically accompany inputdata
dctfile_raw <- paste0(dirname(inputdata),'/meta_',basename(inputdata));
#' data dictionary template-- metadata that should persist accross multiple 
#' versions of the data and data dictionary
dctfile_tpl <- 'datadictionary_static.csv';
#' checked-in file, with added rows and columns, ready-to-use FOR THIS DATASET
#' if it doesn't exist, it will get created in data.R
dctfile <- paste0('dct_',basename(inputdata));
#' `dct_stage` is a status indicator for the stage of completion for the data 
#' dictionary object. If it has already been created and needs no work, it is 
#' `Inf` otherwise it counts _up_ from `1` to whatever the final stage is for
#' for this project. Upon the final stage it should get set to `Inf` in the 
#' script that does it. WIP
dct_stage <- Inf;
#' This is the file that lists levels of discrete variables and what each listed
#' level should be renamed to.
levels_map_file <- 'levels_map.csv';
# fs_templates -----------------------------------------------------------------
#' templates for `fs()` ... note that the actual values inserted depend on 
#' the other arguments of `fs()` and the columns of the data dictionary
fstmplts <- list(
  # [n_ddiag]: #n_ddiag "0390 Date of Diagnosis"
   linkref="[%1$s]: %2$s \"%4$s\"\n"  
  # [`n_ddiag`][#n_ddiag]
  ,link_varname="[`%1$s`][%2$s]"
  # [`0390 Date of Diagnosis`][#n_ddiag]
  ,link_colnamelong="[`%4$s`][%2$s]"
  # `0390 Date of Diagnosis`
  ,code_colnamelong="`%4$s`"
  # 0390 Date of Diagnosis
  ,plain_colnamelong="%4$s"
);
# urls -------------------------------------------------------------------------
urls <- list(
   exploration_rpubs='https://rpubs.com/bokov/kidneycancer'
  ,dict_naaccr='http://datadictionary.naaccr.org/?c=10'
  );
#' RPubs keeps the actual content in an iframe, and this cool little 3-liner 
#' gets the address of that iframe's target so in principle I can now construct
#' links with targets to other documents I published previously, starting with
#' the most recent version of this document.
urls$exploration_raw <- getURL(urls$exploration_rpubs) %>% 
  htmlParse %>% xpathApply('//iframe') %>% `[[`(1) %>% xmlAttrs() %>% 
  paste0('https:',.);
c()
