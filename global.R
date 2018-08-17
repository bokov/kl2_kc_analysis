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
#' ## Load libraries
#+ warning=FALSE, message=FALSE
source('./functions.R');
source('./trailR.R');
instrequire(c('compiler'                                   # just-in-time compilation
             #,'MatchIt','DHARMa'                          # propensity scores and glm residuals
             #,'pscl'                                      # zero-inflated poisson, sigh
             ,'survival','MASS','Hmisc','zoo','coin'      # various analysis methods
             #,'survAUC','survivalROC','pROC'              # evaluating predictive power
             #,'Matrix'                                    # for pd matrices needed by faker()
             ,'readr','dplyr','stringr','magrittr'        # data manipulation & piping
             ,'tools'
             #,'lubridate'
             #,'ggplot2','grid','GGally'                  # plotting
             ,'ggfortify'
             #,'survminer','gridExtra','scales'
             #,'stargazer','broom','janitor'              # table formatting
             ,'pander','tableone'
             #,'knitr','htmltab'
             ));
#' Turn JIT to max: pre-compile all closures, `for`, `while`, and `repeat` loops
#enableJIT(3);
#' ## Load local config file
#' 
source('./config.R');


#' ## Set generic variables
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
