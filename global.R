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
             #,'lubridate'
             #,'ggplot2','ggfortify','grid','GGally'       # plotting
             #,'survminer','gridExtra','scales'
             #,'stargazer','broom', 'tableone','janitor'   # table formatting
             ,'pander'
             #,'knitr','htmltab'
             ));
#' Turn JIT to max: pre-compile all closures, `for`, `while`, and `repeat` loops
#enableJIT(3);
#' ## Load local config file
#' 
source('./config.R');


#' ## Set generic variables
#' 
#' data dictionary:
if(!exists('dctfile')) dctfile <- 'DICTIONARY.csv';
#' saved session data (not used right now)
session <- 'session.rdata';
