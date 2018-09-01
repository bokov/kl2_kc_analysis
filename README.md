# kl2_kc_analysis
The R scripts for my KL2 project (Aims 2 &amp; 3)

Basic usage:

1. If you are a collaborator or mentor of mine, talk to me about getting a copy
   of this data. Otherwise, at some point I will finish my simulation script 
   that creates tables with the same formats and similar joint distributions
   as the original data and then there will be a simulated test dataset to 
   accompany these scripts, but right now the list of data elements is still 
   changing too rapidly for that to happen.
2. Once you have a copy of the data, clone this repo and check out the 
   `integration` or (at the moment) `f_crosschecks` branch. The `master` 
   branch is for frozen snapshots corresponding to formal publications only.
3. Edit the `example_config.R` file that comes with this repository and 
   change the `inputdata` and `.workdir` variables (the others aren't used yet).
   Save the edited version as `config.R` and whenever you check out this repo,
   copy your `config.R` file into it (you'll likely need a different 
   `config.R` for different computers you work on). Never check your own 
   `config.R` into any public repo-- it's included in `.gitignore` for a reason.
4. Launch RStudio from inside your clone of the repository, open `exploration.R`,
   and click on the `Compile Report` button (ctrl-shift-K). The scripts in this
   repository will automatically attempt to satisfy all the necessary R packages,
   process the data, and generate a report similar to the one I have on 
   https://rpubs.com/bokov/kidneycancer . 
       * If you have made edits to any files
   you will need to either uncomment the line at ht
5. The following is what each file here does:
     * `config.R`: sets the path to your local copy of the data
     * `global.R`: calls config.R, then attempts to install all the needed R 
     libraries and sources project-specific functions.
     * `trailR.R`: the set of functions related to audit capabilities... someday to
     become a stand-alone package.
     * `functions.R`: all the other custom functions
     * `data.R`: sources global.R, reads in the data, and creates a dynamic 
     data dictionary from `datadictionary_static.csv`
     * `exploration.R`: takes the processed data created by `data.R` and generates
     tables and plots from it. This is a separate file because `data.R` takes a
     while to run, and at the moment does not change as quickly as does 
     `exploration.R`.
   * `datadictionary_static.csv`: a spreadsheet template that associates various
     properties and metadata with the non-changing parts of the names of the 
     variables used by this project (the full names of the variables may change
     from one version of data to the other, and this allows the scripts to 
     continue working without having to get re-written each time).
   * `update_datadictionary.R`: when the data changes, the old
     `datadictionary_static.csv` will continue to work but it will not see 
     newly-include variables. This script can be run to update the
     `datadictionary_staticl.csv` file so that new variables will now get added.
     It replaces some but not quite all the drudgery of manual editing the CSV.
   * 