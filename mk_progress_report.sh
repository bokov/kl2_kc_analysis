#! /bin/bash

declare OPTARG;
declare OPTIND;
declare opt;
declare input;
declare output;
declare styletemplate;
declare which;

input="exploration.R";
output="NONE";
styletemplate="nt_styletemplate.docx";
which="html+docx";

while getopts "i:o:s:w:" opt; do
        echo "OPT=$opt";
        case $opt in
            i) input="$OPTARG" ;;
            o) output="$OPTARG" ;;
            s) styletemplate="$OPTARG" ;;
            w) which=$OPTARG ;;
            \?) echo "Invalid option: -$OPTARG" ;;
        esac
    done;

if [ "$output" == "NONE" ];
    then output=$(echo $input|sed "s/\.R$\|\.Rmd$//g");
    fi;
#pandoc -s -S --reference-docx $styletemplate $input -o $output;

rcommand="library(rmarkdown); ";

if [[ "$which" == *docx* ]]; then
rcommand="$rcommand render('$input',output_file = '$output.docx'\
    , output_format = word_document(reference_docx = '$styletemplate'\
        ,keep_md=T,pandoc_args=c('--filter','pandoc-crossref')));\
";
fi;

if [[ "$which" == *html* ]]; then
rcommand="$rcommand render('$input',output_file = '$output.html'\
  ,output_format=html_document(keep_md=T\
    ,pandoc_args=c('--filter','pandoc-crossref')));\
";
fi;

echo "WHICH: $which";
echo $rcommand;

R -e "$rcommand";

