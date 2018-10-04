#! /bin/bash
input=${1:-"exploration.R"};
output=${2:-"NONE"};
styletemplate=${3:-"nt_styletemplate.docx"};
if [ "$output" == "NONE" ];
    then output=$(echo $input|sed "s/\.R$\|\.Rmd$//g");
    fi;
#pandoc -s -S --reference-docx $styletemplate $input -o $output;
R -e "library(rmarkdown);\
render('$input',output_file = '$output.docx'\
  , output_format = word_document(reference_docx = '$styletemplate'\
    ,keep_md=T,pandoc_args=c('--filter','pandoc-crossref')));\
render('$input',output_file='$output.html'\
  ,output_format=html_document(keep_md=T\
    ,pandoc_args=c('--filter','pandoc-crossref')))";

