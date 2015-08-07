clear

echo "running 2012"

Rscript -e "library(knitr); knit('gdistsamp_2012_multi_year_occ.rmd')"

tail -20 gdistsamp_2012.md

echo "running 2013"

Rscript -e "library(knitr); knit('gdistsamp_2013.rmd')"

tail -20 gdistsamp_2013.md

echo "running 2014"

Rscript -e "library(knitr); knit('gdistsamp_2014.rmd')"

tail -20 gdistsamp_2014.md

(tail -14 gdistsamp_2012.md; tail -14 gdistsamp_2013.md; tail -14 gdistsamp_2014.md) > modeling_output.md

