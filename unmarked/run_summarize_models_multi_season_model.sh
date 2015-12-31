clear

echo "running 2012 _multi_year_occ"

Rscript -e "library(knitr); knit('gdistsamp_2012_multi_year_occ.rmd')"

tail -40 gdistsamp_2012_multi_year_occ.md

echo "running 2013 _multi_year_occ"

Rscript -e "library(knitr); knit('gdistsamp_2013_multi_year_occ.rmd')"

tail -40 gdistsamp_2013_multi_year_occ.md

echo "running 2014 _multi_year_occ"

Rscript -e "library(knitr); knit('gdistsamp_2014_multi_year_occ.rmd')"

tail -40 gdistsamp_2014_multi_year_occ.md

(tail -14 gdistsamp_2012_multi_year_occ.md; tail -14 gdistsamp_2013_multi_year_occ.md; tail -14 gdistsamp_2014_multi_year_occ.md) > modeling_output_multi_year_occ.md

