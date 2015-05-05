clear

echo "running gdist 2012"

Rscript -e "library(knitr); knit('gdistsamp_2012_vira.rmd')"

tail -20 gdistsamp_2012_vira.md

echo "running gdist 2013"

Rscript -e "library(knitr); knit('gdistsamp_2013_vira.rmd')"

tail -20 gdistsamp_2013_vira.md

echo "running gdist 2014"

Rscript -e "library(knitr); knit('gdistsamp_2014_vira.rmd')"

tail -20 gdistsamp_2014_vira.md

(tail -14 gdistsamp_2012_vira.md; tail -14 gdistsamp_2013_vira.md; tail -14 gdistsamp_2014_vira.md) > vira_gdistsamp_output.md

clear

echo "running colext 2012"

Rscript -e "library(knitr); knit('colext_2012_vira.rmd')"

tail -20 colext_2012_vira.md

echo "running colext 2013"

Rscript -e "library(knitr); knit('colext_2013_vira.rmd')"

tail -20 colext_2013_vira.md

echo "running colext 2014"

Rscript -e "library(knitr); knit('colext_2014_vira.rmd')"

tail -20 colext_2014_vira.md

(tail -14 colext_2012_vira.md; tail -14 colext_2013_vira.md; tail -14 colext_2014_vira.md) > vira_colext_output.md

