clear

echo "running 2012 round 1"

Rscript -e "library(knitr); knit('gdistsamp_12_r1.rmd')"

tail -20 gdistsamp_12_r1.md

echo "running 2012 round 2"

Rscript -e "library(knitr); knit('gdistsamp_12_r2.rmd')"

tail -20 gdistsamp_12_r2.md

echo "running 2012 round 3"

Rscript -e "library(knitr); knit('gdistsamp_12_r3.rmd')"

tail -20 gdistsamp_12_r3.md

echo "running 2013 round 1"

Rscript -e "library(knitr); knit('gdistsamp_13_r1.rmd')"

tail -20 gdistsamp_13_r1.md

echo "running 2013 round 2"

Rscript -e "library(knitr); knit('gdistsamp_13_r2.rmd')"

tail -20 gdistsamp_13_r2.md

echo "running 2013 round 3"

Rscript -e "library(knitr); knit('gdistsamp_13_r3.rmd')"

tail -20 gdistsamp_13_r3.md

echo "running 2013 round 4"

Rscript -e "library(knitr); knit('gdistsamp_13_r4.rmd')"

tail -20 gdistsamp_13_r4.md

echo "running 2014 round 1"

Rscript -e "library(knitr); knit('gdistsamp_14_r1.rmd')"

tail -20 gdistsamp_14_r1.md

echo "running 2014 round 2"

Rscript -e "library(knitr); knit('gdistsamp_14_r2.rmd')"

tail -20 gdistsamp_14_r2.md

echo "running 2014 round 3"

Rscript -e "library(knitr); knit('gdistsamp_14_r3.rmd')"

tail -20 gdistsamp_14_r3.md

echo "running 014 round 4"

Rscript -e "library(knitr); knit('gdistsamp_14_r4.rmd')"

tail -20 gdistsamp_14_r4.md

printLine tail -15 gdistsamp_12_r1.md | tail -15 gdistsamp_12_r2.md | tail -15 gdistsamp_12_r3.md | tail -15 gdistsamp_13_r1.md | tail -15 gdistsamp_13_r2.md | tail -15 gdistsamp_13_r3.md | tail -15 gdistsamp_13_r4.md | tail -15 gdistsamp_14_r1.md | tail -15 gdistsamp_14_r2.md | tail -15 gdistsamp_14_r3.md | tail -15 gdistsamp_14_r4.md  >> modeling_output.md 


