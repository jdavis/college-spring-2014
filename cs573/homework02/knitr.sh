#!/bin/bash

PATTERN="*.Rnw"
FILE="solution.Rnw"
KNITR="echo \"Rerunning Knitr...\"; Rscript -e \"library(knitr); knit('./${FILE}')\""

echo "Watching ${FILE}..."
watchmedo shell-command --patterns="${PATTERN}" --command="${KNITR}" .
