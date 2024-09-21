# MA Econometric Methods WiSe 24/25 Practical Class

This repository containst the material for the practical class part of the course Econometric Methods in Business and Economics (EMBE).

The material, i.e. the slides, videos and additional material, are designed in such a way that they can be used across all semesters, with the exception of a few minor adjustments.

These adjustments include:

* Adjust the link to the ILIAS courses in yaml header in `./index.Rmd`.
  * Link to illustration tool is fixed!
* Adjust the link to datasets upload to ILIAS for each exercise sheet in the yaml header, e.g., in `./01_ExerciseSheet_No01_XX.Rmd`.
* Adjust the link to source helper functions from ILIAS in each exercise sheet, e.g., in `./01_ExerciseSheet_No01_XX.Rmd` (see below)
* Adjust the link also in `./r-scripts/prepare_r_packages_and_helper_functions.R` and `download_r_helper_functions.R`.

## Handling of show/no show of interpretations

* For uploads use:
  * `Sol01[i].style.display = "none";`
  * `NoSol01[i].style.display = "block";`
* For discussion use:
  * `Sol01[i].style.display = "block";`
  * `NoSol01[i].style.display = "none";`

```javascript
// Hide all solutions (by default)
var i, Sol01;
Sol01 = document.getElementsByClassName("showSol01");
for (i = 0; i < Sol01.length; i++) {
  Sol01[i].style.display = "none"; // type "block" for show or "none" (default) for exclude
}
  
var i, NoSol01;
NoSol01 = document.getElementsByClassName("noSol01");
for (i = 0; i < NoSol01.length; i++) {
  NoSol01[i].style.display = "block"; // type "block" (default) for show or "none" for exclude
}
```

## Handling required packages and helper functions

### Required packages

Note on required packages and helper functions, i.e., wrapped built-in functions.

The required packages are downloaded and us used in every session.

```R
# packages we will use in the course
pac <- c("moments", "sandwich", "lmtest", "AER", "car", "plm", "ivreg", "dynlm", "forecast", "urca")

# install and/or load packages
checkpac <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
  }
  require(x, character.only = TRUE)
}

# check if packages are install yet
suppressWarnings(sapply(pac, checkpac))
```

### Helper functions

The helper functions, i.e., wrapped built-in functions can be found in

* `./00_ExerciseSheet_HelperFunctions_XX`
* `./r-scripts/r_helper_functions.R`
* `./r-scripts/prepare_r_packages_and_helper_functions.R`

#### Motivations

The motivations for using the helper functions is rooted in the fact that the basic fitting routines like `lm()` does not return robust standard errors.

Instead the `vcovHC` function of the `sandwich` package has to be used. To avoid this two-step-implementation I provided the follwoing wrapper/helper function

```R
lm_ct_fun <- function(formula, ..., hc.type = "HC1") {
  
  z <- stats::lm(formula, ...)
  z$sum <- summary(z)
  
  if (hc.type == "const") {
    z$ct <- lmtest::coeftest(z, vcov = sandwich::vcovHC(z, type="const")) # test based on ordinary SEs
  } else if (hc.type == "HC1") {
    z$ct <- lmtest::coeftest(z, vcov = sandwich::vcovHC(z, type="HC1")) # test based on heteroskedasticity robust SEs
  }
  
  z
  
}
```

Thus, instead of using `lm()` to fit linear models I use `lm_ct_fun()`. Similar wrapper functions are provided for other estimation procedures. (see `./00_ExerciseSheet_HelperFunctions_XX.Rmd`).

#### Workflow

The code chunk below will download and save `r_helper_functions.R` with the helper functions uploaded on ILIAS to the current directory.

```R
# Download R-helper functions
source("https://ilias.uni-hohenheim.de/data/UHOH/lm_data/lm_1856939/MA_EconometricMethods_WiSe2324_PracticalClass/r-scripts/prepare_r_packages_and_helper_functions.R")
```

The code chunk below will include the helper functions in `r_helper_functions.R` to the current script.

```R
# Include R-helper functions
source("r_helper_functions.R")
```

## Soluation Facultative Graded Assignment

The solution to the facultative graded assignment (`./XX_Assignment_WiSe2324_XX.html`) is also linked in `./index.Rmd`.

## Software Implemetnation S&W in R, STATA and GRETL

The directory `./software-implementations` contains the implementations of the analysis of the most important models in R, STATA and GRETL.

The module is linked in the `./index.Rmd` file.

## Rest

* `./illustration-tool`: Material for linked illustrations, e.g., OVB
* `./data-sets`: Should be clear
* `./r-scripts`:
  * Handle helper functions
    * `download_r_helper_functions.R`
    * `prepare_r_packages_and_helper_functions.R`
    * `r_helper_functions.R`
  * Wrapper functions to edit texreg table generation
    * `texreg_extract_fun.R`
    * ...
  * `DataPreparation.R`: Helper function for data preparation
  * Some functions for checks for myself
    * `R_car_linearHypothesis.R`
    * `urca_ur_df_fun.R`
    * `urca_ur_fun.R`

## Else

Additional material can be found in the OneDrive folder of last year

* `./ExerciseSheets`: `.Rmd` files for the exercise sheets
* `./Assignment`: `.Rmd` files for the facultative graded assignment
* `./Annoucements`: Furteher announcements including
  * Cheat sheet rulds
  * Bonus rules(!)
* `./Exam`: Exams
  * CHECK: An overview on all exams until before last semester can be found in `./MA_EMBE/MA_EMBE_General`!