# Overview
I joined the research team as they were almost done with their first of three papers/aims. Existing files were provided by an exiting colleague, who did the bulk (honestly, pretty much all) of the work to get the data, analyses, and output running. 

Credits to https://github.com/eriliawu/tacobell - look there for more information about what each file entails and the file states that I inherited. Updates to the original scripts are not documented in this readme, unless it underwent a major overhaul. Future users should adopt files from this repository, not the original author, to make sure they are not using outdated scripts. New files are detailed in the following sections.

# New Files - Aim 1

## [suffolkweights.R](https://github.com/lloydheng/tacobell/blob/main/suffolkweights.R)
Using various matching criteria (what and how many covariates), this script attempts to match treated Suffolk county restaurants to untreated ones on the basis of the latter's propensity scores. Density plots following each matching procedure illustrates the extent of overlap for each covariate that was matched on. Using inverse probability weights (IPW), DiD estimates for each matching procedure is also generated.

# Aim 2
Aim 2 sets out to explore subgroup differences by census tract-level (i) income, (ii) race, (iii) RUCA classification.

## [aim2-subgroup-diff.R](https://github.com/lloydheng/tacobell/blob/main/aim2-subgroup-diff.R)
Models are identical to those in aim 1, except a triple interaction is included to estimate subgroup differences. 

Note that some of a chain of functions embedded within the tidy.lm function requires redefining because R, when inverting matrices, treats near-zero values as zeroes. Checks were made to ensure the output of the newly defined tidy.alt, and the original tidy, are identical on matrices containing no near-zero/zero values.

### By Income Category
The proposal defined 4 categories, but its distribution in the data calls for a re-evaluation (barely any restaurants fell in the lowest category). Alternative operationalizations were considered, including by income tertiles, quartiles, down the median were considered and evaluated for robustness. 
