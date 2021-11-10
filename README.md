# Overview
I joined the research team as they were almost done with their first of three papers/aims. Existing files were provided by an exiting colleague, who did the bulk (honestly, pretty much all) of the work to get the data, analyses, and output running. 

Credits to https://github.com/eriliawu/tacobell - look there for more information about what each file entails and the file states that I inherited. Updates to the original scripts are not documented in this readme, unless it underwent a major overhaul. Future users should adopt files from this repository, not the original author, to make sure they are not using outdated scripts. New files are detailed in the following sections.

# New Files - Aim 1

## [suffolkweights.R](https://github.com/lloydheng/tacobell/blob/main/suffolkweights.R)
Using various matching criteria (what and how many covariates), this script attempts to match treated Suffolk county restaurants to untreated ones on the basis of the latter's propensity scores. Density plots following each matching procedure illustrates the extent of overlap for each covariate that was matched on. Using inverse probability weights (IPW), DiD estimates for each matching procedure is also generated.

# Aim 2
Aim 2 sets out to explore subgroup differences by census tract-level (i) income, (ii) race, (iii) RUCA classification.

## [aim2-subgroup-diffs.R](https://github.com/lloydheng/tacobell/blob/main/aim2-subgroup-diffs.R)
(to be updated)
