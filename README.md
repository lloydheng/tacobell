# Overview
I joined the research team as they were almost done with their first of three papers/aims. Existing files were provided by an exiting colleague, who did the bulk (honestly, pretty much all) of the work to get the data, analyses, and output running. 

Credits to https://github.com/eriliawu/tacobell - look there for more information about what each file entails and the file states that I inherited. Contributions to this repository are mainly updates to the original code, unless specified otherwise. New files will be detailed in the following sections.

# New Files

## [suffolkweights.R](https://github.com/lloydheng/tacobell/blob/main/suffolkweights.R)
Using various matching criteria (what and how many covariates), this script attempts to match treated Suffolk county restaurants to untreated ones on the basis of the latter's propensity scores. Density plots following each matching procedure illustrates the extent of overlap for each covariate that was matched on. Using inverse probability weights (IPW), DiD estimates for each matching procedure is also generated.
