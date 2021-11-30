### aim 2 analysis, taco bell
### DiD by subgroup differences (% race, income category, RUCA)
### diff-in-diff with restaurant level random effects ----

getwd()
setwd("C:/Users/qh2030/OneDrive - NYU Langone Health/wu-data/tacobell")

current_warning <- getOption("warn") #not display warnings
options(warn = -1)
#options(warn = current_warning)
options("scipen"=100)
dev.off()

### install and load packages ----
library(dplyr)
library(ggplot2)
library(tidyverse)
#install.packages(c("lme4", "lmerTest")) #random effects and tesing significance
#install.packages("plm")
library(lme4)
library(plm)
library(lmerTest)
#install.packages("stargazer")
library(stargazer)
#install.packages("table1")
library(table1)
library(tableone)
library(broom)
#install.packages("car")
library(car)
#install.packages("usmap") #show state abbr in ggplot map
library(usmap)
library(maps)
library(car) #testing joint significance
#install.packages("zoo") #fill in NAs with the first non-NA value in a column
library(zoo)
#install.packages("sjmisc")
library(sjmisc)



### matched data, preparing data ----
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru.csv", stringsAsFactors = FALSE)
matched$tract_num <- substr(matched$tract_num, 2, 12)
matched$holiday <- ifelse(matched$month==12, 1, 0)
matched <- matched %>%
  filter(complete.cases(matched)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:holiday) %>%
  arrange(id, monthno) %>% #find out for how many months a restaurant was open
  group_by(id, treat, match_place) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(matched$rank) #sanity check, the max should be 106

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator
matched$relative <- matched$monthno - matched$entry +1
matched$relative2 <- matched$monthno - matched$entry #month 0 is first month of ML
matched$post <- ifelse(matched$relative2<0, 0, 1)

# summary stats for restarants continuously open for 106 months
length(unique(matched$id[matched$open_month==106&matched$treat==1])) #67
length(unique(matched$id[matched$open_month==106&matched$treat==0])) #399

# month as relative and factor
# set month 1 as ref group
matched$relative.factor <- factor(matched$relative)
matched <- within(matched, relative.factor<-relevel(relative.factor, ref="1"))
summary(matched$relative)

matched$relative2.factor <- factor(matched$relative2)
matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="0"))

# calculate open_month both before and after ML
matched <- matched %>%
  group_by(id, treat, match_place, post) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
matched$open_before <- ifelse(matched$post==0, matched$open_before, matched$open_month-matched$open_before)
matched$open_after <- ifelse(matched$post==1, matched$open_after, matched$open_month-matched$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, after6,after12,after18,after24) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id", "treat", "match_place"), all = TRUE)
tmp <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, before6,before12,before18,before24) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id", "treat", "match_place"), all = TRUE)
matched$open6 <- ifelse(matched$before6==1&matched$after6==1,1,0)
matched$open12 <- ifelse(matched$before12==1&matched$after12==1,1,0)
matched$open18 <- ifelse(matched$before18==1&matched$after18==1,1,0)
matched$open24 <- ifelse(matched$before24==1&matched$after24==1,1,0)
rm(tmp)

matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="-3"))
matched$id_match <- paste0(matched$id, matched$match_place)

# match RUCA status
  ruca <- read.csv("data/census-data/tract/RUCA_USDA_2010.csv", stringsAsFactors = F)
  
  class(matched$tract_num)
  class(ruca$tract_num)
  
  matched$tract_num <- as.numeric(matched$tract_num)
  ruca$pop_density <- gsub(",", "", ruca$pop_density)
  ruca <- sapply(ruca, as.numeric)

  matched <- merge(matched, ruca, by = "tract_num")
  
  # check that we have no NAs (complete merge)
  descr(matched$ruca)
  descr(matched$pop_density)
  
# recode income as categorical var for subgroup analysis - per proposal
  matched$incomecat <- cut(matched$median_income, 
                           breaks = c(0, 25000, 50000, 75000, Inf),
                           labels = c("<25,000", "25,000 - 49,999", "50,000 - 74,999", ">=75,000"))
  
  levels(matched$incomecat)
  matched$incomecat <- relevel(matched$incomecat, ref = ">=75,000")
  
### subgroup differences by income category ----
# get num of restaurants in each month
#ignore results 2 months before and after ML
  mod.factor <- plm(formula = calorie ~ treat*relative2.factor + 
                                        as.factor(month) + 
                                        treat*relative2.factor*incomecat,
                    data = matched %>% filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                    index = "id_match", weights = weights, model = "within")
  
  tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE)
  
  # clean data
  tidy_mod.factor <- tidy_mod.factor %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    slice(-(1:120)) %>%
    mutate(group = ifelse(startsWith(month, "treat"), "treated", "control")) %>%
    add_row(month="-3",coef.month=0,group="control",low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group="treated",low=0,high=0) %>%
    mutate(groupinc = ifelse(endsWith(month, "25,000"), 1, NA)) %>%
    mutate(groupinc = ifelse(endsWith(month, "49,999"), 2, groupinc)) %>%
    mutate(groupinc = ifelse(endsWith(month, "74,999"), 3, groupinc)) %>%
    mutate(groupinc = ifelse(endsWith(month, "75,000"), 4, groupinc)) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor|:incomecat<25,000|:incomecat25,000 - 49,999|:incomecat50,000 - 74,999|:incomecat>=75,000","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month,groupinc) %>%
    mutate(diff = ifelse(group=="treated",coef.month,NA)) %>%
    mutate(calorie=ifelse(group=="control",coef.month,coef.month+coef.month[1:166])) 
  
# add year and month factor as covariate
  #label
  ggplot(data=subset(tidy_mod.factor, !is.na(groupinc)),aes(x=month, y=calorie,color=as.character(groupinc), group=interaction(group, groupinc))) + 
    geom_hline(yintercept = -300, color="grey", linetype="dashed", size=0.5) +
    geom_hline(yintercept=0, linetype="dashed", color = "grey") +
    geom_point(size=1) + geom_line() +
    facet_wrap(~group) +
    #geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)&!is.na(groupinc)),aes(x=month, y=diff*1-300)) + #add diff between 2 groups
    geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)&month>0&!is.na(groupinc)),aes(x=month, y=diff*1-300)) + #add diff between 2 groups
    #geom_point(data=tidy_mod.factor%>%filter(!is.na(diff)&p<0.05&!is.na(groupinc)),aes(x=month, y=diff*1-300)) + #highlight significant months with dots
    geom_point(data=tidy_mod.factor%>%filter(!is.na(diff)&p<0.05&month>0&!is.na(groupinc)),aes(x=month, y=diff*1-300)) + #highlight significant months with dots
    ggplot2::annotate("rect", xmin = -3, xmax = 3, ymin = -450, ymax = 200, fill = "grey") + #add shaded area
    ggplot2::annotate(geom="label", x=0, y=-150, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
    #ggplot2::annotate(geom="label", x=15, y=-225, label="   P<0.05", size=3) + 
    #geom_point(aes(x=13.5,y=-225),color="grey",size=1) +
    coord_cartesian(expand = FALSE, clip = "off") + 
    scale_y_continuous(limits=c(-450,200),breaks=seq(-450,200,50),
                       sec.axis = sec_axis(~(.+300)/1, name="Difference")) +
    scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
    labs(title="Subgroup differences by census tract median income (ref: >=$75k)", x="Month", y="Calories") + 
    scale_color_discrete(name="Income Category", labels=c("<25k", "25-50k", "50-75k")) +
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
          axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
          axis.title.y = element_text(size = 12),
          legend.text=element_text(size=10),
          plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
  
### subgroup differences by %white ----
    
  mod.factor <- plm(formula = calorie ~ treat*relative2.factor + 
                                        as.factor(month) + 
                                        treat*relative2.factor*white,
                    data = matched %>% filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                    index = "id_match", weights = weights, model = "within")

  # Create  functions for tidy that tolerates a near-zero estimate ----
  # chol2inv function ----
  
  chol2inv_alt <- function (x, ...) 
                  {
                    chk.s(..., which.call = -2)
                    tcrossprod(solve(x, tol = 1e-30)) # we need to force a greater tolerance
                  }
  
  # describe function ----
    describe <- function(x,
                         what = c("model", "effect", "random.method",
                                  "inst.method", "transformation", "ht.method")){
      what <- match.arg(what)
      cl <- x$args
      switch(what,
             "model"          = if(!is.null(cl$model))
               cl$model else  "within",
             "effect"         = if(!is.null(cl$effect)) 
               cl$effect else "individual",
             "random.method"  = if(!is.null(cl$random.method))
               cl$random.method else "swar",
             "inst.method"    = if(!is.null(cl$inst.method))
               cl$inst.method else "bvk",
             "transformation" = if(!is.null(cl$transformation))
               cl$transformation else "d",
             "ht.method"      = if(!is.null(cl$ht.method))
               cl$ht.method else "ht"
      )
    }
  
  # data.name function ----
    data.name <- function(x) {
      ## non-exported, used in various tests
      data.name <- paste(deparse(x$call$formula))
      if (length(data.name) > 1L) paste(data.name[1L], "...")
      else data.name
    }
  # pwaldtest.plm ----
  pwaldtest_alt <- function (x, test = c("Chisq", "F"), vcov = NULL, 
                              df2adj = (test == "F" && !is.null(vcov) && missing(.df2)), 
                              .df1, .df2, ...) 
                    {
                      model <- describe(x, "model")
                      test <- match.arg(test)
                      df1 <- if (model == "within") 
                        length(coef(x))
                      else {
                        length(coef(x)) - has.intercept(x)
                      }
                      df2 <- df.residual(x)
                      vcov_arg <- vcov
                      int <- "(Intercept)"
                      coefs_wo_int <- coef(x)[!(names(coef(x)) %in% int)]
                      if (!length(coefs_wo_int)) 
                        stop(paste("No non-intercept regressors in input model 'x',", 
                                   "cannot perform Wald joint significance test"))
                      if (df2adj == TRUE && (is.null(vcov_arg) || test != "F")) {
                        stop("df2adj == TRUE sensible only for robust F test, i.e., test == \"F\" and !is.null(vcov) and missing(.df2)")
                      }
                      if (!is.null(vcov_arg)) {
                        if (is.matrix(vcov_arg)) 
                          rvcov <- rvcov_orig <- vcov_arg
                        if (is.function(vcov_arg)) 
                          rvcov <- rvcov_orig <- vcov_arg(x)
                        rvcov_name <- paste0(", vcov: ", paste0(deparse(substitute(vcov))))
                        if (int %in% names(coef(x))) {
                          rvcov <- rvcov_orig[!rownames(rvcov_orig) %in% int, 
                                              !colnames(rvcov_orig) %in% int]
                          attr(rvcov, which = "cluster") <- attr(rvcov_orig, 
                                                                 which = "cluster")
                        }
                        if (df2adj == TRUE && test == "F") {
                          if (!is.null(attr(rvcov, which = "cluster"))) {
                            if (inherits(rvcov_orig, "vcovCR")) 
                              rvcov <- trans_clubSandwich_vcov(CSvcov = rvcov, 
                                                               index = attr(model.frame(x), "index"))
                            cluster <- attr(rvcov, which = "cluster")
                            pdim <- pdim(x)
                            df2 <- switch(cluster, group = {
                              if (pdim$nT$n == 1L) df2 else (pdim$nT$n - 
                                                               1L)
                            }, time = {
                              if (pdim$nT$T == 1L) df2 else (pdim$nT$T - 
                                                               1L)
                            }, {
                              df2
                            })
                          }
                          else {
                            warning("no attribute 'cluster' in robust vcov found, no finite-sample adjustment for df2")
                          }
                        }
                      }
                      if (!missing(.df1)) 
                        df1 <- .df1
                      if (!missing(.df2)) 
                        df2 <- .df2
                      if (test == "Chisq") {
                        if (is.null(vcov_arg)) {
                          stat <- as.numeric(crossprod(solve(vcov(x)[names(coefs_wo_int), 
                                                                     names(coefs_wo_int)], coefs_wo_int, tol = 1e-30), coefs_wo_int))
                          names(stat) <- "Chisq"
                          pval <- pchisq(stat, df = df1, lower.tail = FALSE)
                          parameter <- c(df = df1)
                          method <- "Wald test for joint significance"
                        }
                        else {
                          stat <- as.numeric(crossprod(solve(rvcov, coefs_wo_int, tol = 1e-30), 
                                                       coefs_wo_int))
                          names(stat) <- "Chisq"
                          pval <- pchisq(stat, df = df1, lower.tail = FALSE)
                          parameter <- c(df = df1)
                          method <- paste0("Wald test for joint significance (robust)", 
                                           rvcov_name)
                        }
                      }
                      if (test == "F") {
                        if (length(formula(x))[2L] > 1L) 
                          stop("test = \"F\" not sensible for IV models")
                        if (is.null(vcov_arg)) {
                          stat <- as.numeric(crossprod(solve(vcov(x)[names(coefs_wo_int), 
                                                                     names(coefs_wo_int)], coefs_wo_int, tol = 1e-30), coefs_wo_int))/df1
                          names(stat) <- "F"
                          pval <- pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)
                          parameter <- c(df1 = df1, df2 = df2)
                          method <- "F test for joint significance"
                        }
                        else {
                          stat <- as.numeric(crossprod(solve(rvcov, coefs_wo_int, tol = 1e-30), 
                                                       coefs_wo_int)/df1)
                          names(stat) <- "F"
                          pval <- pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)
                          parameter <- c(df1 = df1, df2 = df2)
                          method <- paste0("F test for joint significance (robust)", 
                                           rvcov_name)
                        }
                      }
                      res <- list(data.name = data.name(x), statistic = stat, parameter = parameter, 
                                  p.value = pval, method = method, alternative = "at least one coefficient is not null")
                      class(res) <- "htest"
                      return(res)
  }
  
  # summary function ----
  summary_alt <-  function (object, vcov = NULL, ...) 
                  {
                    vcov_arg <- vcov
                    model <- describe(object, "model")
                    effect <- describe(object, "effect")
                    random.method <- describe(object, "random.method")
                    coef_wo_int <- object$coefficients[!(names(coef(object)) %in% 
                                                           "(Intercept)")]
                    int.only <- !length(coef_wo_int)
                    object$r.squared <- if (!int.only) {
                      c(rsq = r.squared(object), adjrsq = r.squared(object, 
                                                                    dfcor = TRUE))
                    }
                    else {
                      c(rsq = r.squared(object, type = "rss"), adjrsq = r.squared(object, 
                                                                                  type = "rss", dfcor = TRUE))
                    }
                    use.norm.chisq <- if (model == "random" || length(formula(object))[2L] >= 
                                          2L || model == "ht") 
                      TRUE
                    else FALSE
                    if (!int.only) {
                      object$fstatistic <- pwaldtest_alt(object, test = if (use.norm.chisq) 
                        "Chisq"
                        else "F", vcov = vcov_arg)
                    }
                    if (!is.null(vcov_arg)) {
                      if (is.matrix(vcov_arg)) 
                        rvcov <- vcov_arg
                      if (is.function(vcov_arg)) 
                        rvcov <- vcov_arg(object)
                      std.err <- sqrt(diag(rvcov))
                    }
                    else {
                      std.err <- sqrt(diag(stats::vcov(object)))
                    }
                    b <- coefficients(object)
                    z <- b/std.err
                    p <- if (use.norm.chisq) {
                      2 * pnorm(abs(z), lower.tail = FALSE)
                    }
                    else {
                      2 * pt(abs(z), df = object$df.residual, lower.tail = FALSE)
                    }
                    object$coefficients <- cbind(b, std.err, z, p)
                    colnames(object$coefficients) <- if (use.norm.chisq) {
                      c("Estimate", "Std. Error", "z-value", 
                        "Pr(>|z|)")
                    }
                    else {
                      c("Estimate", "Std. Error", "t-value", 
                        "Pr(>|t|)")
                    }
                    if (!is.null(vcov_arg)) {
                      object$rvcov <- rvcov
                      rvcov.name <- paste0(deparse(substitute(vcov)))
                      attr(object$rvcov, which = "rvcov.name") <- rvcov.name
                    }
                    object$df <- c(length(b), object$df.residual, length(object$aliased))
                    class(object) <- c("summary.plm", "plm", "panelmodel")
                    object
                  }
  
  # tidy function ----
  
  tidy_alt <- function (x, conf.int = FALSE, conf.level = 0.95, ...) 
              {
                warn_on_subclass(x)
                ret <- as_tibble(summary_alt(x)$coefficients, rownames = "term")
                colnames(ret) <- c("term", "estimate", "std.error", 
                                   "statistic", "p.value")
                coefs <- tibble::enframe(stats::coef(x), name = "term", 
                                         value = "estimate")
                ret <- left_join(coefs, ret, by = c("term", "estimate"))
                if (conf.int) {
                  ci <- broom_confint_terms(x, level = conf.level)
                  ret <- dplyr::left_join(ret, ci, by = "term")
                }
                ret
              }
  # warn_on_subclass ----
  
  warn_on_subclass <- function(x) {
    if (length(class(x)) > 1 && class(x)[1] != "glm") {
      subclass <- class(x)[1]
      dispatched_method <- class(x)[class(x) %in% c("glm", "lm")][1]
      
      warning(
        "Tidiers for objects of class ", 
        subclass, 
        " are not maintained by the broom team, and are only supported through ",
        "the ", 
        dispatched_method, 
        " tidier method. Please be cautious in interpreting and reporting ",
        "broom output.",
        call. = FALSE
      )
    }
  }
  
  # broom_confint_terms ----
  broom_confint_terms <- function(x, ...) {
    
    # warn on arguments silently being ignored
    ellipsis::check_dots_used()
    ci <- suppressMessages(confint(x, ...))
    
    # confint called on models with a single predictor
    # often returns a named vector rather than a matrix :(
    
    if (is.null(dim(ci))) {
      ci <- matrix(ci, nrow = 1)
      rownames(ci) <- names(coef(x))[1]
    }
    
    ci <- as_tibble(ci, rownames = "term")
    names(ci) <- c("term", "conf.low", "conf.high")
    ci
  }
  # Resume code ----
  
  tidy_mod.factor <- tidy_alt(mod.factor,conf.level = 0.95,conf.int = TRUE)
  
  tidy_mod.factor <- tidy_mod.factor %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group = ifelse(row_number() <= 55, "control", NA)) %>%
    mutate(group = ifelse(row_number() > 55 & row_number() <= 110, "treated", group)) %>%
    mutate(group = ifelse(row_number() > 110 & row_number() <= 275, "control", group)) %>%
    mutate(group = ifelse(row_number() > 275 & row_number() <= 440, "treated", group)) %>%
    add_row(month="-3",coef.month=0,group="control",low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group="treated",low=0,high=0) %>%
    mutate(groupinc = ifelse(endsWith(month, "25,000"), 1, NA)) %>%
    mutate(groupinc = ifelse(endsWith(month, "49,999"), 2, groupinc)) %>%
    mutate(groupinc = ifelse(endsWith(month, "74,999"), 3, groupinc)) %>%
    mutate(groupinc = ifelse(endsWith(month, "75,000"), 4, groupinc)) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor|:incomecat<25,000|:incomecat25,000 - 49,999|:incomecat50,000 - 74,999|:incomecat>=75,000","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month,groupinc) %>%
    mutate(diff = ifelse(group=="treated",coef.month,NA)) %>%
    mutate(calorie=ifelse(group=="control",coef.month,coef.month+coef.month[1:221])) 
  