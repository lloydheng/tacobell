### install and load packages ----
library(dplyr)
library(ggplot2)
library(tidyverse)
#install.packages("tigris") #geocoding, add census tract number to geo coordinates
#install.packages("sf")
#install.packages("tidycensus") #get census tract boundaries
library(tigris)
library(sf)
library(tidycensus)
#install.packages("rollmatch")
library(rollmatch)
#install.packages("cobalt") #check and visualize balances for ps matching/rem
library(cobalt)
#install.packages("MatchIt")
library(MatchIt)
#install.packages("tableone") #calcualte SMD before matching
library(tableone)
#install.packages("CBPS") #post matching covariate balancing
library(CBPS)
#install.packages("Matching")
library(Matching)
#install.packages("optmatch")
library(optmatch)
#install.packages("ebal") #entropy balance weighting after matching
library(ebal)
#install.packages("WeightIt")
library(WeightIt)
#install.packages("broom") #change lm model results into dataframe for estimating individual slopes
library(broom)
#install.packages("sbw")
library(sbw)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("gbm")
library(gbm)
#install.packages("cowplot")
library(cowplot)
#install.packages("patchwork")
library(patchwork)
library(plm)


### Load data ----
setwd("G:/Dr.Brian Elbel’s Projects/CURRENT PROJECTS/Labeling R01/DATA/wu-data/tacobell")

load("suffolk weights.RData")

### clean invalid observations ----
## drop all restaurants with a monthly calorie value (within our sampled range) that is <50% of overall mean
#
#todrop <- restaurant %>%
#  mutate(relative2 = monthno - entry) %>% # defines relative month
#  filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)) %>% # sampled months
#  group_by(address) %>%
#  mutate(meancal = mean(calorie)) %>%
#  filter(calorie < meancal*0.5) %>%
#  subset(, select = c(address))
## subset(, select = c(address, treat, relative2, calorie))
#
##write.csv(todrop, "manuscript/tables/dropped.csv")
#
#restaurant <- restaurant[!restaurant$address %in% todrop$address,] # remove violations


### Alternative cleaning - simple imputation of mean ----
todrop <- restaurant %>%
  mutate(relative2 = monthno - entry) %>% # defines relative month
  filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)) %>% # sampled months
  group_by(address) %>%
  mutate(meancal = mean(calorie)) %>%
  filter(calorie < meancal*0.5) %>%
  mutate(calorie = meancal) %>%
  dplyr::select(-relative2, -meancal)

dropped <- restaurant %>%
  mutate(relative2 = monthno - entry) %>% # defines relative month
  filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)) %>% # sampled months
  group_by(address) %>%
  mutate(meancal = mean(calorie)) %>%
  filter(calorie < meancal*0.5) %>%
  dplyr::select(-relative2, -meancal)

restaurant <- restaurant %>%
  anti_join(dropped)

restaurant <- rbind(restaurant, todrop) %>%
  group_by(address)

# Checking
todrop <- restaurant %>%
  mutate(relative2 = monthno - entry) %>% # defines relative month
  filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)) %>% # sampled months
  group_by(address) %>%
  mutate(meancal = mean(calorie)) %>%
  filter(calorie < meancal*0.5) %>%
  subset(, select = c(address))

### Matching procedure 1 - significant predictors of outcome ----
formula <- calorie~concept+ownership+drive+meal+calorie3+
  slope_calorie+count3+slope_count+dollar3+slope_dollar+
  calorie_all3+slope_calorie_all+count_all3+slope_count_all+dollar_all3+slope_dollar_all+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65+open12+open18+open24


# Significant predictors, stratified by location
sig <- NULL
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")

for (i in 1:10) {
  subset <- subset(restaurant, (treat==1&policy==time[i,1])|treat==0)
  subset$entry <- time[i,2]
  tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
    filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
    filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
    filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
    filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
    dplyr::select(address,open8,open12,open18,open24) %>% distinct()
  subset <- merge(subset,tmp,by="address")
  subset <- subset(subset, open8==1&monthno==251) 
  subset <- subset[complete.cases(subset), ]
  
  model <- summary(lm(formula, subset))
  model_sig <- tidy(model) %>%
    filter(p.value < 0.05) %>%
    subset(, select = c(term))
  
  sig[[i]] <- rbind(sig[i], model_sig)
}

# Creates a list of formulae (10 locations)
formula_list <- data.frame(2, 1:10)
colnames(formula_list) <- c("location", "formula")
formula_list$location <- c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc")

formula_list$formula <- c(treat ~ ownership + calorie3 + calorie_all3 + slope_calorie_all + slope_count_all + black + under18 + open18 + open24,
                          treat ~ ownership + drive + calorie3 + slope_calorie + calorie_all3 + black + under18 + open12 + open18 + open24,
                          treat ~ ownership + calorie3 + slope_calorie + black + open24,
                          treat ~ ownership + calorie3 + slope_calorie + black + capital_income + above65 + open12,
                          treat ~ ownership + calorie3 + slope_calorie + black + capital_income + above65,
                          treat ~ calorie3 + slope_calorie + black + median_income + capital_income + above65,
                          treat ~ ownership + meal + calorie3 + slope_calorie + black + above65,
                          treat ~ ownership + calorie3 + slope_calorie + black + capital_income + above65,
                          treat ~ ownership + calorie3 + slope_calorie + median_income + capital_income + open24,
                          treat ~ ownership + calorie3 + slope_calorie + black + median_income + capital_income + above65 + open12)

formula_list # looks right
rm(model, model_sig, sig, subset, tmp, todrop)

  ### Drop covariates to see if PS overlap improves ----
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")
master <- NULL
matched <- NULL

for (i in c(1:10)) {
  tryCatch({#catch groups that do not have comparison restaurants
    formula <- as.character(formula_list[i, 2])
    formula <- as.formula(formula)
    restaurant2 <- restaurant
    subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
    subset$entry <- time[i,2]
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==time[i,2]) 
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    subset$distance <- subset.match$distance
    
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    summary(match$weights[match$treat==0])
    max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))
    
    while(max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))) {
      tmp <- match[match$weights>=0.05*length(unique(match$address[match$treat==0])),]
      restaurant2 <- anti_join(restaurant2,tmp,by="address")
      subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
      subset$entry <- time[i,2]
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==time[i,2]) 
      subset <- subset[complete.cases(subset), ]
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      subset$distance <- subset.match$distance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- time[i,1]
    }
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
    rm(subset,subset.match,tmp,match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}

# Some restaurants were used as matches multiple times
master_all <- NULL
for (i in c("ca","king","ma","mont","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(entry = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(c(address:above65,treat,policy)) %>%
    rename(entry = entry.x) %>%
    distinct()
  master_all <- rbind(master_all, tmp)
  master_all <- master_all %>% 
    distinct()
}
#write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", row.names = FALSE)
#rm(master_all, i,tmp)

  ### matched data, preparing data ----
matched <- master_all

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


  ### table 3, diff in diff, overall and by location ----
#set up table shell
table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("total","ca","ma","or","suffolk"))
rownames(table3) <- c("total","ca","ma","or","suffolk")
#overall
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|calorie", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
  mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
  mutate(high = ifelse(group==0,high,high+high[1:56]))
# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#by location
for (p in c("ca","ma","or","suffolk")) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place==p), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}
#fitting everything in the right format with blank rows and columns
table3 <- table3 %>%
  add_row(diff_treat_3_12="",.before=2) %>% add_row(diff_treat_3_12="",.before=2) %>%
  add_column(col="",.before=4) %>%
  mutate_all(~replace(., is.na(.), ""))
write.csv(table3, "manuscript/tables/table3-match1-sigpred-imp.csv")
rm(treat,comp,mod.factor,table3,p,presum,tidy_mod.factor)

### Matching procedure 2 - match on only calorie and dollar vars ----
formula <- treat ~ calorie3 + calorie_all3 + dollar3 + dollar_all3 
  ### Drop covariates to see if PS overlap improves ----
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")
master <- NULL
matched <- NULL

for (i in c(1:10)) {
  tryCatch({#catch groups that do not have comparison restaurants
    restaurant2 <- restaurant
    subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
    subset$entry <- time[i,2]
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==time[i,2]) 
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    subset$distance <- subset.match$distance
    
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    summary(match$weights[match$treat==0])
    max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))
    
    while(max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))) {
      tmp <- match[match$weights>=0.05*length(unique(match$address[match$treat==0])),]
      restaurant2 <- anti_join(restaurant2,tmp,by="address")
      subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
      subset$entry <- time[i,2]
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==time[i,2]) 
      subset <- subset[complete.cases(subset), ]
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      subset$distance <- subset.match$distance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- time[i,1]
    }
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
    rm(subset,subset.match,tmp,match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}

# Some restaurants were used as matches multiple times
master_all <- NULL
for (i in c("ca","king","ma","mont","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(entry = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(c(address:above65,treat,policy)) %>%
    rename(entry = entry.x) %>%
    distinct()
  master_all <- rbind(master_all, tmp)
  master_all <- master_all %>% 
    distinct()
}
#write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", row.names = FALSE)
#rm(master_all, i,tmp)


  ### matched data, preparing data ----
matched <- master_all

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


  ### table 3, diff in diff, overall and by location ----
#set up table shell
table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("total","ca","ma","or","suffolk"))
rownames(table3) <- c("total","ca","ma","or","suffolk")
#overall
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|calorie", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
  mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
  mutate(high = ifelse(group==0,high,high+high[1:56]))
# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#by location
for (p in c("ca","ma","or","suffolk")) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place==p), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}
#fitting everything in the right format with blank rows and columns
table3 <- table3 %>%
  add_row(diff_treat_3_12="",.before=2) %>% add_row(diff_treat_3_12="",.before=2) %>%
  add_column(col="",.before=4) %>%
  mutate_all(~replace(., is.na(.), ""))
write.csv(table3, "manuscript/tables/table3-match2-caloriedollar-imp.csv")
rm(treat,comp,mod.factor,table3,p,presum,tidy_mod.factor)

### Matching procedure 3 - match on all 8 calorie and dollar vars ----
formula <- treat ~ calorie3 + calorie_all3 + dollar3 + dollar_all3 +
  slope_calorie + slope_dollar + slope_calorie_all + slope_dollar_all

  ### Drop covariates to see if PS overlap improves ----
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")
master <- NULL
matched <- NULL

for (i in c(1:10)) {
  tryCatch({#catch groups that do not have comparison restaurants
    restaurant2 <- restaurant
    subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
    subset$entry <- time[i,2]
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==time[i,2]) 
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    subset$distance <- subset.match$distance
    
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    summary(match$weights[match$treat==0])
    max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))
    
    while(max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))) {
      tmp <- match[match$weights>=0.05*length(unique(match$address[match$treat==0])),]
      restaurant2 <- anti_join(restaurant2,tmp,by="address")
      subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
      subset$entry <- time[i,2]
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==time[i,2]) 
      subset <- subset[complete.cases(subset), ]
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      subset$distance <- subset.match$distance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- time[i,1]
    }
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
    rm(subset,subset.match,tmp,match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}

# Some restaurants were used as matches multiple times
master_all <- NULL
for (i in c("ca","king","ma","mont","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(entry = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(c(address:above65,treat,policy)) %>%
    rename(entry = entry.x) %>%
    distinct()
  master_all <- rbind(master_all, tmp)
  master_all <- master_all %>% 
    distinct()
}
#write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", row.names = FALSE)
#rm(master_all, i,tmp)


  ### matched data, preparing data ----
matched <- master_all

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


  ### table 3, diff in diff, overall and by location ----
#set up table shell
table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("total","ca","ma","or","suffolk"))
rownames(table3) <- c("total","ca","ma","or","suffolk")
#overall
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|calorie", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
  mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
  mutate(high = ifelse(group==0,high,high+high[1:56]))
# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#by location
for (p in c("ca","ma","or","suffolk")) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place==p), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}
#fitting everything in the right format with blank rows and columns
table3 <- table3 %>%
  add_row(diff_treat_3_12="",.before=2) %>% add_row(diff_treat_3_12="",.before=2) %>%
  add_column(col="",.before=4) %>%
  mutate_all(~replace(., is.na(.), ""))
write.csv(table3, "manuscript/tables/table3-match3-allcaloriedollar-imp.csv")
rm(treat,comp,mod.factor,table3,p,presum,tidy_mod.factor)

### Matching procedure 4 - match on all 8 calorie and dollar vars, plus restaurant-level characteristics ----
formula <- treat~concept+ownership+drive+meal+
  calorie3+slope_calorie+count3+slope_count+dollar3+slope_dollar+
  calorie_all3+slope_calorie_all+count_all3+slope_count_all+dollar_all3+slope_dollar_all+
  open12+open18+open24

  ### Drop covariates to see if PS overlap improves ----
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")
master <- NULL
matched <- NULL

for (i in c(1:10)) {
  tryCatch({#catch groups that do not have comparison restaurants
    restaurant2 <- restaurant
    subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
    subset$entry <- time[i,2]
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==time[i,2]) 
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    subset$distance <- subset.match$distance
    
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    summary(match$weights[match$treat==0])
    max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))
    
    while(max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))) {
      tmp <- match[match$weights>=0.05*length(unique(match$address[match$treat==0])),]
      restaurant2 <- anti_join(restaurant2,tmp,by="address")
      subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
      subset$entry <- time[i,2]
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==time[i,2]) 
      subset <- subset[complete.cases(subset), ]
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      subset$distance <- subset.match$distance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- time[i,1]
    }
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
    rm(subset,subset.match,tmp,match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}

# Some restaurants were used as matches multiple times
master_all <- NULL
for (i in c("ca","king","ma","mont","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(entry = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(c(address:above65,treat,policy)) %>%
    rename(entry = entry.x) %>%
    distinct()
  master_all <- rbind(master_all, tmp)
  master_all <- master_all %>% 
    distinct()
}
#write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", row.names = FALSE)
#rm(master_all, i,tmp)


  ### matched data, preparing data ----
matched <- master_all

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


  ### table 3, diff in diff, overall and by location ----
#set up table shell
table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("total","ca","ma","or","suffolk"))
rownames(table3) <- c("total","ca","ma","or","suffolk")
#overall
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|calorie", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
  mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
  mutate(high = ifelse(group==0,high,high+high[1:56]))
# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#by location
for (p in c("ca","ma","or","suffolk")) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place==p), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}
#fitting everything in the right format with blank rows and columns
table3 <- table3 %>%
  add_row(diff_treat_3_12="",.before=2) %>% add_row(diff_treat_3_12="",.before=2) %>%
  add_column(col="",.before=4) %>%
  mutate_all(~replace(., is.na(.), ""))
write.csv(table3, "manuscript/tables/table3-match4-allcaloriedollar-rest-imp.csv")
rm(treat,comp,mod.factor,table3,p,presum,tidy_mod.factor)


### Matching procedure 5 - significant predictors of outcome for full model ----
formula <- calorie~concept+ownership+drive+meal+calorie3+
  slope_calorie+count3+slope_count+dollar3+slope_dollar+
  calorie_all3+slope_calorie_all+count_all3+slope_count_all+dollar_all3+slope_dollar_all+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65+open12+open18+open24


# Significant predictors, stratified by location
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")

subset_all <- NULL

for (i in c(1:10)) {
  subset <- subset(restaurant, (treat==1&policy==time[i,1])|treat==0)
  subset$entry <- time[i,2]
  tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
    filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
    filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
    filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
    filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
    dplyr::select(address,open8,open12,open18,open24) %>% distinct()
  subset <- merge(subset,tmp,by="address")
  subset <- subset(subset, open8==1&monthno==251) 
  subset <- subset[complete.cases(subset), ]
  
  subset_all <- rbind(subset_all, subset)
}


model <- summary(lm(formula, subset_all))
model_sig <- tidy(model) %>%
  filter(p.value < 0.05) %>%
  subset(, select = c(term))

model_sig

# New formula (predictors of overall model)
formula <- treat ~ concept + ownership + meal + calorie3 + slope_calorie +
  slope_count + slope_dollar + slope_dollar_all + male + white +
  black + asian + median_income + capital_income + collegeup + under18 + above65


  ### Drop covariates to see if PS overlap improves ----
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")
master <- NULL
matched <- NULL

for (i in c(1:10)) {
  tryCatch({#catch groups that do not have comparison restaurants
    restaurant2 <- restaurant
    subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
    subset$entry <- time[i,2]
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==time[i,2]) 
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    subset$distance <- subset.match$distance
    
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    summary(match$weights[match$treat==0])
    max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))
    
    while(max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))) {
      tmp <- match[match$weights>=0.05*length(unique(match$address[match$treat==0])),]
      restaurant2 <- anti_join(restaurant2,tmp,by="address")
      subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
      subset$entry <- time[i,2]
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==time[i,2]) 
      subset <- subset[complete.cases(subset), ]
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      subset$distance <- subset.match$distance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- time[i,1]
    }
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
    rm(subset,subset.match,tmp,match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}

# Some restaurants were used as matches multiple times
master_all <- NULL
for (i in c("ca","king","ma","mont","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(entry = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(c(address:above65,treat,policy)) %>%
    rename(entry = entry.x) %>%
    distinct()
  master_all <- rbind(master_all, tmp) %>% distinct()
}

#write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", row.names = FALSE)
#rm(master_all, i,tmp)

  ### matched data, preparing data ----
matched <- master_all

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


  ### table 3, diff in diff, overall and by location ----
#set up table shell
table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("total","ca","ma","or","suffolk"))
rownames(table3) <- c("total","ca","ma","or","suffolk")
#overall
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|calorie", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
  mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
  mutate(high = ifelse(group==0,high,high+high[1:56]))
# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#by location
for (p in c("ca","ma","or","suffolk")) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place==p), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}


write.csv(table3, "manuscript/tables/table3-match5-overallsigpred-imp.csv")
rm(treat,comp,mod.factor,table3,p,presum,tidy_mod.factor)

### Estimates from unmatched model ----
  ### Load and clean data ----
unmatched <- read.csv("data/calorie-aims/unmatched-restaurants-drive-thru.csv", stringsAsFactors = FALSE,
                      colClasses = c(rep(NA,7),rep("NULL",4),rep(NA,8),rep("NULL",13),rep(NA,2),rep("NULL",12),NA))
unmatched$tract_num <- substr(unmatched$tract_num, 2, 12)
table(unmatched$policy[(unmatched$treat==1&unmatched$monthno==unmatched$entry)])
unmatched <- unmatched %>%
  filter((policy!="nyc"&policy!="ulster"&policy!="westchester")&!is.na(calorie))

time <- unmatched %>% dplyr::select(entry,policy) %>% filter(!duplicated(policy)) %>% filter(!is.na(entry))
comp <- unmatched[unmatched$treat==0, ]
master <- NULL
for (i in 1:10) {
  treat <- unmatched[unmatched$policy==time[i,2], ]
  treat <- rbind(treat,comp)
  treat$match_to <- time[i,2]
  treat$entry <- time[i,1]
  master <- rbind(master, treat)
}

unmatched <- master
rm(master,treat,comp,i,time)
table(unmatched$entry)
table(unmatched$match_to)
unmatched <- unmatched %>%
  filter(!is.na(calorie)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept, match_to)) %>%
  arrange(id, monthno) %>% #find out for how many months a restaurant was open
  group_by(id, treat) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(unmatched$rank) #sanity check, the max should be 106

# set up post ML indicator
unmatched$relative <- unmatched$monthno - unmatched$entry +1
unmatched$relative2 <- unmatched$monthno - unmatched$entry #month 0 is first month of ML
unmatched$post <- ifelse(unmatched$relative2<0,0,1)

# month as relative and factor
# set month 1 as ref group
unmatched$relative.factor <- factor(unmatched$relative)
unmatched <- within(unmatched, relative.factor<-relevel(relative.factor, ref="1"))

unmatched$relative2.factor <- factor(unmatched$relative2)
unmatched <- within(unmatched, relative2.factor<-relevel(relative2.factor, ref="3"))

# calculate open_month both before and after ML
unmatched <- unmatched %>%
  group_by(id, treat, match_to, post) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
unmatched$open_before <- ifelse(unmatched$post==0, unmatched$open_before, unmatched$open_month-unmatched$open_before)
unmatched$open_after <- ifelse(unmatched$post==1, unmatched$open_after, unmatched$open_month-unmatched$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- unmatched %>%
  group_by(id, treat, match_to) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_to, after6,after12,after18,after24) %>%
  distinct()
unmatched <- merge(unmatched, tmp, by=c("id", "treat", "match_to"), all = TRUE)
tmp <- unmatched %>%
  group_by(id, treat, match_to) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_to, before6,before12,before18,before24) %>%
  distinct()
unmatched <- merge(unmatched, tmp, by=c("id", "treat", "match_to"), all = TRUE)
unmatched$open6 <- ifelse(unmatched$before6==1&unmatched$after6==1,1,0)
unmatched$open12 <- ifelse(unmatched$before12==1&unmatched$after12==1,1,0)
unmatched$open18 <- ifelse(unmatched$before18==1&unmatched$after18==1,1,0)
unmatched$open24 <- ifelse(unmatched$before24==1&unmatched$after24==1,1,0)
rm(tmp)

unmatched <- within(unmatched, relative2.factor<-relevel(relative2.factor, ref="-3"))
unmatched$id_match <- paste0(unmatched$id, unmatched$match_place)

  ### Analysis ----

# set up table shell
table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("total","ca","ma","or","suffolk"))
rownames(table3) <- c("total","ca","ma","or","suffolk")

#overall
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|calorie", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
  mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
  mutate(high = ifelse(group==0,high,high+high[1:56]))

# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")

#by location
for (p in c("ca","ma","or","suffolk")) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place==p), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}


write.csv(table3, "manuscript/tables/table3-unmatched-imp.csv")
rm(treat,comp,mod.factor,table3,p,presum,tidy_mod.factor)


