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


### clean invalid observations ----
  # drop all restaurants with a monthly calorie value (within our sampled range) that is <50% of overall mean

  todrop <- restaurant %>%
            mutate(relative2 = monthno - entry) %>% # defines relative month
            filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)) %>% # sampled months
            group_by(address) %>%
            mutate(meancal = mean(calorie)) %>%
            filter(calorie < meancal*0.5) %>%
            subset(, select = c(address))
  
  restaurant <- restaurant[!restaurant$address %in% todrop$address,] # remove violations

### ps matching + iptw weighting, trim extrem weights ----
#ignore 2 months leading to ML
#match on months t-8 to t-3
names(restaurant)
formula <- treat~concept+ownership+drive+meal+
  calorie3+slope_calorie+count3+slope_count+dollar3+slope_dollar+
  calorie_all3+slope_calorie_all+count_all3+slope_count_all+dollar_all3+slope_dollar_all+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65+open12+open18+open24
set.seed(5)
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")
master <- NULL
matched <- NULL

  tryCatch({#catch groups that do not have comparison restaurants
    subset <- subset(restaurant, (treat==1&policy=="suffolk")|treat==0)
    subset$entry <- 251
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==251) 
    #matching
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- "suffolk"
    # combine clusters of restaurants
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
  }, error=function(e){cat(paste0("ERROR : ","suffolk"),conditionMessage(e), "\n")})

master.original <- master
matched.original <- matched
restaurant2 <- restaurant
length(unique(paste0(matched.original$address[matched.original$treat==0],matched.original$match_place[matched.original$treat==0]))) #1518
length(unique(paste0(matched.original$address[matched.original$treat==1],matched.original$match_place[matched.original$treat==1]))) #506
length(unique(paste0(master.original$address[master.original$treat==0],master.original$match_place[master.original$treat==0]))) #1942
length(unique(paste0(master.original$address[master.original$treat==1],master.original$match_place[master.original$treat==1]))) #506

# trim large weights
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
rm(restaurant2,time,tmp,i,subset,subset.match,bal,match)

table(matched$match_place[matched$treat==1])
length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0]))) #1518
length(unique(paste0(matched$address[matched$treat==1],matched$match_place[matched$treat==1]))) #506
length(unique(paste0(master$address[master$treat==0],master$match_place[master$treat==0]))) #1904
length(unique(paste0(master$address[master$treat==1],master$match_place[master$treat==1]))) #506

summary(matched$weights[matched$treat==0])
summary(matched.original$weights[matched.original$treat==0])
par(mfrow=c(2,1))
hist(matched.original$weights[matched.original$treat==0], breaks = 500,xlab = "Weight",main="Histogram of comparison units weights")
hist(matched$weights[matched$treat==0], breaks = 500,xlab = "Weight",main="After trimming")
par(mfrow=c(1,1))

#export results in matched2, combine with all months of restaurant data
names(matched)
length(unique(matched$address)) #some comparison restaurants were matched to multiple treated restaurants
table(matched$match_place)
master_all <- NULL
for (i in c("ca","king","ma","mont","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(entry = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(c(address:above65,treat,policy)) %>%
    rename(entry = entry.x)
  master_all <- rbind(master_all, tmp)
}
write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-sensitivity.csv", row.names = FALSE)
rm(master_all, i,tmp)

result <- cbind(col_w_smd(mat=subset(master.original,select = c(3:4,15:16,20,22:32,35:46,49:52)),
                          treat = master.original$treat,
                          std = TRUE, bin.vars = c(rep(TRUE,2),rep(FALSE,26),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched.original, select = c(3:4,15:16,20,22:32,35:46,49:52)),
                          weights = matched.original$weights, treat = matched.original$treat, s.weights = matched.original$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,26),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched, select = c(3:4,15:16,20,22:32,35:46,49:52)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,26),rep(TRUE,3),FALSE)))
colnames(result)[1:3] <- c("pre", "ps_weight","ps_weight_trim")
result <- data.frame(cbind(result,label=c("Ownership", "Joint brand","% drive-through transactions", "% lunch/dinner transactions",
                                          "% male", "Total population","% white", "% Black", "% Asian",
                                          "% Hispanic", "Household median income", "Income per capita",
                                          "% without HS degree", "% has college degree and up","% under 18","% above 65",
                                          "# of drive-through transactions, t-3","Drive-through mean spending, t-3","Drive-through mean calorie, t-3",
                                          "# of transactions, t-3","Mean spending, t-3","Mean calorie, t-3",
                                          "Drive-through mean spending trend", "# of drive-through transactions trend", "Drive-through mean calorie trend",
                                          "Mean spending trend", "# of transactions trend", "Mean calorie trend",
                                          "Has 12-month baseline data","Has 18-month baseline data","Has 24-month baseline data","Distance")))
result <- reshape(result, direction = "long",
                  varying = list(names(result)[1:3]), v.names = "score",
                  idvar = "label",
                  timevar = "method", times = c("pre","ps_weight","ps_weight_trim"))
result$score <- as.numeric(result$score)

result$label <- factor(result$label,
                       levels=c("Distance","Ownership", "Joint brand","Has 12-month baseline data","Has 18-month baseline data","Has 24-month baseline data",
                                "# of drive-through transactions, t-3","Drive-through mean spending, t-3","Drive-through mean calorie, t-3",
                                "# of transactions, t-3","Mean spending, t-3","Mean calorie, t-3",
                                "Drive-through mean spending trend", "# of drive-through transactions trend", "Drive-through mean calorie trend",
                                "Mean spending trend", "# of transactions trend", "Mean calorie trend",
                                "% drive-through transactions", "% lunch/dinner transactions",
                                "Total population","% male", "% white", "% Black", "% Asian",
                                "% Hispanic", "Household median income", "Income per capita",
                                "% without HS degree", "% has college degree and up","% under 18","% above 65"))
result$method <- factor(result$method, levels = c("pre","ps_weight","ps_weight_trim"))

ggplot(data = result,
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "red", size = 0.5, linetype="dashed") +
  geom_hline(yintercept = -0.1, color = "red", size = 0.5, linetype="dashed") +
  #geom_vline(xintercept = 25.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  labs(title="Covariate balance, match drive-thru data", y="Standardized mean differences", x="", caption="") +
  scale_color_manual(name="Sample", labels=c("Unmatched","PS+IPTW", "PS+IPTW, trimmed"),
                     values =c("orange", "aquamarine3","violet")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/match-drive-thru/covariate-balance-after-trim.jpeg", dpi="retina")

#for paper
ggplot(data = result%>%filter(method!="ps_weight"),
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point() +
  geom_hline(yintercept = 0.25, color = "red", size = 0.5, linetype="dashed") +
  geom_hline(yintercept = -0.25, color = "red", size = 0.5, linetype="dashed") +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  scale_y_continuous(limits = c(-1,3),breaks=c(-1,-0.25,0,0.25,1,2,3)) + 
  labs(y="Standardized mean differences", x="") +
  scale_color_manual(name="Sample", labels=c("Unmatched","Matched"), values =c("orange", "aquamarine3")) +
  theme_bw() + theme(legend.key = element_blank(),
                     plot.title = element_text(hjust = 0.5),
                     plot.caption=element_text(hjust=0, face="italic"))
#ggsave("manuscript/figures/covariate-balance.jpeg", dpi="retina")







### plotmatched <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", stringsAsFactors = FALSE)
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
  
  names(matched)[names(matched) == "s.weights"] <- "s_weights"
  names(matched)[names(matched) == "relative.factor"] <- "relative_factor"
  names(matched)[names(matched) == "relative2.factor"] <- "relative2_factor" of covariate distribution for matched/unmatched restaurants ----


### Creating distribution plots for suffolk ----
  # list of covariates
  covs <- c("dollar3", "dollar_all3", "calorie3", "calorie_all3", "median_income",
            "meal", "drive", "slope_calorie", "count3", "slope_count", "slope_dollar",
            "slope_calorie_all", "count_all3", "slope_count_all", "slope_dollar_all", "total", "male",
            "white", "black", "asian", "hisp", "capital_income", "hsbelow", "collegeup", "under18", "above65")
  
  
  covs2 <- c("concept", "ownership", "open12", "open18", "open24")
  
  match <- match %>% mutate(policy=ifelse(policy=="suffolk", "treated","control"))
  
  suffolk_t <- match %>% filter(policy == "treated") 
  
  suffolk_c <- match %>% filter(policy == "control") 
  
  plots_covs <- lapply(covs, function(var_x){
      ggplot(suffolk_t) +
      aes_string(var_x) +
      geom_density(color = "#00abff") +
      geom_density(data = suffolk_c, color = "red")
  })
  
  
  plots_covs2 <- lapply(covs2, function(var_x){
    ggplot(suffolk_t) +
    aes_string(var_x) +
    geom_histogram(aes(y = (..count..)/16), binwidth = 0.5, fill = NA, color = "#00abff") +
    ylab("Density") +
    geom_histogram(data = suffolk_c, aes(y = (..count..)/48), binwidth = 0.5, fill = NA, color = "red")
  })
  
  plots <- append(plots_covs, plots_covs2) 

  plot_grid(plotlist = plots) 
  
  # Gets us the legend
  p1 <- ggplot(match, aes(x=concept, color = policy)) + geom_density()
  
  legend <- get_legend(
    # create some space to the left of the legend
    p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
  )
  
  plot_grid(plotlist = plots, legend, nrow = 8)
  
  
### Matching procedure 1 - significant predictors of outcome ----
  formula <- calorie3~concept+ownership+drive+meal+
    slope_calorie+count3+slope_count+dollar3+slope_dollar+
    calorie_all3+slope_calorie_all+count_all3+slope_count_all+dollar_all3+slope_dollar_all+
    total+male+white+black+asian+hisp+median_income+capital_income+
    hsbelow+collegeup+under18+above65+open12+open18+open24
  
  # Subsets for suffolk
  subset <- subset(restaurant, (treat==1&policy=="suffolk")|treat==0)
  subset$entry <- 251
  tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
    filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
    filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
    filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
    filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
    dplyr::select(address,open8,open12,open18,open24) %>% distinct()
  subset <- merge(subset,tmp,by="address")
  subset <- subset(subset, open8==1&monthno==251) 
  subset <- subset[complete.cases(subset), ]
  
  # Regression for suffolk
  suffolk_lm <- summary(lm(formula, subset))
  suffolk_sig <- tidy(suffolk_lm) %>%
                      filter(p.value < 0.05) %>%
                      subset(, select = c(term))
  
  suffolk_sig # we would want to match on these 18 covariates
  
  # New formula
  formula <- treat ~ concept + ownership + meal + slope_calorie +
                     dollar3 + slope_dollar + calorie3 + calorie_all3 + slope_calorie_all +
                     slope_count_all + dollar_all3 + slope_dollar_all + male +
                     white + black + asian + median_income +
                     hsbelow + collegeup
  
  ### Drop covariates to see if PS overlap improves ----
  
  master <- NULL
  matched <- NULL
  
  # formula <- treat~concept+ownership+drive+meal+
  #   slope_calorie+count3+slope_count+slope_dollar+
  #   slope_calorie_all+count_all3+slope_count_all+slope_dollar_all+ 
  #   total+male+white+black+asian+hisp+
  #   hsbelow+collegeup+under18+above65+open12+open18+open24+
  #   capital_income
  ## dollar3, dollar_all3, median, calorieall, calorie3
  
  
  tryCatch({#catch groups that do not have comparison restaurants
    subset <- subset(restaurant, (treat==1&policy=="suffolk")|treat==0)
    subset$entry <- 251
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==251) 
    #matching
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- "suffolk"
    
    #trimming
    while(max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))) {
      tmp <- match[match$weights>=0.05*length(unique(match$address[match$treat==0])),]
      restaurant <- anti_join(restaurant,tmp,by="address")
      subset <- subset(restaurant, (treat==1&policy=="suffolk")|treat==0)
      subset$entry <- 251
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==251) 
      subset <- subset[complete.cases(subset), ]
    
      
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      subset$distance <- subset.match$distance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- "suffolk"
    }
    
    # combine clusters of restaurants
    master <- rbind(master, subset)
    matched <- rbind(matched, match) 
  }, error=function(e){cat(paste0("ERROR : ","suffolk"),conditionMessage(e), "\n")})
  
  plot(summary(subset.match))
  summary(bal)
  
  
  names(matched)
  length(unique(matched$address)) #some comparison restaurants were matched to multiple treated restaurants
  table(matched$match_place)
  master_all <- NULL
  for (i in c("ca","king","ma","mont","or","suffolk")) {
    tmp <- matched %>%
      filter(match_place==i) %>%
      dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
      rename(entry = monthno) %>%
      left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
      arrange(address, tract_num, monthno) %>%
      dplyr::select(c(address:above65,treat,policy)) %>%
      rename(entry = entry.x)
    master_all <- rbind(master_all, tmp)
  }
  #write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", row.names = FALSE)
  #rm(master_all, i,tmp)
  
  ### Creating distribution plots for suffolk ----
  # list of covariates
  covs <- c("meal", "slope_calorie",
              "dollar3", "slope_dollar", "calorie3", "calorie_all3", "slope_calorie_all",
              "slope_count_all", "dollar_all3", "slope_dollar_all", "male",
              "white", "black", "asian", "median_income",
              "hsbelow", "collegeup")
  
  
  covs2 <- c("concept", "ownership")
  
  match <- match %>% mutate(policy=ifelse(policy=="suffolk", "treated","control"))
  
  suffolk_t <- match %>% filter(policy == "treated") 
  
  suffolk_c <- match %>% filter(policy == "control") 
  
  plots_covs <- lapply(covs, function(var_x){
    ggplot(suffolk_t) +
      aes_string(var_x) +
      geom_density(color = "#00abff") +
      geom_density(data = suffolk_c, color = "red")
  })
  
  
  plots_covs2 <- lapply(covs2, function(var_x){
    ggplot(suffolk_t) +
      aes_string(var_x) +
      geom_histogram(aes(y = (..count..)/16), binwidth = 0.5, fill = NA, color = "#00abff") +
      ylab("Density") +
      geom_histogram(data = suffolk_c, aes(y = (..count..)/48), binwidth = 0.5, fill = NA, color = "red")
  })
  
  plots <- append(plots_covs, plots_covs2) 
  
  plot_grid(plotlist = plots) 
  
  # Gets us the legend
  p1 <- ggplot(match, aes(x=concept, color = policy)) + geom_density()
  
  legend <- get_legend(
    # create some space to the left of the legend
    p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
  )
  
  plot_grid(plotlist = plots, legend)
  
  
  
### Matching procedure 2 - just match on calorie and dollar
  formula <- treat ~ calorie3 + calorie_all3 + dollar3 + dollar_all3  
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
  
  
  ### Analysis ----
  
  table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
    setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
    add_column(location=c("total","ca","ma","or","suffolk"))
  rownames(table3) <- c("total","ca","ma","or","suffolk")
  
  #overall
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place=="suffolk"), 
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
  table3[5,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[5,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[5,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[5,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[5,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[5,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
### Matching procedure 2 - match on only calorie and dollar vars ----
  formula <- treat ~ calorie3 + calorie_all3 + dollar3 + dollar_all3 
  ### Drop covariates to see if PS overlap improves ----
  
  master <- NULL
  matched <- NULL
  
  # formula <- treat~concept+ownership+drive+meal+
  #   slope_calorie+count3+slope_count+slope_dollar+
  #   slope_calorie_all+count_all3+slope_count_all+slope_dollar_all+ 
  #   total+male+white+black+asian+hisp+
  #   hsbelow+collegeup+under18+above65+open12+open18+open24+
  #   capital_income
  ## dollar3, dollar_all3, median, calorieall, calorie3
  
  
  tryCatch({#catch groups that do not have comparison restaurants
    subset <- subset(restaurant, (treat==1&policy=="suffolk")|treat==0)
    subset$entry <- 251
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==251) 
    #matching
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- "suffolk"
    
    #trimming
    while(max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))) {
      tmp <- match[match$weights>=0.05*length(unique(match$address[match$treat==0])),]
      restaurant <- anti_join(restaurant,tmp,by="address")
      subset <- subset(restaurant, (treat==1&policy=="suffolk")|treat==0)
      subset$entry <- 251
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==251) 
      subset <- subset[complete.cases(subset), ]
      
      
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      subset$distance <- subset.match$distance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- "suffolk"
    }
    
    # combine clusters of restaurants
    master <- rbind(master, subset)
    matched <- rbind(matched, match) 
  }, error=function(e){cat(paste0("ERROR : ","suffolk"),conditionMessage(e), "\n")})
  
  plot(summary(subset.match))
  summary(bal)
  
  
  names(matched)
  length(unique(matched$address)) #some comparison restaurants were matched to multiple treated restaurants
  table(matched$match_place)
  master_all <- NULL
  for (i in c("ca","king","ma","mont","or","suffolk")) {
    tmp <- matched %>%
      filter(match_place==i) %>%
      dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
      rename(entry = monthno) %>%
      left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
      arrange(address, tract_num, monthno) %>%
      dplyr::select(c(address:above65,treat,policy)) %>%
      rename(entry = entry.x)
    master_all <- rbind(master_all, tmp)
  }
  #write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", row.names = FALSE)
  #rm(master_all, i,tmp)
  
  ### Creating distribution plots for suffolk ----
  # list of covariates
  covs <- c("calorie3", "dollar3", "calorie_all3", "dollar_all3")
  
  match <- match %>% mutate(policy=ifelse(policy=="suffolk", "treated","control"))
  
  suffolk_t <- match %>% filter(policy == "treated") 
  
  suffolk_c <- match %>% filter(policy == "control") 
  
  plots_covs <- lapply(covs, function(var_x){
    ggplot(suffolk_t) +
      aes_string(var_x) +
      geom_density(color = "#00abff") +
      geom_density(data = suffolk_c, color = "red")
  })
  

  plot_grid(plotlist = plots_covs) 
  
  # Gets us the legend
  p1 <- ggplot(match, aes(x=concept, color = policy)) + geom_density()
  
  legend <- get_legend(
    # create some space to the left of the legend
    p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
  )
  
  plot_grid(plotlist = plots_covs, legend)
  
  
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
  
  
  ### Analysis ----

  table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
    setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
    add_column(location=c("total","ca","ma","or","suffolk"))
  rownames(table3) <- c("total","ca","ma","or","suffolk")
  
  #overall
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place=="suffolk"), 
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
  table3[5,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[5,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[5,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[5,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[5,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[5,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
### Matching procedure 3 - match on all 8 calorie and dollar vars ----
  formula <- treat ~ calorie3 + calorie_all3 + dollar3 + dollar_all3 +
                     slope_calorie + slope_dollar + slope_calorie_all + slope_dollar_all
  
  ### Drop covariates to see if PS overlap improves ----
  
  master <- NULL
  matched <- NULL
  
  # formula <- treat~concept+ownership+drive+meal+
  #   slope_calorie+count3+slope_count+slope_dollar+
  #   slope_calorie_all+count_all3+slope_count_all+slope_dollar_all+ 
  #   total+male+white+black+asian+hisp+
  #   hsbelow+collegeup+under18+above65+open12+open18+open24+
  #   capital_income
  ## dollar3, dollar_all3, median, calorieall, calorie3
  
  
  tryCatch({#catch groups that do not have comparison restaurants
    subset <- subset(restaurant, (treat==1&policy=="suffolk")|treat==0)
    subset$entry <- 251
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==251) 
    #matching
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- "suffolk"
    
    #trimming
    while(max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))) {
      tmp <- match[match$weights>=0.05*length(unique(match$address[match$treat==0])),]
      restaurant <- anti_join(restaurant,tmp,by="address")
      subset <- subset(restaurant, (treat==1&policy=="suffolk")|treat==0)
      subset$entry <- 251
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==251) 
      subset <- subset[complete.cases(subset), ]
      
      
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      subset$distance <- subset.match$distance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- "suffolk"
    }
    
    # combine clusters of restaurants
    master <- rbind(master, subset)
    matched <- rbind(matched, match) 
  }, error=function(e){cat(paste0("ERROR : ","suffolk"),conditionMessage(e), "\n")})
  
  plot(summary(subset.match))
  summary(bal)
  
  
  names(matched)
  length(unique(matched$address)) #some comparison restaurants were matched to multiple treated restaurants
  table(matched$match_place)
  master_all <- NULL
  for (i in c("ca","king","ma","mont","or","suffolk")) {
    tmp <- matched %>%
      filter(match_place==i) %>%
      dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
      rename(entry = monthno) %>%
      left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
      arrange(address, tract_num, monthno) %>%
      dplyr::select(c(address:above65,treat,policy)) %>%
      rename(entry = entry.x)
    master_all <- rbind(master_all, tmp)
  }
  #write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", row.names = FALSE)
  #rm(master_all, i,tmp)
  
  ### Creating distribution plots for suffolk ----
  # list of covariates
  covs <- c("calorie3", "dollar3", "calorie_all3", "dollar_all3", "slope_calorie", "slope_dollar", "slope_dollar_all", "slope_calorie_all")
  
  match <- match %>% mutate(policy=ifelse(policy=="suffolk", "treated","control"))
  
  suffolk_t <- match %>% filter(policy == "treated") 
  
  suffolk_c <- match %>% filter(policy == "control") 
  
  plots_covs <- lapply(covs, function(var_x){
    ggplot(suffolk_t) +
      aes_string(var_x) +
      geom_density(color = "#00abff") +
      geom_density(data = suffolk_c, color = "red")
  })
  
  
  plot_grid(plotlist = plots_covs) 
  
  # Gets us the legend
  p1 <- ggplot(match, aes(x=concept, color = policy)) + geom_density()
  
  legend <- get_legend(
    # create some space to the left of the legend
    p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
  )
  
  plot_grid(plotlist = plots_covs, legend)
  
  
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
  
  
  ### Analysis ----
  
  table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
    setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
    add_column(location=c("total","ca","ma","or","suffolk"))
  rownames(table3) <- c("total","ca","ma","or","suffolk")
  
  #overall
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place=="suffolk"), 
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
  table3[5,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[5,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[5,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[5,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[5,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[5,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
### Merging with original data ----
  suffolk <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct-suffolk-weights.csv", stringsAsFactors = FALSE)
  matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct.csv", stringsAsFactors = FALSE)
  
  # Drop suffolk in original
  matched_drop <- matched %>%
                  filter(match_place != "suffolk")
  
  matched <- rbind(matched_drop, suffolk)
  
### Fig 2
  ### matched data, preparing data ----
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
  
  
  ### fig 2, diff in diff, by location ----
  trend <- NULL
  presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
  for (p in c("ca","ma","or","suffolk")) {
    mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                      data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place==p), 
                      index = "id_match", weights = weights, model = "within")
    tidy_mod.factor <- tidy(mod.factor)
    tidy_mod.factor <- tidy_mod.factor %>%
      dplyr::select(term,estimate,p.value) %>%
      rename(month=term,coef.month=estimate,p=p.value) %>%
      filter(!grepl("as.factor|calorie", month)) %>%
      mutate(group=c(rep(0,55),rep(1,55))) %>%
      add_row(month="-3",coef.month=0,group=0) %>%
      add_row(month="-3",coef.month=0,group=1) %>%
      mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
      mutate(month=ifelse(month>0,month+1,month)) %>%
      arrange(group,month) %>%
      mutate(diff = ifelse(group==1,coef.month,NA)) %>%
      mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56]))
    tidy_mod.factor$loc <- p
    tmp1 <- tidy_mod.factor %>% 
      filter(month>=-30&month<0&!is.na(diff)) %>% dplyr::select(month, diff) %>%
      mutate(month = -month) %>% arrange(month) %>% mutate(pre_mean = sum(diff[1:6])/6)
    tmp2 <- tidy_mod.factor %>% 
      filter(month>=1&month<=30&!is.na(diff)) %>% dplyr::select(month, diff) %>%
      arrange(month) %>% rename(post_mean = diff)
    tmp1 <- merge(tmp1,tmp2,by="month") %>% group_by(month) %>% arrange(month) %>%
      mutate(mean = post_mean - pre_mean) %>% dplyr::select(-diff)
    tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
    for (i in 4:29) {
      tmp$p[i-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]
    }
    tmp1 <- cbind(tmp1, tmp)
    tmp1$loc <- p
    trend <- rbind(trend,tmp1)
  }
  
  #diff in diff, fig 2
  summary(trend$mean) #[-105,33]
  ggplot() + 
    geom_line(data=trend, aes(x=month, y=mean, color=loc)) + 
    geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color=loc)) +
    ggplot2::annotate(geom="label", x=6, y=13, label="   p<0.05", size=3) + 
    geom_point(aes(x=5.35,y=13),color="black",size=1) +
    geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
    coord_cartesian(expand = FALSE, clip = "off") + 
    scale_y_continuous(limits=c(-125,50),breaks=seq(-125,50,25)) +
    scale_x_continuous(breaks=seq(3,30,1)) +
    labs(title="", x="Month", y="Difference-in-difference estimate", caption="") + 
    scale_color_manual(name="Location", labels=c("California","Massachusetts","Oregon","Suffolk County, New York"),
                       values=c("hotpink","olivedrab3","#13B0E4","orange")) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
          plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
          axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
          axis.title.y = element_text(size = 12),
          legend.text=element_text(size=10), 
          plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
          panel.grid.minor = element_blank())
  #ggsave("manuscript/figures/fig2-diff-in-diff-by-loc.jpeg", dpi="retina")
  
  #remove suffolk county
  ggplot() + 
    geom_line(data=trend%>%filter(loc!="suffolk"), aes(x=month, y=mean, color=loc)) + 
    geom_point(data=trend%>%filter(p<0.05&loc!="suffolk"), aes(x=month, y=mean, color=loc)) +
    ggplot2::annotate(geom="label", x=6, y=13, label="   p<0.05", size=3) + 
    geom_point(aes(x=5.35,y=13),color="black",size=1) +
    geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
    coord_cartesian(expand = FALSE, clip = "off") + 
    scale_y_continuous(limits=c(-125,50),breaks=seq(-125,50,25)) +
    scale_x_continuous(breaks=seq(3,30,1)) +
    labs(title="", x="Month", y="Difference-in-difference estimate", caption="") + 
    scale_color_manual(name="Location", labels=c("California","Massachusetts","Oregon"),
                       values=c("hotpink","olivedrab3","#13B0E4")) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
          plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
          axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
          axis.title.y = element_text(size = 12),
          legend.text=element_text(size=10), 
          plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
          panel.grid.minor = element_blank())
  #ggsave("manuscript/figures/fig2-diff-in-diff-by-loc-wo-suffolk.jpeg", dpi="retina")
  rm(tmp1,tmp2,tmp,mod.factor,i,p,tidy_mod.factor,trend,presum)
  
  summary(trend$mean) #[-105,33]
  ggplot() + 
    geom_line(data=trend, aes(x=month, y=mean, color=loc)) + 
    geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color=loc)) +
    ggplot2::annotate(geom="label", x=6, y=13, label="   p<0.05", size=3) + 
    geom_point(aes(x=5.35,y=13),color="black",size=1) +
    geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
    coord_cartesian(expand = FALSE, clip = "off") + 
    scale_y_continuous(limits=c(-125,50),breaks=seq(-125,50,25)) +
    scale_x_continuous(breaks=seq(3,30,1)) +
    labs(title="", x="Month", y="Difference-in-difference estimate", caption="") + 
    scale_color_manual(name="Location", labels=c("California","Massachusetts","Oregon","Suffolk County, New York"),
                       values=c("hotpink","olivedrab3","#13B0E4","orange")) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
          plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
          axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
          axis.title.y = element_text(size = 12),
          legend.text=element_text(size=10), 
          plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
          panel.grid.minor = element_blank())
  #ggsave("manuscript/figures/fig2-diff-in-diff-by-loc.jpeg", dpi="retina")
  
  #remove suffolk county
  ggplot() + 
    geom_line(data=trend%>%filter(loc!="suffolk"), aes(x=month, y=mean, color=loc)) + 
    geom_point(data=trend%>%filter(p<0.05&loc!="suffolk"), aes(x=month, y=mean, color=loc)) +
    ggplot2::annotate(geom="label", x=6, y=13, label="   p<0.05", size=3) + 
    geom_point(aes(x=5.35,y=13),color="black",size=1) +
    geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
    coord_cartesian(expand = FALSE, clip = "off") + 
    scale_y_continuous(limits=c(-125,50),breaks=seq(-125,50,25)) +
    scale_x_continuous(breaks=seq(3,30,1)) +
    labs(title="", x="Month", y="Difference-in-difference estimate", caption="") + 
    scale_color_manual(name="Location", labels=c("California","Massachusetts","Oregon"),
                       values=c("hotpink","olivedrab3","#13B0E4")) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
          plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
          axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
          axis.title.y = element_text(size = 12),
          legend.text=element_text(size=10), 
          plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
          panel.grid.minor = element_blank())
  #ggsave("manuscript/figures/fig2-diff-in-diff-by-loc-wo-suffolk.jpeg", dpi="retina")
### Look at density plots for covariate, suffolk treat = 1 vs all potential suffolk matches.
  suffolk_t <- subset(subset, (treat==1&policy=="suffolk"))
  suffolk_pc <- subset(subset, (treat==0))
  
  covs <- c("dollar3", "dollar_all3", "calorie3", "calorie_all3", "median_income",
            "meal", "drive", "slope_calorie", "count3", "slope_count", "slope_dollar",
            "slope_calorie_all", "count_all3", "slope_count_all", "slope_dollar_all", "total", "male",
            "white", "black", "asian", "hisp", "capital_income", "hsbelow", "collegeup", "under18", "above65")
  
  
  covs2 <- c("concept", "ownership", "open12", "open18", "open24")
  
  
  ### Creating distribution plots for suffolk ----
  plots_covs <- lapply(covs, function(var_x){
    ggplot(suffolk_t) +
      aes_string(var_x) +
      geom_density(color = "#00abff") +
      geom_density(data = suffolk_pc, color = "red")
  })
  
  
  plots_covs2 <- lapply(covs2, function(var_x){
    ggplot(suffolk_t) +
      aes_string(var_x) +
      geom_histogram(aes(y = (..count..)/16), binwidth = 0.5, fill = NA, color = "#00abff") +
      ylab("Density") +
      geom_histogram(data = suffolk_pc, aes(y = (..count..)/48), binwidth = 0.5, fill = NA, color = "red")
  })
  
  plots <- append(plots_covs, plots_covs2) 
  
  plot_grid(plotlist = plots) 
  
  # Gets us the legend
  p1 <- ggplot(match, aes(x=concept, color = policy)) + geom_density()
  
  legend <- get_legend(
    # create some space to the left of the legend
    p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
  )
  
  title <- ggfdraw() + 
    labs(title = "Density plots for Suffolk county covariates (n = 16 treated; 1839 potential matches)")
  
  plot_grid(plotlist = plots, title, legend, nrow = 8)
