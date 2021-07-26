rm(list=ls(all=T))

setwd(".")

library(readxl)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(scales)
library(investr)
library(drc)
library(growthcurver)
library(zoo)
library(pROC)

filenames <- list.files()
filenames <- filenames[grepl("ICU\\ Projection\\ ",filenames)]
filenames <- filenames[which(file.info(filenames)$mtime == max(file.info(filenames)$mtime))]

covid_line_list <- data.frame(read_excel(filenames, sheet = "Patient Line list"))
covid_inflights_line_list <- data.frame(read_excel(filenames, sheet = "DB - Inflight"))

setwd("D:/Working_Folder/covid19/tiff_files")

#moh_covid_proj <- data.frame(read_excel("moh_proj.xlsx"))

names(covid_line_list) <- covid_line_list[2,]
names(covid_line_list) <- gsub("[[:punct:]]","",names(covid_line_list))
names(covid_line_list) <- gsub("\\ ","",names(covid_line_list))
names(covid_line_list) <- tolower(names(covid_line_list))

names(covid_line_list) <- gsub("admdatemin","adm_datetime",names(covid_line_list))
names(covid_line_list) <- gsub("dischargedatemax","discharge_datetime",names(covid_line_list))

covid_line_list <- plyr:: rename(covid_line_list,c(#"labconfirmedtest" = "lab_confirmed_datetime",
                                                   "ageintextbasedonadmissiondate" = "age_text",
                                                   "agebasedonadmissiondate" = "age_numeric",
                                                   # "currentward" = "current_ward",
                                                   # "currentbed" = "current_bed",
                                                   "numberoficuadmission" = "no_icuhdu_adm"))

names(covid_line_list)[grepl("everto",names(covid_line_list)) & grepl("icuhdu",names(covid_line_list))] <- "icu"
names(covid_line_list)[grepl("totallosin",names(covid_line_list)) & grepl("icuhdu",names(covid_line_list))] <- "icu_los_days"
names(covid_line_list)[grepl("earliestdateto",names(covid_line_list)) & grepl("icuhdu",names(covid_line_list))] <- "first_icuhdu_date"
names(covid_line_list)[grepl("latestdateoutof",names(covid_line_list)) & grepl("icuhd",names(covid_line_list))] <- "last_icuhdu_date"

covid_line_list <- covid_line_list[3:nrow(covid_line_list),]

covid_line_list[,grepl("date|dob|day",names(covid_line_list))] <- lapply(covid_line_list[,grepl("date|dob|day",names(covid_line_list))],
                                                                         function(x) as.numeric(x))

covid_line_list[,grepl("date|dob",names(covid_line_list))] <- lapply(covid_line_list[,grepl("date|dob",names(covid_line_list))],
                                                                     function(x) as.POSIXct(x*24*60*60,origin = "1899-12-30",tz="UTC"))

covid_line_list[,grepl("adm\\_datetime|discharge\\_datetime|lab\\_confirmed\\_date|dob|icuhdu\\_date",names(covid_line_list))] <-
  lapply(covid_line_list[,grepl("adm\\_datetime|discharge\\_datetime|lab\\_confirmed\\_date|dob|icuhdu\\_date",names(covid_line_list))],
         function(x) as.Date(x))

names(covid_line_list) <- gsub("datetime","date",names(covid_line_list))

covid_line_list$age_yrs <- round(with(covid_line_list,time_length(interval(dob,adm_date),unit = "years")),0)

covid_line_list$age_gp <- with(covid_line_list,
                               ifelse(age_yrs<=44,"alesse_44",
                                      ifelse(age_yrs >= 45 & age_yrs <= 64,"bbtwi_4564", "cmoree_65")))
covid_line_list$age_gp <- factor(covid_line_list$age_gp,levels = c("alesse_44","bbtwi_4564","cmoree_65"))

covid_line_list$days_to_icu <- round(with(covid_line_list,time_length(interval(adm_date,first_icuhdu_date),unit = "days")))

# Subset patients with an admission date

latest_adm_date <-  (Sys.Date()+(-6:0))[which(weekdays(Sys.Date()+ (-6:0)) == "Wednesday")]

covid_line_list_dataset <- subset(covid_line_list,!is.na(adm_date) & adm_date <= latest_adm_date)

covid_admissions <- data.frame(covid_line_list_dataset %>% group_by(adm_date,age_gp) %>%
                                 summarize(no_admissions = length(age_gp)))

covid_admissions_wide <- spread(covid_admissions,age_gp,"no_admissions")
covid_admissions_wide <- covid_admissions_wide[order(covid_admissions_wide$adm_date),]

# covid_admissions_varnames <- names(covid_admissions_wide)

covid_admissions_wide <- data.frame(covid_admissions_wide %>% complete(adm_date = seq.Date(min(adm_date),latest_adm_date,by = "day")))
# names(covid_admissions_wide) <- covid_admissions_varnames

covid_admissions <- covid_admissions_wide

covid_admissions$date_order <- NA
covid_admissions$date_order[which(covid_admissions$adm_date >= as.Date("2020-06-19"))] <- # Start of Phase 2
  seq(1:(nrow(covid_admissions)-which(covid_admissions$adm_date == "2020-06-18")))

covid_admissions[,2:(ncol(covid_admissions)-1)] <- lapply(covid_admissions[,2:(ncol(covid_admissions)-1)], function(x) ifelse(is.na(x),0,x))
covid_admissions <- covid_admissions[order(covid_admissions$adm_date),]

##############################################

## (Starting point to run models again before combining)

covid_admissions_long <- gather(covid_admissions,"age_gp","no_admissions",2:(ncol(covid_admissions)-1))
covid_admissions_long <- covid_admissions_long[order(covid_admissions_long$adm_date,covid_admissions_long$age_gp),]

## (Weighting based on admissions)

covid_admissions_long <- data.frame(covid_admissions_long %>% group_by(age_gp) %>%
                                      mutate(weight = (date_order - mean(unique(date_order),na.rm = T))/sd(unique(date_order),na.rm = T)))

covid_admissions_long <- data.frame(covid_admissions_long %>% group_by(age_gp) %>%
                                      mutate(weight = (max(weight,na.rm = T) + weight)/max(weight,na.rm = T)))

covid_admissions_long$no_admissions_wt <- with(covid_admissions_long,
                                               weight*no_admissions)

covid_admissions_long_split <- split(covid_admissions_long,f = covid_admissions_long[,c("age_gp")],drop = T)

for (i in 1:length(covid_admissions_long_split)) {
  covid_admissions_long_split[[i]]$cum_adm <- NA
  covid_admissions_long_split[[i]]$cum_adm_wt <- NA
}

######################### (Choose 1)
# Modeling for Cumulative Admissions
for (i in 1:length(covid_admissions_long_split)) {
  for (j in (which(!is.na(covid_admissions_long_split[[i]]$weight))[1]):nrow(covid_admissions_long_split[[i]])) {
    covid_admissions_long_split[[i]]$cum_adm[j] <- sum(covid_admissions_long_split[[i]]$no_admissions[(which(!is.na(covid_admissions_long_split[[i]]$weight))[1]):j],na.rm = T)
    covid_admissions_long_split[[i]]$cum_adm_wt[j] <- sum(covid_admissions_long_split[[i]]$no_admissions_wt[(which(!is.na(covid_admissions_long_split[[i]]$weight))[1]):j],na.rm=T)
  }
}

# Modeling for admissions
for (i in 1:length(covid_admissions_long_split)) {
  for (j in 1:nrow(covid_admissions_long_split[[i]])) {
    covid_admissions_long_split[[i]]$cum_adm[j] <- covid_admissions_long_split[[i]]$no_admissions[j]
    covid_admissions_long_split[[i]]$cum_adm_wt[j] <- covid_admissions_long_split[[i]]$no_admissions_wt[j]
  }
}
#####################################

covid_admissions_long <- dplyr:: bind_rows(covid_admissions_long_split)
covid_admissions_long <- covid_admissions_long[order(covid_admissions_long$adm_date,covid_admissions_long$age_gp),]

############
cutoff_date <- latest_adm_date
############

##### Summary Statistics ########

# Overall Days to ICU
with(subset(covid_line_list_dataset,adm_date <= cutoff_date & icu == "Yes"), 
     summary(days_to_icu,na.rm=T))

# Days to ICU by age group
with(subset(covid_line_list_dataset,adm_date <= cutoff_date & icu == "Yes"), 
     tapply(days_to_icu,age_gp,summary))
with(subset(covid_line_list_dataset,adm_date <= cutoff_date & icu == "Yes"), 
     round(tapply(days_to_icu,age_gp,sd)),1)

# Total Number of patients having an ICU stay
with(subset(covid_line_list_dataset,adm_date <= cutoff_date), 
     table(icu))

# Total Number of patients having an ICU stay by age_dorm group
with(subset(covid_line_list_dataset,adm_date <= cutoff_date), 
     table(age_gp,icu))

# Overall LOS in ICU
with(subset(covid_line_list_dataset,adm_date <= cutoff_date & icu == "Yes"),
     summary(icu_los_days))
with(subset(covid_line_list_dataset,adm_date <= cutoff_date & icu == "Yes"),
     sd(icu_los_days))

# LOS in ICU by age group
with(subset(covid_line_list_dataset,adm_date <= cutoff_date & icu == "Yes"),
     tapply(icu_los_days,age_gp,summary))
with(subset(covid_line_list_dataset,adm_date <= cutoff_date & icu == "Yes"),
     tapply(icu_los_days,age_gp,sd))

# Overall Proportion having ICU Admissions
with(subset(covid_line_list_dataset,adm_date <= cutoff_date & (is.na(last_icuhdu_date)|last_icuhdu_date != cutoff_date)),
     round(prop.table(table(icu)),3))

# Proportion having ICU Admissions by age group
with(subset(covid_line_list_dataset,adm_date <= cutoff_date & (is.na(last_icuhdu_date)|last_icuhdu_date != cutoff_date)),
     round(prop.table(table(age_gp,icu),1),4))

#################################

mean_los_alesse_44 <- mean(subset(covid_line_list_dataset,
                                 grepl("lesse\\_44",age_gp) & adm_date <= cutoff_date & icu == "Yes")$icu_los_days)

mean_los_bbtwi_4564 <- mean(subset(covid_line_list_dataset,
                                 grepl("btwi\\_4564",age_gp) & adm_date <= cutoff_date & icu == "Yes")$icu_los_days)

mean_los_cmoree_65 <- mean(subset(covid_line_list_dataset,
                                 grepl("cmoree_65",age_gp) & adm_date <= cutoff_date & icu == "Yes")$icu_los_days)

icu_param1 <- data.frame(covid_line_list_dataset %>% filter(adm_date <= cutoff_date)
                         %>% group_by(age_gp,icu) %>% summarize(time2icu = mean(days_to_icu)))

icu_param2 <- data.frame(covid_line_list_dataset %>% filter(adm_date <= cutoff_date & (is.na(last_icuhdu_date)|last_icuhdu_date != cutoff_date))
                         %>% group_by(age_gp,icu) %>% summarize(n = n())
                         %>% group_by(age_gp) %>% mutate(gp_total = sum(n))
                         %>% mutate(prop = n/gp_total))

icu_param <- merge(icu_param1,icu_param2,all.x=T)

prop_icu_lesse44 <- icu_param$prop[which(icu_param$age_gp == "alesse_44" & icu_param$icu == "Yes")]
prop_icu_btwi4564 <- icu_param$prop[which(icu_param$age_gp == "bbtwi_4564" & icu_param$icu == "Yes")]
prop_icu_moree65 <- icu_param$prop[which(icu_param$age_gp == "cmoree_65" & icu_param$icu == "Yes")]

est_time2icu_lesse44 <- icu_param$time2icu[which(icu_param$age_gp == "alesse_44" & icu_param$icu == "Yes")]
est_time2icu_btwi4564 <- icu_param$time2icu[which(icu_param$age_gp == "bbtwi_4564" & icu_param$icu == "Yes")]
est_time2icu_moree65 <- icu_param$time2icu[which(icu_param$age_gp == "cmoree_65" & icu_param$icu == "Yes")]

## Parameter estimates
param_estimate <- list(mean_days2icu_age1 = round(est_time2icu_lesse44),
                       mean_days2icu_age2 = round(est_time2icu_btwi4564),
                       mean_days2icu_age3 = round(est_time2icu_moree65),
                       
                       mean_losicu_age1 = round(mean_los_alesse_44),
                       mean_losicu_age2 = round(mean_los_bbtwi_4564),
                       mean_losicu_age3 = round(mean_los_cmoree_65),
                       
                       prop_icu_age1 = prop_icu_lesse44,
                       prop_icu_age2 = prop_icu_btwi4564,
                       prop_icu_age3 = prop_icu_moree65)

last_proj_date <- ceiling_date(cutoff_date,unit = "month") %m+% period("2 month") - 1

############################################################################################################################

## Cumulative Admissions

######## Visualization #########
# ggplot(data=subset(covid_admissions_long,adm_date <= cutoff_date & adm_date >= as.Date("2020-06-19")),aes(x=adm_date,y=cum_adm,colour=age_gp))+
#   theme(axis.text.x = element_text(angle = 90,vjust = 0.5))+
#   geom_point()+
#   geom_smooth()+
#   scale_x_date(date_labels = "%Y-%m-%d", breaks = "5 days")+
#   ylab("Cumulative Admissions")+
#   xlab("Admission Date")
##########################################

# (Logistic Model)

for(i in 1:length(covid_admissions_long_split)){
  log_covid_fit <- paste("log_covid_fit",names(covid_admissions_long_split)[i],sep="_")
  assign(log_covid_fit,
         SummarizeGrowth(subset(covid_admissions_long_split[[i]], adm_date <= cutoff_date & !is.na(date_order))$date_order,
                                subset(covid_admissions_long_split[[i]], adm_date <= cutoff_date & !is.na(date_order))$cum_adm_wt))
}

log_param_fit <- mget(ls(pattern = "log_covid_fit")[ls(pattern = "log_covid_fit") != "log_covid_fit"])

rm(list = ls(pattern = "log_covid_fit"))

log_covid_adm_fit <- covid_admissions_long_split

for (i in 1:length(log_covid_adm_fit)){
  log_covid_adm_fit[[i]] <- data.frame(log_covid_adm_fit[[i]] %>% complete(adm_date = seq.Date(min(adm_date),last_proj_date,by = "day")))
}

for (i in 1:length(log_covid_adm_fit)){
  for (j in (which(log_covid_adm_fit[[i]]$date_order == max(log_covid_adm_fit[[i]]$date_order,na.rm=T))+1):nrow(log_covid_adm_fit[[i]])){
    log_covid_adm_fit[[i]]$date_order[j] <- log_covid_adm_fit[[i]]$date_order[j-1]+1
    log_covid_adm_fit[[i]]$age_gp[j] <- log_covid_adm_fit[[i]]$age_gp[j-1]
  }
}

for (i in 1:length(log_covid_adm_fit)){
  for (j in 1:nrow(log_covid_adm_fit[[i]])){
    log_covid_adm_fit[[i]]$log_cumfit[j] <- with(log_param_fit[which(grepl(names(log_covid_adm_fit)[i],names(log_param_fit)))][[1]][[1]],
                                                 k / (1 + ((k - n0) / n0) * exp(-r * log_covid_adm_fit[[i]]$date_order[j])))
    log_covid_adm_fit[[i]]$log_fit[j] <- NA
  }}

for (i in 1:length(log_covid_adm_fit)){
  for (j in 2:nrow(log_covid_adm_fit[[i]])){
    log_covid_adm_fit[[i]]$log_fit[j] <- log_covid_adm_fit[[i]]$log_cumfit[j] - log_covid_adm_fit[[i]]$log_cumfit[j-1]
  }}

for (i in 1:length(log_covid_adm_fit)){
  log_covid_adm_fit[[i]]$log_fit[max(which(is.na(log_covid_adm_fit[[i]]$log_fit)))] <- log_covid_adm_fit[[i]]$log_fit[max(which(is.na(log_covid_adm_fit[[i]]$log_fit)))+1]
}

for (i in 1:length(log_covid_adm_fit)){
  for (j in 1:nrow(log_covid_adm_fit[[i]])){
    log_covid_adm_fit[[i]]$model[j] <- "cumlog"
    log_covid_adm_fit[[i]]$estimate[j] <- ifelse(!is.na(log_covid_adm_fit[[i]]$weight[j]),"fit","predict")
  }}

for (i in 1:length(log_covid_adm_fit)){
  log_covid_adm_fit[[i]]$estimate[as.numeric(rownames(log_covid_adm_fit[[i]])) < min(which(!is.na(log_covid_adm_fit[[i]]$weight)))] <- NA
  log_covid_adm_fit[[i]] <- plyr:: rename(log_covid_adm_fit[[i]],c("log_cumfit" = "model_cumadm",
                                                                   "log_fit" = "model_adm"))
}

# for (i in 1:length(log_covid_adm_fit)) {
#   log_diagnostics <- paste("log_diagnostics",names(covid_admissions_long_split)[i],sep = "_")
#   assign(log_diagnostics,
#          nls(cum_adm_wt ~   k / (1 + ((k - n0) / n0) * exp(-r * date_order)),
#                          data = subset(log_covid_adm_fit[[i]],!is.na(weight)),
#                          start = with(log_param_fit[which(grepl(names(log_covid_adm_fit)[i],names(log_param_fit)))][[1]][[1]],
#                                       list(k = k, n0 = n0, r = r))))
# }

# log_diagnostics <- mget(ls(pattern = "log_diagnostics")[ls(pattern = "log_diagnostics") != "log_diagnostics"])
# 
# for (i in 1:length(log_diagnostics)){
#   print(names(log_diagnostics)[i]) 
#   print(paste("AIC = ",round(AIC(log_diagnostics[[i]]),1), sep = ""))
#   print(paste("BIC = ",round(BIC(log_diagnostics[[i]]),1), sep = ""))
# }

## (Exponential Model)

for(i in 1:length(covid_admissions_long_split)){
  exp_covid_fit <- paste("exp_covid_fit",names(covid_admissions_long_split)[i],sep="_")
  assign(exp_covid_fit,
         lm(log(cum_adm_wt+0.001)~date_order,data=subset(covid_admissions_long_split[[i]], adm_date <= cutoff_date & !is.na(date_order))))
}

exp_param_fit <- mget(ls(pattern = "exp_covid_fit")[ls(pattern = "exp_covid_fit") != "exp_covid_fit"])

rm(list = ls(pattern = "exp_covid_fit"))

exp_covid_adm_fit <- covid_admissions_long_split

for (i in 1:length(exp_covid_adm_fit)){
  exp_covid_adm_fit[[i]] <- data.frame(exp_covid_adm_fit[[i]] %>% complete(adm_date = seq.Date(min(adm_date),last_proj_date,by = "day")))
}

for (i in 1:length(exp_covid_adm_fit)){
  for (j in (which(exp_covid_adm_fit[[i]]$date_order == max(exp_covid_adm_fit[[i]]$date_order,na.rm=T))+1):nrow(exp_covid_adm_fit[[i]])){
    exp_covid_adm_fit[[i]]$date_order[j] <- exp_covid_adm_fit[[i]]$date_order[j-1]+1
    exp_covid_adm_fit[[i]]$age_gp[j] <- exp_covid_adm_fit[[i]]$age_gp[j-1]
  }
}

for (i in 1:length(exp_covid_adm_fit)){
  for (j in 1:nrow(exp_covid_adm_fit[[i]])){
    exp_covid_adm_fit[[i]]$exp_cumfit[j] <- exp(exp_param_fit[which(grepl(names(exp_covid_adm_fit)[i],names(exp_param_fit)))][[1]][[1]][1] +
                                                  exp_param_fit[which(grepl(names(exp_covid_adm_fit)[i],names(exp_param_fit)))][[1]][[1]][2] * exp_covid_adm_fit[[i]]$date_order[j])
    exp_covid_adm_fit[[i]]$exp_fit[j] <- NA
  }}

for (i in 1:length(exp_covid_adm_fit)){
  for (j in 1:nrow(exp_covid_adm_fit[[i]])){
    exp_covid_adm_fit[[i]]$exp_fit[j] <- ifelse(exp_covid_adm_fit[[i]]$exp_cumfit[j]<0,0,exp_covid_adm_fit[[i]]$exp_cumfit[j])
  }}

for (i in 1:length(exp_covid_adm_fit)){
  for (j in 1:nrow(exp_covid_adm_fit[[i]])){
    exp_covid_adm_fit[[i]]$model[j] <- "cumexp"
    exp_covid_adm_fit[[i]]$estimate[j] <- ifelse(!is.na(exp_covid_adm_fit[[i]]$weight[j]),"fit","predict")
  }}

for (i in 1:length(exp_covid_adm_fit)){
  exp_covid_adm_fit[[i]]$estimate[as.numeric(rownames(exp_covid_adm_fit[[i]])) < min(which(!is.na(exp_covid_adm_fit[[i]]$weight)))] <- NA
  exp_covid_adm_fit[[i]] <- plyr:: rename(exp_covid_adm_fit[[i]],c("exp_cumfit" = "model_cumadm",
                                                                   "exp_fit" = "model_adm"))
}

# for (i in 1:length(exp_param_fit)){
#   print(names(exp_param_fit)[i])
#   print(paste("AIC = ",round(AIC(exp_param_fit[[i]]))))
#   print(paste("BIC = ",round(BIC(exp_param_fit[[i]]))))
# }

## (Linear Model)

for(i in 1:length(covid_admissions_long_split)){
  lm_covid_fit <- paste("lm_covid_fit",names(covid_admissions_long_split)[i],sep="_")
  assign(lm_covid_fit,
         lm(cum_adm_wt~date_order,data=subset(covid_admissions_long_split[[i]], adm_date <= cutoff_date & !is.na(date_order))))
}

lm_param_fit <- mget(ls(pattern = "lm_covid_fit")[ls(pattern = "lm_covid_fit") != "lm_covid_fit"])

rm(list = ls(pattern = "lm_covid_fit"))

lm_covid_adm_fit <- covid_admissions_long_split

for (i in 1:length(lm_covid_adm_fit)){
  lm_covid_adm_fit[[i]] <- data.frame(lm_covid_adm_fit[[i]] %>% complete(adm_date = seq.Date(min(adm_date),last_proj_date,by = "day")))
}

for (i in 1:length(lm_covid_adm_fit)){
  for (j in (which(lm_covid_adm_fit[[i]]$date_order == max(lm_covid_adm_fit[[i]]$date_order,na.rm=T))+1):nrow(lm_covid_adm_fit[[i]])){
    lm_covid_adm_fit[[i]]$date_order[j] <- lm_covid_adm_fit[[i]]$date_order[j-1]+1
    lm_covid_adm_fit[[i]]$age_gp[j] <- lm_covid_adm_fit[[i]]$age_gp[j-1]
  }
}

for (i in 1:length(lm_covid_adm_fit)){
  for (j in 1:nrow(lm_covid_adm_fit[[i]])){
    lm_covid_adm_fit[[i]]$lm_cumfit[j] <- lm_param_fit[which(grepl(names(lm_covid_adm_fit)[i],names(lm_param_fit)))][[1]][[1]][1] + lm_param_fit[which(grepl(names(lm_covid_adm_fit)[i],names(lm_param_fit)))][[1]][[1]][2] * lm_covid_adm_fit[[i]]$date_order[j]
    lm_covid_adm_fit[[i]]$lm_fit[j] <- NA
  }}

for (i in 1:length(lm_covid_adm_fit)){
  for (j in 1:nrow(lm_covid_adm_fit[[i]])){
    lm_covid_adm_fit[[i]]$lm_fit[j] <- ifelse(lm_covid_adm_fit[[i]]$lm_cumfit[j]<0,0,lm_covid_adm_fit[[i]]$lm_cumfit[j])
  }}

for (i in 1:length(lm_covid_adm_fit)){
  for (j in 1:nrow(lm_covid_adm_fit[[i]])){
    lm_covid_adm_fit[[i]]$model[j] <- "cumlinear"
    lm_covid_adm_fit[[i]]$estimate[j] <- ifelse(!is.na(lm_covid_adm_fit[[i]]$weight[j]),"fit","predict")
  }}

for (i in 1:length(lm_covid_adm_fit)){
  lm_covid_adm_fit[[i]]$estimate[as.numeric(rownames(lm_covid_adm_fit[[i]])) < min(which(!is.na(lm_covid_adm_fit[[i]]$weight)))] <- NA
  lm_covid_adm_fit[[i]] <- plyr:: rename(lm_covid_adm_fit[[i]],c("lm_cumfit" = "model_cumadm",
                                                                 "lm_fit" = "model_adm"))
}

# for (i in 1:length(lm_param_fit)){
#   print(names(lm_param_fit)[i])
#   print(paste("AIC = ",round(AIC(lm_param_fit[[i]]))))
#   print(paste("BIC = ",round(BIC(lm_param_fit[[i]]))))
# }

## (Quadratic Model)

for(i in 1:length(covid_admissions_long_split)){
  quad_covid_fit <- paste("quad_covid_fit",names(covid_admissions_long_split)[i],sep="_")
  assign(quad_covid_fit,
         lm(cum_adm_wt~date_order+I(date_order^2),data=subset(covid_admissions_long_split[[i]], adm_date <= cutoff_date & !is.na(date_order))))
}

quad_param_fit <- mget(ls(pattern = "quad_covid_fit")[ls(pattern = "quad_covid_fit") != "quad_covid_fit"])

rm(list = ls(pattern = "quad_covid_fit"))

quad_covid_adm_fit <- covid_admissions_long_split

for (i in 1:length(quad_covid_adm_fit)){
  quad_covid_adm_fit[[i]] <- data.frame(quad_covid_adm_fit[[i]] %>% complete(adm_date = seq.Date(min(adm_date),last_proj_date,by = "day")))
}

for (i in 1:length(quad_covid_adm_fit)){
  for (j in (which(quad_covid_adm_fit[[i]]$date_order == max(quad_covid_adm_fit[[i]]$date_order,na.rm=T))+1):nrow(quad_covid_adm_fit[[i]])){
    quad_covid_adm_fit[[i]]$date_order[j] <- quad_covid_adm_fit[[i]]$date_order[j-1]+1
    quad_covid_adm_fit[[i]]$age_gp[j] <- quad_covid_adm_fit[[i]]$age_gp[j-1]
  }
}

for (i in 1:length(quad_covid_adm_fit)){
  for (j in 1:nrow(quad_covid_adm_fit[[i]])){
    quad_covid_adm_fit[[i]]$quad_cumfit[j] <- quad_param_fit[which(grepl(names(quad_covid_adm_fit)[i],names(quad_param_fit)))][[1]][[1]][1] + 
      quad_param_fit[which(grepl(names(quad_covid_adm_fit)[i],names(quad_param_fit)))][[1]][[1]][2] * quad_covid_adm_fit[[i]]$date_order[j] +
      quad_param_fit[which(grepl(names(quad_covid_adm_fit)[i],names(quad_param_fit)))][[1]][[1]][3] * (quad_covid_adm_fit[[i]]$date_order[j])^2
    quad_covid_adm_fit[[i]]$quad_fit[j] <- NA
  }}

for (i in 1:length(quad_covid_adm_fit)){
  for (j in 1:nrow(quad_covid_adm_fit[[i]])){
    quad_covid_adm_fit[[i]]$quad_fit[j] <- ifelse(quad_covid_adm_fit[[i]]$quad_cumfit[j]<0,0,quad_covid_adm_fit[[i]]$quad_cumfit[j])
    
  }}

for (i in 1:length(quad_covid_adm_fit)){
  for (j in 1:nrow(quad_covid_adm_fit[[i]])){
    quad_covid_adm_fit[[i]]$model[j] <- "cumquad"
    quad_covid_adm_fit[[i]]$estimate[j] <- ifelse(!is.na(quad_covid_adm_fit[[i]]$weight[j]),"fit","predict")
  }}

for (i in 1:length(quad_covid_adm_fit)){
  quad_covid_adm_fit[[i]]$estimate[as.numeric(rownames(quad_covid_adm_fit[[i]])) < min(which(!is.na(quad_covid_adm_fit[[i]]$weight)))] <- NA
  quad_covid_adm_fit[[i]] <- plyr:: rename(quad_covid_adm_fit[[i]],c("quad_cumfit" = "model_cumadm",
                                                                     "quad_fit" = "model_adm"))
}

# for (i in 1:length(quad_param_fit)){
#   print(names(quad_param_fit)[i])
#   print(paste("AIC = ",round(AIC(quad_param_fit[[i]]))))
#   print(paste("BIC = ",round(BIC(quad_param_fit[[i]]))))
# }

### Combine Models and Admissions ###

log_models_data <- dplyr:: bind_rows(log_covid_adm_fit)
exp_models_data <- dplyr:: bind_rows(exp_covid_adm_fit)
lm_models_data <- dplyr:: bind_rows(lm_covid_adm_fit)
quad_models_data <- dplyr:: bind_rows(quad_covid_adm_fit)

models_data <- rbind(log_models_data,exp_models_data,lm_models_data,quad_models_data)

data.frame(models_data %>% filter(estimate == "fit") %>% 
             group_by(model,age_gp) %>% 
             summarize(rmse = sqrt(mean((cum_adm - model_cumadm)^2))))

#RMSE of admissions for models (only applicable for logistic as cumulative admissions was fitted)
data.frame(models_data %>% filter(estimate == "fit") %>%
             group_by(model,age_gp) %>%
             summarize(rmse = sqrt(mean((no_admissions - model_adm)^2))))

models_data$proj_icu <- with(models_data,
                             ifelse(age_gp == "alesse_44",
                                    ifelse(adm_date <= cutoff_date,param_estimate$prop_icu_age1*no_admissions_wt,param_estimate$prop_icu_age1*model_adm),
                                    ifelse(age_gp == "bbtwi_4564",
                                           ifelse(adm_date <= cutoff_date,param_estimate$prop_icu_age2*no_admissions_wt,param_estimate$prop_icu_age2*model_adm),
                                           ifelse(age_gp == "cmoree_65",
                                                  ifelse(adm_date <= cutoff_date,param_estimate$prop_icu_age3*no_admissions_wt,param_estimate$prop_icu_age3*model_adm),
                                                         NA))))

## Models Export ##

models_data_export <- models_data

write.csv(
  data.frame(models_data_export %>% group_by(adm_date,date_order,age_gp, model,estimate) %>% 
               summarize(no_admissions = sum(no_admissions),
                         model_cumadm = sum(model_cumadm),
                         model_adm = sum(model_adm)))
  ,"test.csv",na="",row.names = F)

############ Plots ###############

models_data_plot <- models_data

tiff(paste("adm_proj","_",format(Sys.Date(),"%d%m%y"),".tiff",sep=""),
     width = 6, height = 4, units = "in", res = 80)

ggplot(data=subset(models_data_plot,model == "cumquad" & adm_date >= as.Date("2020-06-19")),
       aes(x=adm_date,colour=factor(paste(age_gp,model,sep="_"))))+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        legend.position = "top")+
  geom_point(aes(y=no_admissions),show.legend = F)+
  
  # geom_line(data=subset(models_data_plot,model == "cumlog" & adm_date >= as.Date("2020-06-19") & estimate == "fit"),aes(y=model_adm, colour = factor(paste(age_gp,model,sep="_"))),lwd = 1, lty = 1)+
  # geom_line(data=subset(models_data_plot,model == "cumlog" & adm_date >= as.Date("2020-06-19") & estimate == "predict"),aes(y=model_adm, colour = factor(paste(age_gp,model,sep="_"))),lwd = 1, lty = 2)+

  # geom_line(data=subset(models_data_plot,model == "cumexp" & adm_date >= as.Date("2020-06-19") & estimate == "fit"),aes(y=model_adm, colour = factor(paste(age_gp,model,sep="_"))),lwd = 1, lty = 1)+
  # geom_line(data=subset(models_data_plot,model == "cumexp" & adm_date >= as.Date("2020-06-19") & estimate == "predict"),aes(y=model_adm, colour = factor(paste(age_gp,model,sep="_"))),lwd = 1, lty = 2)+

  # geom_line(data=subset(models_data_plot,model == "cumlinear" & adm_date >= as.Date("2020-06-19") & estimate == "fit"),aes(y=model_adm, colour = factor(paste(age_gp,model,sep="_"))),lwd = 1, lty = 1)+
  # geom_line(data=subset(models_data_plot,model == "cumlinear" & adm_date >= as.Date("2020-06-19") & estimate == "predict"),aes(y=model_adm, colour = factor(paste(age_gp,model,sep="_"))),lwd = 1, lty = 2)+

  geom_line(data=subset(models_data_plot,model == "cumquad" & adm_date >= as.Date("2020-06-19") & estimate == "fit"),aes(y=model_adm, colour = factor(paste(age_gp,model,sep="_"))),lwd = 1, lty = 1)+
  geom_line(data=subset(models_data_plot,model == "cumquad" & adm_date >= as.Date("2020-06-19") & estimate == "predict"),aes(y=model_adm, colour = factor(paste(age_gp,model,sep="_"))),lwd = 1, lty = 2)+

  scale_x_date(date_labels = "%Y-%m-%d", breaks = "20 days")+
  xlab("Admission Date")+
  ylab("Admissions")+
  
  #scale_y_continuous(limits = c(0,40))+
  
  scale_colour_discrete(name = "Age Group", labels = c("<=44","45-64",">=65"))

dev.off()

################## ICU Inflights Analysis #####################

names(covid_inflights_line_list) <- gsub("[[:punct:]]","",names(covid_inflights_line_list))
names(covid_inflights_line_list) <- gsub("[[:space:]]","",names(covid_inflights_line_list))
names(covid_inflights_line_list) <- tolower(names(covid_inflights_line_list))

covid_inflights_line_list <- plyr:: rename(covid_inflights_line_list,c("inflightdate" = "inflight_date"))

covid_inflights_line_list$age_gp <- with(covid_inflights_line_list,
                                         ifelse(age<=44,"alesse_44",
                                                ifelse(age >= 45 & age <= 64,"bbtwi_4564", "cmoree_65")))
covid_inflights_line_list$age_gp <- factor(covid_inflights_line_list$age_gp,levels = c("alesse_44","bbtwi_4564","cmoree_65"))

covid_inflights_dataset <- data.frame(with(covid_inflights_line_list,table(inflight_date,age_gp)))
covid_inflights_dataset <- spread(covid_inflights_dataset,"age_gp","Freq")

covid_inflights_dataset$inflight_date <- as.Date(as.character(covid_inflights_dataset$inflight_date))
covid_inflights_dataset$no_inflights <- rowSums(covid_inflights_dataset[,2:ncol(covid_inflights_dataset)])

covid_inflights_dataset <- covid_inflights_dataset[order(covid_inflights_dataset$inflight_date),]
covid_inflights_dataset <- data.frame(covid_inflights_dataset %>% complete(inflight_date = seq.Date(min(inflight_date),cutoff_date,by="day")))
covid_inflights_dataset[,2:ncol(covid_inflights_dataset)] <- lapply(covid_inflights_dataset[,2:ncol(covid_inflights_dataset)],
                                                                    function(x) ifelse(is.na(x),0,x))

names(covid_inflights_dataset)[2:(ncol(covid_inflights_dataset)-1)] <- paste("no_inflights",names(covid_inflights_dataset)[2:(ncol(covid_inflights_dataset)-1)],sep = "_")

new_icu_admissions <- models_data

new_icu_admissions <- data.frame(new_icu_admissions %>% group_by(model,age_gp) 
                                 %>% mutate(proj_icu_lag = ifelse(age_gp == "alesse_44",lag(proj_icu,param_estimate$mean_days2icu_age1),
                                                                  ifelse(age_gp == "bbtwi_4564",lag(proj_icu,param_estimate$mean_days2icu_age2),
                                                                         ifelse(age_gp == "cmoree_65",lag(proj_icu,param_estimate$mean_days2icu_age3),NA)))))

new_icu_admissions <- new_icu_admissions[order(new_icu_admissions$model,
                                               new_icu_admissions$age_gp,
                                               new_icu_admissions$adm_date),]

new_icu_admissions <- data.frame(new_icu_admissions %>% group_by(model,age_gp) 
                                 %>% mutate(proj_icu_lag_end = ifelse(age_gp == "alesse_44",lag(proj_icu_lag,param_estimate$mean_losicu_age1),
                                                                      ifelse(age_gp == "bbtwi_4564",lag(proj_icu_lag,param_estimate$mean_losicu_age2),
                                                                             ifelse(age_gp == "cmoree_65",lag(proj_icu_lag,param_estimate$mean_losicu_age3),NA)))))

new_icu_admissions[,grepl("lag",names(new_icu_admissions))] <- lapply(new_icu_admissions[,grepl("lag",names(new_icu_admissions))],
                                                                      function(x) ifelse(is.na(x),0,x))

new_icu_admissions_split <- split(new_icu_admissions, f = paste(new_icu_admissions$model,new_icu_admissions$age_gp))

for (i in 1:length(new_icu_admissions_split)) {
  for(j in 1:nrow(new_icu_admissions_split[[i]])) {
    new_icu_admissions_split[[i]]$proj_icu_lag_inflights[j] <- sum(new_icu_admissions_split[[i]]$proj_icu_lag[1:j]) - sum(new_icu_admissions_split[[i]]$proj_icu_lag_end[1:j])
  }}

new_icu_admissions_unsplit <- dplyr:: bind_rows(new_icu_admissions_split)

write.csv(new_icu_admissions_unsplit,"test.csv",row.names = F,na="")

new_icu_admissions_tot_inflights <- new_icu_admissions_unsplit
new_icu_admissions_tot_inflights <- data.frame(new_icu_admissions_tot_inflights %>%
                                                 group_by(model,adm_date) %>% summarise(proj_icu_lag_inflights = sum(proj_icu_lag_inflights)
                                                 ))
new_icu_admissions_tot_inflights <- merge(new_icu_admissions_tot_inflights,covid_inflights_dataset,
                                          by.x = "adm_date", by.y = "inflight_date", all.x = T)

write.csv(new_icu_admissions_tot_inflights,"test.csv",row.names = F, na="")

tiff(paste("covid_inflights_proj_",
           format(Sys.Date(),"%d%m%y"),".tiff",sep = ""),
     width = 8, height = 4, units = "in", res = 80)

ggplot(data=subset(new_icu_admissions_tot_inflights,adm_date >= as.Date("2020-06-19")),aes(x=adm_date,y=proj_icu_lag_inflights,colour = model))+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        legend.position = "top",
        legend.title = element_blank())+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "20 days")+
  geom_point(aes(y=no_inflights),colour="black")+
  geom_line(lwd = 1, lty = 2)+
  geom_line(data = subset(new_icu_admissions_tot_inflights,adm_date >= "2020-06-19" & adm_date <= cutoff_date), aes(y=proj_icu_lag_inflights) ,lwd = 1, colour = "#00838F")+
  
  #scale_y_continuous(limits = c(0,8))+
  
  scale_colour_discrete(name = "", labels = c("Exponential","Linear","Logistic","Quadratic"))+
  
  xlab("Inflight Date")+
  ylab("ICU Inflights")

# geom_hline(yintercept = 108, colour = "darkgreen", lty = 2, lwd = 1)+
# geom_hline(yintercept = 188, colour = "darkorange", lty = 2, lwd = 1)+
# geom_hline(yintercept = 196, colour = "darkred", lty = 2, lwd = 1)+
# annotate("text",
#          x = as.Date("2020-06-12"),y = 90,
#          label = "y = 108", colour = "darkgreen")+
# annotate("text",
#          x = as.Date("2020-06-12"),y = 160,
#          label = "y = 188", colour = "darkorange")+
# annotate("text",
#          x = as.Date("2020-06-12"),y = 230,
#          label = "y = 196", colour = "darkred")

dev.off()
