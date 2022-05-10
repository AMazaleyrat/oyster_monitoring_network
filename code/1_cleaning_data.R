# This code was written by Anna Mazaleyrat and Julien Normand.

rm(list = ls())
library(stringr)
library(reshape2)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(formattable)
library(ggpubr)
library(here)


# Data loading and prep-------------------------------------------------------------------------------------------------


# On date 2021-07-20, we downloaded the data set:
# RESCO REMORA Database : National monitoring network of mortality and growth rates of the sentinel oyster Crassostrea
# gigas. SEANOE.
# Fleury Elodie, Normand Julien, Lamoureux Alice, Bouget Jean-Francois, Lupo Coralie, Cochennec-Laureau Nathalie,
# Petton Sebastien, Petton Bruno, Pouvreau Stephane (2021).
# https://doi.org/10.17882/53007

# Dataset loading

# First, unzip the file
zipF<- "data/raw/AllDataResco.zip"
outDir<-"data/raw"
unzip(zipF, exdir=outDir)
rm(outDir, zipF)

TAB <- read.csv("data/raw/AllDataResco.csv", header = TRUE)

# We only keep the columns that interest us
TAB <- TAB[, c(30, 3, 2, 17, 18, 9, 8, 23, 22, 28, 25, 26, 29, 32, 19, 12, 14, 16)]

# We rename the columns
colnames(TAB) <- c(
  "program", "mnemonic_site", "site", "class_age", "ploidy", "date", "mnemonic_date", "param", "code_param",
  "unit_measure", "fraction", "method", "id_ind", "value", "mnemonic_sampling", "long", "lat", "pop_init_batch"
)

# We transform some variables into factors
cols_to_make_factor <- TAB %>%
  select(-date, -value, -lat, long, -mnemonic_sampling, -pop_init_batch) %>%
  colnames()

TAB <- TAB %>% mutate_at(cols_to_make_factor, factor)
TAB$date <- as.Date(TAB$date, format = "%d/%m/%Y")
rm(cols_to_make_factor)

# We check for duplicated rows.
# There are 116 of them.
# TAB_duplic=TAB[duplicated(TAB), ] # To see the duplicated rows
# We delete them
TAB <- unique(TAB)

# Creation of the column annual campaign: "campaign" ==> from 1993 to 2019
# Recoding of annual campaign: replacing "RE93", "RE94", ... , "RE16", "RE17" by "1993", "1994", ... , "2016", "2017"
TAB$campaign <- as.factor(substr(TAB$pop_init_batch, 3, 4))
stock <- as.character(seq(93, 99, by = 1))
TAB <- TAB %>%
  mutate(camp = ifelse(campaign %in% stock, paste0(19, campaign), paste0(20, campaign))) %>%
  select(-campaign) %>%
  rename(campaign = camp)
TAB$campaign <- as.factor(TAB$campaign)
rm(stock)

# Creation of the column origin of the initial spat group: "spat" ==> 2 levels:
# "WILD" = wildcaught, "HATCH" = hatchery
TAB$spat <- "WILD"
TAB[which(substr(TAB$pop_init_batch, 9, 10) %in% c("ET", "NS")), "spat"] <- "HATCH"
TAB$spat <- as.factor(TAB$spat)

# Recoding birthplace of the initial spat group: "site_spat" ==> 8 levels:
# "AR" = Bay of Arcachon, "BO" = Bay of Bourgneuf, "MO" = Pertuis d'Antioche/Bay of Marennes-Oléron,
# "E1" = Hatchery 1, "E2" = Hatchery 2, "E3" = Hatchery 3, "E4" = Ifremer Hatchery of Argenton,
# "ME" = Mix of several batches
TAB$site_spat <- NA
TAB[which(TAB$spat == "WILD"), "site_spat"] <- "AR"
TAB[which(substr(TAB$pop_init_batch, 14, 15) == "BO"), "site_spat"] <- "BO"
TAB[which(substr(TAB$pop_init_batch, 14, 15) == "MO"), "site_spat"] <- "MO"
TAB[which(substr(TAB$pop_init_batch, 9, 10) == "NS"), "site_spat"] <- "E4"
TAB[which(substr(TAB$pop_init_batch, 14, 15) == "GO"), "site_spat"] <- "E1"
TAB[which(substr(TAB$pop_init_batch, 14, 15) == "FT"), "site_spat"] <- "E3"
TAB[which(substr(TAB$pop_init_batch, 14, 15) == "VN"), "site_spat"] <- "E2"
TAB[which(substr(TAB$pop_init_batch, 14, 15) == "Me"), "site_spat"] <- "ME"
TAB$site_spat <- as.factor(TAB$site_spat)

# Removing of the locations that only appear after 2019
stock <- levels(TAB$site)[c(2, 3, 4, 5, 7, 9, 10, 11, 12, 13, 15, 17, 18)] # These are our 13 sites
TAB <- TAB %>% filter(site %in% stock)
TAB <- droplevels(TAB)
rm(stock)
# levels(TAB$site)

# Recoding of the location name : "BLAIN" = Blainville-sur-mer; "CANCA" = Cancale; "COUPE" = Coupelasse; "
# AGNAS" = Banc d'Agnas; "GEFOS" = Géfosse; "LARMO" = LArmor-Baden; "LETES" = Le Tés; "LOIXR" = Loix-en-Ré;
# "MARSE" = Marseillan; "MORLX" = Morlaix; "PENRF" = Pénerf; "BREST" = Brest, "QUIBE" = Quiberon
levels(TAB$site) <- c(
  "BLAIN", "CANCA", "COUPE", "AGNAS", "GEFOS", "LARMO", "LETES", "LOIXR",
  "MARSE", "MORLX", "PENRF", "BREST", "QUIBE"
)

# Recoding ploidy : "ploidy" ==> 2 levels : "2n", "3n"
levels(TAB$ploidy) <- c("2n", "3n")

# Creating a column "batch". One batch is a group of individuals born during one reproductive event in the wild
# [wildcaught spat] or in hatchery having followed the same zoo technical path.
# Each batch can be identified by aggregating "campaign","class_age", "spat", "ploidy" and "site_spat"
TAB$batch <- as.factor(paste(TAB$campaign, TAB$class_age, TAB$spat, TAB$ploidy, TAB$site_spat, sep = "_"))

# From 2009 to 2014, different batches of spat were monitored in each site. Some of them have a similar background
# than the spat batches monitored before 2009 (i.e. wild-caught spat from natural spatfall in Bay of Arcachon),
# others have not. We need to eliminate the others in order to not bias the results.
batch_to_remove <- as.data.frame(as.factor(unique(TAB[which(TAB$campaign %in% 2009:2014 & TAB$class_age == "N0" &
  substr(TAB$batch, 6, 18) != "N0_WILD_2n_AR"), "batch"])))

colnames(batch_to_remove)[1] <- "batch"
to_remove <- TAB %>% filter(batch %in% batch_to_remove$batch)
TAB <- anti_join(TAB, to_remove)
rm(batch_to_remove, to_remove)

# We only keep the data collected the year the oysters were installed (i.e., DOY <= 365)
TAB <- TAB %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(TAB$campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  filter(DOY <= 365) %>%
  select(-DOY)

# We only keep spat (class_age = N0) and half-grown oysters (class_age = J1).
TAB <- TAB %>%
  filter(class_age == "N0" | class_age == "J1")

# Some "mnemonic prelevement" codes are unusual in BREST 2009_N0_WILD_2n_AR". Some batches are identified ISO.
# We remove those data
batchISO <- TAB %>% filter(str_detect(mnemonic_sampling, "ISO"))
TAB <- anti_join(TAB, batchISO)
rm(batchISO)

# At each sampling date, some measures were taken at the individual level (for ex.: individual mass) although others
# were made at the bag level. For now, we will only focus on the data taken at the bag level.
# We therefore only keep data collected on one group of oyster contained in one bag. The variables retained are:
# - "TOTVIVPOI" = total weight of living individuals in the bag
# - "INDVMORNB" = number of individuals that died since the last visit
# - "INDVVIVNB" = number of living oysters.
# The data set "bag_data" thus contains the information at the bag level (i.e. without code_param=="INDVPOID").
bag_data <- droplevels(subset(TAB, TAB$code_param != "INDVPOID"))



# Mortality data: cleaning ---------------------------------------------------------------------------------------------


# In 1995, INDMORNB in GEFOSS for N0 is always 0. 
bag_data %>%
  filter(batch == "1995_N0_WILD_2n_AR" & site == "GEFOS" & code_param != "TOTVIVPOI") %>%
  View()
# this is inconsistent with the REMORA report. According to that report, there was approximately 5% of mortality that
# year at that site. As we do not know if it is the INDVVIVNB or the INDVMORNB that is incorrect (which might affect 
# the mass), we delete all data for that site x classe age x year combination.
to_remove <- bag_data %>%
  filter(batch == "1995_N0_WILD_2n_AR" & site == "GEFOS")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# We check if the number of INDVIVNB and INDVIVMORNB data is identical for each batch x site x date combination.
# We calculate "diff_dead_alive" as the difference between the number of INDVVIVNB and INDVMORNB data.
# If this difference is not equal to  zero, then we may have an issue.
# We first check the issues when diff_dead_alive < 0 (i.e. more INDVVIVNB data than INDVMORNB data)
bag_data %>%
  filter(!code_param == "TOTVIVPOI") %>%
  group_by(batch, site, date, code_param) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB) %>%
  filter(diff_dead_alive < 0 | is.na(diff_dead_alive) == T) %>%
  View()
# The number of INDVVIVNB is often (i.e., for 356 batch x site x date combinations) larger than the number of INDVMORNB.
# We noticed that for some batch x site x date combinations, there were no INDVMORNB data at the seeding date, for some or
# all id_ind. It can make sense since there are no dead oysters at the seeding date but we will need those to
# calculate a cumulative mortality.
# To add those data, we first create a column (date_min) that indicates the minimal date possible for the batch x site
# combination of the row. We then only keep the rows for INDVMORNB data.
pipo <- bag_data %>%
  group_by(batch, site) %>%
  mutate(date_min = min(date)) %>%
  ungroup() %>%
  filter(code_param == "INDVMORNB")
# Second, for the INDVMORNB data, we indicate the minimal date (date_min_mor) for each batch x site x id_ind combination.
# Third, we only keep the combinations of batch x site x date x id_ind for which the INDVMORNB date is equal to the
# minimal date of INDVMORNB data but different to the minimal date possible for the batch x site combination
# (whether INDVMORNB data or INDVVIVNB data). This means that we kept the INDVMORNB data for specific combination
# batch x site x date x id_ind when its minimal date was not the same than the minimum date possible (i.e. including the
# min date for INDVVIVNB data).
# For those data, we replace the date by the minimal date possible and the value by 0 (0 death at the seeding date).
pipo <- pipo %>%
  group_by(batch, site, id_ind) %>%
  mutate(date_min_mor = min(date)) %>%
  filter(date == date_min_mor & date != date_min) %>%
  ungroup() %>%
  mutate(date = date_min, value = 0) %>%
  select(-date_min_mor, -date_min)
bag_data <- rbind(pipo, bag_data)
rm(pipo)
# Because of this operation (i.e duplication of rows and replacement of the date by date_min) some info such as
# "mnemonic passage" and "mnemonic prelevement" are wrong! We can't rely on these informations anymore.


# We check if the lines we add ruled out a few issues
bag_data %>%
  filter(!code_param == "TOTVIVPOI") %>%
  group_by(batch, site, date, code_param) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB) %>%
  filter(diff_dead_alive < 0 | is.na(diff_dead_alive) == TRUE)
# It did ! Only 22 issues remaining.


# We check those issues one by one:
# 2008_J1_WILD_2n_AR CANCA: INDVMORNB data is missing on 2008-04-08
bag_data %>%
  filter(batch == "2008_J1_WILD_2n_AR" & site == "CANCA") %>%
  arrange(date) %>%
  View()
# There is only 1 data for this site and year combination. We remove this data.
to_remove <- bag_data %>% filter(batch == "2008_J1_WILD_2n_AR" & site == "CANCA")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# 2008_N0_WILD_2n_AR AGNAS: INDVIVMORNB is missing on 2008-06-03
bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "AGNAS" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)

bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "AGNAS") %>%
  arrange(date) %>%
  View()
# INDVMORNB is missing on 2008-06-03. Without this data, we can't calculate the cumulative mortality at that date. 
# We thus will lose the entire site x year combination (since we need the cumulative mortality at that date to calculate 
# the one at the next date).
# We could add INDVMORNB = 70 on 2008-06-03 (since INDVVIVNB = 230 at that date and 300 at the previous one). However,
# this is too risky since oysters could have been taken from the bag (e.g. for pathogen analysis) between the last visit
# (2008-04-08) and this visit (2008-06-03). In that case we will greatly over-estimate the cumulative mortality.
# There was no RESCO report for this year. We thus can't check if oysters were removed from the bag.
# We thus remove all data from that batch x site x year combination (including TOTVIVPOI data since there are only 3).
to_remove <- bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "AGNAS")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# 2008_N0_WILD_2n_AR LOIXR: INDVMORNB is missing on 2008-06-02
bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "LOIXR" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)

bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "LOIXR") %>%
  arrange(date) %>%
  View()
# Same issue as the previous batch x site x year combination.
# We delete those data (including the TOTVIVPOID since there are only 2 data)
to_remove <- bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "LOIXR")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2009_J1_WILD_2n_AR MARSE issue on date 2009-05-25 and 2009-09-21
bag_data %>%
  filter(batch == "2009_J1_WILD_2n_AR" & site == "MARSE") %>%
  arrange(date) %>%
  View()
# Some bag identified NA have the same values than bag identified with another id_ind.
# These id_ind NA seems to be duplicated values for bags 7, 8, 9. We remove those NAs.
to_remove <- bag_data %>% filter(batch == "2009_J1_WILD_2n_AR" & site == "MARSE" & is.na(id_ind) == TRUE)
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2009_N0_WILD_2n_AR MARSE
bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & site == "MARSE") %>%
  arrange(date, value) %>%
  View()
# Same issue as the previous batch x site x year combination.
# some bags are identified NAs and are duplicated values of identified bags
# On date 2009-09-21, there are only NAs but the data don't corresponds to INDVIVNB data at the previous date.
# We remove all NAs
to_remove <- bag_data %>% filter(batch == "2009_N0_WILD_2n_AR" & site == "MARSE" & is.na(id_ind) == TRUE)
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2010_J1_WILD_2n_AR BLAIN: INDVMORNB is missing on 2010-06-16
bag_data %>%
  filter(batch == "2010_J1_WILD_2n_AR" & site == "BLAIN" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)

bag_data %>%
  filter(batch == "2010_J1_WILD_2n_AR" & site == "BLAIN") %>%
  arrange(date) %>%
  View()
# Several issues here. On 2010-06-16, there is one bag identified id_ind = 1 and another one with id_ind = NA.
# However, id_ind = 2 and 3 were sampled before and after that date. There is also on id_ind = NA on seeding date.
# First, We remove the NAs
to_remove <- bag_data %>%
  filter(batch == "2010_J1_WILD_2n_AR" & site == "BLAIN" & is.na(id_ind) == TRUE)
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# As there is no data for bag 2 and 3 on date 2010-06-16 and dead oysters may have been taken away from these bags
# before the next visit (2010-06-28). we can't calculate a cumulative mortality for these bags after 2010-06-16
# because we might underestimate it.
# Second, we delete the INDVMORNB for bags 2 and 3 after 2010-06-16. We keep the INDVIVNB data to be able to calculate
# the mean mass of individuals.
to_remove <- bag_data %>%
  filter(batch == "2010_J1_WILD_2n_AR" & site == "BLAIN" & code_param == "INDVMORNB" & id_ind != "1" &
    date > "2010-06-16")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2010_N0_WILD_2n_AR AGNAS
bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "AGNAS" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
# Several issues here :
# 1- the number of bags followed through the year vary
# 2- the number of INDVVIVNB and INDVMORNB data are not the same on 2010-04-27
bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "AGNAS") %>%
  arrange(date) %>%
  View()
# Bags 2 and 3 are not sampled after the seeding date (except on 2010-06-14).
# We removed data from bag 2 and 3 to only keep data from id_ind =="1".
to_remove <- bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "AGNAS" & id_ind != "1")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2010_N0_WILD_2n_AR LETES
bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "LETES" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
# Several issues here:
# 1- the number of bags followed through the year vary
# 2- the number of INDVVIVNB and INDVMORNB data are not the same on dates 2010-04-26 and 2010-06-28
bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "LETES" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# We only keep data from bag 1 because it was followed the all year
to_remove <- bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "LETES" & id_ind != "1")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2011_N0_WILD_2n_AR the issues is the same in BLAIN, AGNAS, GEFOS, LARMO, LOIXR:
# the bag 1 is followed the all year, whereas, there are INDVVIVNB data for bag 2 and 3 popping up at the seeding date.
# We remove data from bag 2 and 3.
to_remove <- bag_data %>%
  group_by(batch, site, code_param, date) %>%
  filter(batch == "2011_N0_WILD_2n_AR" &
    (site == "BLAIN" | site == "AGNAS" | site == "GEFOS" | site == "LARMO" | site == "LOIXR") & id_ind != "1")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2012_J1_WILD_2n_AR MARSE on date 2012-02-21
bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "MARSE" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "MARSE") %>%
  arrange(date) %>%
  View()
# There are several issues:
# 1- INDVVIVNB at seeding date (2012-02-21) is indicated for only one bag instead of 18.
# 2- INDVIVNB at seeding date is wrong (50 oysters were installed not 300).
# 3- On date 2012-11-12 there is an INDVMORNB data missing for bag 6.
# To address those issues, we first remove the wrong INDVVIVNB value at seeding date
to_remove <- bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "MARSE" & date == "2012-02-21" & code_param == "INDVVIVNB")
bag_data <- anti_join(bag_data, to_remove)
# We then copy the rows for INDVVIVNB on date 2012-03-09. We replace the value with 50 and date with 2012-02-21
# (i.e. INDVVIVNB = 50 at seeding date)
to_add <- bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "MARSE" & date == "2012-03-09" & code_param == "INDVVIVNB") %>%
  mutate(value = as.numeric("50"), date = as.Date("2012-02-21"))
bag_data <- rbind(bag_data, to_add)
rm(to_add, to_remove)
# Accordingly the TOTVIVPI at seeding date is also wrong we remove it (it was for bags with INDVVIVNB =  300)
to_remove <- bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "MARSE" & date == "2012-02-21" & code_param == "TOTVIVPOI")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# INDVMORNB is missing for id_ind = 6 on 2012-11-12. INDVVIVNB = 36 at that visit and INDVMORNB = 0 and INDVVIVNB = 36
# at the next visit.
# We thus add an INDVMORNB = 0 on date 2012-11-12
to_add <- bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "MARSE" & date == "2012-11-12" & code_param == "INDVVIVNB" &
    id_ind == "6") %>%
  mutate(value = as.numeric("0"), code_param = as.factor("INDVMORNB"))
bag_data <- rbind(bag_data, to_add)
rm(to_add)
# From 2012-09-17, only 12 out of the 18 bags have been followed and that's ok !

# 2013_J1_WILD_2n_AR GEFOS: on date 2013-06-11
bag_data %>%
  filter(batch == "2013_J1_WILD_2n_AR" & site == "GEFOS" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
bag_data %>%
  filter(batch == "2013_J1_WILD_2n_AR" & site == "GEFOS") %>%
  arrange(date) %>%
  View()
# 3 bag with id_ind=NA were added on date 2013-06-11
# We delete those bags
to_remove <- bag_data %>%
  filter(batch == "2013_J1_WILD_2n_AR" & site == "GEFOS" & is.na(id_ind) == "TRUE")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2013_J1_WILD_2n_AR MORLX
bag_data %>%
  filter(batch == "2013_J1_WILD_2n_AR" & site == "MORLX" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
bag_data %>%
  filter(batch == "2013_J1_WILD_2n_AR" & site == "MORLX") %>%
  arrange(date) %>%
  View()
# On dates 2013-09-20 and 2013-08-10, INDVMORNB is indicated for one bag (id_ind=="1"), whereas, INDVVIVNB is indicated
# for 3 bags (id_ind = 2 and id_ind = 3).
# We remove INDVMORNB data from bag 2 and 3 when date > "2013-09-06" but keep INDVVIVNB data to later calculate
# the mean mass of individuals
to_remove <- bag_data %>%
  filter(batch == "2013_J1_WILD_2n_AR" & site == "MORLX" & code_param == "INDVMORNB" & id_ind != "1" &
    date > "2013-09-06")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2013_N0_WILD_2n_AR GEFOS on date 2013-12-02
bag_data %>%
  filter(batch == "2013_N0_WILD_2n_AR" & site == "GEFOS" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
bag_data %>%
  filter(batch == "2013_N0_WILD_2n_AR" & site == "GEFOS") %>%
  arrange(date) %>%
  View()
# There is an additional bag with id_ind ==NA on 2013-12-02. This is a duplicated row from a bag with an id_ind
# we remove this bag
to_remove <- bag_data %>%
  filter(batch == "2013_N0_WILD_2n_AR" & site == "GEFOS" & is.na(id_ind) == TRUE)
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2014_N0_WILD_2n_AR MORLX on date 2014-05-26
bag_data %>%
  filter(batch == "2014_N0_WILD_2n_AR" & site == "MORLX" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB) #
bag_data %>%
  filter(batch == "2014_N0_WILD_2n_AR" & site == "MORLX" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# On date 2014-05-26, two bags are identified id_ind=="1" with INDVVIVNB = 288 or 282. On this date, INDVMORNB = 2.
# At the previous visit, INDVVIV = 290. Then it makes more sense to keep the bag INDVVIVNB = 288 on date 2014-05-26.
# We remove the bag INDVVIVNB = 282.
to_remove <- bag_data %>%
  filter(batch == "2014_N0_WILD_2n_AR" & site == "MORLX" & date == "2014-05-26" & id_ind == "1" &
    code_param == "INDVVIVNB" & value == "282")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# We also remove TOTVIVPOI data on 2014-05-26 as there are 2 bags id_ind 1, 2 bags id_ind = 2 and 2 bags id_ind =3
to_remove <- bag_data %>%
  filter(batch == "2014_N0_WILD_2n_AR" & site == "MORLX" & date == "2014-05-26" & code_param == "TOTVIVPOI")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# INDVVIVNB at seeding date (2014-03-18) is also missing for id_ind = 2 and 3
to_add <- bag_data %>%
  filter(batch == "2014_N0_WILD_2n_AR" & site == "MORLX" & date == "2014-03-18" & code_param == "INDVVIVNB") %>%
  slice(rep(1:n(), each = 2)) %>%
  mutate(id_ind = as.factor(c("2", "3")))
bag_data <- rbind(bag_data, to_add)
rm(to_add)



# Are there some issues remaining ?
bag_data %>%
  filter(!code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB) %>%
  filter(diff_dead_alive < 0)
# 2 issues remain but they are due to the fact that we kept INDVVIVNB data and deleted INDVMORNB data (to calculate
# the mean mass of individuals but not the cumulative mortality).
# Thus, we have solved all issues regarding a larger number of INDVVIVNB data than INDVMORNB data.

# Let's check the issues related to a larger number of INDVMORNB than INDVVIVNB data.
# For the class_age = N0:
bag_data %>%
  filter(class_age == "N0" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, date, code_param) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB) %>%
  filter(diff_dead_alive > 0)
# There are 19 issues.

# 2008_N0_WILD_2n_AR COUPE
bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "COUPE") %>%
  arrange(date) %>%
  View()
# The id_ind vary through the year. There are INDVIVMORNB and INDVVIVNB data for bag id_ind =="1" at seeding date
# but then id_ind is NA (and there is no INDVIVNB data for id_ind = NA at seeding date).
# We first remove the data with id_ind =="1"
to_remove <- bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "COUPE" & id_ind == "1")
bag_data <- anti_join(bag_data, to_remove)
# The we add INDVIVNB data for id_ind== NA at seeding date.
to_add <- to_remove %>%
  filter(code_param == "INDVVIVNB") %>%
  mutate(id_ind = replace(id_ind, id_ind == "1", NA))
bag_data <- rbind(bag_data, to_add)
rm(to_remove, to_add)


# 2008_N0_WILD_2n_AR BREST
bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "BREST") %>%
  arrange(date) %>%
  View()
# Issues on dates 2008-04-07, 2008-04-07 and 2008-12-12. There are data with id_ind =="1" popping-up through the year.
# Some of them seem to be duplicated of rows with id_ind = NA.
# on 2008-08-29 and 2008-12-12, INDVMORNB data with id_ind = 1 (value = 43 AND 2) better matches INDVVIVNB data with 
# id_ind = NA (XXX and ) than INDVVIVNB data id_ind = NA (XX and XX). Otherwise the number of oysters 
# (INDVVIVNB + INDVMORNB) will be larger than INDVVIVNB at the previous visit.
# We thus remove all data with id_ind = 1 except those equal to 2 and 43
to_remove <- bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & id_ind == "1" & !(value == "43" | value == "2"))
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# Then on 2008-08-29 and 2008-12-12, we remove INDVMORNB data with id_ind = NA
to_remove <- bag_data %>%
  filter(batch == "2008_N0_WILD_2n_AR" & site == "BREST" & is.na(id_ind) == TRUE & code_param == "INDVMORNB" &
    (date == "2008-08-29" | date == "2008-12-12"))
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# We then replace the id_ind = 1 by id_ind = NA
bag_data <- bag_data %>%
  mutate(id_ind = replace(id_ind, batch == "2008_N0_WILD_2n_AR" & site == "BREST" & id_ind == "1", NA))


# 2009_N0_WILD_2n_AR BLAIN and CANCA,
# 2010_N0_WILD_2n_AR CANCA AND GEFOSS
# 2011_N0_WILD_2n_AR PENRF
bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & (site == "BLAIN" | site == "CANCA")) %>%
  arrange(site, date) %>%
  View()
bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & (site == "CANCA" | site == "GEFOS")) %>%
  arrange(site, date) %>%
  View()
bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "PENRF") %>%
  arrange(date) %>%
  View()
# The issues are the same. Bags 2 and 3 sometimes pop-up. There are also id_ind = NA at GEFOS.
# TOTVIVPOI data are not always associated with these bags and some of them are only followed at one or two dates
# We delete the data with id_ind =2 and 3 to only keep the ones related to id_ind 1
to_remove <- bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & (site == "BLAIN" | site == "CANCA") & id_ind != "1")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

to_remove <- bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & (site == "CANCA" | site == "GEFOS") & (id_ind != "1" | is.na(id_ind) == TRUE))
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

to_remove <- bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "PENRF" & id_ind != "1")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2012_N0_WILD_2n_AR MARSE
bag_data %>%
  filter(batch == "2012_N0_WILD_2n_AR" & site == "MARSE" & code_param != "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# there are several issues:
# 1- there are missing INDVVIVNB data for the seeding date (for id_ind 2-->6)
# 2- INDVVIVNB at seeding date is wrong. INDVIVNB = 50 not 300 (according to INDVIVNB and INDMORNB at the next visit)
# First, we remove the wrong data at the seeding date
to_remove <- bag_data %>%
  filter(batch == "2012_N0_WILD_2n_AR" & site == "MARSE" & code_param == "INDVVIVNB" & date == "2012-02-21")
bag_data <- anti_join(bag_data, to_remove)
# Then we add the correct ones for the 6 bags
to_modify <- to_remove %>%
  slice(rep(1:n(), each = 6)) %>%
  mutate(value = as.numeric(replace(value, value == "300", "50")), id_ind = as.factor(seq(1:6)))
bag_data <- rbind(bag_data, to_modify)
rm(to_remove, to_modify)


# 2014_N0_WILD_2n_AR (except MORLX, no data at QUIBE)
# On seeding date, INDVMORNB is indicated for the 3 bags whereas INDVVIVNB is only indicated for bag 1.
# The issue is the same for J1. We thus add INDVIVNB data for both N0 and J1.
to_add <- bag_data %>%
  filter(campaign == "2014" & code_param == "INDVMORNB" & class_age == "N0"& date == "2014-03-18" & id_ind != "1" &
    site != "MORLX") %>%
  mutate(code_param = as.factor("INDVVIVNB"), value = as.numeric("300"))
bag_data <- rbind(bag_data, to_add)
rm(to_add)


# Let's see if we solved all the issues:
bag_data %>%
  filter(class_age == "N0" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, date, code_param) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB) %>%
  filter(diff_dead_alive > 0) # Yes we did !


# Let's see the issues related to class_age J1
bag_data %>%
  filter(class_age == "J1" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, date, code_param) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB) %>%
  filter(diff_dead_alive > 0) # 29 issues !


# 2008_J1_WILD_2n_AR GEFOS on date 2008-04-03
bag_data %>%
  filter(batch == "2008_J1_WILD_2n_AR" & site == "GEFOS" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
bag_data %>%
  filter(batch == "2008_J1_WILD_2n_AR" & site == "GEFOS" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# the id_ind=1 are duplicated of in_ind=="NA"
# We remove the id_ind=1
to_remove <- bag_data %>%
  filter(batch == "2008_J1_WILD_2n_AR" & site == "GEFOS" & id_ind == "1")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# 2008_J1_WILD_2n_AR BREST
bag_data %>%
  filter(batch == "2008_J1_WILD_2n_AR" & site == "BREST" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
bag_data %>%
  filter(batch == "2008_J1_WILD_2n_AR" & site == "BREST" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# Several issues:
# 1- for the seeding date there is no INDVVIVNB data for bag 1,2,3.
# 2- Some bags are identified NA although most of them are 1,2,3
# 3- the number of bag followed through time vary
# We first remove the NAs
to_remove <- bag_data %>%
  filter(batch == "2008_J1_WILD_2n_AR" & site == "BREST" & is.na(id_ind) == TRUE)
bag_data <- anti_join(bag_data, to_remove)
# Then we add the data at seeding date
to_add <- to_remove %>%
  filter(date == min(date) & code_param == "INDVVIVNB") %>%
  slice(rep(1:n(), each = 3)) %>% # we replicate this line 3 times
  mutate(id_ind = as.factor(c("1", "2", "3")))
bag_data <- rbind(bag_data, to_add)
rm(to_add, to_remove)
# on dates 2008-07-22, 2008-11-12 and 2008-12-12, the bag 3 was not followed. We thus delete INDVMORNB for bag 3 after
# 2008-07-22
to_remove <- bag_data %>%
  filter(batch == "2008_J1_WILD_2n_AR" & site == "BREST" & code_param == "INDVMORNB" & id_ind == "3" &
    date > "2008-07-22")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)



# 2009_J1_WILD_2n_AR MARSE
bag_data %>%
  filter(batch == "2009_J1_WILD_2n_AR" & site == "MARSE" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
bag_data %>%
  filter(batch == "2009_J1_WILD_2n_AR" & site == "MARSE") %>%
  arrange(date) %>%
  View()
# Several issues here:
# 1- id_ind 3 and 5 are not followed the all year because all the oyster died in the bags.
# 2- INDVVIVNB is missing at seeding date for bag 4 --> 9 and wrong for bag 1 --> 3.Indeed, INDVIVNB can't be equal to
# 300 since at the next visit INDVVNB <100 and INDVMORNB=0 .
# First, we remove the data at seeding date that are wrong
to_remove <- bag_data %>%
  filter(batch == "2009_J1_WILD_2n_AR" & site == "MARSE" & code_param == "INDVVIVNB" & date == "2009-03-09")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# Then we add the correct data for the 9 bags at seeding date (2009-03-09). Since INDVMORNB = 0  at the next visit,
# INDVMORNB at seeding date is equal to that at the next visit
to_add <- bag_data %>%
  filter(batch == "2009_J1_WILD_2n_AR" & site == "MARSE" & code_param == "INDVVIVNB" & date == "2009-03-20") %>%
  mutate(date = as.Date("2009-03-09")) %>%
  ungroup()
bag_data <- rbind(bag_data, to_add)
rm(to_add)


# 2011_J1_WILD_2n_AR: BLAIN and PENRF
bag_data %>%
  filter(batch == "2011_J1_WILD_2n_AR" & (site == "BLAIN" | site == "PENRF") & code_param != "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB)
# INDVVIVNB on seeding date (2011-03-17) is not indicated for bags 2 and 3.Since at the next visit, INDVMORNB > 0,
# INDVVIVNB at seeding date can't be the same as the one at the next visit.
# We thus need to compute INDVIVNB at seeding date as the sum of INDVIVNB and INDVMORNB at the next visit (2011-04-15
# in BLAIN and 2011-04-19 for PENRF)
# let's found out the sum of INDVVIVNB + INDMORNB on the minimal date (i.e. date after seeding date).
max <- bag_data %>%
  group_by(batch, site, id_ind) %>%
  filter(class_age == "J1" & campaign == "2011" & (site == "PENRF" | site == "BLAIN") & !code_param == "TOTVIVPOI" &
    date > "2011-03-17") %>%
  filter(date == min(date)) %>%
  summarize(value = sum(value)) # these will be INDVVIVNB at seeding date
# Then let's found the row to add and add those informationa
pipoINDVVIVNB <- bag_data %>%
  filter(campaign == "2011" & code_param == "INDVVIVNB" & class_age == "J1" & (site == "PENRF" | site == "BLAIN")) %>%
  group_by(batch, site, id_ind) %>%
  mutate(date_min = as.Date("2011-03-17")) %>%
  mutate(date_min_mor = min(date)) %>%
  filter(date == date_min_mor & date_min_mor != date_min) %>%
  ungroup() %>%
  mutate(date = date_min) %>%
  select(-date_min_mor, -date_min, -value) %>% # we then merge with max
  merge(., max, by = c("batch", "site", "id_ind"))
# Re-order column to fit the order in bag_data data set
target <- as.vector(names(bag_data))
t <- select(pipoINDVVIVNB, which(names(pipoINDVVIVNB) %in% target))
gg <- match(target, names(t))
pipoINDVVIVNB <- t[, gg]
bag_data <- rbind(bag_data, pipoINDVVIVNB)
rm(pipoINDVVIVNB, max, target, t, gg)


# 2012_J1_WILD_2n_AR for lots of sites
# Same issue as previously the data at seeding date are not indicated
max <- bag_data %>%
  filter(class_age == "J1" & campaign == "2012" & !code_param == "TOTVIVPOI" & !site == "MARSE" &
    date > "2012-02-21") %>%
  group_by(batch, site, id_ind) %>%
  filter(date == min(date)) %>%
  summarize(value = sum(value))

pipoINDVVIVNB <- bag_data %>%
  filter(campaign == "2012" & class_age == "J1" & !(site == "MARSE")) %>%
  filter(code_param == "INDVVIVNB") %>%
  group_by(batch, site, id_ind) %>%
  mutate(date_min = as.Date("2012-02-21")) %>%
  mutate(date_min_mor = min(date)) %>%
  filter(date == date_min_mor & date_min_mor != date_min) %>%
  ungroup() %>%
  mutate(date = date_min) %>%
  select(-date_min_mor, -date_min, -value) %>%
  merge(., max, by = c("batch", "site", "id_ind"))
# Merge and re-order
target <- as.vector(names(bag_data))
t <- select(pipoINDVVIVNB, which(names(pipoINDVVIVNB) %in% target))
gg <- match(target, names(t))
pipoINDVVIVNB <- t[, gg]
bag_data <- rbind(bag_data, pipoINDVVIVNB)
rm(pipoINDVVIVNB, max, t, gg, target)


# 2014_J1_WILD_2n_AR MORLX
bag_data %>%
  filter(batch == "2014_J1_WILD_2n_AR" & site == "MORLX" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, code_param, date) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_mort_viv = INDVMORNB - INDVVIVNB)
bag_data %>%
  filter(batch == "2014_J1_WILD_2n_AR" & site == "MORLX" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# On date 2014-05-26, there are 2 bags identified 1, 2 and 3 (a total of 6 bags instead of 3).
# The bags with value <10 do not fit INDVIVNB and INDVMORNB data. We remove them
to_remove <- bag_data %>%
  filter(date == "2014-05-26" & value < 10 & batch == "2014_J1_WILD_2n_AR" & site == "MORLX")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# We also delete the TOTVIVPOI data for this visit as we do not know which one fit the INDVIVNB data
to_remove <- bag_data %>%
  filter(date == "2014-05-26" & batch == "2014_J1_WILD_2n_AR" & site == "MORLX" & code_param == "TOTVIVPOI")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2014_J1_WILD_2n_AR (all sites except MORLX and QUIBE)
# On seeding date, INDVMORNB is indicated for the 3 bags whereas INDVVIVNB is only indicated for bag 1.
# we add the data for bag 2 and 3.
to_add <- bag_data %>%
  filter(campaign == "2014" & code_param == "INDVMORNB" & class_age == "J1" & date == "2014-03-18" & id_ind != "1") %>%
  mutate(code_param = as.factor("INDVVIVNB"), value = as.numeric("300"))
bag_data <- rbind(bag_data, to_add)
rm(to_add)


# Let's see if we solved the issues
bag_data %>%
  filter(class_age == "J1" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, date, code_param) %>%
  summarize(count = n()) %>%
  reshape2::dcast(batch + site + date ~ code_param, value = count) %>%
  mutate(diff_dead_alive = INDVMORNB - INDVVIVNB, class_age = substr(batch, 6, 7)) %>%
  filter(diff_dead_alive > 0) # No more issues !


# So far, we have checked that the number of INDVVIVNB and INDVMORNB is identical at each date.
# Another things to be careful of is to make sure that we followed the same bag the all year and that bags are not
# popping-up.
# Let's see if we have this kind of issues for N0 :
bag_data %>%
  filter(class_age == "N0" & code_param == "INDVMORNB") %>%
  group_by(site, date, campaign) %>%
  summarize(nbr_id_diff = length(unique(id_ind))) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  ggplot(aes(x = DOY, y = nbr_id_diff)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign, scales = "free_y") +
  ggtitle("N0 INDVMORNB") # Yes we have those issues.


# 2009_N0_WILD_2n_AR: GEFOS, MORLX and PENRF.
# There are bag identified 2 and 3 at the seeding date but not after. We delete those:
to_remove <- bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & (site == "MORLX" | site == "PENRF" | site == "GEFOS") &
    (id_ind == "2" | id_ind == "3"))
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2009_N0_WILD_2n_AR: COUPE
bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & site == "COUPE") %>%
  arrange(date) %>%
  View()
# Basg identified id_2 or 3 pop up through the year
# We delete INDVMORNB for bags 2 and 3 to be able to calculate the mean mass of individuals but not the cumulative
# mortality
to_remove <- bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & site == "COUPE" & code_param == "INDVMORNB" & id_ind != "1")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2009_N0_WILD_2n_AR LOIXR : data are missing on date 2009-06-10 for id_ind = 3
bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & site == "LOIXR") %>%
  arrange(date) %>%
  View()
# We delete all INDVMORNB data related to this bag when date >= 2009-06-10
to_remove <- bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & site == "LOIXR" & id_ind == "3" & code_param == "INDVMORNB" &
    date > "2009-06-10")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2009_N0_WILD_2n_AR LETES
bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & site == "LETES") %>%
  arrange(date) %>%
  View()
# On date 2009-06-08, there was only one bag followed although bag 2 and 3 were sampled before and after this visit.
# We remove the INDVMORNB for bag 2 and 3 after date 2009-06-08 to only compute cumulative mortality from bag 1 but
# mean mass of individuals for all bags.
to_remove <- bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & site == "LETES" & id_ind != "1" & code_param == "INDVMORNB" &
    date > "2009-06-08")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2010_N0_WILD_2n_AR BLAIN AND LOIXR
bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "BLAIN") %>%
  arrange(date) %>%
  View() # On 2010-05-17 and 2010-05-26 only the bag 1 is followed.
bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "LOIXR") %>%
  arrange(date) %>%
  View() # On certain dates, the bag 1 was the only bag followed. 
# we remove data from bag 2 and 3
to_remove <- bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & (site == "BLAIN" | site == "LOIXR") & id_ind != "1" & code_param == "INDVMORNB")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2010_N0_WILD_2n_AR BREST
# 2011_N0_WILD_2n_AR BREST
bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "BREST" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View() # Only the bag 1 is followed on 2010-05-25
bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "BREST" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View() # Only the bag 1 is followed on 2011-08-12
# We remove INDVMORNB data for bag 2 and 3 to only calculate cumulative mortality for this bag but calculate mean
# mass for all 3 bags.
to_remove <- bag_data %>%
  filter(batch == "2010_N0_WILD_2n_AR" & site == "BREST" & id_ind != "1" & code_param == "INDVMORNB" &
    date > "2010-05-25")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

to_remove <- bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "BREST" & id_ind != "1" & code_param == "INDVMORNB" &
    date > "2011-08-12")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2011_N0_WILD_2n_AR CANCA COUPE LETES MORLX
# Bags 2 and 3 are indicated at seeding date but not followed afterwards. We delete those.
to_remove <- bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" &
    (site == "CANCA" | site == "COUPE" | site == "LETES" | site == "MORLX") & id_ind != "1")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2011_N0_WILD_2n_AR MARSE
bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "MARSE" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View() # id_ind=="3" and id_ind=="4" are not followed after 2011-09-13.
# not an issue we keep all the data. Cumulative mortality will be the mean of the 4 bags until 2011-09-13 and based on
# data from bag 2 and 3 afterwards.


# Let's see if we solved all the issues
bag_data %>%
  filter(class_age == "N0" & code_param == "INDVMORNB") %>%
  group_by(site, date, campaign) %>%
  summarize(nbr_id_diff = length(unique(id_ind))) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  ggplot(aes(x = DOY, y = nbr_id_diff)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign, scales = "free_y") +
  ggtitle("N0 INDVMORNB") # Yes we did !
# The issues remaining are due to the fact that we deleted INDVMORNB data to not calculate mortality for specific bags
# but kept the INDVVIVNB data to calculate the individual mean mass.


# Let's see the issues for J1
bag_data %>%
  filter(class_age == "J1" & code_param == "INDVMORNB") %>%
  group_by(site, date, campaign) %>%
  summarize(nbr_id_diff = length(unique(id_ind))) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  ggplot(aes(x = DOY, y = nbr_id_diff)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign, scales = "free_y") +
  ggtitle("J1 INDVMORNB") # There are a few issues


# 2011_J1_WILD_2n_AR CANCA
bag_data %>%
  filter(batch == "2011_J1_WILD_2n_AR" & site == "CANCA" & code_param != "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# On date 2011-06-02, the only bag followed is id 3.
# we remove data from bag 1 and 2 after 2011-06-02.
to_remove <- bag_data %>%
  filter(batch == "2011_J1_WILD_2n_AR" & site == "CANCA" & id_ind != "3" & date > "2011-06-02" & 	
           code_param=="INDVMORNB")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2012_J1_WILD_2n_AR CANCA
bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "CANCA" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# On date 2012-06-21 the only bag followed is id_ind=="1"
# we delete INDVMORNB id_ind!="1" and date >2012-06-21
to_remove <- bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "CANCA" & code_param == "INDVMORNB" & id_ind != "1" &
    date > "2012-06-21")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2013_J1_WILD_2n_AR LOIXR
bag_data %>%
  filter(batch == "2013_J1_WILD_2n_AR" & site == "LOIXR" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# On date 2013-10-09, the only bag followed is id_ind = 1.
# We delete INDVMORNB for bag 2 and 3 after 2013-10-09
to_remove <- bag_data %>%
  filter(batch == "2013_J1_WILD_2n_AR" & site == "LOIXR" & code_param == "INDVMORNB" & id_ind != "1" &
    date > "2013-10-09")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# 2015_J1_HATCH_2n_E4 CANCA
bag_data %>%
  filter(batch == "2015_J1_HATCH_2n_E4" & site == "CANCA" & !code_param == "TOTVIVPOI") %>%
  arrange(date) %>%
  View()
# On date 2015-07-03 the only bag followed is id_ind=="1" although there are data for other bags before and after that
# date.
# We remove INDVMORNB data for id_ind=="2" and id_ind=="3" with date >2015-07-03
to_remove <- bag_data %>%
  filter(batch == "2015_J1_HATCH_2n_E4" & site == "CANCA" & id_ind != "1" & code_param == "INDVMORNB" &
    date > "2015-07-03")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# Let's see if we solved all the issues
bag_data %>%
  filter(class_age == "J1" & code_param == "INDVMORNB") %>%
  group_by(site, date, campaign) %>%
  summarize(nbr_id_diff = length(unique(id_ind))) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  ggplot(aes(x = DOY, y = nbr_id_diff)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign, scales = "free_y") +
  ggtitle("J1 INDVMORNB") # NO more issues !


# We think that data at seeding date may be missing sometimes.
# Data that might be added at the seeding date:
# - INDVIVIVNB: the sum of INDVVIVNB and INDVMORNB at the next visit (or INDVVIVNB at the next visit if INDVMORNB = 0)
# - INDVMORNB: 0 (no death at the seeding date).
# To know if data are missing, we've read the REMORA, RESCO, ECOSCOPA reportw to find out the seeding date or
# the number of visit per site and per year. We've also checked the minimal date for other sites or class_age within
# the same year.

# Before 2009, there were 4 visits per year. We compute the number of INDVVIVNB data and the minimal date per class_age 
# x site x campaign combination (only one bag was followed those years).
# We only visualize the combinations when the number of data is < 4 but > 2. Indeed, if INDVVIVNB is missing for these 
# combinations, we could have 4 INDVVIVNB data per year. Later, we will only keep combinations when we have more at least 
# 4 INDVVIVNB data per site x campaign combination. Therefore, combinations for which we have only 1 or 2 INDVVIVNB will 
# be deleted.
bag_data %>%
  filter(as.numeric(as.character(campaign)) < "2009" & code_param == "INDVVIVNB") %>%
  group_by(site, class_age, campaign) %>%
  summarize(count = n(), min_date = min(date)) %>%
  arrange(campaign, class_age) %>%
  filter(count < 4 & count >2) # 13 potential issues
# site  class_age campaign count min_date
# MARSE J1         1994         3 1994-02-25
# AGNAS J1         1996         3 1996-02-22
# AGNAS N0         1996         3 1996-03-19
# MORLX N0         1998         3 1998-03-02
# GEFOS N0         1999         3 1999-03-02
# QUIBE J1         2000         3 2000-03-21
# QUIBE J1         2003         3 2003-03-21
# -> Seeding date is correct for those combinations. Missing data are later on the year. 

# LARMO J1 2005
bag_data %>%
  filter(campaign == "2005" & code_param == "INDVVIVNB" & class_age == "J1" & site == "LARMO") %>%
  group_by(site, class_age) %>%
  summarize(date = unique(date)) %>%
  arrange(date)
# The data missing are at the seeding date. The seeding date for the other class_age (N0) is 2005-04-01.
to_modify <- bag_data %>%
  filter(campaign == "2005" & site == "LARMO" & class_age == "J1" & date == "2005-06-23") 
# 2005-06-23 is the minimal date
pipoINDVVIVNB <- to_modify %>%
  filter(code_param == "INDVVIVNB") %>%
  mutate(value = as.numeric("220"), date = as.Date("2005-04-01")) # replace by the date at se3ding date and INDVVIVNB
# by INDVIVNB at seeding date. 220 = sum of INDVVIVNB and INDVMORNB at the next visit
pipoINDVMORNB <- to_modify %>%
  filter(code_param == "INDVMORNB") %>%
  mutate(value = as.numeric("0"), date = as.Date("2005-04-01"))
# 0 death at seeding date
bag_data <- rbind(bag_data, pipoINDVVIVNB)
bag_data <- rbind(bag_data, pipoINDVMORNB)
rm(pipoINDVMORNB, pipoINDVVIVNB, to_modify)


# LOIXR and LARMO 2005 --> missing data are not at seeding date 


# COUPE N0 2005
bag_data %>%
  filter(campaign == "2005" & code_param == "INDVVIVNB" & class_age == "N0" & site == "COUPE") %>%
  group_by(site, class_age) %>%
  summarize(date = unique(date)) %>%
  arrange(date)
# The data missing is at the seeding date. The seeding date for COUPE J1 is 2005-03-24. Since INDVMORNB = 0 after the 
# seeding date (2005-03-24), INDVVIVNB at the seeding date = INDVVIVNB at the next visit
to_add <- bag_data %>%
  filter(site == "COUPE" & class_age == "N0" & date == "2005-06-24" & code_param != "TOTVIVPOI") %>%
  mutate(date = as.Date("2005-03-24"))
bag_data <- rbind(bag_data, to_add)
rm(to_add)


# 2005 LOIXR QUIBE --> missing data are not at seeding date.



# For year > 2008 we will read the report and add data when necessary
bag_data %>%
  filter(as.numeric(as.character(campaign)) > "2008" & code_param == "INDVVIVNB") %>%
  group_by(site, class_age, campaign) %>%
  summarize(count = n(), min_date = min(date)) %>%
  arrange(campaign, class_age) %>%
  filter(month(min_date) > 3) %>% # only see combinations when seeding date is after march (which is unusual).
  View() # There might be issues in 2009, 2010, 2011

# 2009
bag_data %>%
  filter((campaign == "2009") & code_param == "INDVVIVNB") %>%
  group_by(class_age, site) %>%
  summarize(date_min = min(date)) %>%
  View()
# The info is only missing for N0 and there is already the info for QUIBE and for J1 .
# Based on the REMORA report the seeding date was 2009-03-09.
max <- bag_data %>%
  filter(class_age == "N0" & campaign == "2009" & !code_param == "TOTVIVPOI" & !site == "QUIBE") %>%
  group_by(batch, site, id_ind) %>%
  filter(date == min(date)) %>%
  summarize(value = sum(value)) # To compute the sum of INDVIVNB and INDVMORND at the second date

to_add <- bag_data %>%
  filter(campaign == "2009" & class_age == "N0" & code_param == "INDVMORNB" & site != "QUIBE") %>%
  group_by(site, class_age, id_ind) %>%
  filter(date == min(date)) %>%
  mutate(date = as.Date("2009-03-09"), value = as.numeric("0")) %>%
  ungroup()
bag_data <- rbind(bag_data, to_add)
rm(to_add)

pipoINDVVIVNB <- bag_data %>%
  filter(campaign == "2009" & class_age == "N0" & code_param == "INDVVIVNB" & site != "QUIBE") %>%
  mutate(date_min = as.Date("2009-03-09")) %>%
  group_by(site, id_ind) %>%
  mutate(date_min_mor = min(date)) %>%
  filter(date == date_min_mor & date_min_mor != date_min) %>%
  ungroup() %>%
  mutate(date = date_min) %>%
  select(-date_min_mor, -date_min, -value)
# Merge and re-order column
to_add <- merge(pipoINDVVIVNB, max, by = c("batch", "site", "id_ind"))
target <- as.vector(names(bag_data))
t <- select(to_add, which(names(to_add) %in% target))
gg <- match(target, names(t))
to_add <- t[, gg]
bag_data <- rbind(bag_data, to_add)
rm(to_add, pipoINDVVIVNB, max, t, gg, target)


# 2010
bag_data %>%
  filter((campaign == "2010") & code_param == "INDVVIVNB") %>%
  group_by(class_age, site) %>%
  filter(date == min(date)) %>%
  summarize(min = min(as.Date(date))) %>%
  View()
# Data are missing for all sites and class_age.
# According to the report, the seeding date is in week 13 for N0 and 11 for J1.
# We thus add INDVMORNB= 0 on date 2010-03-29 for N0 and 2010-03-14 for J1
max <- bag_data %>%
  filter(campaign == "2010" & code_param != "TOTVIVPOI") %>%
  group_by(batch, site, id_ind) %>%
  filter(date == min(date)) %>%
  summarize(value = sum(value), count=n())
# the sum of INDVVIVNB + INDVMORNB is likely wrong for bag 2 and 3 at N0 LOIXR and BLAIN in 2010 because we removed 
# INDVMORNB data after seeding date to not calculate the cumulative mortality. However, the aim here is to make the 
# cumulative mortality = 0 at seeding date so the INDVVIVNB data is of little importance.
to_add <- bag_data %>%
  filter(campaign == "2010" & code_param == "INDVMORNB") %>%
  group_by(site, class_age, id_ind) %>%
  filter(date == min(date)) %>%
  mutate(date = if_else(class_age == "N0", as.Date("2010-03-29"), as.Date("2010-03-14")), value = as.numeric("0")) %>%
  ungroup()

bag_data <- rbind(bag_data, to_add)
rm(to_add)

pipoINDVVIVNB <- bag_data %>%
  filter(campaign == "2010" & code_param == "INDVVIVNB") %>%
  mutate(date_min = if_else(class_age == "N0", "2010-03-29", "2010-03-14")) %>%
  group_by(batch, site, id_ind) %>%
  mutate(date_min_mor = min(date)) %>%
  filter(date == date_min_mor & date_min_mor != date_min) %>%
  ungroup() %>%
  mutate(date = date_min) %>%
  select(-date_min_mor, -date_min, -value) 
# merge and re-order
to_add <- merge(pipoINDVVIVNB, max, by = c("batch", "site", "id_ind"))
target <- as.vector(names(bag_data))
t <- select(to_add, which(names(to_add) %in% target))
gg <- match(target, names(t))
to_add <- t[, gg]
bag_data <- rbind(bag_data, to_add)
rm(to_add, pipoINDVVIVNB, max, t, gg, target)


# 2011
bag_data %>%
  filter((campaign == "2011") & code_param == "INDVVIVNB") %>%
  group_by(class_age, site) %>%
  filter(date == min(date)) %>%
  summarize(min = min(as.Date(date))) %>%
  View()
# For class_age N0, the seeding date is indicated for PENRF on date 2011-03-17.
# For clase_age J1, the seeding date is indicated for BLAIN and PENRF on 2011-03-17.
# The seeding date will be 2011-03-17 for all sites.
max <- bag_data %>%
  filter(campaign == "2011" & !code_param == "TOTVIVPOI") %>%
  group_by(batch, site, id_ind) %>%
  filter(date == min(date)) %>%
  summarize(value = sum(value))

to_add <- bag_data %>%
  filter(campaign == "2011" & code_param == "INDVMORNB" & !(site == "PENRF" & class_age == "N0") &
    !((site == "BLAIN" | site == "PENRF") & class_age == "J1")) %>%
  group_by(site, class_age, id_ind) %>%
  filter(date == min(date)) %>%
  mutate(date = as.Date("2011-03-17"), value = as.numeric("0")) %>%
  ungroup()
bag_data <- rbind(bag_data, to_add)
rm(to_add)

pipoINDVVIVNB <- bag_data %>%
  filter(campaign == "2011" & code_param == "INDVVIVNB" & !(site == "PENRF" & class_age == "N0") &
    !((site == "BLAIN" | site == "PENRF") & class_age == "J1")) %>%
  mutate(date_min = as.Date("2011-03-17")) %>%
  group_by(batch, site, id_ind) %>%
  mutate(date_min_mor = min(date)) %>%
  filter(date == date_min_mor & date_min_mor != date_min) %>%
  ungroup() %>%
  mutate(date = date_min) %>%
  select(-date_min_mor, -date_min, -value)

to_add <- merge(pipoINDVVIVNB, max, by = c("batch", "site", "id_ind"))
target <- as.vector(names(bag_data))
t <- select(to_add, which(names(to_add) %in% target))
gg <- match(target, names(t))
to_add <- t[, gg]
bag_data <- rbind(bag_data, to_add)
rm(to_add, pipoINDVVIVNB, max, gg, t, target)

# bag identified NA are now identified as "unidentified"
bag_data <- bag_data %>%
  mutate(id_ind = if_else(is.na(id_ind), paste("unidentified"), paste(id_ind)))
bag_data$id_ind <- as.factor(bag_data$id_ind)



# Mass data: cleaning and calculation ----------------------------------------------------------------------------------

# Mass at individual level is obtained by calculating the mean of individual mass for each site x date combination.
mass_ind <- TAB %>%
  filter(code_param == "INDVPOID" & fraction == "Sans objet" & method == "Pesée simple sans préparation") %>%
  select(batch, site, value, date) %>%
  group_by(batch, site, date) %>%
  summarize(mean_mtotg = mean(value)) %>%
  mutate(code_param = as.factor("INDVPOID"))

# Mass at bag level is obtained by dividing TOTVIVPOI by INDVVIVNB and then average values by date x batch x site x
# campaign combination
bag_data$campaign <- as.numeric(as.character(bag_data$campaign))

mass_complet <- bag_data %>%
  reshape2::dcast(batch + site + date + id_ind ~ code_param, value.var = "value") %>%
  mutate(campaign = as.numeric(substr(batch, 1, 4))) %>%
  filter((!(is.na(TOTVIVPOI) == TRUE | is.na(INDVVIVNB) == TRUE))) %>%
  mutate(mtot.g = TOTVIVPOI / INDVVIVNB) %>%
  group_by(batch, site, date) %>%
  summarize(mean_mtotg = mean(mtot.g)) %>%
  mutate(code_param = as.factor("TOTVIVPOI")) %>%
  bind_rows(., mass_ind)

mass_complet %>%
  filter(substr(batch, 6, 7) == "N0") %>%
  mutate(campaign = substr(batch, 1, 4)) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  ggplot(aes(x = DOY, y = mean_mtotg, col = code_param)) +
  geom_point(alpha = 0.5) +
  facet_grid(site ~ campaign, scale = "free_y") +
  ggtitle("N0")

mass_complet %>%
  filter(substr(batch, 6, 7) == "J1") %>%
  mutate(campaign = substr(batch, 1, 4)) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  ggplot(aes(x = DOY, y = mean_mtotg, col = code_param)) +
  geom_point(alpha = 0.5) +
  facet_grid(site ~ campaign, scale = "free_y") +
  ggtitle("J1")
# The protocol has changed over the year and we cannot not only use mass data taken at the bag level, otherwise we will
# greatly break the continuity of the time series. Indeed, there is an important gap in data acquisition at the bag
# level between 1999 and 2005. We will thus use mass data taken at individual level until 2008 and data taken at the bag
# level after that.
# We will thus remove the TOTVIVPOI data taken before 2009
to_remove <- bag_data %>%
  filter(campaign < 2009 & code_param == "TOTVIVPOI")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove, mass_complet)


# We will now show the temporal pattern of mass for each id_ind to see if they are some unusual values
# Let's start with N0.
bag_data %>%
  reshape2::dcast(batch + site + date + id_ind ~ code_param, value.var = "value") %>%
  filter((!(is.na(TOTVIVPOI) == TRUE | is.na(INDVVIVNB) == TRUE))) %>%
  mutate(
    campaign = as.numeric(substr(batch, 1, 4)),
    mean_mass_of_individuals = TOTVIVPOI / INDVVIVNB,
    DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))
  ) %>%
  filter(substr(batch, 6, 7) == "N0") %>%
  ggplot(aes(x = DOY, y = mean_mass_of_individuals, col = id_ind)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign, scale = "free_y")
# There are indeed some odd values especially at the end of the year (sharp increase or decrease in mass). This is
# unusual since  the growth should be reduced at the end of the year(according to literature).
# We thus assume that every sharp change in mass at the end of the year is due to errors in either the TOTVIVPOI data
# or INDVVIVNB data. To correct that, we will remove those data from the data set.

# To identify those issues, we first only kept id_ind for which the last visit was in October, November or December 
# (i.e. at the end of the year) when growth should me minimal. For these individuals, we will found the last mass
last_mass <- bag_data %>%
  group_by(batch, site, id_ind) %>%
  mutate(keep = ifelse(substr(max(date), 6, 7) >= 10, "yes", "no")) %>%
  filter(keep == "yes" & date == max(date)) %>%
  reshape2::dcast(batch + site + date + id_ind ~ code_param, value.var = "value") %>%
  filter((!(is.na(TOTVIVPOI) == TRUE | is.na(INDVVIVNB) == TRUE))) %>%
  mutate(mean_mass_of_individuals = TOTVIVPOI / INDVVIVNB) %>%
  select(batch, site, id_ind, date, mean_mass_of_individuals) %>%
  rename(last_date = date, final_mass = mean_mass_of_individuals)
# Then we can find out the second last mass (i.e. the maximum date in bag data once the max date is removed)
second_last_mass <- bag_data %>%
  group_by(batch, site, id_ind) %>%
  mutate(keep = ifelse(substr(max(date), 6, 7) >= 10, "yes", "no")) %>%
  filter(keep == "yes" & date != max(date)) %>%
  filter(date == max(date)) %>%
  reshape2::dcast(batch + site + date + id_ind ~ code_param, value.var = "value") %>%
  filter((!(is.na(TOTVIVPOI) == TRUE | is.na(INDVVIVNB) == TRUE))) %>%
  mutate(mean_mass_of_individuals = TOTVIVPOI / INDVVIVNB) %>%
  select(batch, site, id_ind, date, mean_mass_of_individuals) %>%
  rename(second_last_date = date, second_last_mass = mean_mass_of_individuals)
# we then merge the 2 data sets. Calculate the change in mass and compute the difference in days between the mass data.
# We compile the issues : i.e. when there is an increase or decrease in mass that represents more than 25% of the second
# last mass and that the time elapsed between these date < 40 days.
tab_issues <- merge(last_mass, second_last_mass, by = c("batch", "site", "id_ind"), all.x = TRUE, all.y = TRUE) %>%
  mutate(change_mass = (final_mass * 100) / second_last_mass) %>%
  mutate(diff_date = last_date - second_last_date) %>%
  filter(diff_date < 40 & (change_mass > 125 | change_mass < 75)) %>%
  mutate(issue = "yes") %>%
  select(batch, site, id_ind, last_date, issue) %>%
  rename(date = last_date)

# we then merge back with bag_data to see those issues:
bag_data %>%
  reshape2::dcast(batch + site + date + id_ind + campaign ~ code_param, value.var = "value") %>%
  filter((!(is.na(TOTVIVPOI) == TRUE | is.na(INDVVIVNB) == TRUE))) %>%
  mutate(
    individual_mean_mass = TOTVIVPOI / INDVVIVNB,
    DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))
  ) %>%
  merge(., tab_issues, by = c("batch", "site", "date", "id_ind"), all.x = TRUE) %>%
  mutate(issue = if_else(is.na(issue) == TRUE, paste("no"), paste("yes"))) %>%
  filter(substr(batch, 6, 7) == "N0") %>%
  ggplot(aes(x = DOY, y = individual_mean_mass, col = id_ind)) +
  geom_point(aes(shape = issue, size = issue), alpha = 0.5) +
  geom_line() +
  facet_grid(site ~ campaign, scale = "free_y")

# As we do not know wether these issues come from wrong INDVVIVNB or TOTVIVPOI data, we will check those errors one by
# one to determine if we need to delete the INDVVIVNB and/or TOTVIVPOI data.

# LOIXR 2011_N0_WILD_2n_AR
bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "LOIXR") %>%
  arrange(date) %>%
  View()
# INDVVIVNB and INDVMORNB data are consistent. The issue seems to be on TOTVIVPOI. We thus delete it
to_remove <- bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "LOIXR" & code_param == "TOTVIVPOI" & date == "2011-11-23")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# MORLX 2011_N0_WILD_2n_AR
bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "MORLX") %>%
  arrange(date) %>%
  View() # INDVVIVNB and INDVMORNB data are inconsistent.
# On the last visit, INDVVIVNB + INDVMORNB = 172 whereas INDVVIVNB + INDVMORNB = 137 at the previous visit.
# INDVVIVNB is likely worong, We delete the last visit.
to_remove <- bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "MORLX" & date == "2011-11-23")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# BREST 2011_N0_WILD_2n_AR
bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "BREST" & id_ind == "3") %>%
  arrange(date) %>%
  View() # TOTVIVPOI is inconsistent for id_ind = 3.
# We delete the last TOTVIVPOI data.
to_remove <- bag_data %>%
  filter(batch == "2011_N0_WILD_2n_AR" & site == "BREST" & id_ind == "3" & date == "2011-11-25" & code_param == "TOTVIVPOI")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# LOIXR 2015_N0_HATCH_2n_E4 id_ind = 2 and 3
bag_data %>%
  filter(batch == "2015_N0_HATCH_2n_E4" & site == "LOIXR" & id_ind != "1") %>%
  arrange(date) %>%
  View() # For id_ind 3, the issue is probably linked to INDVVIVNB or INDVMORNB, whereas, for id_ind = 2 the issues is
# on TOTVVIVPOID
# We delete the last visit for bag 3
to_remove <- bag_data %>%
  filter(batch == "2015_N0_HATCH_2n_E4" & site == "LOIXR" & id_ind == "3" & date == "2015-12-14")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# we delete the last TOTVVIVPOI for bag 2
to_remove <- bag_data %>%
  filter(batch == "2015_N0_HATCH_2n_E4" & site == "LOIXR" & id_ind == "2" & date == "2015-12-14" & code_param == "TOTVIVPOI")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# AGNAS 2017_N0_HATCH_2n_E4 and 2018_N0_HATCH_2n_E4 id_ind = 1
bag_data %>%
  filter(batch == "2017_N0_HATCH_2n_E4" & site == "AGNAS" & id_ind == "1") %>%
  arrange(date) %>%
  View()
bag_data %>%
  filter(batch == "2018_N0_HATCH_2n_E4" & site == "AGNAS" & id_ind == "1") %>%
  arrange(date) %>%
  View()
# For both years, the issues is on TOTVIVPOI. We delete the last TOTVIVPOI data.
to_remove <- bag_data %>%
  filter((batch == "2017_N0_HATCH_2n_E4" | batch == "2018_N0_HATCH_2n_E4") & site == "AGNAS" & id_ind == "1" &
    (date == "2017-12-04" | date == "2018-12-07") & code_param == "TOTVIVPOI")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# LETES 2018 does not appear as an aberrant data compared to the other id_ind. We thus do not classify it as an issue
# and won't delete the last data

# We have ruled out some issues at the end of the year but others remain. We have verified those issues but for most of them, 
# we cannot not have a systematic solution. In some cases, the TOTVVIVPOI is likely wrong. We decided to only delete data at
# LOIXR in 2009. Indeed, there is an aberrant mass data on date 2009-08-24.
# The 3 bags are really heavy on that date but they are almost twice lighter at the next visit even though INDVMORNB = 0
bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & site == "LOIXR" & date > "2009-07-22" & date < "2009-09-21") %>%
  arrange(date) %>%
  View()
# We remove TOTVIVPOI data on date 2009-08-24
to_remove <- bag_data %>%
  filter(batch == "2009_N0_WILD_2n_AR" & site == "LOIXR" & date == "2009-08-24" & code_param == "TOTVIVPOI")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove,last_mass, second_last_mass)


# Let's see if we have some issues for J1.
bag_data %>%
  reshape2::dcast(batch + site + date + id_ind + campaign ~ code_param, value.var = "value") %>%
  filter((!(is.na(TOTVIVPOI) == TRUE | is.na(INDVVIVNB) == TRUE))) %>%
  mutate(
    individual_mean_mass = TOTVIVPOI / INDVVIVNB,
    DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))
  ) %>%
  merge(., tab_issues, by = c("batch", "site", "date", "id_ind"), all.x = TRUE) %>%
  mutate(issue = if_else(is.na(issue) == TRUE, paste("no"), paste("yes"))) %>%
  filter(substr(batch, 6, 7) == "J1") %>%
  ggplot(aes(x = DOY, y = individual_mean_mass, col = id_ind)) +
  geom_point(aes(shape = issue, size = issue), alpha = 0.5) +
  geom_line() +
  facet_grid(site ~ campaign, scale = "free_y") # doing this plot we have a warning message "Removed 2 rows containing 
# missing values (geom_point)." This is not an issue. Indeed, we sometimes have INDVVIVNB = 0 (i.e. all individual 
# are dead). We keep those data because cumulative mortality will be 100 %, but we can't calculate the mean mass by 
# dividing INDVVIVNB by TOTVIVPOI. This is impossible (NaN). Hence, this warning message.

# Some issues are flagged because the second last mass is odd (e.g., GEFOSS 2012). We will not automaticly removed those 
# issues. We checked them one by one. We will only modify the following obvious issues:

# COUPE 2009
# There is a sharp decrease in the mean mass of individuals in August and September. Let's compare the trend in mean 
# mass of individuals and the mean of individual mass.
Coupe_2009_bag_data <- bag_data %>%
  filter(class_age == "J1" & site == "COUPE" & campaign == "2009") %>%
  reshape2::dcast(batch + site + date + id_ind + campaign ~ code_param, value.var = "value") %>%
  filter((!(is.na(TOTVIVPOI) == TRUE | is.na(INDVVIVNB) == TRUE))) %>%
  mutate(individual_mean_mass = TOTVIVPOI / INDVVIVNB) %>%
  ggplot(aes(x = date, y = individual_mean_mass, col = id_ind)) +
  geom_point() +
  geom_line() +
  ylim(20, 50) +
  scale_x_date(limits = as.Date(c("2009-03-01", "2009-12-05"))) +
  theme(legend.position = "none") +
  ylab("mean mass of individuals")

Coupe_2009_individual_data <- TAB %>%
  filter(class_age == "J1" & site == "COUPE" & campaign == "2009" & code_param == "INDVPOID" &
    fraction == "Sans objet" & method == "Pesée simple sans préparation") %>%
  group_by(date) %>%
  summarize(mean_mass = mean(value)) %>%
  ggplot(aes(x = date, y = mean_mass)) +
  geom_point() +
  geom_line() +
  ylim(20, 50) +
  ylab("mean of individual mass") +
  scale_x_date(limits = as.Date(c("2009-03-01", "2009-12-05")))

ggarrange(Coupe_2009_bag_data, Coupe_2009_individual_data)
rm(Coupe_2009_bag_data, Coupe_2009_individual_data)
# This decrease in mean mass of individuals is odd since it is not observed with mass taken at the individual level 
# (mean of individual mass).
# The INDVVIVNB on date 2009-09-17 and 2009-08-18 seem consistent with previous dates. It is thus more likely that
# there is a mistake on TOTVIVPOI.
# we delete the mass for all bags on date 2009-09-17 and 2009-08-18.
to_remove <- bag_data %>%
  filter(site == "COUPE" & class_age == "J1" & code_param == "TOTVIVPOI" & (date == "2009-08-18" | date == "2009-09-17"))
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# GEFOS 2010 :
bag_data %>%
  filter(batch == "2010_J1_WILD_2n_AR" & site == "GEFOS") %>%
  arrange(date) %>%
  View()
# On date 2010-07-12, mass for id_ind = 2 is 1050g, whereas, mass before and after is around 10000g.
# I've checked the field sheet and TOTVIVPOI should be 5.42 kg + 5.08 kg, hence 10500 g. A "0" is missing.
# We correct that.
to_remove <- bag_data %>%
  filter(batch == "2010_J1_WILD_2n_AR" & site == "GEFOS" & date == "2010-07-12" & code_param == "TOTVIVPOI" &
    id_ind == "2")
bag_data <- anti_join(bag_data, to_remove)
# we add the row with the good information
to_add <- to_remove %>%
  mutate(value = as.numeric("10500"))
bag_data <- rbind(bag_data, to_add)
rm(to_remove, to_add)

# MARSE 2012 :
bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "MARSE") %>%
  arrange(date) %>%
  View()
# For id_ind = 6: on date 2012-06-04, INDVVIVNB = 13 and INDVMORNB = 3.
# However, on the next visit (2012-06-18) INDVVIVNB = 40 and INDVIVMORNB = 2.INVVIVNB on date 2012-06-04 is likely wrong.
# we delete INDVVIVNB on that date and INDVMORNB data after that date since it will influence the cumulative mortality.
# For bag 4 the issue is the same. INDVVIVNB = 16  and INDVMORNB = 0. However on the previous visit and the next one
# INDVVIVNB is around 40.
to_remove <- bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "MARSE" & date > "2012-05-21" & code_param == "INDVMORNB" &
    (id_ind == "4" | id_ind == "6"))
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)
# We remove INDVVIVNB for that date.
to_remove <- bag_data %>%
  filter(batch == "2012_J1_WILD_2n_AR" & site == "MARSE" & date == "2012-06-04" & code_param == "INDVVIVNB" &
    (id_ind == "4" | id_ind == "6"))
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)



# MORLX 2014 : id_ind = 3
bag_data %>%
  filter(batch == "2014_J1_WILD_2n_AR" & site == "MORLX" & id_ind == "3") %>%
  arrange(date) %>%
  View()
# on the last visit INDVVIVNB = 163, whereas at the previous visit INDVVIVNB + INDVMORNB = 105. This is very
# inconsistent and we remove the last visit for bag 3.
to_remove <- bag_data %>%
  filter(batch == "2014_J1_WILD_2n_AR" & site == "MORLX" & date == "2014-12-08" & id_ind == "3")
bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)


# BLAIN 2016 : id_ind =2
bag_data %>%
  filter(batch == "2016_J1_HATCH_2n_E4" & site == "BLAIN" & id_ind=="2") %>%
  arrange(date) %>%
  View()
# On date 2016-11-18 INDVVIVNB = 238 but later on 2016-12-01 INDVVIVNB = 328. There is a mistake on INDVVIVNB.
# It should have been 238 (because INDVMORNB = 0).
# we remove the wrong INDVVIVNB and add the good one.
to_remove <- bag_data %>%
  filter(batch == "2016_J1_HATCH_2n_E4" & site == "BLAIN" & date == "2016-12-01" & id_ind == "2" &
    code_param == "INDVVIVNB")
bag_data <- anti_join(bag_data, to_remove)
to_add <- to_remove %>%
  mutate(value = as.numeric("238"))
bag_data <- rbind(bag_data, to_add)
rm(to_add, to_remove)


bag_data %>%
  reshape2::dcast(batch + site + date + id_ind + campaign ~ code_param, value.var = "value") %>%
  filter((!(is.na(TOTVIVPOI) == TRUE | is.na(INDVVIVNB) == TRUE))) %>%
  mutate(
    individual_mean_mass = TOTVIVPOI / INDVVIVNB,
    DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))
  ) %>%
  merge(., tab_issues, by = c("batch", "site", "date", "id_ind"), all.x = TRUE) %>%
  mutate(issue = if_else(is.na(issue) == TRUE, paste("no"), paste("yes"))) %>%
  filter(substr(batch, 6, 7) == "J1") %>%
  ggplot(aes(x = DOY, y = individual_mean_mass, col = id_ind)) +
  geom_point(aes(shape = issue, size = issue), alpha = 0.5) +
  geom_line() +
  facet_grid(site ~ campaign, scale = "free_y")
# We remove some of the issues!
rm(tab_issues)



# we will now recalculate the mean mass based on the cleaned data set
# For data taken after 2008 (mass taken at the bag level), we first compute a mean mass per bag x batch x site
# x date combination (by dividing TOTVIVPOI by INDVVIVNB) and then compute the mean mass of individuals per batch x site
# x date combination
mass_bag_level <- bag_data %>%
  reshape2::dcast(batch + site + date + id_ind ~ code_param, value.var = "value") %>%
  mutate(campaign = as.numeric(substr(batch, 1, 4))) %>%
  filter((!(is.na(TOTVIVPOI) == TRUE | is.na(INDVVIVNB) == TRUE))) %>%
  filter(campaign > 2008) %>%
  mutate(mean_mass_of_individuals = TOTVIVPOI / INDVVIVNB) %>%
  group_by(batch, site, date) %>%
  summarize(avg_mean_mass_of_individuals = mean(mean_mass_of_individuals))

# For data taken at the individual level, we compute the mean of individual mass per batch x site x date x combination 
# as they are no bag identification for these data. Indeed, there are id_ind for INDVVIVPOID but it refers to the id_ind
# of the individual not the bag
mass_ind_level <- TAB %>%
  droplevels() %>%
  filter(code_param == "INDVPOID" & fraction == "Sans objet" & method == "Pesée simple sans préparation") %>%
  group_by(batch, site, date) %>%
  summarize(mean_of_individual_mass = mean(value)) %>%
  ungroup() %>%
  group_by(batch, site) %>%
  filter(!(substr(batch, 1, 4) > 2008 & date > min(date))) # we keep the data at the min date because sometimes, data were
# only taken at the individual level at seeding date.

# We merge the two data sets
mass_final <- merge(mass_bag_level, mass_ind_level, by = c("batch", "site", "date"), all.x = TRUE, all.y = TRUE)
rm(mass_bag_level, mass_ind_level)

mass_final %>%
  pivot_longer(
    cols = c(mean_of_individual_mass, avg_mean_mass_of_individuals), names_to = "variables",
    values_to = "mesures"
  ) %>%
  mutate(class_age = as.factor(substr(batch, 6, 7))) %>%
  mutate(campaign = as.factor(substr(batch, 1, 4))) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  filter(class_age == "N0") %>%
  ggplot() +
  geom_point(aes(y = mesures, x = DOY, col = variables, shape = variables), alpha = 0.5, size = 2) +
  facet_grid(site ~ campaign, scales = "free_y") +
  theme(legend.position = "bottom")

# In 2009,2010 and 2011 we do not have TOTVVIVPOI at seeding date (the same is true for class_age = J1).
# To be consistent, we keep the data taken at the individual level at seeding date for all year x combination,
# whether or not we had one at the bag level.
mass_final <- mass_final %>%
  mutate(mean_mass = if_else(is.na(mean_of_individual_mass) != T, as.numeric(paste(mean_of_individual_mass)),
    as.numeric(paste(avg_mean_mass_of_individuals))
  )) %>%
  select(-avg_mean_mass_of_individuals, -mean_of_individual_mass)

# We only keep site x year combination x class_age for which we have at least 4 data a year:
to_remove <- mass_final %>%
  group_by(batch, site) %>%
  summarise(n_count = n()) %>%
  mutate(keep = ifelse(n_count >= 4, "yes", "no")) %>%
  filter(keep == "yes") %>%
  ungroup() %>%
  select(-keep, -n_count)

mass_final <- left_join(x = to_remove, y = mass_final)
rm(to_remove)

# To ensure that the life-cycle indicators are as comparable as possible between campaign and site
# (i.e. estimated in a common restricted time window), we remove site x campaign combinations when the campaign ended
# before October, because the growth or mortality may still be in the exponential phase during these end-of-follow-up
# periods.
# Lets see the data for which data acquisition ended before October
mass_final %>%
  group_by(batch, site) %>%
  mutate(keep = ifelse(substr(max(date), 6, 7) >= 10, "yes", "no")) %>%
  filter(keep == "no") %>%
  ungroup() %>%
  group_by(batch, site, date) %>%
  mutate(campaign = as.numeric(substr(batch, 1, 4)), class_age = substr(batch, 6, 7)) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  ggplot(aes(x = DOY, y = mean_mass, col = class_age)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign) +
  scale_x_continuous(limits = c(0, 365))

# we remove those data
mass_final <- mass_final %>%
  group_by(batch, site) %>%
  mutate(keep = ifelse(substr(max(date), 6, 7) >= 10, "yes", "no")) %>%
  filter(keep == "yes") %>%
  select(-keep)

# graphics
mass_final %>%
  mutate(campaign = substr(batch, 1, 4)) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  mutate(class_age = substr(batch, 6, 7)) %>%
  ggplot(aes(x = DOY, y = mean_mass, col = class_age)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign) +
  scale_x_continuous(limits = c(0, 365))


# Mortality data: calculation ------------------------------------------------------------------------------------------

# For MARSE J1 2011 and 2009, both INDVVIVNB and INDVMORNB are sometimes equal to 0. We need to remove those lines as we 
# can't calculate a cumulative mortality. Indeed, the instantaneous mortality (INDVMORNB / (INDVVIVNB + INDVMORNB)) can't 
# be calculated (0/0 + 0 does not exists).
to_remove <- bag_data %>%
  filter(site == "MARSE" & ( (batch == "2011_J1_WILD_2n_AR" & id_ind == "1" & (date == "2011-09-26" | date == "2011-09-13"))
                              | (batch == "2009_J1_WILD_2n_AR" & id_ind==5 & date=="2009-06-23")))

bag_data <- anti_join(bag_data, to_remove)
rm(to_remove)

# We only calculate the cumulative mortality when we have a value for both INDVMORNB and INDVVIVNB
morta <- bag_data %>%
  reshape2::dcast(batch + site + date + id_ind ~ code_param, value.var = "value") %>%
  mutate(campaign = as.numeric(substr(batch, 1, 4))) %>%
  filter(!(is.na(INDVMORNB) == TRUE | is.na(INDVVIVNB) == TRUE)) %>%
  select(-TOTVIVPOI)

# We create a table to store the computed cumulative mortality
morta2 <- morta[1, ]
morta2$date <- as.Date(morta2$date[1], format = "%Y-%m-%d")
morta2$INDVVIVNB <- NA
morta2$INDVMORNB <- NA
morta2$MI <- 0 # mortality at the visit
morta2$MC <- 0 # cumulative mortality since the beginning of the annual campaign
morta2$id_ind <- NA

for (c in levels(as.factor(morta$campaign))) { # selection of the annual campaign
  camp <- droplevels(subset(morta, campaign == c))

  for (s in levels(as.factor(camp$site))) { # selection of the location
    site <- droplevels(subset(camp, site == s))

    for (a in levels(as.factor(site$batch))) { # selection of the batch
      batch <- droplevels(subset(site, batch == a))

      for (b in levels(as.factor(batch$id_ind))) { # selection of the poche
        id_ind <- droplevels(subset(batch, id_ind == b))
        annee <- substr(id_ind$date[1], 1, 4)
        id_ind[length(id_ind$batch) + 1, ] <- id_ind[1, ]
        # I add the origin of the annual time-serie : January the 1st
        id_ind[length(id_ind$batch), "date"] <- as.Date(paste(annee, "/01/01", sep = ""))
        # At this date, number of living individuals = the oyster count at deployment and INDVMORNB = 0
        id_ind[length(id_ind$batch), "INDVVIVNB"] <- id_ind[1, "INDVVIVNB"]
        id_ind[length(id_ind$batch), "INDVMORNB"] <- 0
        id_ind <- id_ind[order(id_ind$date), ]
        id_ind$MI <- id_ind$INDVMORNB / (id_ind$INDVMORNB + id_ind$INDVVIVNB)
        id_ind$MC <- 0

        for (i in 2:length(id_ind$date)) {
          id_ind[i, "MC"] <- 1 - ((1 - id_ind[i - 1, "MC"]) * (1 - id_ind[i, "MI"]))
        }
        rm(i)
        morta2 <- rbind(morta2, id_ind[-1, ])
        rm(id_ind)
      }
      rm(batch)
    }
    rm(site)
  }
  rm(camp)
}
morta2 <- morta2[-1, ] # remove the row we created to initialize the loop

rm(morta, a, annee, b, c, s)

morta2$id_ind <- as.factor(morta2$id_ind)

# We first see if there are odd cumulative mortality when we followed several bags (after 2008)
morta2 %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  filter(substr(batch, 6, 7) == "N0" & campaign > 2008) %>%
  ggplot(aes(x = DOY, y = MC, col = id_ind)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign, scales = "free_y") +
  ggtitle("N0") # Nothing suspicious

morta2 %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  filter(substr(batch, 6, 7) == "J1" & campaign > 2008) %>%
  ggplot(aes(x = DOY, y = MC, col = id_ind)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign, scales = "free_y") +
  ggtitle("J1") # Nothing suspicious

# We now compute the mean CM per batch x site x date combination
morta_final <- morta2 %>%
  group_by(batch, site, date) %>%
  summarize(mean_CM = mean(MC))

# We only keep site  x year combination for which we have at least 4 observations
morta_final <- morta_final %>%
  group_by(batch, site) %>%
  mutate(n_count = n()) %>%
  mutate(keep = ifelse(n_count >= 4, "yes", "no")) %>%
  filter(keep == "yes") %>%
  ungroup() %>%
  select(-keep, -n_count)

rm(morta2)

# To ensure that the life-cycle indicators are as comparable as possible between campaign and site
# (i.e. estimated in a common restricted time window), we remove site x campaign combinations when the campaign ended
# before October, because the mortality may still be in the exponential phase during these end-of-follow-up
# periods
# Lets see the data for which data acquisition ended before October
morta_final %>%
  mutate(class_age = as.factor(substr(batch, 6, 7))) %>%
  mutate(campaign = as.factor(substr(batch, 1, 4))) %>%
  group_by(batch, site) %>%
  mutate(keep = ifelse(substr(max(date), 6, 7) >= 10, "yes", "no")) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  filter(keep == "no") %>%
  ungroup() %>%
  ggplot(aes(x = DOY, y = mean_CM, col = class_age)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign) +
  scale_x_continuous(limits = c(0, 365))


morta_final <- morta_final %>%
  group_by(batch, site) %>%
  mutate(keep = ifelse(substr(max(date), 6, 7) >= 10, "yes", "no")) %>%
  filter(keep == "yes") %>%
  select(-keep)

morta_final %>%
  mutate(campaign = substr(batch, 1, 4)) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d"))) %>%
  mutate(class_age = substr(batch, 6, 7)) %>%
  ggplot(aes(x = DOY, y = mean_CM, col = class_age)) +
  geom_point() +
  geom_line() +
  facet_grid(site ~ campaign) +
  scale_x_continuous(limits = c(0, 365))


# Aggregation of cumulative mortality and mass data --------------------------------------------------------------------

morta_mass_complet <- merge(morta_final, mass_final, by = c("batch", "site", "date"), all.x = TRUE, all.y = TRUE)
rm(mass_final, morta_final)

morta_mass_complet <- morta_mass_complet %>%
  mutate(class_age = as.factor(substr(batch, 6, 7))) %>%
  mutate(campaign = as.factor(substr(batch, 1, 4))) %>%
  mutate(DOY = julian(date) - julian(as.Date(paste(campaign, "/01/01", sep = ""), format = "%Y/%m/%d")))

morta_mass_complet$mean_CM <- formattable(morta_mass_complet$mean_CM, digits = 3, format = "f")
morta_mass_complet$mean_mass <- formattable(morta_mass_complet$mean_mass, digits = 2, format = "f")

# Final graphics
morta_mass_complet %>%
  select(-mean_mass) %>%
  drop_na() %>%
  ggplot(aes(x = DOY, y = mean_CM, col = class_age, group = class_age)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 365, by = 100), limits = c(0, 365)) +
  facet_grid(site ~ campaign, scale = "free_y")

morta_mass_complet %>%
  select(-mean_CM) %>%
  drop_na() %>%
  ggplot(aes(x = DOY, y = mean_mass, col = class_age, group = class_age)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 365, by = 100), limits = c(0, 365)) +
  facet_grid(site ~ campaign, scale = "free_y")



# We add the information about the sites
sites <- read.csv(here("data/raw/sites.csv"))
sites <- sites %>%
  mutate(site = as.factor(site), num = as.factor(paste(num))) %>% 
  select(-zone_fr)

morta_mass_complet = merge(morta_mass_complet, sites, by=c("site"))

# reorder column
morta_mass_complet <- morta_mass_complet[, c(9,1,10,11,12,13,7,6,2,3,8,4,5)]

# save clean dataset
write.csv(morta_mass_complet, "data/clean/DataResco_clean.csv", row.names = FALSE, quote = TRUE, na = "NA")

# This data set is available on Zenodo (10.5281/zenodo.5744977)