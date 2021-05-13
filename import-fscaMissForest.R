source("code/libraries.R")

##############
# Main FSCA BS
##############

path <- paste0("data/fsca/", list.files("data/fsca/", pattern = "Report_F_"))

ldf <- path %>% 
  map(read_excel, skip = 2)

names(ldf) <- substr(list.files("data/fsca/", pattern = "Report_F_"),10,13)

ldf <- lapply(ldf, function(x) gather(x, item, value, 1:40))

df_fsca_raw <- bind_rows(ldf, .id = "period") %>%
  as_tibble()

names(df_fsca_raw) <- c("period", "fund_no", "fund_name", "fund_class", "fund_subtype", "fund_type",
                    "fund_substatus", "benefit_structure", "item", "value")

write_rds(df_fsca_raw, "data/fsca/df_fsca_raw.rds","gz")

###########################
# Mapping FSCA BS Detail
##########################

path <- paste0("data/fsca/", list.files("data/fsca/", pattern = "Report_Invest_"))

ldf <- path %>% 
  map(read_excel, skip = 2)

names(ldf) <- substr(list.files("data/fsca/", pattern = "Report_Invest_"),15,18)

ldf <- lapply(ldf, function(x) x %>%
                gather(item, value, 1:14) %>%
                #select(item,value) %>%
                mutate(value = gsub(",","", value)))


fsca_ret_inv <- bind_rows(ldf, .id = "period") %>%
  as_tibble()

names(fsca_ret_inv) <- c("period", "fund_no", "fund_name",
                         "fund_class", "fund_subtype",
                         "fund_type", "fund_substatus",
                         "benefit_structure", "item", "value")

write_rds(fsca_ret_inv, "data/fsca/fsca_ret_inv.rds","gz")

########################
# Mapping FSCA BS Aggregate
########################

fsca_mod_ass_dom <- read_excel(path = "data/fsca/Private pension data.xlsx",
                               sheet = "FSCA mapping",
                               range = params_range_privpens_dom,
                               col_names = F) %>%
  as_tibble()

names(fsca_mod_ass_dom) <- c("item", 2010:params_fsca_year) 



#########filter###########

filterB <- setNames(data.frame(t(fsca_mod_ass_dom[,-1])), fsca_mod_ass_dom[[1]])

x<- filterB
outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)}

numeric_cols<-names(filterB)[sapply(filterB, is.numeric)]
numeric_data<-filterB[,names(filterB)%in%numeric_cols]
Outlier_miss<-as.data.frame(sapply(numeric_data,outlierTreament))


filter1_fixB<-missForest(Outlier_miss)$xim
period <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018)
filter1_fixB$period <- period
names(filter1_fixB) #41

###make after 2014 zero####

which(filter1_fixB$period == "2014")
filter1_fixB$`Loans (other than housing loans)`[which(filter1_fixB$period == "2014")] <- 0

which(filter1_fixB$period == "2015")
filter1_fixB$`Loans (other than housing loans)`[which(filter1_fixB$period == "2015")] <- 0

which(filter1_fixB$period == "2016")
filter1_fixB$`Loans (other than housing loans)`[which(filter1_fixB$period == "2016")] <- 0

which(filter1_fixB$period == "2017")
filter1_fixB$`Loans (other than housing loans)`[which(filter1_fixB$period == "2017")] <- 0

which(filter1_fixB$period == "2018")
filter1_fixB$`Loans (other than housing loans)`[which(filter1_fixB$period == "2018")] <- 0


filter1_fixB1 <- setNames(data.frame(t(filter1_fixB[,-41])), filter1_fixB[[41]])
filter1_fixB1$item <- rownames(filter1_fixB1)

fsca_mod_ass_dom<-filter1_fixB1

write_rds(fsca_mod_ass_dom, "data/fsca/fsca_mod_ass_dom.rds","gz")

################End#########

fsca_mod_ass_for <- bind_cols(
  read_excel(path = "data/fsca/Private pension data.xlsx",
             sheet = "FSCA mapping",
             range = params_range_privpens_dom,
             col_names = F)[1],
  read_excel(path = "data/fsca/Private pension data.xlsx",
             sheet = "FSCA mapping",
             range = params_range_privpens_for,
             col_names = F) 
) %>%
  as_tibble()

names(fsca_mod_ass_for) <- c("item", 2010:params_fsca_year) 



#################filter##########

filterB1 <- setNames(data.frame(t(fsca_mod_ass_for[,-1])), fsca_mod_ass_for[[1]])

x<- filterB1
outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)}

numeric_cols<-names(filterB1)[sapply(filterB1, is.numeric)]
numeric_data<-filterB1[,names(filterB1)%in%numeric_cols]
Outlier_miss<-as.data.frame(sapply(numeric_data,outlierTreament))

filter1_fixB1<-missForest(Outlier_miss)$xim
period <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018)

filter1_fixB1$period <- period
filter1_fixB1['Property, plant and equipment'] <- NA
filter1_fixB1['Land and buildings'] <- NA
filter1_fixB1['Computer equipment and software'] <- NA
filter1_fixB1['Office equipment'] <- NA
filter1_fixB1['Furniture and Fittings'] <- NA
filter1_fixB1['Leasehold and improvements'] <- NA
filter1_fixB1['Cash at bank'] <- NA
filter1_fixB1['Kruger Rands'] <- NA
filter1_fixB1['Transfers receivable'] <- NA
filter1_fixB1['Accounts receivable'] <- NA
filter1_fixB1['Contributions receivable'] <- NA
filter1_fixB1['Surplus improperly utilised receivable'] <- NA
filter1_fixB1['tot_ass_for'] <- NA



names(filter1_fixB1) #28

filter1_fixB2 <- setNames(data.frame(t(filter1_fixB1[,-28])), filter1_fixB1[[28]])
filter1_fixB2$item <- rownames(filter1_fixB2)


fsca_mod_ass_for <- filter1_fixB2

write_rds(fsca_mod_ass_for, "data/fsca/fsca_mod_ass_for.rds","gz")


############End#################

fsca_mod_liab_dom <- read_excel(path = "data/fsca/Private pension data.xlsx",
                                sheet = "FSCA mapping",
                                range = params_range_privpens_liab,
                                col_names = F) %>%
  as_tibble()

names(fsca_mod_liab_dom) <- c("item", 2010:params_fsca_year) 




############filter#######
filterB1 <- setNames(data.frame(t(fsca_mod_liab_dom[,-1])), fsca_mod_liab_dom[[1]])

x<- filterB1
outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)}

numeric_cols<-names(filterB1)[sapply(filterB1, is.numeric)]
numeric_data<-filterB1[,names(filterB1)%in%numeric_cols]
Outlier_miss<-as.data.frame(sapply(numeric_data,outlierTreament))



filter1_fixB1<-missForest(Outlier_miss)$xim
period <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018)
filter1_fixB1$period <- period
names(filter1_fixB1) #18

filter1_fixB2 <- setNames(data.frame(t(filter1_fixB1[,-18])), filter1_fixB1[[18]])
filter1_fixB2$item <- rownames(filter1_fixB2)

fsca_mod_liab_dom <- filter1_fixB2

write_rds(fsca_mod_liab_dom, "data/fsca/fsca_mod_liab_dom.rds","gz")

############End#########

#################
# Mapping FSCA IS
#################

path <- "data/fsca/FSCA - K48 Mapping Income Statement.xlsx"

df_fsca_is <- read_excel(
  path = path,
  col_names = F,
  sheet = "FSCA Mapping")[-1,] %>%
  as_tibble()

names(df_fsca_is) <- c("item", df_fsca_is[1,2:length(df_fsca_is)])

df_fsca_is <- df_fsca_is[-1,] %>%
  gather(period, value, 2:length(.)) %>%
  filter(!is.na(item))



#########filter#############

df_filter <- df_fsca_is %>% pivot_wider(names_from = "item", values_from = "value")

x<- df_filter
outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)}

numeric_cols<-names(df_filter)[sapply(df_filter, is.numeric)]
numeric_data<-df_filter[,names(df_filter)%in%numeric_cols]
Outlier_miss<-as.data.frame(sapply(numeric_data,outlierTreament))

Impute_rf<-missForest(Outlier_miss)$ximp   

period <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018)
Impute_rf$period <- period

names(Impute_rf) ##68

df_fsca_is_rf <- Impute_rf %>% pivot_longer((names(df_filter)[-1]), names_to = "item", values_to = "value")
df_fsca_is_rf$period <- as.character(as.numeric(df_fsca_is_rf$period))


df_fsca_is <- df_fsca_is_rf

write_rds(df_fsca_is, "data/fsca/df_fsca_is.rds","gz")

#########End###############

###########################
# Mapping FSCA IS Detail
##########################

path <- paste0("data/fsca/", list.files("data/fsca/", pattern = "Report_G"))

ldf <- path %>% 
  map(read_excel, skip = 2)

names(ldf) <- substr(list.files("data/fsca/", pattern = "Report_G"),10,13)

ldf <- lapply(ldf, function(x) x %>%
                gather(item, value, 1:36))

fsca_is_report_g <- bind_rows(ldf, .id = "period") %>%
  as_tibble()

names(fsca_is_report_g) <- c("period", "fund_no", "fund_name",
                         "fund_class", "fund_subtype",
                         "fund_type", "fund_substatus",
                         "benefit_structure", "item", "value")

write_rds(fsca_is_report_g, "data/fsca/fsca_is_report_g.rds","gz")

rm(list=ls()[!str_detect(ls(),"params")])
