source("code/libraries.R")

df_fsca <- read_rds("data/fsca/df_fsca_raw.rds") %>%
  mutate(fund_name = tolower(fund_name),
         #fund_name = gsub("\\s*\\([^\\)]+\\)","",fund_name),
         fund_name = gsub("the","",fund_name),
         fund_name = gsub("s a","sa",fund_name),
         fund_name = gsub("\\.","",fund_name),
         fund_name = gsub("-"," ",fund_name),
         fund_name = gsub("'","",fund_name),
         fund_name = gsub("south african","sa",fund_name),
         fund_name = gsub("south africa","sa",fund_name),
         fund_name = str_squish(fund_name))

write_rds(df_fsca, "data/fsca/df_fsca.rds","gz")

###########################

url <- "http://srv01657.resbank.co.za:10010/wsstack/services/SARB_TSA_PRD?wsdl"

tSeries <- c("CMJM073E", "JFIA001E")

#tSeriesAnnual <- c("BATT144A", "BATT145A")

for (i in tSeries) {
  
  body <- paste0('<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:urn="urn:com-softwareag-entirex-rpc:RMT1-RMT931N1">
                 <soap:Header/>
                 <soap:Body>
                 <urn:RMT931N1> 
                 <User-id>P521752</User-id>
                 <Code>',i,"</Code>
                 <List>
                 <!--0 to 151 repetitions:-->
                 <string>?</string>
                 </List>
                 <EOF>?</EOF>
                 <FIRST-PERIOD>20111100</FIRST-PERIOD>
                 <LAST-PERIOD>","20401200","</LAST-PERIOD>
                 </urn:RMT931N1>
                 </soap:Body>
                 </soap:Envelope>")
  
  raw <- POST(url = url, 
              body = body)
  Sys.sleep(0.01)
  #stop_for_status(raw)
  tmp <- read_xml(raw)
  
  # df %>% 
  #   xml_find_all("//*") %>% 
  #   xml_path()
  
  df <- 
    tmp %>% 
    xml_find_all("//List") %>% 
    str_split(pattern = "\n", 
              simplify = T) 
  
  df <- df[-1:-3]
  df <- df[-length(df)]
  
  df <- data.frame(do.call("rbind",
                           strsplit(as.character(df),
                                    "~",
                                    fixed = TRUE)))
  
  
  df$X4 <- as.numeric(as.character(df$X4))
  df$X3 <- as.Date(paste0(substr(df$X3,1,4),
                          "-",
                          substr(df$X3,5,6),
                          "-",
                          "01"))
  
  df <-
    df %>%
    select(code = X2,
           date = X3,
           value = X4) %>%
    mutate(code = as.character(code))
  
  assign(paste0(i),df)
  
}

write_rds(CMJM073E, "data/CMJM073E.rds","gz")
write_rds(JFIA001E, "data/JFIA001E.rds","gz")

forecast <- bind_rows(read_rds("data/CMJM073E.rds"), read_rds("data/JFIA001E.rds")) %>%
  group_by(code, quarter = quarter(date), year = year(date)) %>%
  filter(date == max(date) & value != 0) %>%
  group_by(code) %>%
  mutate(qq = value/lag(value)-1) %>%
  group_by(date) %>%
  summarise(comp = mean(qq, na.rm = T)) %>%
  group_by(year = year(date)) %>%
  summarise(final = sum(comp)) %>%
  mutate(year = as.character(year)) %>%
  filter(year == as.character(params_forecast1) | 
           year == as.character(params_forecast2) |
           year == as.character(params_forecast3)) %>%
  mutate(year = case_when(year == as.character(params_forecast1) ~ as.character(params_forecast1-1),
                          year == as.character(params_forecast2) ~ as.character(params_forecast2-1),
                          year == as.character(params_forecast3) ~ as.character(params_forecast3-1),
                          TRUE ~ NA_character_))

########################## chec filter at the bottom

tot_fsca <- rbind(
  
  #df_fsca %>%
    #filter(item == "TOTAL ASSETS" & fund_substatus == "NORMAL ACTIVE FUND") %>%
    #group_by(period, fund_class) %>%
    #summarise(value = sum(value,na.rm=T)) %>%
  df_fsca_is_rf1 %>% #filter#
    ungroup(),
  
  #df_fsca %>%
    #filter(item == "TOTAL ASSETS" & fund_substatus == "NORMAL ACTIVE FUND") %>%
    #group_by(period, fund_class) %>%
    #summarise(value = sum(value,na.rm=T)) %>%
  df_fsca_is_rf1 %>%  #filter#
    left_join(., forecast, by = c("period" = "year")) %>%
    filter(period == as.character(params_forecast1-1)) %>%
    mutate(value = value*(1+final)) %>%
    ungroup() %>%
    mutate(period = as.character(params_forecast2-1)) %>%
    select(-final)
  )

tot_fsca <- rbind(
  tot_fsca,
  tot_fsca %>%
    left_join(., forecast, by = c("period" = "year")) %>%
    filter(period == as.character(params_forecast2-1)) %>%
    mutate(value = value*(1+final)) %>%
    ungroup() %>%
    mutate(period = as.character(params_forecast2)) %>%
    select(-final)
)

tot_fsca <- rbind(
  tot_fsca,
  tot_fsca %>%
    left_join(., forecast, by = c("period" = "year")) %>%
    filter(period == as.character(params_forecast3-1)) %>%
    mutate(value = value*(1+final)) %>%
    ungroup() %>%
    mutate(period = as.character(params_forecast3)) %>%
    select(-final)
)

write_rds(tot_fsca, "data/fsca/tot_fsca.rds","gz")

##############Filter###########
df_fsca %>%
  filter(item == "TOTAL ASSETS" & fund_substatus == "NORMAL ACTIVE FUND") %>%
  group_by(period, fund_class) %>%
  summarise(value = sum(value,na.rm=T)) -> df_fsca_filter

df_fsca_filter1 <- df_fsca_filter %>% pivot_wider(names_from = "fund_class", values_from = "value")

x<- df_fsca_filter1
outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)}

numeric_cols<-names(df_fsca_filter1)[sapply(df_fsca_filter1, is.numeric)]
numeric_data<-df_fsca_filter1[,names(df_fsca_filter1)%in%numeric_cols]
Outlier_miss<-as.data.frame(sapply(numeric_data,outlierTreament))

Impute_rf<-missForest(Outlier_miss)$ximp   

period <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018)
Impute_rf$period <- period

names(Impute_rf) ##1

df_fsca_is_rf1 <- Impute_rf %>% pivot_longer((names(df_fsca_filter1)[-1]), names_to = "fund_class", values_to = "value")
df_fsca_is_rf1$period <- as.character(as.numeric(df_fsca_is_rf1$period))


#############End##################

df_fsca_is_mod1 <- read_rds("data/fsca/df_fsca_is.rds") %>%
  mutate(item = str_squish(item)) %>%
  mutate(value_tot = case_when(item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                                 item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
                                 item == "BENEFITS (TOTAL CURRENT PERIOD)" ~
                                 -value,
                               item == "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)" |
                                 item == "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)" |
                                 item == "OTHER INCOME (TOTAL CURRENT PERIOD)" |
                                 item == "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)" ~
                                 value,
                               TRUE ~ NA_real_)) %>%
  group_by(period) %>%
  mutate(value_tot = case_when(item == "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)" |
                                 item == "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)" |
                                 item == "OTHER INCOME (TOTAL CURRENT PERIOD)" |
                                 item == "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                                 item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                                 item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
                                 item == "BENEFITS (TOTAL CURRENT PERIOD)" ~ sum(value_tot, na.rm = T),
                               TRUE ~ NA_real_)) %>%
  filter(item == "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)" |
           item == "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)" |
           item == "OTHER INCOME (TOTAL CURRENT PERIOD)" |
           item == "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)" |
           item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
           item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
           item == "BENEFITS (TOTAL CURRENT PERIOD)") %>%
  mutate(value = case_when(item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                             item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
                             item == "BENEFITS (TOTAL CURRENT PERIOD)" ~
                             -value,
                           TRUE ~ value)) %>%
  select(-value_tot) %>%
  write_rds("data/fsca/df_fsca_is_mod1.rds","gz")

tot_fsca_is <- bind_rows(
  
  read_rds("data/fsca/fsca_is_report_g.rds") %>%
    filter(fund_substatus == "NORMAL ACTIVE FUND" &
             fund_class != "Beneficiary" &
             (item == "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)" |
                item == "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)" |
                item == "OTHER INCOME (TOTAL CURRENT PERIOD)" |
                item == "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
                item == "BENEFITS (TOTAL CURRENT PERIOD)")) %>%
    mutate(value = case_when(item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                               item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
                               item == "BENEFITS (TOTAL CURRENT PERIOD)" ~ -value,
                             TRUE ~ value)) %>%
    group_by(period, fund_class, item) %>%
    summarise(tot_item_tmp = sum(value, na.rm = T)) %>%
    group_by(period,item) %>%
    mutate(adj_prop = abs(tot_item_tmp/sum(tot_item_tmp, na.rm = T))) %>%
    left_join(.,
              df_fsca_is_mod1,
              by = c("period" = "period",
                     "item" = "item")) %>%
    ungroup() %>%
    mutate(adj_tot_item = adj_prop*value) %>%
    group_by(period, fund_class) %>%
    summarise(tot_item = sum(adj_tot_item, na.rm = T)),####filter here####
  
  filter1_fix2
  
  # read_rds("data/fsca/fsca_is_report_g.rds") %>%
  #   filter(fund_substatus == "NORMAL ACTIVE FUND" &
  #            fund_class != "Beneficiary" &
  #            (item == "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)" |
  #               item == "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)" |
  #               item == "OTHER INCOME (TOTAL CURRENT PERIOD)" |
  #               item == "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)" |
  #               item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
  #               item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
  #               item == "BENEFITS (TOTAL CURRENT PERIOD)")) %>%
  #   mutate(value = case_when(item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
  #                              item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
  #                              item == "BENEFITS (TOTAL CURRENT PERIOD)" ~ -value,
  #                            TRUE ~ value)) %>%
  #   group_by(period, fund_class, item) %>%
  #   summarise(tot_item_tmp = sum(value, na.rm = T)) %>%
  #   group_by(period,item) %>%
  #   mutate(adj_prop = abs(tot_item_tmp/sum(tot_item_tmp, na.rm = T))) %>%
  #   left_join(.,
  #             df_fsca_is_mod1,
  #             by = c("period" = "period",
  #                    "item" = "item")) %>%
  #   ungroup() %>%
  #   mutate(adj_tot_item = adj_prop*value) %>%
  #   group_by(period, fund_class) %>%
  #   summarise(tot_item = sum(adj_tot_item, na.rm = T)) %>%
  #   left_join(., forecast, by = c("period" = "year")) %>%
  #   filter(period == as.character(params_forecast1-1)) %>%
  #   mutate(tot_item = tot_item*(1+final)) %>%
  #   ungroup() %>%
  #   mutate(period = as.character(params_forecast1)) %>%
  #   select(-final)
  
)

tot_fsca_is <- bind_rows(
  tot_fsca_is,
  tot_fsca_is %>%
    left_join(., forecast, by = c("period" = "year")) %>%
    filter(period == as.character(params_forecast1)) %>%
    mutate(tot_item = tot_item*(1+final)) %>%
    ungroup() %>%
    mutate(period = as.character(params_forecast2)) %>%
    select(-final)
)

tot_fsca_is <- bind_rows(
  tot_fsca_is,
  tot_fsca_is %>%
    left_join(., forecast, by = c("period" = "year")) %>%
    filter(period == as.character(params_forecast2)) %>%
    mutate(tot_item = tot_item*(1+final)) %>%
    ungroup() %>%
    mutate(period = as.character(params_forecast3)) %>%
    select(-final)
)


write_rds(tot_fsca_is, "data/fsca/tot_fsca_is.rds","gz")

############filter#############

read_rds("data/fsca/fsca_is_report_g.rds") %>%
  filter(fund_substatus == "NORMAL ACTIVE FUND" &
           fund_class != "Beneficiary" &
           (item == "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)" |
              item == "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)" |
              item == "OTHER INCOME (TOTAL CURRENT PERIOD)" |
              item == "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)" |
              item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
              item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
              item == "BENEFITS (TOTAL CURRENT PERIOD)")) %>%
  mutate(value = case_when(item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                             item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
                             item == "BENEFITS (TOTAL CURRENT PERIOD)" ~ -value,
                           TRUE ~ value)) %>%
  group_by(period, fund_class, item) %>%
  summarise(tot_item_tmp = sum(value, na.rm = T)) %>%    
  
  
  unite("item1",fund_class,item, sep = "/" ) %>%
  pivot_wider(names_from = "item1", values_from = "tot_item_tmp") -> filter1

x<- filter1
outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)}

numeric_cols<-names(filter1)[sapply(filter1, is.numeric)]
numeric_data<-filter1[,names(filter1)%in%numeric_cols]
Outlier_miss<-as.data.frame(sapply(numeric_data,outlierTreament))



filter1_fix<-missForest(Outlier_miss)$xim

period <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018)
filter1_fix$period <- period

filter1_fix1 <- filter1_fix %>% pivot_longer((names(filter1_fix)[-22]), names_to = "item1", values_to = "tot_item_tmp") %>%
  separate(item1, into = c("fund_class", "item"),sep = "/")

filter1_fix1$period <- as.character(as.numeric(filter1_fix1$period))

filter1_fix1 %>%
  group_by(period,item) %>%
  mutate(adj_prop = abs(tot_item_tmp/sum(tot_item_tmp, na.rm = T))) %>%
  left_join(.,
            df_fsca_is_mod1,
            by = c("period" = "period",
                   "item" = "item")) %>%
  ungroup() %>%
  mutate(adj_tot_item = adj_prop*value) %>%
  group_by(period, fund_class) %>%
  summarise(tot_item = sum(adj_tot_item, na.rm = T)) %>%
  left_join(., forecast, by = c("period" = "year")) %>%
  filter(period == as.character(params_forecast1-1)) %>%
  mutate(tot_item = tot_item*(1+final)) %>%
  ungroup() %>%
  mutate(period = as.character(params_forecast1)) %>%
  select(-final) ->filter1_fix2 ### point###


#############End################

fsca_is_report_g_tr <- read_rds("data/fsca/fsca_is_report_g.rds") %>%
  filter(fund_substatus == "NORMAL ACTIVE FUND" &
           fund_class != "Beneficiary" &
           (item == "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)" |
              item == "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)" |
              item == "OTHER INCOME (TOTAL CURRENT PERIOD)" |
              item == "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)" |
              item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
              item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
              item == "BENEFITS (TOTAL CURRENT PERIOD)")) %>%
  mutate(value = case_when(item == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                             item == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
                             item == "BENEFITS (TOTAL CURRENT PERIOD)" ~ -value,
                           TRUE ~ value)) %>%
  group_by(period,fund_class,item) %>%
  summarise(tot_item = sum(value, na.rm = T)) %>%
  group_by(period,item) %>%
  mutate(adj_prop = abs(tot_item/sum(tot_item, na.rm = T))) %>%
  left_join(.,
            df_fsca_is_mod1,
            by = c("period" = "period",
                   "item" = "item")) %>%
  ungroup() %>%
  mutate(adj_tot_item = adj_prop*value) %>%
  group_by(period, fund_class) %>%
  mutate(prop_fc = abs(adj_tot_item/sum(adj_tot_item, na.rm = T))) %>%
  select(period, fund_class, item, prop_fc)

fsca_is_report_g_tr <- bind_rows(
  
  fsca_is_report_g_tr,
  
  fsca_is_report_g_tr %>%
    ungroup() %>%
    filter(period == "2010") %>%
    mutate(period = as.character(params_forecast1),
           prop_fc = NA_real_),
  
  fsca_is_report_g_tr %>%
    ungroup() %>%
    filter(period == "2010") %>%
    mutate(period = as.character(params_forecast2),
           prop_fc = NA_real_),
  
  fsca_is_report_g_tr %>%
    ungroup() %>%
    filter(period == "2010") %>%
    mutate(period = as.character(params_forecast3),
           prop_fc = NA_real_)
  
) %>%
  group_by(fund_class, item) %>%
  fill(prop_fc, .direction = "down")

##################################


# tmp_prop_fc <- fsca_is_report_g_tr
# 
# ldf_is_pf <- map(unique(tmp_prop_fc$item),
#               function(x) prop_fc_q  <- tmp_prop_fc %>%
#                 ungroup() %>%
#                 filter(item == x & fund_class == "Pension Fund") %>%
#                 select(prop_fc) %>%
#                 unname() %>%
#                 unlist() %>%
#                 ts(start = 2010, frequency = 1))
# 
# names(ldf_is_pf) <- unique(tmp_prop_fc$item)
# 
# for (i in 1:length(unique(tmp_prop_fc$item))) {
# 
#   x <- ldf_is_pf[[i]]
# 
#   denton <- td(x ~ 1, to = "quarterly", method = "denton-cholette", conversion = "average")
#   denton <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
#                    prop_fc = as.matrix(predict(denton)),
#                    fund_class = "Pension Fund",
#                    item = names(ldf_is_pf)[i])
#   assign(paste0("pension-",names(ldf_is_pf)[i]), denton)
# 
# }
# 
# ldf_is_pf2 <- map(unique(tmp_prop_fc$item),
#                  function(x) prop_fc_q  <- tmp_prop_fc %>%
#                    ungroup() %>%
#                    filter(item == x & fund_class == "Provident Fund") %>%
#                    select(prop_fc) %>%
#                    unname() %>%
#                    unlist() %>%
#                    ts(start = 2010, frequency = 1))
# 
# names(ldf_is_pf2) <- unique(tmp_prop_fc$item)
# 
# for (i in 1:length(unique(tmp_prop_fc$item))) {
# 
#   x <- ldf_is_pf2[[i]]
# 
#   denton <- td(x ~ 1, to = "quarterly", method = "denton-cholette", conversion = "average")
#   denton <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
#                    prop_fc = as.matrix(predict(denton)),
#                    fund_class = "Provident Fund",
#                    item = names(ldf_is_pf)[i])
#   assign(paste0("provident-",names(ldf_is_pf2)[i]), denton)
# 
# }
# 
# ldf_is_ra <- map(unique(tmp_prop_fc$item),
#                   function(x) prop_fc_q  <- tmp_prop_fc %>%
#                     ungroup() %>%
#                     filter(item == x & fund_class == "Provident Fund") %>%
#                     select(prop_fc) %>%
#                     unname() %>%
#                     unlist() %>%
#                     ts(start = 2010, frequency = 1))
# 
# names(ldf_is_ra) <- unique(tmp_prop_fc$item)
# 
# for (i in 1:length(unique(tmp_prop_fc$item))) {
# 
#   x <- ldf_is_ra[[i]]
# 
#   denton <- td(x ~ 1, to = "quarterly", method = "denton-cholette", conversion = "average")
#   denton <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
#                    prop_fc = as.matrix(predict(denton)),
#                    fund_class = "Retirement Annuity",
#                    item = names(ldf_is_pf)[i])
#   assign(paste0("retirement-",names(ldf_is_ra)[i]), denton)
# 
# }
# 
# fsca_is_report_g_tr <- bind_rows(mget(ls(pattern="TOTAL CURRENT PERIOD")))


###################

write_rds(fsca_is_report_g_tr, "data/fsca/fsca_is_report_g_tr.rds","gz")

###############

rm(list=ls()[!str_detect(ls(),"params")])
