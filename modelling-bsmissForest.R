source("code/libraries.R")

## Temporal disaggregation - Balance Sheet

tmp1 <- read_rds("data/fsd/tot_fsd_fc_tidy.rds")

hdf <- map(unique(tmp1$fund_class), 
           function(x) tot_fsd_fc_tidy <- tmp1 %>%
             ungroup() %>%
             filter(fund_class == x) %>% 
             select(value) %>% 
             unname() %>%
             unlist() %>%
             ts(start = c(2010,1), frequency = 4))

names(hdf) <- unique(tmp1$fund_class)

tmp2 <- read_rds("data/fsca/tot_fsca.rds")

ldf <- map(unique(tmp2$fund_class), 
           function(x) tot_fsca  <- tmp2 %>% 
             ungroup() %>%
             filter(fund_class == x) %>% 
             select(value) %>% 
             unname() %>%
             unlist() %>%
             ts(start = 2010, frequency = 1))

names(ldf) <- unique(tmp1$fund_class)

hdf["NA"] <- NULL

for (i in seq_along(hdf)) {
  
  x <- ldf[[i]]
  y <- hdf[[i]]
  
  denton <- td(x ~ 0 + y, to = "quarterly", method = "denton-cholette", conversion = "average")
  denton <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
                   denton = as.matrix(predict(denton)), 
                   fund_class = names(ldf[i]))
  assign(paste0(names(ldf)[i]), denton)
  
}

tmp_denton <- bind_rows(`Pension Fund`,
                        `Provident Fund`,
                        `Retirement Annuity`)  %>%
  group_by(period) %>%
  mutate(fund_prop = denton/sum(denton,na.rm=T))
  

# cmff_agg <- left_join(cmff_agg_prop, 
#                            tmp_denton, 
#                            by = c("period","fund_class")) %>%
#     mutate(value_adj = prop*denton/100,
#            value_adj_factor = value_adj/value)

cmff_det <- left_join(read_rds("data/cmff/cmff_det_prop.rds"), 
                      tmp_denton, 
                      by = c("period","fund_class"))

# %>%
#   mutate(value_adj = prop*denton/100,
#          value_adj_factor = value_adj/tot)

cmff_liab <- left_join(read_rds("data/cmff/cmff_liab_det_prop.rds"),
                       tmp_denton,
                       by = c("period", "fund_class"))
# %>%
#   mutate(value_adj = prop*denton/100,
#          value_adj_factor = value_adj/tot)

##########################################
####filter here####

fsca_ret_inv <-read_rds("data/fsca/fsca_ret_inv.rds") %>%
  filter(fund_substatus == "NORMAL ACTIVE FUND" &
          item != "TOTAL (TOTAL CURRENT PERIOD)" &
          item != "TOTAL ASSETS" &
           !is.na(fund_class)) %>%
  mutate(value = as.numeric(as.character(value))) %>%
  group_by(period, fund_class, item) %>%
  summarise(tot = sum(value, na.rm = T)) %>%
  group_by(period,fund_class) %>%
  mutate(prop_de = tot/sum(tot, na.rm = T),
         cat = "asset") %>%
  select(-tot)

############filter############

fsca_ret_inv <- read_rds("data/fsca/fsca_ret_inv.rds") %>%
  filter(fund_substatus == "NORMAL ACTIVE FUND" &
           item != "TOTAL (TOTAL CURRENT PERIOD)" &
           item != "TOTAL ASSETS" &
           !is.na(fund_class)) %>%
  mutate(value = as.numeric(as.character(value))) %>%
  group_by(period, fund_class, item) %>%
  summarise(tot = sum(value, na.rm = T)) %>%
  
  
  unite("item1",fund_class,item, sep = "/" ) %>%
  pivot_wider(names_from = "item1", values_from = "tot") 

x<- fsca_ret_inv
outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)}

numeric_cols<-names(fsca_ret_inv)[sapply(fsca_ret_inv, is.numeric)]
numeric_data<-fsca_ret_inv[,names(fsca_ret_inv)%in%numeric_cols]
Outlier_miss<-as.data.frame(sapply(numeric_data,outlierTreament))

fsca_ret_inv<-missForest(Outlier_miss)$xim

period <- c(2014,2015,2016,2017,2018)
fsca_ret_inv$period <- period

names(fsca_ret_inv)

fsca_ret_inv <- fsca_ret_inv %>% pivot_longer((names(fsca_ret_inv)[-49]), names_to = "item1", values_to = "tot") %>%
  separate(item1, into = c("fund_class", "item"),sep = "/")

fsca_ret_inv$period <- as.character(as.numeric(fsca_ret_inv$period))



#############End##############

tmp1 <- fsca_ret_inv %>%
  filter(period == "2014" |
           period == "2015" |
           period == "2016") %>%
  group_by(fund_class,item) %>%
  summarise(prop_de = median(prop_de, na.rm = T)) %>%
  mutate(period = "2010")

tmp2 <- tmp1 %>%
  mutate(period = "2011")

tmp3 <- tmp1 %>%
  mutate(period = "2012")

tmp4 <- tmp1 %>%
  mutate(period = "2013")

tmp5 <- tmp1 %>%
  mutate(period = as.character(params_forecast1))

tmp6 <- tmp1 %>%
  mutate(period = as.character(params_forecast2))

fsca_ret_inv <- bind_rows(tmp1, tmp2, tmp3, tmp4, fsca_ret_inv, tmp5, tmp6) %>%
  mutate(fsca_item = case_when(item == "CASH (TOTAL CURRENT PERIOD)" 
                               ~ "Total cash and deposits",
                               item == "COLLCECTIVE INVESTMENT SCHEMES (TOTAL CURRENT PERIOD)" 
                               ~ "Collective investment schemes",
                               item == "COMMODITIES (TOTAL CURRENT PERIOD)" 
                               ~ "Commodities",
                               item == "DEBT INSTRUMENTS (TOTAL CURRENT PERIOD)"
                               ~ "Bills, bonds and securities / Debt instruments",
                               item == "DERIVATIVE MARKET INSTRUMENTS (TOTAL CURRENT PERIOD)" 
                               ~ "Derivative market instruments",
                               item == "EQUITIES (TOTAL CURRENT PERIOD)" 
                               ~ "Equities",
                               item == "HEDGE FUNDS (TOTAL CURRENT PERIOD)"
                               ~ "Hedge Funds",
                               item == "INSURANCE POLICIES (TOTAL CURRENT PERIOD)" 
                               ~ "Insurance policies:",
                               item == "INVESTMENT AND OWNER OCCUPIED PROPERTIES (TOTAL CURRENT PERIOD)"
                               ~ "Investment properties",
                               item == "INVESTMENT IN PARTICIPATING EMPLOYERS (TOTAL CURRENT PERIOD)"
                               ~ "Investment in participating employer(s)",
                               item == "OTHER PORTFOLIO ASSETS (TOTAL CURRENT PERIOD)"
                               ~ "Other assets",
                               item == "PRIVATE EQUITY FUNDS (TOTAL CURRENT PERIOD)"
                               ~ "Private Equity Funds")) %>%
  mutate(period = as.numeric(period),
         cat = "asset",
         prop_de = case_when(period == params_forecast1 |
                               period == params_forecast2 ~ NA_real_,
                             TRUE ~ prop_de)) %>%
  group_by(fsca_item, fund_class) %>%
  fill(prop_de, .direction = "down")

##############################################
##############################################

# ldf <- map(unique(fsca_ret_inv$fund_class),
#            function(x) tidy_ret_inv  <- fsca_ret_inv %>% 
#              ungroup() %>%
#              filter(fund_class == x &
#                       fsca_item == "Total cash and deposits") %>% 
#              select(prop_de) %>% 
#              unname() %>%
#              unlist() %>%
#              ts(start = 2010, frequency = 1))
# 
# names(ldf) <- unique(fsca_ret_inv$fund_class)
# 
# for (i in seq_along(ldf)) {
#   
#   x <- ldf[[i]]
#   
#   
#   denton <- td(x ~ 1, to = "quarterly", method = "denton", conversion = "average", truncated.rho = 0)
#   denton <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
#                    denton = as.matrix(predict(denton)), 
#                    fund_class = names(ldf[i]))
#   assign(paste0(names(ldf)[i]), denton)
#   
# }






############################################

fsca_mod_ass_dom <- read_rds("data/fsca/fsca_mod_ass_dom.rds") %>%
  gather(period,value,-item) %>%
  group_by(period) %>%
  mutate(total = case_when(item == "tot_ass_dom" ~ value,
                           TRUE ~ NA_real_)) %>%
  fill(total, .direction = "up") %>%
  ungroup() %>%
  #filter(item != "Total assets") %>%
  mutate(prop_ag = value/total) %>%
  select(item,period,prop_ag) %>%
  mutate(type = "domestic",
         cat = "asset")

fsca_mod_ass_dom_cur <- read_rds("data/fsca/fsca_mod_ass_dom.rds") %>%
  gather(period,value,-item) %>%
  mutate(type = "domestic",
         cat = "asset")


################################################

fsca_mod_ass_for <- read_rds("data/fsca/fsca_mod_ass_for.rds") %>%
  gather(period,value,-item) %>%
  group_by(period) %>%
  mutate(total = case_when(item == "tot_ass_dom" ~ value,
                           TRUE ~ NA_real_)) %>%
  fill(total, .direction = "up") %>%
  ungroup() %>%
  #filter(item != "Total assets") %>%
  mutate(prop_ag = value/total) %>%
  select(item,period,prop_ag) %>%
  mutate(type = "foreign",
         cat = "asset")

fsca_mod_ass_for_cur <- read_rds("data/fsca/fsca_mod_ass_for.rds") %>%
  gather(period,value,-item) %>%
  mutate(type = "foreign",
         cat = "asset")

fsca_mod_liab_dom <- read_rds("data/fsca/fsca_mod_liab_dom.rds") %>%
  gather(period,value,-item) %>%
  group_by(period) %>%
  mutate(total = case_when(item == "Total funds and liabilities" ~ value,
                           TRUE ~ NA_real_),
         item = case_when(item == "Members’ funds and surplus account" ~ "Members funds and surplus account",
                          TRUE ~ item)) %>%
  fill(total, .direction = "up") %>%
  ungroup() %>%
  #filter(item != "Total funds and liabilities") %>%
  mutate(prop_ag = value/total) %>%
  select(item,period,prop_ag) %>%
  mutate(type = "domestic",
         cat = "liability")

##################################

fsca_mod <- bind_rows(fsca_mod_ass_dom,
                      fsca_mod_ass_for,
                      fsca_mod_liab_dom) %>%
  ungroup() %>%
  mutate(period = as.numeric(period),
         prop_ag = case_when(is.na(prop_ag)
                             ~ 0,
                             TRUE ~ prop_ag))

fsca_mod <- bind_rows(fsca_mod, 
                      fsca_mod %>% 
                        filter(period == 2018) %>% 
                        ungroup() %>% 
                        mutate(period = params_forecast1,
                               prop_ag = NA_real_),
                      fsca_mod %>% 
                        filter(period == 2018) %>% 
                        ungroup() %>% 
                        mutate(period = params_forecast2,
                               prop_ag = NA_real_)) %>%
  mutate(prop_ag = case_when(item == "Loans (other than housing loans)" & period == 2013 & type == "domestic" ~ NA_real_,
                             TRUE ~ prop_ag)) %>%
  group_by(item, type, cat) %>%
  fill(prop_ag, .direction = "down") %>%
  mutate(prop_ag = case_when(prop_ag == 0
                             ~ NA_real_,
                             TRUE ~ prop_ag))

fsca_mod_ass_cur <- bind_rows(fsca_mod_ass_dom_cur,
                              fsca_mod_ass_for_cur) %>%
  group_by(item,period) %>%
  mutate(prop_cur = value/sum(value,na.rm=T))  %>%
  ungroup() %>%
  mutate(period = as.numeric(period),
         prop_cur = case_when(is.na(prop_cur)
                             ~ 0,
                             TRUE ~ prop_cur))

fsca_mod_ass_cur <- bind_rows(fsca_mod_ass_cur, 
                              fsca_mod_ass_cur %>% 
                                filter(period == 2018) %>% 
                                ungroup() %>% 
                                mutate(period = params_forecast1,
                                       prop_ag = NA_real_),
                              fsca_mod_ass_cur %>% 
                                filter(period == 2018) %>% 
                                ungroup() %>% 
                                mutate(period = params_forecast2,
                                       prop_ag = NA_real_)) %>%
  group_by(item, type, cat) %>%
  fill(prop_cur, .direction = "down") %>%
  mutate(prop_cur = case_when(prop_cur == 0
                             ~ NA_real_,
                             TRUE ~ prop_cur)) %>%
  select(-value,-prop_ag)


#################################


cmff_det1 <- cmff_det %>%
  mutate(fsca_item = case_when(item == "C48.0067 - Land and buildings" |
                                 item == "C48.0068 - Vehicles" |
                                 item == "C48.0069 - Computer equipment" |
                                 item == "C48.0070 - Software" |
                                 item == "C48.0071 - Other non-financial assets" 
                               ~ "Property, plant and equipment",
                               item == "C48.0146 - Transferable deposits" |
                                 item == "C48.0147 - Other deposits"
                               ~ "Total cash and deposits",
                               item == "C48.0139 - Loans (including repos and security lending): Household"
                               ~ "Housing loan facilities",
                               item == "C48.0137 - Loans (including repos and security lending): Banks" |
                                 item == "C48.0138 - Loans (including repos and security lending): Non-bank financial institutions" |
                                 item == "C48.0140 - Other loans (including repos and security lending)"
                               ~ "Loans (other than housing loans)",
                               item==	"C48.0093 - Non-participating preference shares: Public sector non-financial corporations"	|
                                 item==	"C48.0105 - Long-term interest bearing securities: Public sector financial corporations"	|
                                 item==	"C48.0107 - Long-term interest bearing securities: National government"	|
                                 item==	"C48.0108 - Long-term interest bearing securities: Local government"	|
                                 item==	"C48.0110 - Long-term interest bearing securities: Public sector non-financial corporations"	|
                                 item==	"C48.0116 - Short-term interest bearing securities: Treasury bills"	|
                                 item==	"C48.0117 - Short-term interest bearing securities: Land Bank bills"	|
                                 item==	"C48.0122 - Short-term interest bearing securities: Public sector financial corporations"	|
                                 item==	"C48.0124 - Short-term interest bearing securities: Government"	|
                                 item==	"C48.0126 - Short-term interest bearing securities: Public sector non-financial corporations"	|
                                 item==	"C48.0089 - Non-participating preference shares: Banks"	|
                                 item==	"C48.0090 - Non-participating preference shares: Insurers"	|
                                 item==	"C48.0091 - Non-participating preference shares: Other non-bank financial institutions"	|
                                 item==	"C48.0092 - Non-participating preference shares: Private sector non-financial corporations"	|
                                 item==	"C48.0103 - Long-term interest bearing securities: Banks"	|
                                 item==	"C48.0104 - Long-term interest bearing securities: Insurers"	|
                                 item==	"C48.0106 - Long-term interest bearing securities: Other non-bank financial institutions"	|
                                 item==	"C48.0109 - Long-term interest bearing securities: Private sector non-financial corporations"	|
                                 item==	"C48.0115 - Short-term interest bearing securities: Negotiable certificates of deposit"	|
                                 item==	"C48.0118 - Short-term interest bearing securities: Promissory notes issued by banks"	|
                                 item==	"C48.0120 - Short-term interest bearing securities: Banks"	|
                                 item==	"C48.0121 - Short-term interest bearing securities: Insurers"	|
                                 item==	"C48.0123 - Short-term interest bearing securities: Other non-bank financial institutions"	|
                                 item==	"C48.0125 - Short-term interest bearing securities: Private sector non-financial corporations"	|
                                 item==	"C48_0088 - Non-participating preference shares"	|
                                 item==	"C48_0102 - Long-term interest bearing securities"	|
                                 item==	"C48_0119 - Short-term interest bearing securities"
                               ~ "Bills, bonds and securities / Debt instruments",
                               item==	"C48.0074 - Listed ordinary shares including participating preference shares: Banks"	|
                                 item==	"C48.0075 - Listed ordinary shares including participating preference shares: Insurers"	|
                                 item==	"C48.0076 - Listed ordinary shares including participating preference shares: Property companies"	|
                                 item==	"C48.0077 - Listed ordinary shares including participating preference shares: Other non-bank financial institutions"	|
                                 item==	"C48.0078 - Listed ordinary shares including participating preference shares: Private sector non-financial corporations"	|
                                 item==	"C48.0079 - Listed ordinary shares including participating preference shares: Public sector non-financial corporations"	|
                                 item==	"C48.0080 - Companies with secondary listings on the JSE"	|
                                 item==	"C48.0073 - Listed ordinary shares including participating preference shares: Foreign"	|
                                 item==	"C48.0081 - Other equity instruments listed on the JSE"	
                               ~ "Equities",
                               item==	"C48.0142 - Linked policies"	|
                                 item==	"C48.0143 - Non-linked policies"	|
                                 item==	"C48.0144 - Pensioner annuity policies"	
                               ~ "Insurance policies:",
                               item==	"C48.0095 - Money market unit trusts: Domestic unit trusts"	|
                                 item==	"C48.0098 - Non-money market collective investment schemes: Domestic unit trusts"	|
                                 item==	"C48.0101 - Non-money market collective investment schemes: Hedge funds"	|
                                 item==	"C48.0096 - Money market unit trusts: Foreign unit trusts"	|
                                 item==	"C48.0099 - Non-money market collective investment schemes: Foreign unit trusts"
                               ~ "Collective investment schemes",
                               item==	"C48.0129 - Financial derivatives options: Banks"	|
                                 item==	"C48.0130 - Financial derivatives options: Non-bank financial institutions"	|
                                 item==	"C48.0131 - Financial derivatives options: Other"	|
                                 item==	"C48.0133 - Financial derivatives futures, forwards and swaps: Banks"	|
                                 item==	"C48.0134 - Financial derivatives futures, forwards and swaps: Non-bank financial institutions"	|
                                 item==	"C48.0135 - Financial derivatives futures, forwards and swaps: Other"	
                               ~ "Derivative market instruments",
                               
                               item == "K48.0067 - Accrued investment income"
                               ~ "Accounts receivable",
                               item == "K48.0068 - Sundry debtors"
                               ~ "Contributions receivable",
                               TRUE ~ "OTHER")) %>%
  ungroup() %>%
  mutate(year = as.numeric(year(as_date(period))),
         type = case_when(item == "K48.0032 - Foreign borrowers: mortgage loans to" |
                            item == "K48.0006 - Foreign securities - investment in" |
                            item == "K48.0007 - Foreign properties - investment in" |
                            item == "K48.0050 - Foreigners" |
                            item == "K48.0063 - All other foreign assets not incl under item 6, 7 and 32: Short-term - less than one year" |
                            item == "K48.0064 - Long-term - one year or longer" |
                            item == "K48.0072 - In respect of obligations to members of foreigncountries, if not available, include under item 71" |
                            item == "K48.0077 - All other foreign liabilities not incl under item 72 Short-term - less than one year" |
                            item == "K48.0078 - Long-term - one year or longer"
                          ~ "foreign",
                          TRUE ~ "domestic"),
         cat = "asset") 

ra_ins <- fsca_mod %>%
  filter(item == "Non-linked related policies" |
           item == "Linked related policies") %>%
  group_by(item, type) %>%
  summarise(spec = median(prop_ag, na.rm = T)) %>%
  group_by(type) %>%
  mutate(spec = spec/sum(spec, na.rm = T),
         item = case_when(item == "Non-linked related policies"
                          ~ "K48.0168 - Guaranteed policies",
                          item == "Linked related policies" 
                          ~ "K48.0167 - Linked policies"))

cmff_fin <- left_join(cmff_det1,
                      fsca_mod,
                      by = c("fsca_item" = "item", 
                             "year" = "period", 
                             "type" = "type", 
                             "cat" = "cat")) %>%
  left_join(., fsca_mod_ass_cur,
                         by = c("fsca_item" = "item", 
                                "year" = "period", 
                                "type" = "type", 
                                "cat" = "cat")) %>%
  left_join(., ra_ins, by = c("item" = "item", "type" = "type")) %>%
  mutate(tot = case_when(fund_class == "Retirement Annuity" & 
                           (item == "K48.0066 - Deposit administration contracts (with insurers)" |
                              item == "K48.0169 - Other policies") 
                         ~ NA_real_,
                         TRUE ~ tot))


tmp <- tmp_denton %>%
  #filter(fund_class != "Beneficiary") %>%
  group_by(period) %>%
  mutate(prop_f = denton/sum(denton, na.rm = T)) %>%
  select(-denton)


cmff_fin1 <- left_join(cmff_fin,
                       tmp,
                       by = c("period" = "period",
                              "fund_class" = "fund_class")) 
denton_ag <- tmp_denton %>%
  group_by(period) %>%
  summarise(denton = sum(denton, na.rm = T))

cmff_fin2 <- left_join(cmff_fin1,
                       fsca_ret_inv,
                       by = c("fsca_item" = "fsca_item",
                              "year" = "period",
                              "cat" = "cat",
                              "fund_class" = "fund_class")) %>%
  select(-denton) %>%
  left_join(., denton_ag, by = c("period" = "period")) %>%
  mutate(prop_de = case_when((is.na(prop_de) & fund_class != "Retirement Annuity") |
                               type == "foreign" & fund_class != "Retirement Annuity" ~ prop_ag,
                             TRUE ~ prop_de),
         prop_cur2 = case_when(fsca_item != "Bills, bonds and securities / Debt instruments" &
                                type == "domestic" ~ 1,
                              fsca_item != "Bills, bonds and securities / Debt instruments" &
                                type == "foreign" ~ 0,
                              TRUE ~ prop_cur)) %>%
  group_by(period, fund_class, fsca_item,type) %>%
  mutate(cur_type1 = sum(tot,na.rm=T)) %>%
  group_by(period, fund_class, fsca_item) %>%
  mutate(cur_type2 = sum(tot,na.rm=T)) %>%
  ungroup() %>%
  mutate(prop_cur1 = cur_type1/cur_type2) %>%
  group_by(period, fund_class, fsca_item, cat,type) %>%
  mutate(prop_int = tot/sum(tot, na.rm = T))  %>%
  #group_by(period,fund_class,fsca_item,cat) %>%
  
  # group_by(period, fsca_item, type, cat) %>%
  # mutate(prop_int = case_when((item.x == "K48.0168 - Guaranteed policies" |
  #                                item.x == "K48.0167 - Linked policies") & fund_class == "Retirement Annuity"
  #                             ~ spec,
  #                             TRUE ~ prop_int)) %>%
  # ungroup() %>%
  # mutate(prop_int = case_when(fund_class == "Retirement Annuity" &
  #                               item.x != "K48.0168 - Guaranteed policies" &
  #                               item.x != "K48.0167 - Linked policies" ~ NA_real_,
  #                             TRUE ~ prop_int)) %>%
  ungroup() %>%
  mutate(prop_int_pf = case_when(fund_class == "Pension Fund" ~ prop_int,
                                 TRUE ~ NA_real_),
         prop_cur1_pf = case_when(fund_class == "Pension Fund" ~ prop_cur1,
                                 TRUE ~ NA_real_)) %>%
  group_by(period,item.x,fsca_item,type,cat) %>%
  fill(prop_int_pf, .direction = "up") %>%
  fill(prop_int_pf, .direction = "down") %>%
  fill(prop_cur1_pf, .direction = "up") %>%
  fill(prop_cur1_pf, .direction = "down") %>%
  ungroup() %>%
  mutate(prop_int = case_when(fund_class == "Retirement Annuity" ~ prop_int_pf,
                              TRUE ~ prop_int),
         prop_cur1 = case_when(fund_class == "Retirement Annuity" ~ prop_cur1_pf,
                              TRUE ~ prop_cur1)) %>%
  rowwise() %>%
  mutate(tst = denton*prop_f*prop_de*prop_cur2*prop_int)


cmff_fin3 <- cmff_fin2 %>%
  ungroup() %>%
  group_by(period, fund_class) %>%
  summarise(tot1 = sum(tst, na.rm = T)) %>%
  left_join(.,tmp_denton, by = c("period" = "period", "fund_class" = "fund_class")) %>%
  mutate(diff = denton-tot1) %>%
  select(period, fund_class, diff) 

cmff_fin4 <- cmff_fin2 %>%
  ungroup() %>%
  filter(fsca_item == "OTHER") %>%
  group_by(period, fund_class,item.x) %>%
  summarise(pp = sum(tot, na.rm = T)) %>%
  mutate(pp1 = pp/sum(pp, na.rm = T)) %>%
  ungroup() %>%
  mutate(pp2 = case_when(fund_class == "Pension Fund" ~ pp1,
                         TRUE ~ NA_real_)) %>%
  group_by(period,item.x) %>%
  fill(pp2, .direction = "down") %>%
  ungroup() %>%
  mutate(pp1 = case_when(fund_class == "Retirement Annuity" ~ pp2,
                         TRUE ~ pp1)) %>%
  select(period, fund_class, pp1, item.x)

cmff_fin5 <- cmff_fin2 %>%
  left_join(.,cmff_fin4, by = c("period" = "period", "fund_class" = "fund_class", "item.x" = "item.x")) %>%
  left_join(.,cmff_fin3, by = c("period" = "period", "fund_class" = "fund_class")) %>%
  ungroup() %>%
  mutate(tst_diff = diff*pp1,
         tst = case_when(fsca_item == "OTHER" ~ tst_diff,
                         TRUE ~ tst))

write_rds(cmff_fin5, "data/cmff/cmff_fin5.rds","gz")


ove_ass <- cmff_fin5 %>%
  ungroup() %>%
  group_by(period, fund_class) %>%
  summarise(dte = sum(tst, na.rm = T)) %>%
  left_join(.,tmp_denton, by = c("period" = "period", "fund_class" = "fund_class")) %>%
  mutate(eq = denton-dte)

prop_f <- cmff_fin5 %>%
  ungroup() %>%
  mutate(tst = denton*prop_f) %>%
  distinct(period,fund_class,tst) %>%
  filter(!is.na(tst)) %>%
  left_join(., tmp_denton,
            by = c("period" = "period",
                   "fund_class" = "fund_class"))


prop_int <- cmff_fin5 %>%
  group_by(fsca_item,fund_class,period) %>%
  summarise(n = sum(prop_int,na.rm=T))

prop_de <- cmff_fin5 %>%
  distinct(period,fund_class,fsca_item,type,.keep_all = T) %>%
  select(period,fund_class,fsca_item,prop_de,type) %>%
  group_by(period,fund_class,type) %>%
  summarise(n=sum(prop_de,na.rm=T))

prop_cur <- cmff_fin5 %>%
  distinct(period,fund_class,fsca_item,.keep_all = T) %>%
  select(period,fund_class,fsca_item,prop_de)

prop_fsca <- cmff_fin5 %>%
  group_by(period,fsca_item,type) %>%
  summarise(tst = sum(tst,na.rm=T)) %>%
  group_by(period) %>%
  mutate(prep = sum(tst,na.rm=T)) %>%
  ungroup() %>%
  mutate(end = tst/prep)

#################################

cmff_liab1 <- cmff_liab %>%
  mutate(fsca_item = case_when(item == "K48.0071 - LIABILITIES Accumulated funds: In respect of obligations to members in SA" |
                                 item == "K48.0072 - In respect of obligations to members of foreigncountries, if not available, include under item 71"
                               ~ "Members funds and surplus account",
                               item == "K48.0073 - Reserves and provisions"
                               ~ "Reserves",
                               item == "K48.0076 - Other creditors"
                               ~ "Accounts payable",
                               item == "K48.0075 - Sundry creditors: Amountsdue to members"
                               ~ "Benefits payable",
                               item == "K48.0074 - Bank overdraft" 
                               ~ "Bank Overdraft",
                               TRUE ~ "OTHER")) %>%
  ungroup() %>%
  mutate(year = as.numeric(year(as_date(period))),
         type = case_when(item == "K48.0077 - All other foreign liabilities not incl under item 72 Short-term - less than one year" |
                            item == "K48.0078 - Long-term - one year or longer" |
                            item == "K48.0072 - In respect of obligations to members of foreigncountries, if not available, include under item 71"
                          ~ "foreign",
                          TRUE ~ "domestic"),
         cat = "liability") 

cmff_finliab <- left_join(cmff_liab1,
                          fsca_mod,
                          by = c("fsca_item" = "item", 
                                 "year" = "period", 
                                 "type" = "type", 
                                 "cat" = "cat"))

tmp <- tmp_denton %>%
  #filter(fund_class != "Beneficiary") %>%
  group_by(period) %>%
  mutate(prop_f = denton/sum(denton, na.rm = T)) %>%
  select(-denton)


cmff_finliab1 <- left_join(cmff_finliab,
                           tmp,
                           by = c("period" = "period",
                                  "fund_class" = "fund_class")) 

denton_ag <- tmp_denton %>%
  group_by(period) %>%
  summarise(denton = sum(denton, na.rm = T))

cmff_finliab2 <- left_join(cmff_finliab1,
                       fsca_ret_inv,
                       by = c("fsca_item" = "fsca_item",
                              "year" = "period",
                              "cat" = "cat",
                              "fund_class" = "fund_class")) %>%
  select(-denton) %>%
  left_join(., denton_ag, by = c("period" = "period")) %>%
  #mutate(prop_de = prop_f) %>%
  group_by(period, fund_class, fsca_item, cat,type) %>%
  mutate(prop_int = tot/sum(tot, na.rm = T))  %>%
  #group_by(period,fund_class,fsca_item,cat) %>%
  
  # group_by(period, fsca_item, type, cat) %>%
  # mutate(prop_int = case_when((item.x == "K48.0168 - Guaranteed policies" |
  #                                item.x == "K48.0167 - Linked policies") & fund_class == "Retirement Annuity"
  #                             ~ spec,
  #                             TRUE ~ prop_int)) %>%
  # ungroup() %>%
  # mutate(prop_int = case_when(fund_class == "Retirement Annuity" &
  #                               item.x != "K48.0168 - Guaranteed policies" &
  #                               item.x != "K48.0167 - Linked policies" ~ NA_real_,
#                             TRUE ~ prop_int)) %>%
  ungroup() %>%
  mutate(prop_int_pf = case_when(fund_class == "Pension Fund" ~ prop_int,
                                 TRUE ~ NA_real_)) %>%
  group_by(period,item.x,fsca_item,type,cat) %>%
  fill(prop_int_pf, .direction = "up") %>%
  fill(prop_int_pf, .direction = "down") %>%
  ungroup() %>%
  mutate(prop_int = case_when(fund_class == "Retirement Annuity" ~ prop_int_pf,
                              TRUE ~ prop_int)) %>%
  rowwise() %>%
  mutate(tst = denton*prop_f*prop_ag*prop_int)


cmff_finliab3 <- cmff_finliab2 %>%
  ungroup() %>%
  group_by(period, fund_class) %>%
  summarise(tot1 = sum(tst, na.rm = T)) %>%
  left_join(.,tmp_denton, by = c("period" = "period", "fund_class" = "fund_class")) %>%
  mutate(diff = denton-tot1) %>%
  select(period, fund_class, diff) 

cmff_finliab4 <- cmff_finliab2 %>%
  ungroup() %>%
  filter(fsca_item == "OTHER") %>%
  group_by(period, fund_class,item.x) %>%
  summarise(pp = sum(tot, na.rm = T)) %>%
  mutate(pp1 = pp/sum(pp, na.rm = T)) %>%
  ungroup() %>%
  mutate(pp2 = case_when(fund_class == "Pension Fund" ~ pp1,
                         TRUE ~ NA_real_)) %>%
  group_by(period,item.x) %>%
  fill(pp2, .direction = "down") %>%
  ungroup() %>%
  mutate(pp1 = case_when(fund_class == "Retirement Annuity" ~ pp2,
                         TRUE ~ pp1),
         pp1 = case_when(fund_class == "Provident Fund" &
                           item.x == "K48.0079 - Other domestic liabilities" &
                           period == "2015-09-01" 
                         ~ 1,
                         TRUE ~ pp1)) %>%
  select(period, fund_class, pp1, item.x)

cmff_finliab5 <- cmff_finliab2 %>%
  left_join(.,cmff_finliab4, by = c("period" = "period", "fund_class" = "fund_class", "item.x" = "item.x")) %>%
  left_join(.,cmff_finliab3, by = c("period" = "period", "fund_class" = "fund_class")) %>%
  ungroup() %>%
  mutate(tst_diff = diff*pp1,
         tst = case_when(fsca_item == "OTHER" ~ tst_diff,
                         TRUE ~ tst))

write_rds(cmff_finliab5, "data/cmff/cmff_finliab5.rds","gz")

ove_liab <- cmff_finliab5 %>%
  ungroup() %>%
  group_by(period, fund_class) %>%
  summarise(dte = sum(tst, na.rm = T)) %>%
  left_join(.,tmp_denton, by = c("period" = "period", "fund_class" = "fund_class")) %>%
  mutate(eq = denton-dte)


last <- cmff_fin5 %>%
  ungroup() %>%
  group_by(period, fsca_item) %>%
  summarise(dte = sum(tst, na.rm = T)) %>%
  mutate(prop = dte/sum(dte, na.rm = T)*100)

###############

prop_f <- cmff_finliab5 %>%
  ungroup() %>%
  mutate(tst = denton*prop_f) %>%
  distinct(period,fund_class,tst) %>%
  filter(!is.na(tst)) %>%
  left_join(., tmp_denton,
            by = c("period" = "period",
                   "fund_class" = "fund_class"))


prop_int <- cmff_finliab5 %>%
  group_by(fsca_item,fund_class,period,type) %>%
  summarise(n = sum(prop_int,na.rm=T))

prop_ag <- cmff_finliab5 %>%
  distinct(period,fund_class,fsca_item,.keep_all = T) %>%
  select(period,fund_class,fsca_item,prop_ag) %>%
  group_by(period,fund_class) %>%
  summarise(n=sum(prop_ag,na.rm=T))

prop_cur <- cmff_finliab5 %>%
  distinct(period,fund_class,fsca_item,.keep_all = T) %>%
  select(period,fund_class,fsca_item,prop_de)



################

rm(list=ls()[!str_detect(ls(),"params")])
