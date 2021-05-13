source("code/libraries.R")

# Cleaning of fund names for better matching prospects

df_cmff <- read_rds("data/cmff/df_cmff_raw.rds") %>%
  mutate(inst_no = substr(fund_name,1,8),
         fund_name = substr(fund_name,12,nchar(fund_name)),
         fund_name = tolower(fund_name),
         #fund_name = gsub("\\s*\\([^\\)]+\\)","",fund_name),
         fund_name = gsub("the","",fund_name),
         fund_name = gsub("s a","sa",fund_name),
         fund_name = gsub("\\.","",fund_name),
         fund_name = gsub("-"," ",fund_name),
         fund_name = gsub("'","",fund_name),
         fund_name = gsub("south african","sa",fund_name),
         fund_name = gsub("south africa","sa",fund_name),
         fund_name = str_squish(fund_name)) %>%
  filter(inst_no != "I0149845" &
           inst_no != "I0534196" &
           inst_no != "I0534218" &
           inst_no != "I0534242")

write_rds(df_cmff, "data/cmff/df_cmff.rds","gz")

# Jaro Winkler matching algorithm application

df_fsca <- read_rds("data/fsca/df_fsca.rds")

match_list_cmff <- list()

for (i in seq_along(unique(df_cmff$fund_name))) {
  
  match_list_cmff[[i]] <- as_tibble(cbind(unique(df_cmff$fund_name)[i],
                                            unique(df_fsca$fund_name),
                                            stringdist(unique(df_cmff$fund_name)[i], 
                                                       unique(df_fsca$fund_name), 
                                                       method = "jw",
                                                       p=0.1)))
  
}

match_cmff <- bind_rows(match_list_cmff, .id = 'id') 

match_cmff_n <- match_cmff %>%
  group_by(V1) %>%
  top_n(-1, V3) %>%
  filter(V3 < 0.035) %>%
  arrange(V3) 

for (i in seq_along(match_cmff_n$V1)) {
  
  df_cmff$fund_name[df_cmff$fund_name == as.character(match_cmff_n$V1)[i]] <-
    as.character(match_cmff_n$V2)[i]
  
}

unmatch_cmff_n <- match_cmff %>%
  group_by(V1) %>%
  top_n(-1, V3) %>%
  filter(V3 >= 0.035) %>%
  arrange(V3)

# Manual matching

df_cmff <- df_cmff %>%
  mutate(fund_name = case_when(fund_name == "kzn municipal pension" ~
                                 "kzn municipal pension fund",
                               fund_name == "motor industry pension fund" ~
                                 "motor industry pension fund (2005)",
                               fund_name == "arcelormittal sa selector pension fund" ~
                                 "arcelor mittal sa selector pension fund",
                               fund_name == "gauteng building industry pension fund" ~
                                 "gauteng building industry pension scheme",
                               fund_name == "african oxygen ltd pension fund" ~
                                 "african oxygen limited pension fund",
                               fund_name == "retirement online pension fund" ~
                                 "retirement on line pension fund",
                               fund_name == "mondi provident fund" ~
                                 "mondi mpact group fund provident section",
                               fund_name == "predikante pensioenfonds van die ng kerk in suid afr" ~
                                 "predikante pensioenfonds van die ned geref kerk in suid afrika",
                               fund_name == "ibm sa1994 provident fund" ~
                                 "ibm sa 1994 provident fund",
                               fund_name == "krugersdorp municipal pension fund" ~
                                 "krugersdorp munisipale pensioenfonds",
                               fund_name == "mondi pension fund" ~
                                 "mondi mpact group fund pension section",
                               fund_name == "university of kzn retirement fund" ~
                                 "university of kwazulu natal pension fund",
                               fund_name == "clothing industry (natal) provident fund" ~
                                 "clothing industry retirement fund",
                               fund_name == "retirement online provident fund" ~
                                 "retirement on line retirement fund",
                               fund_name == "mines 1970 provident fund" ~
                                 "mines 1970 unclaimed benefits preservation provident fund",
                               fund_name == "dorbyl pension fund eb finance: scheme accounti" ~
                                 "dorbyl pension fund",
                               fund_name == "mines 1970 pension fund" ~
                                 "mines 1970 unclaimed benefits preservation pension fund",
                               fund_name == "natal joint municipal pension fund (retirement)" ~
                                 "kwazulu natal joint municipal pension fund (retirement)",
                               fund_name == "natal joint municipal pension fund (superannuation)" ~
                                 "kwazulu natal joint municipal pension fund(superannuation)",
                               fund_name == "boart longyear pension fund" ~
                                 "boart provident fund",
                               TRUE ~ fund_name))

# Merge cmff entities based on JW match, manual matching and add fund class for outstanding entities

fsca_map <- df_fsca %>%
  #filter(fund_substatus == "NORMAL ACTIVE FUND") %>%
  distinct(fund_name,fund_class)

merge_cmff_fsca <- left_join(df_cmff,fsca_map,by = "fund_name") %>%
  mutate(fund_class = case_when(fund_name == "transnet retirement fund" ~ "Pension Fund",
                                fund_name == "transport pension fund" ~ "Pension Fund",
                                fund_name == "tongaat hulett pension fund fifth quadrant" ~ "Pension Fund",
                                fund_name == "post office retirement fund" ~ "Provident Fund",
                                fund_name == "sa breweries provident fund" ~ "Provident Fund",
                                fund_name == "first national bank group pension fund" ~ "Pension Fund",
                                fund_name == "sa reserve bank pension fund" ~ "Pension Fund",
                                fund_name == "putco pension fund" ~ "Pension Fund",
                                fund_name == "cape clothing industry providentfund" ~ "Provident Fund",
                                fund_name == "transnet second defined benefit fund" ~ "Pension Fund",
                                fund_name == "absa group disability protectionpolicy" ~ "Pension Fund",
                                fund_name == "barlows pension fund" ~ "Pension Fund",
                                
                                fund_name == "medi clinic retirement fund" ~ "Pension Fund",
                                fund_name == "kwazulu natal joint municipal provident fund" ~ "Provident Fund",
                                fund_name == "fnb pension preservation fund" ~ "Pension Fund",
                                
                                TRUE ~ fund_class)) 


# cmff_agg_prop <- merge_cmff_fsca %>%
#   mutate(value = as.numeric(value,na.rm=T)*10^3,
#          item = str_squish(item)) %>%
#   group_by(period, fund_class) %>%
#   summarise(`XKB2330A - Coin, bankn & dep` = sum(value[item == "K48.0022 - Mutual bank shares" |
#                                                          item == "K48.0026 - Investment in participation mortgage bonds" |
#                                                          item == "K48.0150 - Negotiable certificates of deposit (NCD's) with: Banks" |
#                                                          item == "K48.0051 - Cash and demand deposits: Banknotes and coin" |
#                                                          item == "K48.0052 - Gold coins" |
#                                                          item == "K48.0053 - Demand deposits with banks (incl current banking accounts)" |
#                                                          item == "K48.0054 - Balance with SARB" |
#                                                          item == "K48.0055 - Other deposits with and call loans to: Banks" |
#                                                          item == "K48.0056 - Other deposits incl NCD's with and call loans to: Corporation for Public Deposits (COD)" |
#                                                          item == "K48.0057 - Other deposits incl NCD's with and call loans to: Land Bank" |
#                                                          item == "K48.0058 - Mutual banks (fixed and savings deposits)" |
#                                                          item == "K48.0059 - Local authorities" |
#                                                          item == "K48.0060 - Public Investment Commissioners (PIC)" |
#                                                          item == "K48.0061 - Finance companies" |
#                                                          item == "K48.0062 - Other (specify)"], #15
#                                                  na.rm = T),
#             `XKB2331A - Fix intr sec: Gov` = sum(value[item == "K48.0008 - RSA Govt stock issued inSA" |
#                                                          item == "K48.0162 - RSA Govt stock issued abroad" |
#                                                          item == "K48.0010 - Provincial govt stock - investment in" |
#                                                          item == "K48.0025 - INVESTMENT IN DEFENCE BONDS"], #4
#                                                  na.rm = T),
#             `XKB2332A - Fix intr sec: Loc Auth` = sum(value[item == "K48.0012 - Stock of local authorities in SA - investment in"], #1
#                                                       na.rm = T),
#             `XKB2333A - Fix intr sec: Pub Entp` = sum(value[item == "K48.0013 - Public corporation stock in SA - investment in" |
#                                                               item == "K48.0165 - Non-financial corporations - investment in" |
#                                                               item == "K48.0166 - Financial corporations - investment in"], #4
#                                                       na.rm = T),
#             `XKB2334A - Fix intr sec: Other` = sum(value[item == "K48.0006 - Foreign securities - investment in" |
#                                                            item == "K48.0007 - Foreign properties - investment in" |
#                                                            item == "K48.0011 - Other guarranteed & approved securities - inv in" |
#                                                            item == "K48.0014 - Listed: company stock, debentures and notes" |
#                                                            item == "K48.0015 - Unlisted: company stock, debentures and notes" |
#                                                            item == "K48.0016 - Listed: preference shares" |
#                                                            item == "K48.0017 - Unlisted: preference shares" |
#                                                            item == "K48.0040 - Bills, bankers acceptances & commercial paper issued by: Government (Treasury bills)" |
#                                                            item == "K48.0041 - Land Bank (Land Bank bills)" |
#                                                            item == "K48.0042 - Local authorities" |
#                                                            item == "K48.0043 - Bills, bankers acceptances & commercial paper issued by: (continued) Non-financial public corporations" |
#                                                            item == "K48.0044 - Government enterprises" |
#                                                            item == "K48.0045 - Financial public corporations" |
#                                                            item == "K48.0046 - Banks" |
#                                                            item == "K48.0047 - Other financial institutions" |
#                                                            item == "K48.0048 - Private sector companies" |
#                                                            item == "K48.0049 - Households (unincorporated enterprises)" |
#                                                            item == "K48.0050 - Foreigners"], #18
#                                                    na.rm = T),
#             `XKB2335A - Ordinary Shares` = sum(value[item == "K48.0018 - Listed: ordinary shares of public corporations" |
#                                                        item == "K48.0019 - Unlisted: ordinary shares of public corporations" |
#                                                        item == "K48.0020 - Listed: ordinary shares of companies" |
#                                                        item == "K48.0021 - Unlisted: ordinary shares of companies" |
#                                                        item == "K48.0023 - Investment in units of: Unit trusts" |
#                                                        item == "K48.0024 - Property trusts"], #6
#                                                na.rm = T),
#             `XKB2336A - Loans: Mortages` = sum(value[item == "K48.0027 - Farmers: mortgage loans to" |
#                                                        item == "K48.0028 - Other non-corp buss: mortgage loans to" |
#                                                        item == "K48.0029 - Farming companies: mortgage loans to" |
#                                                        item == "K48.0030 - Other corp buss: mortgage loans to" |
#                                                        item == "K48.0031 - Indiv and non-profit instit: mortgage loans to" |
#                                                        item == "K48.0032 - Foreign borrowers: mortgage loans to" |
#                                                        item == "K48.0033 - Others: mortgage loans to"], #7
#                                                na.rm = T),
#             `XKB2337A - Loans: Public Sector` = sum(value[item == "K48.0034 - Prov govt, universities and other approved instit: other loans to" |
#                                                             item == "K48.0035 - Local authorities: other loans to" |
#                                                             item == "K48.0036 - Non-financial public corp: other loans to"], #3
#                                                     na.rm = T),
#             `XKB2338A - Loans: Other` = sum(value[item == "K48.0037 - Corp buss: other loans to" |
#                                                     item == "K48.0038 - Indiv & non-profit institutions: other loans to" |
#                                                     item == "K48.0039 - Others: other loans to"], #3
#                                             na.rm = T),
#             `XKB2339A - Fixed Property` = sum(value[item == "K48.0001 - Lease backs - land and buildings" |
#                                                       item == "K48.0002 - Other property - land and buildings" |
#                                                       item == "K48.0005 - Leverage lease equity participation(s) - Equipment and vehicles"], #3
#                                               na.rm = T),
#             `XKB2341A - Other Assets` = sum(value[item == "K48.0003 - Lease backs - equipment and vehicles" |
#                                                     item == "K48.0004 - Other equipment and vehicles" |
#                                                     item == "K48.0063 - All other foreign assets not incl under item 6, 7 and 32: Short-term - less than one year" |
#                                                     item == "K48.0064 - Long-term - one year or longer" |
#                                                     item == "K48.0067 - Accrued investment income" |
#                                                     item == "K48.0068 - Sundry debtors" |
#                                                     item == "K48.0069 - Other (specify)" |
#                                                     item == "K48.0170 - Derivative market instruments - other dom assets"], #8
#                                             na.rm = T),
#             `XKB2340A - Funds inv with ins` = sum(value[item == "K48.0065 - Other domestic assets: Insurance policies:" |
#                                                           item == "K48.0066 - Deposit administration contracts (with insurers)" |
#                                                           item == "K48.0167 - Linked policies" |
#                                                           item == "K48.0168 - Guaranteed policies" |
#                                                           item == "K48.0169 - Other policies"], #5
#                                                   na.rm = T)) %>%
#   rowwise() %>%
#   mutate(`Fixed interest securities` = sum(`XKB2331A - Fix intr sec: Gov`,
#                                            `XKB2332A - Fix intr sec: Loc Auth`,
#                                            `XKB2333A - Fix intr sec: Pub Entp`,
#                                            `XKB2334A - Fix intr sec: Other`,
#                                            na.rm = T),
#          `Loans` = sum(`XKB2336A - Loans: Mortages`,
#                        `XKB2337A - Loans: Public Sector`,
#                        `XKB2338A - Loans: Other`,
#                        na.rm = T),
#          `XKB2342A - Total Assets` = sum(`XKB2330A - Coin, bankn & dep`,
#                                          `Fixed interest securities`,
#                                          `XKB2335A - Ordinary Shares`,
#                                          `Loans`,
#                                          `XKB2339A - Fixed Property`,
#                                          `XKB2341A - Other Assets`,
#                                          `XKB2340A - Funds inv with ins`,
#                                          na.rm = T)) %>%
#   gather(item,value,3:length(.)) %>%
#   filter(item == "XKB2330A - Coin, bankn & dep" |
#            item == "Fixed interest securities" |
#            item == "XKB2335A - Ordinary Shares" |
#            item == "Loans" |
#            item == "XKB2339A - Fixed Property" |
#            item == "XKB2341A - Other Assets" |
#            item == "XKB2340A - Funds inv with ins") %>%
#   group_by(period, fund_class) %>%
#   mutate(total = sum(value, na.rm = T),
#           prop = value/total*100)

cmff_det_prop <- merge_cmff_fsca %>%
  mutate(value = as.numeric(value)*10^3,
         item = str_squish(item)) %>%
  filter(item == "K48.0022 - Mutual bank shares" |
           item == "K48.0026 - Investment in participation mortgage bonds" |
           item == "K48.0150 - Negotiable certificates of deposit (NCD's) with: Banks" |
           item == "K48.0051 - Cash and demand deposits: Banknotes and coin" |
           item == "K48.0052 - Gold coins" |
           item == "K48.0053 - Demand deposits with banks (incl current banking accounts)" |
           item == "K48.0054 - Balance with SARB" |
           item == "K48.0055 - Other deposits with and call loans to: Banks" |
           item == "K48.0056 - Other deposits incl NCD's with and call loans to: Corporation for Public Deposits (COD)" |
           item == "K48.0057 - Other deposits incl NCD's with and call loans to: Land Bank" |
           item == "K48.0058 - Mutual banks (fixed and savings deposits)" |
           item == "K48.0059 - Local authorities" |
           item == "K48.0060 - Public Investment Commissioners (PIC)" |
           item == "K48.0061 - Finance companies" |
           item == "K48.0062 - Other (specify)" |
           item == "K48.0008 - RSA Govt stock issued inSA" |
           item == "K48.0162 - RSA Govt stock issued abroad" |
           item == "K48.0010 - Provincial govt stock - investment in" |
           item == "K48.0025 - INVESTMENT IN DEFENCE BONDS" |
           item == "K48.0012 - Stock of local authorities in SA - investment in" |
           item == "K48.0013 - Public corporation stock in SA - investment in" |
           item == "K48.0013 - Public corporation stock in SA - investment in" |
           item == "K48.0165 - Non-financial corporations - investment in" |
           item == "K48.0166 - Financial corporations - investment in" |
           item == "K48.0006 - Foreign securities - investment in" |
           item == "K48.0007 - Foreign properties - investment in" |
           item == "K48.0011 - Other guarranteed & approved securities - inv in" |
           item == "K48.0014 - Listed: company stock, debentures and notes" |
           item == "K48.0015 - Unlisted: company stock, debentures and notes" |
           item == "K48.0016 - Listed: preference shares" |
           item == "K48.0017 - Unlisted: preference shares" |
           item == "K48.0040 - Bills, bankers acceptances & commercial paper issued by: Government (Treasury bills)" |
           item == "K48.0041 - Land Bank (Land Bank bills)" |
           item == "K48.0042 - Local authorities" |
           item == "K48.0043 - Bills, bankers acceptances & commercial paper issued by: (continued) Non-financial public corporations" |
           item == "K48.0044 - Government enterprises" |
           item == "K48.0045 - Financial public corporations" |
           item == "K48.0046 - Banks" |
           item == "K48.0047 - Other financial institutions" |
           item == "K48.0048 - Private sector companies" |
           item == "K48.0049 - Households (unincorporated enterprises)" |
           item == "K48.0050 - Foreigners" |
           item == "K48.0018 - Listed: ordinary shares of public corporations" |
           item == "K48.0019 - Unlisted: ordinary shares of public corporations" |
           item == "K48.0020 - Listed: ordinary shares of companies" |
           item == "K48.0021 - Unlisted: ordinary shares of companies" |
           item == "K48.0023 - Investment in units of: Unit trusts" |
           item == "K48.0024 - Property trusts" |
           item == "K48.0027 - Farmers: mortgage loans to" |
           item == "K48.0028 - Other non-corp buss: mortgage loans to" |
           item == "K48.0029 - Farming companies: mortgage loans to" |
           item == "K48.0030 - Other corp buss: mortgage loans to" |
           item == "K48.0031 - Indiv and non-profit instit: mortgage loans to" |
           item == "K48.0032 - Foreign borrowers: mortgage loans to" |
           item == "K48.0033 - Others: mortgage loans to" |
           item == "K48.0034 - Prov govt, universities and other approved instit: other loans to" |
           item == "K48.0035 - Local authorities: other loans to" |
           item == "K48.0036 - Non-financial public corp: other loans to" |
           item == "K48.0037 - Corp buss: other loans to" |
           item == "K48.0038 - Indiv & non-profit institutions: other loans to" |
           item == "K48.0039 - Others: other loans to" |
           item == "K48.0001 - Lease backs - land and buildings" |
           item == "K48.0002 - Other property - land and buildings" |
           item == "K48.0005 - Leverage lease equity participation(s) - Equipment and vehicles" |
           item == "K48.0003 - Lease backs - equipment and vehicles" |
           item == "K48.0004 - Other equipment and vehicles" |
           item == "K48.0063 - All other foreign assets not incl under item 6, 7 and 32: Short-term - less than one year" |
           item == "K48.0064 - Long-term - one year or longer" |
           item == "K48.0067 - Accrued investment income" |
           item == "K48.0068 - Sundry debtors" |
           item == "K48.0069 - Other (specify)" |      
           item == "K48.0170 - Derivative market instruments - other dom assets" |
           item == "K48.0065 - Other domestic assets: Insurance policies:" |
           item == "K48.0066 - Deposit administration contracts (with insurers)" |
           item == "K48.0167 - Linked policies" |              
           item == "K48.0168 - Guaranteed policies" |
           item == "K48.0169 - Other policies") %>%
  mutate(item = case_when(item == "K48.0022 - Mutual bank shares" & fund_class == "Pension Fund" ~ "K48.0023 - Investment in units of: Unit trusts",
                          item == "K48.0007 - Foreign properties - investment in" & fund_class == "Provident Fund" ~ "K48.0006 - Foreign securities - investment in",
                          item == "K48.0162 - RSA Govt stock issued abroad" & fund_class == "Pension Fund" ~ "K48.0008 - RSA Govt stock issued inSA",
                          item == "K48.0054 - Balance with SARB" & fund_class == "Pension Fund" ~ "K48.0056 - Other deposits incl NCD's with and call loans to: Corporation for Public Deposits (COD)",
                          item == "K48.0049 - Households (unincorporated enterprises)" & (fund_class == "Pension Fund" | fund_class == "Provident Fund") ~ "K48.0048 - Private sector companies",
                          item == "K48.0010 - Provincial govt stock - investment in" & (fund_class == "Pension Fund" | fund_class == "Provident Fund") ~ "K48.0008 - RSA Govt stock issued inSA",
                          item == "K48.0042 - Local authorities" & fund_class == "Provident Fund" ~ "K48.0041 - Land Bank (Land Bank bills)",
                          item == "K48.0017 - Unlisted: preference shares" & fund_class == "Pension Fund" ~ "K48.0021 - Unlisted: ordinary shares of companies",
                          item == "K48.0026 - Investment in participation mortgage bonds" & fund_class == "Pension Fund" ~ "K48.0037 - Corp buss: other loans to",
                          item == "K48.0061 - Finance companies" & fund_class == "Pension Fund" ~ "K48.0053 - Demand deposits with banks (incl current banking accounts)",
                          item == "K48.0062 - Other (specify)" & fund_class == "Pension Fund" ~ "K48.0167 - Linked policies",
                          item == "K48.0033 - Others: mortgage loans to" & fund_class == "Provident Fund" ~ "K48.0031 - Indiv and non-profit instit: mortgage loans to",
                          item == "K48.0019 - Unlisted: ordinary shares of public corporations" & (fund_class == "Pension Fund" | fund_class == "Provident Fund") ~ "K48.0021 - Unlisted: ordinary shares of companies",
                          TRUE ~ item)) %>%
  group_by(period, fund_class, item) %>%
  summarise(tot = sum(value, na.rm = T)) %>%
  group_by(period, fund_class) %>%
  mutate(summate = sum(tot,na.rm=T)) %>%
  group_by(period,fund_class) %>%
  mutate(prop = tot/sum(tot, na.rm = T)*100)

forecast <- bind_rows(read_rds("data/CMJM073E.rds"), read_rds("data/JFIA001E.rds")) %>%
  group_by(code, quarter = quarter(date), year = year(date)) %>%
  filter(date == max(date) & value != 0) %>%
  group_by(code) %>%
  mutate(qq = value/lag(value)) %>%
  group_by(date) %>%
  summarise(comp = mean(qq, na.rm = T)) %>%
  mutate(date = as.Date(date))

cmff_det_prop <-
  bind_rows(
    cmff_det_prop,
    cmff_det_prop %>%
      ungroup() %>%
      filter(period == "2010-03-01") %>%
      mutate(period = as.Date("2020-12-01"),
             prop = NA_real_,
             summate = NA_real_)) %>%
  group_by(item,fund_class) %>%
  fill(prop,summate,.direction="down") %>%
  left_join(.,forecast,by=c("period"="date")) %>%
  mutate(tot = case_when(period == "2020-12-01" ~ summate*comp*prop/100,
                             TRUE ~ tot)) %>%
  ungroup() %>%
  select(-c(summate,comp))

write_rds(cmff_det_prop, "data/cmff/cmff_det_prop.rds","gz")

# cmff_liab_agg_prop <- merge_cmff_fsca %>%
#   mutate(value = as.numeric(value)*10^3,
#          item = str_squish(item)) %>%
#   filter(item == "K48.0071 - LIABILITIES Accumulated funds: In respect of obligations to members in SA" |
#            item == "K48.0072 - In respect of obligations to members of foreigncountries, if not available, include under item 71" |
#            item == "K48.0073 - Reserves and provisions" |
#            item == "K48.0074 - Bank overdraft" |
#            item == "K48.0075 - Sundry creditors: Amountsdue to members" |
#            item == "K48.0076 - Other creditors" |
#            item == "K48.0077 - All other foreign liabilities not incl under item 72 Short-term - less than one year" |
#            item == "K48.0078 - Long-term - one year or longer" |
#            item == "K48.0079 - Other domestic liabilities" |
#            item == "K48.0171 - Derivative market instruments - liabilities") %>%
#   group_by(period, fund_class) %>%
#   summarise(tot = sum(value, na.rm = T)) %>%
#   group_by(period) %>%
#   mutate(prop = tot/sum(tot, na.rm = T)*100)

cmff_liab_det_prop <- merge_cmff_fsca %>%
  mutate(value = as.numeric(value)*10^3,
         item = str_squish(item)) %>%
  filter(item == "K48.0071 - LIABILITIES Accumulated funds: In respect of obligations to members in SA" |
           item == "K48.0072 - In respect of obligations to members of foreigncountries, if not available, include under item 71" |
           item == "K48.0073 - Reserves and provisions" |
           item == "K48.0074 - Bank overdraft" |
           item == "K48.0075 - Sundry creditors: Amountsdue to members" |
           item == "K48.0076 - Other creditors" |
           item == "K48.0077 - All other foreign liabilities not incl under item 72 Short-term - less than one year" |
           item == "K48.0078 - Long-term - one year or longer" |
           item == "K48.0079 - Other domestic liabilities" |
           item == "K48.0171 - Derivative market instruments - liabilities") %>%
  group_by(period, fund_class, item) %>%
  summarise(tot = sum(value, na.rm = T)) %>%
  group_by(period, fund_class, item) %>%
  mutate(summate = sum(tot,na.rm=T)) %>%
  group_by(period, fund_class) %>%
  mutate(prop = tot/sum(tot, na.rm = T)*100)

cmff_liab_det_prop <-
  bind_rows(
    cmff_liab_det_prop,
    cmff_liab_det_prop %>%
      ungroup() %>%
      filter(period == "2010-03-01") %>%
      mutate(period = as.Date("2020-12-01"),
             prop = NA_real_,
             summate = NA_real_)) %>%
  group_by(item,fund_class) %>%
  fill(prop,summate,.direction="down") %>%
  left_join(.,forecast,by=c("period"="date")) %>%
  mutate(tot = case_when(period == "2020-12-01" ~ summate*comp*prop/100,
                         TRUE ~ tot)) %>%
  ungroup() %>%
  select(-c(summate,comp))

write_rds(cmff_liab_det_prop, "data/cmff/cmff_liab_det_prop.rds","gz")

cmff_tot <- merge_cmff_fsca %>%
  mutate(value = as.numeric(value)*10^3,
         item = str_squish(item)) %>%
  filter(item == "K48.0082 - Income from own and other non-insurance administrated funds: Interest" |
           item == "K48.0083 - Dividends" |
           item == "K48.0084 - Rent" |
           item == "K48.0085 - Contributions by: Members" |
           item == "K48.0086 - Employers" |
           item == "K48.0096 - Benefits paid during period: Annuities" |
           item == "K48.0097 - Lump sum payments on retirement or death" |
           item == "K48.0098 - Amounts paid on resignation" |
           item == "K48.0099 - Other (incl monthly payments)" |
           item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
           item == "K48.0093 - Other (incl reinsurance premiums)" |
           item == "K48.0095 - Other provisions (excl tax)" |
           # item == "K48.0163 - Reinsurance recoveries" |
           # item == "K48.0164 - Unclaimed benefits forfeited" |
           # item == "K48.0088 - Profit on sale of assets or redemption of investments" |
           item == "K48.0090 - Other") %>%
           # item == "K48.0094 - Depreciation") %>%
           # item == "K48.82C - Inc own, other non-ins" |
           # item == "K48.83C - Inc own, other non-ins" |
           # item == "K48.84C - Inc own, other non-ins" |
           # item == "K48.85C - Contrib by: Members" |
           # item == "K48.86C - Contrib by: Employers" |
           # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
           # item == "K48.0103 - Other" |
           # item == "K48.92C - Admin exp: Wages, salar" |
           # item == "K48.93C - Admin exp: Other" |
           # item == "K48.94C - Depreciation" |
           # item == "K48.95C - Other provisions" |
           # item == "K48.96C - Benf: Annuities" |
           # item == "K48.97C - Benf: Lump sum ret" |
           # item == "K48.98C - Benf: Amounts on resign" |
           # item == "K48.99C - Benf: Other" |
           # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
           # item == "K48.0157 - Interest" |
           # item == "K48.0158 - Other" |
           # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
           # item == "K48.0160 - Interest" |
           # item == "K48.0161 - Other" |
           # item == "K48.0081 - INCOME: Investment income received from insurers i.r.o. policies and othe insurer administrated funds" |
           # item == "K48.0087 - Amounts recieved on transfer from other funds"  |
           # item == "K48.0089 - Amount by which investments were writtenup" |
           # item == "K48.0100 - Amounts transferred to other funds") %>%
           #item == "K48.0102 - Amount by which investments were writtendown") %>%
  mutate(value = case_when(
    # Current expenditure
    item == "K48.0096 - Benefits paid during period: Annuities" |
      item == "K48.0097 - Lump sum payments on retirement or death" |
      item == "K48.0098 - Amounts paid on resignation" |
      item == "K48.0099 - Other (incl monthly payments)" |
      item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
      item == "K48.0093 - Other (incl reinsurance premiums)" |
      item == "K48.0095 - Other provisions (excl tax)" |
      #Net cap prof minus
      # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
      # item == "K48.0103 - Other" |
      # item == "K48.92C - Admin exp: Wages, salar" |
      # item == "K48.93C - Admin exp: Other" |
      # item == "K48.94C - Depreciation" |
      # item == "K48.95C - Other provisions" |
      # item == "K48.96C - Benf: Annuities" |
      # item == "K48.97C - Benf: Lump sum ret" |
      # item == "K48.98C - Benf: Amounts on resign" |
      # item == "K48.99C - Benf: Other" |
      # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
      # item == "K48.0157 - Interest" |
      # item == "K48.0158 - Other" |
      # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
      # item == "K48.0160 - Interest" |
      # item == "K48.0161 - Other" |
      # Other
      item == "K48.0100 - Amounts transferred to other funds"  |
      item == "K48.0102 - Amount by which investments were writtendown" ~
      -value,
    TRUE ~ value)) %>%
  group_by(period, fund_class, item) %>%
  summarise(value_orig = sum(value, na.rm = T)) %>%
  mutate(k48_class = case_when(item == "K48.0085 - Contributions by: Members" ~ 
                                 "XKB2117A - Contributions by: Members",
                               item == "K48.0086 - Employers" ~ 
                                 "XKB2118A - Contributions by: Employers",
                               item == "K48.0083 - Dividends" ~ 
                                 "XKB2120A - Investment income: Dividends",
                               item == "K48.0082 - Income from own and other non-insurance administrated funds: Interest" ~ 
                                 "XKB2115A - Investment income: Interest",
                               item == "K48.0084 - Rent" ~ 
                                 "XKB2116A - Investment income: Rent",
                               item == "K48.0081 - INCOME: Investment income received from insurers i.r.o. policies and othe insurer administrated funds" ~ 
                                 "XKB2153A - Inv inc from Insurers",
                               item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
                                 item == "K48.0093 - Other (incl reinsurance premiums)" |
                                 item == "K48.0095 - Other provisions (excl tax)" ~
                                 "XKB2150A - Admin Expenses",
                               item == "K48.0089 - Amount by which investments were writtenup" |
                                 item == "K48.0102 - Amount by which investments were writtendown" ~
                                 "XKB2155A - Net Asset revaluation",
                               # item == "K48.0163 - Reinsurance recoveries" |
                               #   item == "K48.0164 - Unclaimed benefits forfeited" |
                                 # item == "K48.0088 - Profit on sale of assets or redemption of investments" |
                                 item == "K48.0090 - Other" ~
                                 # item == "K48.0094 - Depreciation" ~
                                 # item == "K48.82C - Inc own, other non-ins" |
                                 # item == "K48.83C - Inc own, other non-ins" |
                                 # item == "K48.84C - Inc own, other non-ins" |
                                 # item == "K48.85C - Contrib by: Members" |
                                 # item == "K48.86C - Contrib by: Employers" |
                                 # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
                                 # item == "K48.0103 - Other" |
                                 # item == "K48.92C - Admin exp: Wages, salar" |
                                 # item == "K48.93C - Admin exp: Other" |
                                 # item == "K48.94C - Depreciation" |
                                 # item == "K48.95C - Other provisions" |
                                 # item == "K48.96C - Benf: Annuities" |
                                 # item == "K48.97C - Benf: Lump sum ret" |
                                 # item == "K48.98C - Benf: Amounts on resign" |
                                 # item == "K48.99C - Benf: Other" |
                                 # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
                                 # item == "K48.0157 - Interest" |
                                 # item == "K48.0158 - Other" |
                                 # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
                                 # item == "K48.0160 - Interest" |
                                 # item == "K48.0161 - Other" ~
                                 "XKB2152A - Net Cap prof and other",
                               TRUE ~ item)) %>%
  mutate(k48_class_agg = case_when(k48_class == "XKB2117A - Contributions by: Members" |
                                     k48_class == "XKB2118A - Contributions by: Employers" ~
                                     "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)",
                                   k48_class == "XKB2120A - Investment income: Dividends" |
                                     k48_class == "XKB2115A - Investment income: Interest" |
                                     k48_class == "XKB2116A - Investment income: Rent" |
                                     k48_class == "XKB2153A - Inv inc from Insurers" |
                                     k48_class == "XKB2155A - Net Asset revaluation" ~
                                     "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)",
                                   k48_class == "XKB2152A - Net Cap prof and other" ~
                                     "OTHER INCOME (TOTAL CURRENT PERIOD)",
                                   k48_class == "K48.0087 - Amounts recieved on transfer from other funds" ~
                                     "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)",
                                   k48_class == "XKB2150A - Admin Expenses" ~
                                     "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)",
                                   k48_class == "K48.0100 - Amounts transferred to other funds" ~
                                     "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)",
                                   k48_class == "K48.0096 - Benefits paid during period: Annuities" |
                                     k48_class == "K48.0097 - Lump sum payments on retirement or death" |
                                     # k48_class == "K48.0098 - Amounts paid on resignation" ~
                                     k48_class == "K48.0099 - Other (incl monthly payments)" ~
                                     "BENEFITS (TOTAL CURRENT PERIOD)",
                                   TRUE ~ NA_character_),
         year = as.character(year(period))) %>%
  group_by(period, k48_class_agg) %>%
  summarise(value_tot = sum(value_orig, na.rm = T))

# cmff_is_det_prop2 <- merge_cmff_fsca %>%
#   mutate(value = as.numeric(value)*10^3,
#          item = str_squish(item)) %>%
#   # mutate(item = case_when(fund_class == "Retirement Annuity" &
#   #                            item == "K48.0098 - Amounts paid on resignation" ~
#   #                            "K48.0099 - Other (incl monthly payments",
#   #                          TRUE ~ item)) %>%
#   filter(item == "K48.0082 - Income from own and other non-insurance administrated funds: Interest" |
#            item == "K48.0083 - Dividends" |
#            item == "K48.0084 - Rent" |
#            item == "K48.0085 - Contributions by: Members" |
#            item == "K48.0086 - Employers" |
#            item == "K48.0096 - Benefits paid during period: Annuities" |
#            item == "K48.0097 - Lump sum payments on retirement or death" |
#            #item == "K48.0098 - Amounts paid on resignation" |
#            item == "K48.0099 - Other (incl monthly payments)" |
#            item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
#            item == "K48.0093 - Other (incl reinsurance premiums)" |
#            item == "K48.0095 - Other provisions (excl tax)" |
#            # item == "K48.0163 - Reinsurance recoveries" |
#            # item == "K48.0164 - Unclaimed benefits forfeited" |
#            # item == "K48.0088 - Profit on sale of assets or redemption of investments" |
#            item == "K48.0090 - Other" |
#            # item == "K48.0094 - Depreciation" |
#            # item == "K48.82C - Inc own, other non-ins" |
#            # item == "K48.83C - Inc own, other non-ins" |
#            # item == "K48.84C - Inc own, other non-ins" |
#            # item == "K48.85C - Contrib by: Members" |
#            # item == "K48.86C - Contrib by: Employers" |
#            # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
#            # item == "K48.0103 - Other" |
#            # item == "K48.92C - Admin exp: Wages, salar" |
#            # item == "K48.93C - Admin exp: Other" |
#            # item == "K48.94C - Depreciation" |
#            # item == "K48.95C - Other provisions" |
#            # item == "K48.96C - Benf: Annuities" |
#            # item == "K48.97C - Benf: Lump sum ret" |
#            # item == "K48.98C - Benf: Amounts on resign" |
#            # item == "K48.99C - Benf: Other" |
#            # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
#            # item == "K48.0157 - Interest" |
#            # item == "K48.0158 - Other" |
#            # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
#            # item == "K48.0160 - Interest" |
#            # item == "K48.0161 - Other" |
#            item == "K48.0081 - INCOME: Investment income received from insurers i.r.o. policies and othe insurer administrated funds" |
#            item == "K48.0087 - Amounts recieved on transfer from other funds"  |
#            item == "K48.0089 - Amount by which investments were writtenup" |
#            item == "K48.0100 - Amounts transferred to other funds") %>%
#            #item == "K48.0102 - Amount by which investments were writtendown") %>%
#     mutate(value = case_when(
#     # Current expenditure
#     item == "K48.0096 - Benefits paid during period: Annuities" |
#       item == "K48.0097 - Lump sum payments on retirement or death" |
#       #item == "K48.0098 - Amounts paid on resignation" |
#       item == "K48.0099 - Other (incl monthly payments)" |
#       item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
#       item == "K48.0093 - Other (incl reinsurance premiums)" |
#       item == "K48.0095 - Other provisions (excl tax)" |
#       #Net cap prof minus
#       # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
#       # item == "K48.0103 - Other" |
#       # item == "K48.92C - Admin exp: Wages, salar" |
#       # item == "K48.93C - Admin exp: Other" |
#       # item == "K48.94C - Depreciation" |
#       # item == "K48.95C - Other provisions" |
#       # item == "K48.96C - Benf: Annuities" |
#       # item == "K48.97C - Benf: Lump sum ret" |
#       # item == "K48.98C - Benf: Amounts on resign" |
#       # item == "K48.99C - Benf: Other" |
#       # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
#       # item == "K48.0157 - Interest" |
#       # item == "K48.0158 - Other" |
#       # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
#       # item == "K48.0160 - Interest" |
#       # item == "K48.0161 - Other" |
#       # Other
#       item == "K48.0100 - Amounts transferred to other funds"  |
#       item == "K48.0102 - Amount by which investments were writtendown" ~
#       -abs(value),
#     TRUE ~ abs(value))) %>%
#   group_by(period, fund_class, item) %>%
#   summarise(value_orig = sum(value, na.rm = T)) %>%
#   mutate(value = case_when(
#     # Current expenditure
#     item == "K48.0096 - Benefits paid during period: Annuities" |
#       item == "K48.0097 - Lump sum payments on retirement or death" |
#       #item == "K48.0098 - Amounts paid on resignation" |
#       item == "K48.0099 - Other (incl monthly payments)" |
#       item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
#       item == "K48.0093 - Other (incl reinsurance premiums)" |
#       item == "K48.0095 - Other provisions (excl tax)" |
#       #Net cap prof minus
#       # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
#       # item == "K48.0103 - Other" |
#       # item == "K48.92C - Admin exp: Wages, salar" |
#       # item == "K48.93C - Admin exp: Other" |
#       # item == "K48.94C - Depreciation" |
#       # item == "K48.95C - Other provisions" |
#       # item == "K48.96C - Benf: Annuities" |
#       # item == "K48.97C - Benf: Lump sum ret" |
#       # item == "K48.98C - Benf: Amounts on resign" |
#       # item == "K48.99C - Benf: Other" |
#       # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
#       # item == "K48.0157 - Interest" |
#       # item == "K48.0158 - Other" |
#       # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
#       # item == "K48.0160 - Interest" |
#       # item == "K48.0161 - Other" |
#       # Other
#       item == "K48.0100 - Amounts transferred to other funds"  |
#       item == "K48.0102 - Amount by which investments were writtendown" ~
#       -abs(value_orig),
#     TRUE ~ abs(value_orig))) %>%
#   mutate(k48_class = case_when(item == "K48.0085 - Contributions by: Members" ~ 
#                                  "XKB2117A - Contributions by: Members",
#                                item == "K48.0086 - Employers" ~ 
#                                  "XKB2118A - Contributions by: Employers",
#                                item == "K48.0083 - Dividends" ~ 
#                                  "XKB2120A - Investment income: Dividends",
#                                item == "K48.0082 - Income from own and other non-insurance administrated funds: Interest" ~ 
#                                  "XKB2115A - Investment income: Interest",
#                                item == "K48.0084 - Rent" ~ 
#                                  "XKB2116A - Investment income: Rent",
#                                item == "K48.0081 - INCOME: Investment income received from insurers i.r.o. policies and othe insurer administrated funds" ~ 
#                                  "XKB2153A - Inv inc from Insurers",
#                                item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
#                                  item == "K48.0093 - Other (incl reinsurance premiums)" |
#                                  item == "K48.0095 - Other provisions (excl tax)" ~
#                                  "XKB2150A - Admin Expenses",
#                                item == "K48.0089 - Amount by which investments were writtenup" |
#                                  item == "K48.0102 - Amount by which investments were writtendown" ~
#                                  "XKB2155A - Net Asset revaluation",
#                                # item == "K48.0163 - Reinsurance recoveries" |
#                                #   item == "K48.0164 - Unclaimed benefits forfeited" |
#                                  # item == "K48.0088 - Profit on sale of assets or redemption of investments" |
#                                  item == "K48.0090 - Other" ~
#                                  # item == "K48.0094 - Depreciation" ~
#                                  # item == "K48.82C - Inc own, other non-ins" |
#                                  # item == "K48.83C - Inc own, other non-ins" |
#                                  # item == "K48.84C - Inc own, other non-ins" |
#                                  # item == "K48.85C - Contrib by: Members" |
#                                  # item == "K48.86C - Contrib by: Employers" ~
#                                  # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
#                                  # item == "K48.0103 - Other" |
#                                  # item == "K48.92C - Admin exp: Wages, salar" |
#                                  # item == "K48.93C - Admin exp: Other" |
#                                  # item == "K48.94C - Depreciation" |
#                                  # item == "K48.95C - Other provisions" |
#                                  # item == "K48.96C - Benf: Annuities" |
#                                  # item == "K48.97C - Benf: Lump sum ret" |
#                                  # item == "K48.98C - Benf: Amounts on resign" |
#                                  # item == "K48.99C - Benf: Other" |
#                                  # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
#                                  # item == "K48.0157 - Interest" |
#                                  # item == "K48.0158 - Other" |
#                                  # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
#                                  # item == "K48.0160 - Interest" |
#                                  # item == "K48.0161 - Other" ~
#                              "XKB2152A - Net Cap prof and other",
#                            TRUE ~ item)) %>%
#   mutate(k48_class_agg = case_when(k48_class == "XKB2117A - Contributions by: Members" |
#                                      k48_class == "XKB2118A - Contributions by: Employers" ~
#                                      "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)",
#                                    k48_class == "XKB2120A - Investment income: Dividends" |
#                                      k48_class == "XKB2115A - Investment income: Interest" |
#                                      k48_class == "XKB2116A - Investment income: Rent" |
#                                      k48_class == "XKB2153A - Inv inc from Insurers" |
#                                      k48_class == "XKB2155A - Net Asset revaluation" ~
#                                      "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)",
#                                    k48_class == "XKB2152A - Net Cap prof and other" ~
#                                      "OTHER INCOME (TOTAL CURRENT PERIOD)",
#                                    k48_class == "K48.0087 - Amounts recieved on transfer from other funds" ~
#                                      "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)",
#                                    k48_class == "XKB2150A - Admin Expenses" ~
#                                      "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)",
#                                    k48_class == "K48.0100 - Amounts transferred to other funds" ~
#                                      "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)",
#                                    k48_class == "K48.0096 - Benefits paid during period: Annuities" |
#                                      k48_class == "K48.0097 - Lump sum payments on retirement or death" |
#                                      #k48_class == "K48.0098 - Amounts paid on resignation" |
#                                      k48_class == "K48.0099 - Other (incl monthly payments)" ~
#                                      "BENEFITS (TOTAL CURRENT PERIOD)",
#                                    TRUE ~ NA_character_),
#          year = as.character(year(period))) %>%
#   group_by(period, fund_class, k48_class_agg) %>%
#   mutate(k48_tot = sum(value_orig, na.rm = T)) %>%
#   group_by(period, item, fund_class) %>%
#   mutate(k48_class_prop = sum(value_orig, na.rm = T)/sum(k48_tot, na.rm = T)) %>%
#   # group_by(year, fund_class, k48_class_agg) %>%
#   # mutate(k48_class_prop_rev = sum(value_orig, na.rm = T)/sum(k48_tot, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(k48_class_prop = case_when(fund_class == "Retirement Annuity" ~ NA_real_,
#                                     TRUE ~ k48_class_prop)) %>%
#   # group_by(period, item) %>%
#   # mutate(k48_class_prop1 = mean(k48_class_prop, na.rm=T)) %>%
#   mutate(k48_class_prop1 = case_when(fund_class == "Pension Fund" ~ k48_class_prop,
#                                      TRUE ~ NA_real_)) %>%
#   group_by(period,item) %>%
#   fill(k48_class_prop1, .direction = "down") %>%
#   fill(k48_class_prop1, .direction = "up") %>%
#   ungroup() %>%
#   mutate(k48_class_prop = case_when(fund_class == "Retirement Annuity"  ~ k48_class_prop1,
#                                     TRUE ~ k48_class_prop))

cmff_is_det_prop2 <- merge_cmff_fsca %>%
  mutate(value = as.numeric(value)*10^3,
         item = str_squish(item),
         period = as.character(year(period))) %>%
  # mutate(item = case_when(fund_class == "Retirement Annuity" &
  #                            item == "K48.0098 - Amounts paid on resignation" ~
  #                            "K48.0099 - Other (incl monthly payments",
  #                          TRUE ~ item)) %>%
  filter(item == "K48.0082 - Income from own and other non-insurance administrated funds: Interest" |
           item == "K48.0083 - Dividends" |
           item == "K48.0084 - Rent" |
           item == "K48.0085 - Contributions by: Members" |
           item == "K48.0086 - Employers" |
           item == "K48.0096 - Benefits paid during period: Annuities" |
           item == "K48.0097 - Lump sum payments on retirement or death" |
           item == "K48.0098 - Amounts paid on resignation" |
           item == "K48.0099 - Other (incl monthly payments)" |
           item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
           item == "K48.0093 - Other (incl reinsurance premiums)" |
           item == "K48.0095 - Other provisions (excl tax)" |
           # item == "K48.0163 - Reinsurance recoveries" |
           # item == "K48.0164 - Unclaimed benefits forfeited" |
           # item == "K48.0088 - Profit on sale of assets or redemption of investments" |
           item == "K48.0090 - Other" |
           # item == "K48.0094 - Depreciation" |
           # item == "K48.82C - Inc own, other non-ins" |
           # item == "K48.83C - Inc own, other non-ins" |
           # item == "K48.84C - Inc own, other non-ins" |
           # item == "K48.85C - Contrib by: Members" |
           # item == "K48.86C - Contrib by: Employers" |
           # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
           # item == "K48.0103 - Other" |
           # item == "K48.92C - Admin exp: Wages, salar" |
           # item == "K48.93C - Admin exp: Other" |
           # item == "K48.94C - Depreciation" |
         # item == "K48.95C - Other provisions" |
         # item == "K48.96C - Benf: Annuities" |
         # item == "K48.97C - Benf: Lump sum ret" |
         # item == "K48.98C - Benf: Amounts on resign" |
         # item == "K48.99C - Benf: Other" |
         # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
         # item == "K48.0157 - Interest" |
         # item == "K48.0158 - Other" |
         # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
         # item == "K48.0160 - Interest" |
         # item == "K48.0161 - Other" |
         item == "K48.0081 - INCOME: Investment income received from insurers i.r.o. policies and othe insurer administrated funds" |
           item == "K48.0087 - Amounts recieved on transfer from other funds"  |
           item == "K48.0089 - Amount by which investments were writtenup" |
           item == "K48.0100 - Amounts transferred to other funds") %>%
  #item == "K48.0102 - Amount by which investments were writtendown") %>%
  mutate(value = case_when(
    # Current expenditure
    item == "K48.0096 - Benefits paid during period: Annuities" |
      item == "K48.0097 - Lump sum payments on retirement or death" |
      item == "K48.0098 - Amounts paid on resignation" |
      item == "K48.0099 - Other (incl monthly payments)" |
      item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
      item == "K48.0093 - Other (incl reinsurance premiums)" |
      item == "K48.0095 - Other provisions (excl tax)" |
      #Net cap prof minus
      # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
      # item == "K48.0103 - Other" |
      # item == "K48.92C - Admin exp: Wages, salar" |
      # item == "K48.93C - Admin exp: Other" |
      # item == "K48.94C - Depreciation" |
      # item == "K48.95C - Other provisions" |
      # item == "K48.96C - Benf: Annuities" |
      # item == "K48.97C - Benf: Lump sum ret" |
      # item == "K48.98C - Benf: Amounts on resign" |
      # item == "K48.99C - Benf: Other" |
    # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
    # item == "K48.0157 - Interest" |
    # item == "K48.0158 - Other" |
    # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
    # item == "K48.0160 - Interest" |
    # item == "K48.0161 - Other" |
    # Other
    item == "K48.0100 - Amounts transferred to other funds"  |
      item == "K48.0102 - Amount by which investments were writtendown" ~
      -abs(value),
    TRUE ~ abs(value))) %>%
  group_by(period, fund_class, item) %>%
  summarise(value_orig = sum(value, na.rm = T)) %>%
  mutate(value = case_when(
    # Current expenditure
    item == "K48.0096 - Benefits paid during period: Annuities" |
      item == "K48.0097 - Lump sum payments on retirement or death" |
      item == "K48.0098 - Amounts paid on resignation" |
      item == "K48.0099 - Other (incl monthly payments)" |
      item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
      item == "K48.0093 - Other (incl reinsurance premiums)" |
      item == "K48.0095 - Other provisions (excl tax)" |
      #Net cap prof minus
      # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
      # item == "K48.0103 - Other" |
      # item == "K48.92C - Admin exp: Wages, salar" |
      # item == "K48.93C - Admin exp: Other" |
      # item == "K48.94C - Depreciation" |
      # item == "K48.95C - Other provisions" |
      # item == "K48.96C - Benf: Annuities" |
      # item == "K48.97C - Benf: Lump sum ret" |
      # item == "K48.98C - Benf: Amounts on resign" |
      # item == "K48.99C - Benf: Other" |
    # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
    # item == "K48.0157 - Interest" |
    # item == "K48.0158 - Other" |
    # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
    # item == "K48.0160 - Interest" |
    # item == "K48.0161 - Other" |
    # Other
    item == "K48.0100 - Amounts transferred to other funds"  |
      item == "K48.0102 - Amount by which investments were writtendown" ~
      -abs(value_orig),
    TRUE ~ abs(value_orig))) %>%
  mutate(k48_class = case_when(item == "K48.0085 - Contributions by: Members" ~ 
                                 "XKB2117A - Contributions by: Members",
                               item == "K48.0086 - Employers" ~ 
                                 "XKB2118A - Contributions by: Employers",
                               item == "K48.0083 - Dividends" ~ 
                                 "XKB2120A - Investment income: Dividends",
                               item == "K48.0082 - Income from own and other non-insurance administrated funds: Interest" ~ 
                                 "XKB2115A - Investment income: Interest",
                               item == "K48.0084 - Rent" ~ 
                                 "XKB2116A - Investment income: Rent",
                               item == "K48.0081 - INCOME: Investment income received from insurers i.r.o. policies and othe insurer administrated funds" ~ 
                                 "XKB2153A - Inv inc from Insurers",
                               item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
                                 item == "K48.0093 - Other (incl reinsurance premiums)" |
                                 item == "K48.0095 - Other provisions (excl tax)" ~
                                 "XKB2150A - Admin Expenses",
                               item == "K48.0089 - Amount by which investments were writtenup" |
                                 item == "K48.0102 - Amount by which investments were writtendown" ~
                                 "XKB2155A - Net Asset revaluation",
                               # item == "K48.0163 - Reinsurance recoveries" |
                               #   item == "K48.0164 - Unclaimed benefits forfeited" |
                               # item == "K48.0088 - Profit on sale of assets or redemption of investments" |
                               item == "K48.0090 - Other" ~
                                 # item == "K48.0094 - Depreciation" ~
                                 # item == "K48.82C - Inc own, other non-ins" |
                                 # item == "K48.83C - Inc own, other non-ins" |
                                 # item == "K48.84C - Inc own, other non-ins" |
                                 # item == "K48.85C - Contrib by: Members" |
                                 # item == "K48.86C - Contrib by: Employers" ~
                                 # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
                                 # item == "K48.0103 - Other" |
                                 # item == "K48.92C - Admin exp: Wages, salar" |
                                 # item == "K48.93C - Admin exp: Other" |
                                 # item == "K48.94C - Depreciation" |
                               # item == "K48.95C - Other provisions" |
                               # item == "K48.96C - Benf: Annuities" |
                               # item == "K48.97C - Benf: Lump sum ret" |
                               # item == "K48.98C - Benf: Amounts on resign" |
                               # item == "K48.99C - Benf: Other" |
                               # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
                               # item == "K48.0157 - Interest" |
                               # item == "K48.0158 - Other" |
                               # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
                               # item == "K48.0160 - Interest" |
                               # item == "K48.0161 - Other" ~
                               "XKB2152A - Net Cap prof and other",
                               TRUE ~ item)) %>%
  mutate(k48_class_agg = case_when(k48_class == "XKB2117A - Contributions by: Members" |
                                     k48_class == "XKB2118A - Contributions by: Employers" ~
                                     "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)",
                                   k48_class == "XKB2120A - Investment income: Dividends" |
                                     k48_class == "XKB2115A - Investment income: Interest" |
                                     k48_class == "XKB2116A - Investment income: Rent" |
                                     k48_class == "XKB2153A - Inv inc from Insurers" |
                                     k48_class == "XKB2155A - Net Asset revaluation" ~
                                     "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)",
                                   k48_class == "XKB2152A - Net Cap prof and other" ~
                                     "OTHER INCOME (TOTAL CURRENT PERIOD)",
                                   k48_class == "K48.0087 - Amounts recieved on transfer from other funds" ~
                                     "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)",
                                   k48_class == "XKB2150A - Admin Expenses" ~
                                     "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)",
                                   k48_class == "K48.0100 - Amounts transferred to other funds" ~
                                     "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)",
                                   k48_class == "K48.0096 - Benefits paid during period: Annuities" |
                                     k48_class == "K48.0097 - Lump sum payments on retirement or death" |
                                     #k48_class == "K48.0098 - Amounts paid on resignation" ~
                                     k48_class == "K48.0099 - Other (incl monthly payments)" ~
                                     "BENEFITS (TOTAL CURRENT PERIOD)",
                                   TRUE ~ NA_character_)) %>%
  group_by(period, fund_class, k48_class_agg) %>%
  mutate(k48_tot = sum(value_orig, na.rm = T)) %>%
  group_by(period, item, fund_class) %>%
  mutate(k48_class_prop = sum(value_orig, na.rm = T)/sum(k48_tot, na.rm = T)) %>%
  # group_by(year, fund_class, k48_class_agg) %>%
  # mutate(k48_class_prop_rev = sum(value_orig, na.rm = T)/sum(k48_tot, na.rm = T)) %>%
  ungroup() %>%
  mutate(k48_class_prop = case_when(fund_class == "Retirement Annuity" ~ NA_real_,
                                    TRUE ~ k48_class_prop)) %>%
  # group_by(period, item) %>%
  # mutate(k48_class_prop1 = mean(k48_class_prop, na.rm=T)) %>%
  mutate(k48_class_prop1 = case_when(fund_class == "Pension Fund" ~ k48_class_prop,
                                     TRUE ~ NA_real_)) %>%
  group_by(period,item) %>%
  fill(k48_class_prop1, .direction = "down") %>%
  fill(k48_class_prop1, .direction = "up") %>%
  ungroup() %>%
  mutate(k48_class_prop = case_when(fund_class == "Retirement Annuity"  ~ k48_class_prop1,
                                    TRUE ~ k48_class_prop)) 

cmff_is_det_prop2 <-
  bind_rows(
    cmff_is_det_prop2,
    cmff_is_det_prop2 %>%
      filter(period == "2010") %>%
      mutate(k48_class_prop = NA_real_,
             period = case_when(period == "2010" ~ as.character(params_cmff_year+1)))
  ) %>%
  group_by(fund_class,item,k48_class_agg) %>%
  fill(k48_class_prop, .direction = "down")

write_rds(cmff_is_det_prop2, "data/cmff/cmff_is_det_prop2.rds","gz")

tmp_det_prop_hf <- merge_cmff_fsca %>%
  mutate(value = as.numeric(value)*10^3,
         item = str_squish(item)) %>%
  # mutate(item = case_when(fund_class == "Retirement Annuity" &
  #                            item == "K48.0098 - Amounts paid on resignation" ~
  #                            "K48.0099 - Other (incl monthly payments",
  #                          TRUE ~ item)) %>%
  filter(item == "K48.0082 - Income from own and other non-insurance administrated funds: Interest" |
           item == "K48.0083 - Dividends" |
           item == "K48.0084 - Rent" |
           item == "K48.0085 - Contributions by: Members" |
           item == "K48.0086 - Employers" |
           item == "K48.0096 - Benefits paid during period: Annuities" |
           item == "K48.0097 - Lump sum payments on retirement or death" |
           item == "K48.0098 - Amounts paid on resignation" |
           item == "K48.0099 - Other (incl monthly payments)" |
           item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
           item == "K48.0093 - Other (incl reinsurance premiums)" |
           item == "K48.0095 - Other provisions (excl tax)" |
           # item == "K48.0163 - Reinsurance recoveries" |
           # item == "K48.0164 - Unclaimed benefits forfeited" |
           # item == "K48.0088 - Profit on sale of assets or redemption of investments" |
           item == "K48.0090 - Other" |
           # item == "K48.0094 - Depreciation" |
           # item == "K48.82C - Inc own, other non-ins" |
           # item == "K48.83C - Inc own, other non-ins" |
           # item == "K48.84C - Inc own, other non-ins" |
           # item == "K48.85C - Contrib by: Members" |
           # item == "K48.86C - Contrib by: Employers" |
           # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
           # item == "K48.0103 - Other" |
           # item == "K48.92C - Admin exp: Wages, salar" |
           # item == "K48.93C - Admin exp: Other" |
           # item == "K48.94C - Depreciation" |
         # item == "K48.95C - Other provisions" |
         # item == "K48.96C - Benf: Annuities" |
         # item == "K48.97C - Benf: Lump sum ret" |
         # item == "K48.98C - Benf: Amounts on resign" |
         # item == "K48.99C - Benf: Other" |
         # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
         # item == "K48.0157 - Interest" |
         # item == "K48.0158 - Other" |
         # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
         # item == "K48.0160 - Interest" |
         # item == "K48.0161 - Other" |
         item == "K48.0081 - INCOME: Investment income received from insurers i.r.o. policies and othe insurer administrated funds" |
           item == "K48.0087 - Amounts recieved on transfer from other funds"  |
           item == "K48.0089 - Amount by which investments were writtenup" |
           item == "K48.0100 - Amounts transferred to other funds") %>%
  #item == "K48.0102 - Amount by which investments were writtendown") %>%
  mutate(value = case_when(
    # Current expenditure
    item == "K48.0096 - Benefits paid during period: Annuities" |
      item == "K48.0097 - Lump sum payments on retirement or death" |
      item == "K48.0098 - Amounts paid on resignation" |
      item == "K48.0099 - Other (incl monthly payments)" |
      item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
      item == "K48.0093 - Other (incl reinsurance premiums)" |
      item == "K48.0095 - Other provisions (excl tax)" |
      #Net cap prof minus
      # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
      # item == "K48.0103 - Other" |
      # item == "K48.92C - Admin exp: Wages, salar" |
      # item == "K48.93C - Admin exp: Other" |
      # item == "K48.94C - Depreciation" |
      # item == "K48.95C - Other provisions" |
      # item == "K48.96C - Benf: Annuities" |
      # item == "K48.97C - Benf: Lump sum ret" |
      # item == "K48.98C - Benf: Amounts on resign" |
      # item == "K48.99C - Benf: Other" |
    # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
    # item == "K48.0157 - Interest" |
    # item == "K48.0158 - Other" |
    # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
    # item == "K48.0160 - Interest" |
    # item == "K48.0161 - Other" |
    # Other
    item == "K48.0100 - Amounts transferred to other funds"  |
      item == "K48.0102 - Amount by which investments were writtendown" ~
      -abs(value),
    TRUE ~ abs(value))) %>%
  group_by(period, fund_class, item) %>%
  summarise(value_orig = sum(value, na.rm = T)) %>%
  mutate(value = case_when(
    # Current expenditure
    item == "K48.0096 - Benefits paid during period: Annuities" |
      item == "K48.0097 - Lump sum payments on retirement or death" |
      item == "K48.0098 - Amounts paid on resignation" |
      item == "K48.0099 - Other (incl monthly payments)" |
      item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
      item == "K48.0093 - Other (incl reinsurance premiums)" |
      item == "K48.0095 - Other provisions (excl tax)" |
      #Net cap prof minus
      # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
      # item == "K48.0103 - Other" |
      # item == "K48.92C - Admin exp: Wages, salar" |
      # item == "K48.93C - Admin exp: Other" |
      # item == "K48.94C - Depreciation" |
      # item == "K48.95C - Other provisions" |
      # item == "K48.96C - Benf: Annuities" |
      # item == "K48.97C - Benf: Lump sum ret" |
      # item == "K48.98C - Benf: Amounts on resign" |
      # item == "K48.99C - Benf: Other" |
    # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
    # item == "K48.0157 - Interest" |
    # item == "K48.0158 - Other" |
    # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
    # item == "K48.0160 - Interest" |
    # item == "K48.0161 - Other" |
    # Other
    item == "K48.0100 - Amounts transferred to other funds"  |
      item == "K48.0102 - Amount by which investments were writtendown" ~
      -abs(value_orig),
    TRUE ~ abs(value_orig))) %>%
  mutate(k48_class = case_when(item == "K48.0085 - Contributions by: Members" ~ 
                                 "XKB2117A - Contributions by: Members",
                               item == "K48.0086 - Employers" ~ 
                                 "XKB2118A - Contributions by: Employers",
                               item == "K48.0083 - Dividends" ~ 
                                 "XKB2120A - Investment income: Dividends",
                               item == "K48.0082 - Income from own and other non-insurance administrated funds: Interest" ~ 
                                 "XKB2115A - Investment income: Interest",
                               item == "K48.0084 - Rent" ~ 
                                 "XKB2116A - Investment income: Rent",
                               item == "K48.0081 - INCOME: Investment income received from insurers i.r.o. policies and othe insurer administrated funds" ~ 
                                 "XKB2153A - Inv inc from Insurers",
                               item == "K48.0092 - EXPENDITURE: Administration expenditure Wages and salaries" |
                                 item == "K48.0093 - Other (incl reinsurance premiums)" |
                                 item == "K48.0095 - Other provisions (excl tax)" ~
                                 "XKB2150A - Admin Expenses",
                               item == "K48.0089 - Amount by which investments were writtenup" |
                                 item == "K48.0102 - Amount by which investments were writtendown" ~
                                 "XKB2155A - Net Asset revaluation",
                               # item == "K48.0163 - Reinsurance recoveries" |
                               #   item == "K48.0164 - Unclaimed benefits forfeited" |
                               # item == "K48.0088 - Profit on sale of assets or redemption of investments" |
                               item == "K48.0090 - Other" ~
                                 # item == "K48.0094 - Depreciation" ~
                                 # item == "K48.82C - Inc own, other non-ins" |
                                 # item == "K48.83C - Inc own, other non-ins" |
                                 # item == "K48.84C - Inc own, other non-ins" |
                                 # item == "K48.85C - Contrib by: Members" |
                                 # item == "K48.86C - Contrib by: Employers" ~
                                 # item == "K48.0101 - Loss on sale of assets or redemption of investments" |
                                 # item == "K48.0103 - Other" |
                                 # item == "K48.92C - Admin exp: Wages, salar" |
                                 # item == "K48.93C - Admin exp: Other" |
                                 # item == "K48.94C - Depreciation" |
                               # item == "K48.95C - Other provisions" |
                               # item == "K48.96C - Benf: Annuities" |
                               # item == "K48.97C - Benf: Lump sum ret" |
                               # item == "K48.98C - Benf: Amounts on resign" |
                               # item == "K48.99C - Benf: Other" |
                               # item == "K48.0156 - Tax paid on current earnings: Investment income Rent" |
                               # item == "K48.0157 - Interest" |
                               # item == "K48.0158 - Other" |
                               # item == "K48.0159 - Provision for tax on current earnings: Investment income Rent" |
                               # item == "K48.0160 - Interest" |
                               # item == "K48.0161 - Other" ~
                               "XKB2152A - Net Cap prof and other",
                               TRUE ~ item)) %>%
  mutate(k48_class_agg = case_when(k48_class == "XKB2117A - Contributions by: Members" |
                                     k48_class == "XKB2118A - Contributions by: Employers" ~
                                     "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)",
                                   k48_class == "XKB2120A - Investment income: Dividends" |
                                     k48_class == "XKB2115A - Investment income: Interest" |
                                     k48_class == "XKB2116A - Investment income: Rent" |
                                     k48_class == "XKB2153A - Inv inc from Insurers" |
                                     k48_class == "XKB2155A - Net Asset revaluation" ~
                                     "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)",
                                   k48_class == "XKB2152A - Net Cap prof and other" ~
                                     "OTHER INCOME (TOTAL CURRENT PERIOD)",
                                   k48_class == "K48.0087 - Amounts recieved on transfer from other funds" ~
                                     "TRANSFERS FROM OTHER FUNDS (TOTAL CURRENT PERIOD)",
                                   k48_class == "XKB2150A - Admin Expenses" ~
                                     "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)",
                                   k48_class == "K48.0100 - Amounts transferred to other funds" ~
                                     "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)",
                                   k48_class == "K48.0096 - Benefits paid during period: Annuities" |
                                     k48_class == "K48.0097 - Lump sum payments on retirement or death" |
                                     # k48_class == "K48.0098 - Amounts paid on resignation" ~
                                     k48_class == "K48.0099 - Other (incl monthly payments)" ~
                                     "BENEFITS (TOTAL CURRENT PERIOD)",
                                   TRUE ~ NA_character_)) %>%
  mutate(value = case_when(fund_class == "Retirement Annuity" ~ NA_real_,
                                    TRUE ~ value)) %>%
  group_by(item,period) %>%
  summarise(value = sum(value,na.rm=T)) %>%
  ungroup() %>%
  mutate(year = year(period)) %>%
  group_by(period) %>%
  mutate(tot = sum(value,na.rm=T),
         prop = value/sum(value,na.rm=T))
  
  # mutate(value_pf = case_when(fund_class == "Pension Fund" ~ value,
  #                                    TRUE ~ NA_real_)) %>%
  # group_by(period,item) %>%
  # fill(value_pf, .direction = "down") %>%
  # fill(value_pf, .direction = "up") %>%
  # ungroup() %>%
  # mutate(value = case_when(fund_class == "Retirement Annuity"  ~ value_pf,
  #                                   TRUE ~ value),
  #        year = year(period))

# tmp_det_prop_hf <-
#   bind_rows(
#     tmp_det_prop_hf,
#     tmp_det_prop_hf %>%
#       filter(year == 2010) %>%
#       mutate(value = NA_real_,
#              year = case_when(year == 2010 ~ params_cmff_year+1,
#                               TRUE ~ NA_real_),
#              period = as.Date(paste0(params_cmff_year+1,"-",substr(period,6,7),"-01")))
#              
#   ) %>%
#   group_by(item,quarter=quarter(period)) %>%
#   fill(value, .direction = "down")

tmp_det_prop_hf <-
  bind_rows(
    tmp_det_prop_hf,
    tmp_det_prop_hf %>%
      ungroup() %>%
      filter(period == as.Date("2010-03-01")) %>%
      mutate(value = NA_real_,
             year = 2020,
             period = as.Date("2020-12-01"),
             tot = NA_real_,
             prop = NA_real_)) %>%
  group_by(item) %>%
  fill(tot,prop,.direction = "down") %>%
  left_join(.,forecast,
            by = c("period"="date")) %>%
  ungroup() %>%
  mutate(tot = case_when(period == "2020-12-01" ~ tot*comp,
                         TRUE ~ tot),
         value = case_when(period == "2020-12-01" ~ tot*prop,
                         TRUE ~ value)) %>%
  select(-c(comp,tot,prop))



write_rds(tmp_det_prop_hf, "data/cmff/tmp_det_prop_hf.rds","gz")

rm(list=ls()[!str_detect(ls(),"params")])

