source("code/libraries.R")

## Temporal disaggregation - Balance Sheet

tmp2 <- read_rds("data/fsca/tot_fsca_is.rds")

ldf_is <- map(unique(tmp2$fund_class), 
           function(x) tot_fsca  <- tmp2 %>% 
             ungroup() %>%
             filter(fund_class == x) %>% 
             select(tot_item) %>% 
             unname() %>%
             unlist() %>%
             ts(start = 2010, frequency = 1))

names(ldf_is) <- c("Pension Fund", "Provident Fund", "Retirement Annuity")

for (i in 1:3) {
  
  x <- ldf_is[[i]]
  
  denton <- td(x ~ 1, to = "quarterly", method = "denton-cholette", conversion = "sum")
  denton <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
                   denton = as.matrix(predict(denton)), 
                   fund_class = names(ldf_is[i]))
  assign(paste0(names(ldf_is)[i]), denton)
  
}

tmp_denton_is <- bind_rows(`Pension Fund`,
                        `Provident Fund`,
                        `Retirement Annuity`)

df_fsca_is_mod3 <- read_rds("data/fsca/df_fsca_is.rds") %>%
  mutate(item = str_squish(item)) %>%
  mutate(value = case_when(item == "Less: Amount allocated to unclaimed benefits"  ~ -value,
                           TRUE ~ value),
         k48_class = case_when(item == "Member contributions -received and accrued" |
                                 item == "Additional voluntary contributions – members" ~
                                 "XKB2117A - Contributions by: Members",
                               item == "Employer contributions - received and accrued" |
                                 item == "Additional contribution in respect of actuarial shortfall" |
                                 item == "Additional voluntary contributions – employer" |
                                 item == "Other (please specify)" ~
                                 "XKB2118A - Contributions by: Employers",
                               item == "Dividends" ~
                                 "XKB2120A - Investment income: Dividends",
                               item == "Interest" ~
                                 "XKB2115A - Investment income: Interest",
                               item == "Rentals" ~
                                 "XKB2116A - Investment income: Rent",
                               item == "Income from insurance policies" ~
                                 "XKB2153A - Inv inc from Insurers",
                               item == "Adjustment to fair value" ~
                                 "XKB2155A - Net Asset revaluation",
                               item == "Collective investment schemes distribution" |
                                 item == "Interest on late payment of contributions" |
                                 item == "Interest levied on surplus improperly utilised" |
                                 item == "Bad debts recovered" |
                                 item == "Other_other_income" ~
                                 "XKB2152A - Net Cap prof and other",
                               item == "From:In terms of section 14" |
                                 item == "From:Transfers in terms of section 15B" |
                                 item == "From:Individual transfers" |
                                 item == "From:Prospective approvals in terms of section 14" ~
                                 "K48.0087 - Amounts recieved on transfer from other funds",
                               item == "Actuarial fees" |
                                 item == "Administration fees" |
                                 item == "- Audit services" |
                                 item == "- Expenses" |
                                 item == "- Other" |
                                 item == "Consultancy fees" |
                                 item == "Depreciation - at cost" |
                                 item == "Depreciation - at revaluation" |
                                 item == "Fidelity Insurance" |
                                 item == "Levies" |
                                 item == "Other_admin_exp" |
                                 item == "Office expenses" |
                                 item == "Operating lease payments" |
                                 item == "Penalties" |
                                 item == "Principal officer expenses" |
                                 item == "Staff expenses" |
                                 item == "Secretarial fees" |
                                 item == "Trustee fees and remuneration" |
                                 item == "Less: Amount allocated to unclaimed benefits" |
                                 item == "Less: Expenses incurred in managing investments" |
                                 item == "Less: Interest paid on borrowings" ~
                                 "XKB2150A - Admin Expenses",
                               item == "To:In terms of section 14" |
                                 item == "To:Transfers in terms of section 15B" |
                                 item == "To:Individual transfers" |
                                 item == "To:Prospective approvals in terms of section 14" ~
                                 "K48.0100 - Amounts transferred to other funds",
                               item == "Monthly pensions" ~
                                 "K48.0096 - Benefits paid during period: Annuities",
                               # item == "Withdrawal benefits" ~
                               #   "K48.0098 - Amounts paid on resignation",
                               item == "Lump sums on retirements" ~
                                 "K48.0097 - Lump sum payments on retirement or death",
                               item == "Lump sums before retirement" ~
                                 "K48.0099 - Other (incl monthly payments)",
                               TRUE ~ item)) %>%
  group_by(k48_class, period) %>%
  summarise(value = sum(value, na.rm = T),
            value = abs(value)) %>%
  group_by(period) %>%
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
         value = case_when(k48_class_agg == "ADMINISTRATION EXPENSES (TOTAL CURRENT PERIOD)" |
                             k48_class_agg == "TRANSFERS TO OTHER FUNDS (TOTAL CURRENT PERIOD)" |
                             k48_class_agg == "BENEFITS (TOTAL CURRENT PERIOD)" |
                             (k48_class == "XKB2116A - Investment income: Rent" & period == "2017") ~
                             -value,
                           TRUE ~ value)) %>%
  filter(!is.na(k48_class_agg)) %>%
  left_join(.,
            read_rds("data/fsca/df_fsca_is_mod1.rds"),
            by = c("k48_class_agg" = "item",
                   "period" = "period")) %>%
  mutate(prop_sub = value.x/value.y) %>%
  select(-value.x,-value.y)

df_fsca_is_mod3 <-
  bind_rows(
    
    df_fsca_is_mod3,
    
    df_fsca_is_mod3 %>%
      group_by(k48_class_agg,period) %>%
      summarise(tot = sum(prop_sub,na.rm=T)) %>%
      filter(k48_class_agg == "BENEFITS (TOTAL CURRENT PERIOD)") %>%
      mutate(Other = 1-tot) %>%
      select(-tot) %>%
      gather(k48_class,prop_sub,Other)
  ) 

df_fsca_is_mod3 <- bind_rows(df_fsca_is_mod3,
                             
                             df_fsca_is_mod3 %>%
                               filter(period == "2017") %>%
                               mutate(prop_sub = NA_real_) %>%
                               ungroup() %>%
                               mutate(period = as.character(params_forecast1)),
                             
                             df_fsca_is_mod3 %>%
                               filter(period == "2017") %>%
                               mutate(prop_sub = NA_real_) %>%
                               ungroup() %>%
                               mutate(period = as.character(params_forecast2))) %>%
  
  group_by(k48_class) %>%
  mutate(prop_sub1 = mean(prop_sub,na.rm=T),
         prop_sub = case_when(period == as.character(params_forecast1) |
                                period == as.character(params_forecast2) ~ prop_sub1,
                              TRUE ~ prop_sub))

cmff_is_det_prop3 <- read_rds("data/cmff/cmff_is_det_prop2.rds") %>%
  bind_rows(.,
            tibble(
              period = rep(2010:params_forecast2,3),
              item = rep("Other", length(2010:params_forecast2)*3),
              fund_class = c(rep("Pension Fund",length(2010:params_forecast2)), rep("Provident Fund", length(2010:params_forecast2)), rep("Retirement Annuity", length(2010:params_forecast2))),
              k48_class = rep("Other",length(2010:params_forecast2)*3),
              k48_class_agg = rep("BENEFITS (TOTAL CURRENT PERIOD)",length(2010:params_forecast2)*3)) %>%
              mutate(period = as.character(period))) %>%
  left_join(., 
            read_rds("data/fsca/fsca_is_report_g_tr.rds"),
            by = c("period" = "period",
                   "fund_class" = "fund_class",
                   "k48_class_agg" = "item")) %>%
  left_join(.,
            read_rds("data/fsca/tot_fsca_is.rds"),
            by = c("period" = "period",
                   "fund_class" = "fund_class")) %>%
  # left_join(.,
  #           tmp_denton_is,
  #           by = c("period" = "period",
  #                  "fund_class" = "fund_class")) %>%
  left_join(.,
            df_fsca_is_mod3 %>%
              select(-k48_class_agg),
            by = c("period" = "period",
                   "k48_class" = "k48_class")) %>%
  ungroup() %>%
  mutate(k48_class_prop = case_when(k48_class_agg == "NET INVESTMENT INCOME (TOTAL CURRENT PERIOD)" |
                                       k48_class_agg == "BENEFITS (TOTAL CURRENT PERIOD)" |
                                       k48_class_agg == "CONTRIBUTIONS RECEIVED AND ACCRUED (TOTAL CURRENT PERIOD)"
                                    ~ prop_sub,
                                    TRUE ~ k48_class_prop),
         # k48_class_prop = case_when(item == "K48.0085 - Contributions by: Members" &
         #                              fund_class == "Retirement Annuity"
         #                            ~ 1,
         #                            item == "K48.0086 - Employers" &
         #                              fund_class == "Retirement Annuity"
         #                            ~ 0,
         #                            TRUE ~ k48_class_prop),
         tst = tot_item*prop_fc*k48_class_prop,
         tst = case_when(value_orig < 0 &
                           tst > 0 ~
                           -tst,
                         value_orig > 0 &
                           tst < 0 ~
                           -tst,
                         value_orig == 0 &
                           k48_tot < 0 ~
                           -tst,
                         item == "Other" ~
                           -tst,
                         TRUE ~ tst),
         # tst = case_when(k48_class == "XKB2116A - Investment income: Rent" &
         #                   period == "2017" ~
         #                   -abs(tst),
         #                 TRUE ~ tst),
         
         tst = case_when(
                              item == "K48.0088 - Profit on sale of assets or redemption of investments" |
                              item == "K48.0094 - Depreciation" |
                              item == "K48.0163 - Reinsurance recoveries" |
                              item == "K48.0164 - Unclaimed benefits forfeited" |
                              item == "K48.82C - Inc own, other non-ins" |
                              item == "K48.83C - Inc own, other non-ins" |
                              item == "K48.85C - Contrib by: Members" ~
                              abs(tst),
                            item == "K48.0100 - Amounts transferred to other funds" |
                                 item == "K48.0101 - Loss on sale of assets or redemption of investments" |
                                 item == "K48.0103 - Other" |
                                 item == "K48.0158 - Other" |
                                 item == "K48.0160 - Interest" |
                                 item == "K48.0161 - Other" |
                                 item == "K48.93C - Admin exp: Other" |
                                 item == "K48.97C - Benf: Lump sum ret" |
                                 item == "K48.98C - Benf: Amounts on resign" |
                                 item == "K48.99C - Benf: Other" ~
                           -abs(tst),
                         TRUE ~ tst)) %>%
  select(period,fund_class,item,k48_class,k48_class_agg,tst)

cmff_is_det_prop3 <-
  
  bind_rows(
    
    cmff_is_det_prop3 %>%
      filter(item != "K48.0098 - Amounts paid on resignation"),

cmff_is_det_prop3 %>%
  filter(item == "K48.0099 - Other (incl monthly payments)") %>%
  left_join(.,
            bind_rows(
            read_rds("data/fsca/df_fsca_is.rds") %>%
              filter(item == "BENEFITS (TOTAL CURRENT PERIOD)" |
                       item == "Withdrawal benefits") %>%
              mutate(total = case_when(item == "BENEFITS (TOTAL CURRENT PERIOD)" ~ value,
                                       TRUE ~ NA_real_)) %>%
              group_by(period) %>%
              fill(total, .direction = "down") %>%
              filter(item == "Withdrawal benefits") %>%
              mutate(prop = value/total,
                     item = case_when(item == "Withdrawal benefits" ~ "K48.0099 - Other (incl monthly payments)",
                                      TRUE ~ NA_character_)) %>%
              select(-value,-total),
              
            tibble(
              item = rep("K48.0099 - Other (incl monthly payments)",2),
              period = c(as.character(params_forecast1),as.character(params_forecast2)),
              prop = NA_real_
            )) %>%
              ungroup() %>%
              fill(prop, .direction = "down"),
            by = c("item"="item",
                   "period" = "period")) %>%
  ungroup() %>%
  mutate(tst = tst*prop,
         item = case_when(item == "K48.0099 - Other (incl monthly payments)" ~
                            "K48.0098 - Amounts paid on resignation",
                          TRUE ~ item),
         k48_class = case_when(k48_class == "K48.0099 - Other (incl monthly payments)" ~
                            "K48.0098 - Amounts paid on resignation",
                          TRUE ~ k48_class)) %>%
  select(-prop)
)



#########################

library(mFilter)

tmp_det_prop <- cmff_is_det_prop3 %>%
  filter(k48_class != "Other")

tmp_det_prop_hf <- read_rds("data/cmff/tmp_det_prop_hf.rds")

hdf_pf <- map(unique(tmp_det_prop_hf$item), 
              
           function(x) {
             
             tmp <- tmp_det_prop_hf %>%
             ungroup() %>%
             filter(item == x) %>% 
             select(value) %>% 
             unname() %>%
             unlist() %>%
             ts(start = c(2010,1), frequency = 4) %>%
             hpfilter(.)
             
             tmp <- tmp$trend
           })

names(hdf_pf) <- unique(tmp_det_prop_hf$item)

ldf_is_pf <- map(unique(tmp_det_prop$item),
                 
                 function(x) prop_fc_q  <- tmp_det_prop %>%
                   ungroup() %>%
                   filter(item == x & fund_class == "Pension Fund") %>%
                   select(tst) %>%
                   unname() %>%
                   unlist() %>%
                   ts(start = 2010, frequency = 1))

names(ldf_is_pf) <- unique(tmp_det_prop$item)

for (i in 1:length(unique(tmp_det_prop$item))) {
  
  x <- ldf_is_pf[[i]]
  y <- hdf_pf[[i]]
  
  denton <- td(x ~ 0+y, to = "quarterly", method = "chow-lin-maxlog", conversion = "sum",truncated.rho=0)
  denton <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
                   tst = as.matrix(predict(denton)),
                   fund_class = "Pension Fund",
                   item = names(ldf_is_pf)[i])
  assign(paste0("pension-",names(ldf_is_pf)[i]), denton)
  
}

hdf_pf2 <- map(unique(tmp_det_prop_hf$item), 
               
               function(x) {
                 
                 tmp <- tmp_det_prop_hf %>%
                   ungroup() %>%
                   filter(item == x) %>% 
                   select(value) %>% 
                   unname() %>%
                   unlist() %>%
                   ts(start = c(2010,1), frequency = 4) %>%
                   hpfilter(.)
                 
                 tmp <- tmp$trend
               })

names(hdf_pf2) <- unique(tmp_det_prop_hf$item)

ldf_is_pf2 <- map(unique(tmp_det_prop$item),
                  function(x) prop_fc_q  <- tmp_det_prop %>%
                    ungroup() %>%
                    filter(item == x & fund_class == "Provident Fund") %>%
                    select(tst) %>%
                    unname() %>%
                    unlist() %>%
                    ts(start = 2010, frequency = 1))

names(ldf_is_pf2) <- unique(tmp_det_prop$item)

for (i in 1:length(unique(tmp_det_prop$item))) {
  
  x <- ldf_is_pf2[[i]]
  y <- hdf_pf2[[i]]
  
  denton <- td(x ~ 0 + y, to = "quarterly", method = "chow-lin-maxlog", conversion = "sum",truncated.rho=0)
  denton <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
                   tst = as.matrix(predict(denton)),
                   fund_class = "Provident Fund",
                   item = names(ldf_is_pf2)[i])
  assign(paste0("provident-",names(ldf_is_pf2)[i]), denton)
  
}

hdf_ra <- map(unique(tmp_det_prop_hf$item), 
              function(x) {
                
                tmp <- tmp_det_prop_hf %>%
                  ungroup() %>%
                  filter(item == x) %>% 
                  select(value) %>% 
                  unname() %>%
                  unlist() %>%
                  ts(start = c(2010,1), frequency = 4) %>%
                  hpfilter(.)
                
                tmp <- tmp$trend
              })

names(hdf_ra) <- unique(tmp_det_prop_hf$item)

ldf_is_ra <- map(unique(tmp_det_prop$item),
                 function(x) prop_fc_q  <- tmp_det_prop %>%
                   ungroup() %>%
                   filter(item == x & fund_class == "Retirement Annuity") %>%
                   select(tst) %>%
                   unname() %>%
                   unlist() %>%
                   ts(start = 2010, frequency = 1))

names(ldf_is_ra) <- unique(tmp_det_prop$item)

for (i in 1:length(unique(tmp_det_prop$item))) {
  
  x <- ldf_is_ra[[i]]
  y <- hdf_ra[[i]]
  
  denton <- td(x ~ 0+y, to = "quarterly", method = "chow-lin-maxlog", conversion = "sum",truncated.rho=0)
  denton <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
                   tst = as.matrix(predict(denton)),
                   fund_class = "Retirement Annuity",
                   item = names(ldf_is_ra)[i])
  assign(paste0("retirement-",names(ldf_is_ra)[i]), denton)
  
}

##################################

tmp_det_prop <- cmff_is_det_prop3

other_pf_K48 <- tmp_det_prop %>%
                   ungroup() %>%
                   filter(item == "Other" & fund_class == "Pension Fund") %>%
                   select(tst) %>%
                   unname() %>%
                   unlist() %>%
                   ts(start = 2010, frequency = 1)

other_pf_K48 <- td(other_pf_K48 ~ 1, to = "quarterly", method = "denton-cholette", conversion = "sum")
other_pf_K48 <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
                   tst = as.matrix(predict(other_pf_K48)),
                   fund_class = "Pension Fund",
                   item = "Other")

other_pf2_K48 <- tmp_det_prop %>%
  ungroup() %>%
  filter(item == "Other" & fund_class == "Provident Fund") %>%
  select(tst) %>%
  unname() %>%
  unlist() %>%
  ts(start = 2010, frequency = 1)

other_pf2_K48 <- td(other_pf2_K48 ~ 1, to = "quarterly", method = "denton-cholette", conversion = "sum")
other_pf2_K48 <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
                       tst = as.matrix(predict(other_pf2_K48)),
                       fund_class = "Provident Fund",
                       item = "Other")

other_ra_K48 <- tmp_det_prop %>%
  ungroup() %>%
  filter(item == "Other" & fund_class == "Retirement Annuity") %>%
  select(tst) %>%
  unname() %>%
  unlist() %>%
  ts(start = 2010, frequency = 1)

other_ra_K48 <- td(other_ra_K48 ~ 1, to = "quarterly", method = "denton-cholette", conversion = "sum")
other_ra_K48 <- tibble(period = as_date(unique(read_rds("data/fsd/tot_fsd_fc_tidy.rds")$period)),
                        tst = as.matrix(predict(other_ra_K48)),
                        fund_class = "Retirement Annuity",
                        item = "Other")

################################

cmff_is_det_prop3 <- bind_rows(mget(ls(pattern="K48"))) %>%
  left_join(.,
            read_rds("data/cmff/cmff_is_det_prop2.rds") %>%
              ungroup() %>%
              distinct(item,k48_class,k48_class_agg),
            by = "item") %>%
  mutate(k48_class = case_when(item == "Other" ~ "Other",
                               TRUE ~ k48_class),
         k48_class_agg = case_when(item == "Other" ~ "BENEFITS (TOTAL CURRENT PERIOD)",
                                   TRUE ~ k48_class_agg))

write_rds(cmff_is_det_prop3, "data/cmff/cmff_is_det_prop3.rds","gz")


rm(list=ls()[!str_detect(ls(),"params")])
