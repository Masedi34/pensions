################
# Data files ###
################

cmff_fin5 <- read_rds("data/cmff/cmff_fin5.rds")
cmff_finliab5 <- read_rds("data/cmff/cmff_finliab5.rds")


write.csv(
  
  cmff_fin5 %>%
    ungroup() %>%
    group_by(period) %>%
    summarise(orig = sum(tot, na.rm = T)/10^9,
              revised = sum(tst, na.rm = T)/10^9),
  
  "report/tot_ass.csv")

write.csv(
  
  cmff_finliab5 %>%
    ungroup() %>%
    group_by(period) %>%
    summarise(orig = sum(tot, na.rm = T)/10^9,
              revised = sum(tst, na.rm = T)/10^9),
  
  "report/tot_liab.csv")

write.csv(
  
  cmff_fin5 %>%
    ungroup() %>%
    group_by(period, fund_class) %>%
    summarise(orig = sum(tot, na.rm = T)/10^9,
              revised = sum(tst, na.rm = T)/10^9),
  
  "report/ass_fund_class.csv")


write.csv(
  
  cmff_finliab5 %>%
    ungroup() %>%
    group_by(period, fund_class) %>%
    summarise(orig = sum(tot, na.rm = T)/10^9,
              revised = sum(tst, na.rm = T)/10^9),
  
  "report/liab_fund_class.csv")

write.csv(
  
  cmff_fin5 %>%
    ungroup() %>%
    group_by(period, fsca_item) %>%
    summarise(orig = sum(tot, na.rm = T)/10^9,
              revised = sum(tst, na.rm = T)/10^9),
  
  "report/ass_fsca_cat.csv")

write.csv(
  
  cmff_finliab5 %>%
    ungroup() %>%
    group_by(period, fsca_item) %>%
    summarise(orig = sum(tot, na.rm = T)/10^9,
              revised = sum(tst, na.rm = T)/10^9),
  
  "report/liab_fsca_cat.csv")

write.csv(
  
  cmff_fin5 %>%
    ungroup() %>%
    group_by(period, fsca_item, type) %>%
    summarise(orig = sum(tot, na.rm = T)/10^9,
              revised = sum(tst, na.rm = T)/10^9),
  
  "report/ass_fsca_type.csv")

write.csv(
  
  cmff_finliab5 %>%
    ungroup() %>%
    group_by(period, fsca_item, type) %>%
    summarise(orig = sum(tot, na.rm = T)/10^9,
              revised = sum(tst, na.rm = T)/10^9),
  
  "report/liab_fsca_type.csv")


write.csv(
  
  cmff_fin5 %>%
    ungroup() %>%
    group_by(period, fund_class, fsca_item) %>%
    summarise(orig = sum(tot, na.rm = T)/10^6,
              revised = sum(tst, na.rm = T)/10^6),
  
  "report/ass_class_fsca.csv")

write.csv(
  
  cmff_finliab5 %>%
    ungroup() %>%
    group_by(period, fund_class, fsca_item) %>%
    summarise(orig = sum(tot, na.rm = T)/10^6,
              revised = sum(tst, na.rm = T)/10^6),
  
  "report/liab_class_fsca.csv")



write.csv(
  
  cmff_fin5 %>%
    ungroup() %>%
    group_by(period, fund_class, fsca_item, item.x) %>%
    summarise(orig = sum(tot, na.rm = T)/10^6,
              revised = sum(tst, na.rm = T)/10^6),
  
  "report/ass_class_cat_item.csv")

write.csv(
  
  cmff_finliab5 %>%
    ungroup() %>%
    group_by(period, fund_class, fsca_item, item.x) %>%
    summarise(orig = sum(tot, na.rm = T)/10^6,
              revised = sum(tst, na.rm = T)/10^6),
  
  "report/liab_class_cat_item.csv")

########################
# HTML Report Generate
#######################

rmarkdown::render(input = "code/balance-sheet.Rmd", 
                  output_format = "html_document",
                  output_dir = "report/")

rm(list = ls())

