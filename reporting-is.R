################
# Data files ###
################

cmff_is_det_prop3 <- read_rds("data/cmff/cmff_is_det_prop3.rds") %>%
  filter(item != "K48.0098 - Amounts paid on resignation")

cmff_is_det_prop4 <- read_rds("data/cmff/cmff_is_det_prop3.rds")


write.csv(
  
  cmff_is_det_prop3 %>%
    ungroup() %>%
    group_by(period) %>%
    summarise(revised = sum(tst, na.rm = T)/10^9),
  
  "report/tot_is.csv")

write.csv(
  
  cmff_is_det_prop3 %>%
    ungroup() %>%
    group_by(period, fund_class) %>%
    summarise(revised = sum(tst, na.rm = T)/10^9),
  
  "report/is_fund_class.csv")

write.csv(
  
  cmff_is_det_prop3 %>%
    ungroup() %>%
    group_by(period, k48_class_agg) %>%
    summarise(revised = sum(tst, na.rm = T)/10^9),
  
  "report/is_class_agg.csv")

write.csv(
  
  cmff_is_det_prop3 %>%
    ungroup() %>%
    group_by(period, k48_class) %>%
    summarise(revised = sum(tst, na.rm = T)/10^9),
  
  "report/is_class_det.csv")

write.csv(
  
  cmff_is_det_prop3 %>%
    ungroup() %>%
    group_by(period, item) %>%
    summarise(revised = sum(tst, na.rm = T)/10^9),
  
  "report/is_item.csv")


write.csv(
  
  cmff_is_det_prop3 %>%
    ungroup() %>%
    group_by(period, fund_class, k48_class_agg) %>%
    summarise(revised = sum(tst, na.rm = T)/10^6),
  
  "report/is_class_agg_fc.csv")

write.csv(
  
  cmff_is_det_prop3 %>%
    ungroup() %>%
    group_by(period, fund_class, k48_class) %>%
    summarise(revised = sum(tst, na.rm = T)/10^6),
  
  "report/is_class_fc.csv")

write.csv(
  
  cmff_is_det_prop3 %>%
    ungroup() %>%
    group_by(period, fund_class, item) %>%
    summarise(revised = sum(tst, na.rm = T)/10^6),
  
  "report/is_item_fc.csv")

write.csv(
  
  cmff_is_det_prop4 %>%
    ungroup() %>%
    group_by(period, fund_class, item, k48_class, k48_class_agg) %>%
    summarise(revised = sum(tst, na.rm = T)/10^6),
  
  "report/is_detailed.csv")

########################
# HTML Report Generate
#######################

rmarkdown::render(input = "code/income-statement.Rmd", 
                  output_format = "html_document",
                  output_dir = "report/")

rm(list = ls())
