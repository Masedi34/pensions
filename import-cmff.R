source("code/libraries.R")

path_bs <- "data/cmff/CMFF - Retirement funds - 2010 and 2018 (Assets and Liabilities).xlsx"

ldf_bs <- path_bs %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, 
      path = path_bs, 
      col_names = F) 

ldf_bs <- ldf_bs[-1] 

ldf_bs <- lapply(ldf_bs, function(x) {
  
  colnames(x) <- c("item", x[8,2:ncol(x)])
  
  x <- x %>%
    filter(!is.na(item) &
             !str_detect(item, "Quarter") &
             item != "RSA" &
             item != "Last Imported" &
             item != "Present" &
             item != "Previous" &
             item != "Net Assets" &
             #item != "XKB2342A - Total Assets" &
             item != "Test" &
             item != "TEST" &
             item != "XKB2319A - Accumulated Funds" &
             item != "XKB2159A - Resrv, provs, other") %>%
    mutate(item = case_when(item == "K48.0080 - TOTAL LIABILITIES (= item 70)" ~ "XKB2322A - Total Liabilities",
                          TRUE ~ item))

  return(x)
  
})

ldf_bs <- lapply(ldf_bs, function(x) gather(x, fund_name, value, 2:ncol(x)))

ldf_bs <- bind_rows(ldf_bs, .id = "period") %>%
  as_tibble() %>%
  mutate(period = as_date(paste0(substr(period,1,4),"-",as.character(as.numeric(substr(period,6,6))*3),"-","01")),
         value = as.numeric(value),
         form = "bs")




path_is <- "data/cmff/CMFF - Retirement funds - Income Statement.xlsx"

ldf_is <- path_is %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, 
      path = path_is, 
      col_names = F) 

ldf_is <- ldf_is[grepl("Inc", names(ldf_is))] 

ldf_is <- lapply(ldf_is, function(x) {
  
  colnames(x) <- c("item", x[8,2:ncol(x)])
  
  x <- x %>%
    filter(!is.na(item) &
             !str_detect(item, "Quarter") &
             item != "Income: Calc" &
             item != "RSA" &
             item != "Last Imported" &
             item != "Present" &
             item != "Previous") 
  
  return(x)
  
})

ldf_is <- lapply(ldf_is, function(x) gather(x, fund_name, value, 2:ncol(x)))

ldf_is <- bind_rows(ldf_is, .id = "period") %>%
  as_tibble() %>%
  mutate(period = as_date(paste0(substr(period,4,7),"-",as.character(as.numeric(substr(period,2,2))*3),"-","01")),
         value = as.numeric(value),
         form = "is")

df_cmff_raw <- bind_rows(ldf_bs, ldf_is) %>%
  write_rds("data/cmff/df_cmff_raw.rds","gz")

rm(list=ls()[!str_detect(ls(),"params")])




