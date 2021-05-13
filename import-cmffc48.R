source("code/libraries.R")



#####  C48 form ########

#####Balance sheet######

####Assets######


path_bs <- "data/cmff/C48 Balance Sheet Assets - C48 estimation process.xlsx"

ldf_bs <- path_bs %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, 
      path = path_bs, 
      col_names = F) 

ldf_bs <- lapply(ldf_bs, function(x) {
  
  colnames(x) <- c("item", x[2,2:ncol(x)])
  names(x) <- paste(names(x), x[4, ], sep = "_")
  x <- x[-c(1:4),]
  
  return(x)
  
})

ldf_bs <- lapply(ldf_bs, function(x) gather(x, fund_name, value, 2:ncol(x)))

ldf_bs <- bind_rows(ldf_bs, .id = "period") %>%
  as_tibble() %>%
  mutate(period = as_date(paste0(substr(period,1,4),"-",as.character(as.numeric(substr(period,6,6))*3),"-","01")),
         value = as.numeric(value),
         form = "bs")

########Liability##########

path_ls <- "data/cmff/C48 Balance Sheet Funds and Liabilities - C48 estimation process.xlsx"

ldf_ls <- path_ls %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, 
      path = path_ls, 
      col_names = F) 

ldf_ls <- lapply(ldf_ls, function(x) {
  
  colnames(x) <- c("item", x[2,2:ncol(x)])
  names(x) <- paste(names(x), x[4, ], sep = "_")
  x <- x[-c(1:4),]
  
  return(x)
  
})

ldf_ls <- lapply(ldf_ls, function(x) gather(x, fund_name, value, 2:ncol(x)))

ldf_ls <- bind_rows(ldf_ls, .id = "period") %>%
  as_tibble() %>%
  mutate(period = as_date(paste0(substr(period,1,4),"-",as.character(as.numeric(substr(period,6,6))*3),"-","01")),
         value = as.numeric(value),
         form = "bs")

###########Income Statement########
path_is <- "data/cmff/C48 Income Statement - C48 estimation process.xlsx"

ldf_is <- path_is %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, 
      path = path_is, 
      col_names = F) 

ldf_is <- lapply(ldf_is, function(x) {
  
  colnames(x) <- c("item", x[2,2:ncol(x)])
  names(x) <- paste(names(x), x[4, ], sep = "_")
  x <- x[-c(1:4),]
  
  return(x)
  
})

ldf_is <- lapply(ldf_is, function(x) gather(x, fund_name, value, 2:ncol(x)))

ldf_is <- bind_rows(ldf_is, .id = "period") %>%
  as_tibble() %>%
  mutate(period = as_date(paste0(substr(period,1,4),"-",as.character(as.numeric(substr(period,6,6))*3),"-","01")),
         value = as.numeric(value),
         form = "is")

df_cmff_c48 <- bind_rows(ldf_bs, ldf_is, ldf_ls) %>%
  write_rds("data/cmff/df_cmff_c48.rds","gz")

rm(list=ls()[!str_detect(ls(),"params")])


