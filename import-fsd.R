source("code/libraries.R")

###############
# Main FSD ####
###############

path <- paste0("data/fsd/", list.files("data/fsd/"))
tmp <- str_detect(path,"20")
path <- path[tmp]

ldf <- path %>% 
  map(read_excel, col_types = c("text","date","numeric","numeric","text",rep("numeric",84)))

ldf <- lapply(ldf, function(x) gather(x, item, value, 6:89))

df_fsd_raw <- bind_rows(ldf) %>%
  mutate(period = as_date(paste0(substr(period,1,8),"01")))

names(df_fsd_raw)[5] <- "fund_name"

df_fsd_raw <- df_fsd_raw %>%
  filter(item == "RD_TOTAL_ASSETS" | 
           item == "RF_TOTAL_ASSETS" |
           item == "RFC_TOTAL_ASSETS")

write_rds(df_fsd_raw, "data/fsd/df_fsd_raw.rds","gz")

################
# Unmatched FSD #
################

fsd_unm <- read_excel("data/fsd/Umatched FSD.xlsx",
                      sheet = "Matched and Unmatched",
                      range = "A3:C1989",
                      col_names = F)

names(fsd_unm) <- c("fund_name_fsd", "fund_name_fsca", "fund_class")

write_rds(fsd_unm, "data/fsd/fsd_unm.rds","gz")

rm(list=ls()[!str_detect(ls(),"params")])
