source("code/libraries.R")

df_fsd <- read_rds("data/fsd/df_fsd_raw.rds") %>%
  mutate(fund_name = tolower(fund_name),
         #fund_name = gsub("\\s*\\([^\\)]+\\)","",fund_name),
         fund_name = gsub("the","",fund_name),
         fund_name = gsub("s a","sa",fund_name),
         fund_name = gsub("\\.","",fund_name),
         fund_name = gsub("-"," ",fund_name),
         fund_name = gsub("'","",fund_name),
         fund_name = gsub("south african","sa",fund_name),
         fund_name = gsub("south africa","sa",fund_name),
         fund_name = str_squish(fund_name)) %>%
  filter(fund_name != "government employees pension fund")

df_fsca <- read_rds("data/fsca/df_fsca.rds")

# match_list_fsd <- list()
# 
# for (i in seq_along(unique(df_fsd$fund_name))) {
#   
#   match_list_fsd[[i]] <- tibble(cbind(unique(df_fsd$fund_name)[i],
#                                            unique(df_fsca$fund_name),
#                                            stringdist(unique(df_fsd$fund_name)[i], 
#                                                       unique(df_fsca$fund_name), 
#                                                       method = "jw",
#                                                       p=0.1)))
#   
# }

fsd_tmp <- unique(df_fsd$fund_name)
fsca_tmp <- unique(df_fsca$fund_name)

match_list_fsd <- map(fsd_tmp,
                      
                      function(x) {
                        
                        as.data.frame(cbind(x,
                              fsca_tmp,
                              stringdist(x, 
                                         fsca_tmp, 
                                         method = "jw",
                                         p=0.1)))
                        
                      })

match_fsd <- bind_rows(match_list_fsd, .id = "id") 

names(match_fsd) <- c("id", "V1", "V2", "V3")

match_fsd_n <- match_fsd %>%
  group_by(V1) %>%
  top_n(-1, V3) %>%
  filter(V3<0.02) %>%
  distinct(id, .keep_all = T) %>%
  arrange(V3)

for (i in seq_along(match_fsd_n$V1)) {
  
  df_fsd$fund_name[df_fsd$fund_name == as.character(match_fsd_n$V1)[i]] <-
    as.character(match_fsd_n$V2)[i]
}

fsca_map <- read_rds("data/fsca/df_fsca.rds") %>%
  distinct(fund_name,fund_class)

df_fsd1 <- df_fsd
df_fsd1 <- left_join(df_fsd1, fsca_map, by = "fund_name")

unmatch_fsd_n <- match_fsd %>%
  group_by(V1) %>%
  top_n(-1, V3) %>%
  filter(V3 >= 0.02) %>%
  distinct(id, .keep_all = T) %>%
  arrange(V3)

#rm(match_list_fsd)

fsd_unm <- read_rds("data/fsd/fsd_unm.rds") %>%
  mutate(fund_name_fsca = tolower(fund_name_fsca),
         #fund_name_fsca = gsub("\\s*\\([^\\)]+\\)","",fund_name_fsca),
         fund_name_fsca = gsub("the","",fund_name_fsca),
         fund_name_fsca = gsub("s a","sa",fund_name_fsca),
         fund_name_fsca = gsub("\\_","",fund_name_fsca),
         fund_name_fsca = gsub("-"," ",fund_name_fsca),
         fund_name_fsca = gsub("'","",fund_name_fsca),
         fund_name_fsca = gsub("south african","sa",fund_name_fsca),
         fund_name_fsca = gsub("south africa","sa",fund_name_fsca),
         fund_name_fsca = str_squish(fund_name_fsca)) 

m_unm_fsd <- anti_join(fsca_map, match_fsd_n, by = c("fund_name" = "V1"))

m_unm_fsd1 <- inner_join(m_unm_fsd, fsd_unm, by = c("fund_name" = "fund_name_fsca", "fund_class")) %>%
  select(fund_name = fund_name_fsd, fund_class)

merge_fsd_fsca <- left_join(df_fsd1, m_unm_fsd1, by = "fund_name") %>%
  unite("fund_class", c("fund_class.x","fund_class.y"), sep = "") %>% 
  mutate(fund_class = str_replace(fund_class,"NA",""))

tot_fsd <- merge_fsd_fsca %>% 
  filter(item == "RD_TOTAL_ASSETS" | 
           item == "RF_TOTAL_ASSETS" |
           item == "RFC_TOTAL_ASSETS") %>%
  group_by(period, fund_class) %>%
  #filter(value > quantile(value, 0.01) & value < quantile(value, 0.99)) %>%
  group_by(period, fund_class) %>%
  summarise(value = sum(value,na.rm=T)) %>%
  # group_by(fund_class) %>%
  # mutate(tst = stats::filter(value, filter = wts, sides = 2)) %>%
  filter(fund_class != "NA")

forecast <- bind_rows(read_rds("data/CMJM073E.rds"), read_rds("data/JFIA001E.rds")) %>%
  group_by(code, quarter = quarter(date), year = year(date)) %>%
  filter(date == max(date) & value != 0) %>%
  group_by(code) %>%
  mutate(qq = value/lag(value)) %>%
  group_by(date) %>%
  summarise(comp = mean(qq, na.rm = T))

#tot_fsd4 <- tot_fsd

tot_fsd4 <- bind_rows(tot_fsd,
                      tot_fsd %>%
                        filter(period == "2010-03-01") %>%
                        ungroup() %>%
                        mutate(value = NA_real_,
                               period = case_when(period == "2010-03-01" ~ as_date("2020-12-01"))))


tot_fsd_fc_tidy <- tot_fsd4 %>%
  left_join(., forecast, by = c("period" = "date")) %>%
  group_by(fund_class) %>%
  fill(comp, .direction = "down") %>%
  mutate(value1 = case_when(!is.na(value) ~ value,
                            TRUE ~ NA_real_),
         value1 = case_when(is.na(value1) ~ lag(value1)*comp,
                            TRUE ~ value1),
         value = case_when(!is.na(value) ~ value,
                           TRUE ~ value1)) %>%
  select(-value1)

write_rds(tot_fsd_fc_tidy, "data/fsd/tot_fsd_fc_tidy.rds","gz")

rm(list=ls()[!str_detect(ls(),"params")])


