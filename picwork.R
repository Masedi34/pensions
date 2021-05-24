source("code/libraries.R")


##Assets###

path <- paste0("PIC/upd/", list.files("PIC/upd/"))

ldf <- path %>% 
  map(read_excel, sheet = "Integrated Economics", range = "B14:AE66",col_names = FALSE, col_types = c("text", "text", "numeric",
                                                                                                       "blank", "blank", "blank", "blank",
                                                                                                       "blank", "blank", "blank", "blank", 
                                                                                                       "blank", "blank", "blank", "blank",
                                                                                                       "blank", "blank", "blank", "blank",
                                                                                                       "blank", "blank", "blank", "blank",
                                                                                                       "blank", "blank", "blank", "blank",
                                                                                                       "blank", "blank", "numeric"))

names(ldf) <- substr(list.files("PIC/upd/"),1,10)

ldf_p <- bind_rows(ldf, .id = "period") %>%
  as_tibble()

names(ldf_p) <- c("period", "Instrument_code", "Instrument_classification", "stock", "ROW") 

ldf_pp <- ldf_p %>%
  unite(new,"Instrument_code","Instrument_classification", sep = "/")




ldf_q <- ldf_pp %>% pivot_wider(names_from = "new", values_from = c("stock","ROW"))



ldf_q <- ldf_q %>% select_if(~sum(!is.na(.)) > 0)



df_qq <- ldf_q %>%
  mutate (period= (.[[1]]),
          cash=(.[[3]]),
          public_sec= (.[[11]] + .[[12]] + .[[15]] + .[[16]] + .[[17]]) - (.[[48]] + .[[53]] + .[[51]] + .[[52]]),
          private_sec= (.[[13]]+ .[[18]]) - (.[[54]] + .[[49]]),
          foreign_debt= (.[[48]] + .[[53]] + .[[51]] + .[[52]] +.[[54]] + .[[49]]),
          equity= (.[[24]]+.[[25]] + .[[27]]),
          loan=(.[[19]]),
          non_financial = (.[[26]]),
          other_asset= (.[[30]]+.[[32]]+.[[36]]),
          total=(.[[2]]),
          totalrow=(.[[39]]))



table_q <- select(df_qq,"period","stock_F2/Currency and deposits","public_sec","private_sec","foreign_debt","equity","loan","non_financial","other_asset","stock_NA/Total financial assets","ROW_NA/Total financial assets")

names(ldf_q)

write_csv(table_q, "PIC/timeseries12.csv")

df_qq <- ldf_q %>%
  mutate (period= (.[[1]]),
          #cash=(.[[3]]),
          #public_sec= (.[[11]] + .[[12]] + .[[15]] + .[[16]] + .[[17]]) - (.[[47]] + .[[51]] + .[[50]] + .[[52]]),
         # private_sec= (.[[13]]+ .[[18]]) - (.[[53]] + .[[48]]),
          #foreign_debt= (.[[47]] + .[[51]] + .[[50]] + .[[52]] +.[[53]] + .[[48]]),
          #equity= (.[[24]]+.[[25]] + .[[27]]),
          #loan=(.[[19]]),
          derivatives = (.[[33]]),
          Insurance_pension_and_standardised_guarantee_schemes = (.[[30]]),
          Other_accounts_receivable_payable = (.[[36]]),
          other_asset= (.[[30]]+.[[33]]+.[[36]]))
          #derivativesf=(.[[66]]),
          #totalrow=(.[[39]]))
                                                                                                                                          
table_q <- select(df_qq,"period","derivatives","other_asset","Insurance_pension_and_standardised_guarantee_schemes", "Other_accounts_receivable_payable")         

write_csv(table_q, "PIC/timeseriesdev.csv")


##Liabilities###

path <- paste0("PIC/upd/", list.files("PIC/upd/"))

ldf <- path %>% 
  map(read_excel, sheet = "Integrated Economics", range = "B106:AE109", col_names = FALSE, col_types = c("text", "text", "numeric",
                                                                                                      "blank", "blank", "blank", "blank",
                                                                                                      "blank", "blank", "blank", "blank", 
                                                                                                      "blank", "blank", "blank", "blank",
                                                                                                      "blank", "blank", "numeric", "blank",
                                                                                                      "blank", "blank", "blank", "blank",
                                                                                                      "numeric", "blank", "numeric", "numeric",
                                                                                                      "numeric", "numeric", "blank"))

names(ldf) <- substr(list.files("PIC/upd/"),1,10)

ldf_p <- bind_rows(ldf, .id = "period") %>%
  as_tibble()

names(ldf_p) <- c("period", "Instrument_code", "Instrument_classification","Counterpart_closing stock","Financial auxiliaries","Pension Funds",
                  "Central and provincial (including social security funds)","Local government",
                  "Of which : Social security funds","Households and NPISH")


 
write_csv(ldf_p, "PIC/timeseriesLabUP.csv")     
                  
rm(list = ls())                 
                  


              

