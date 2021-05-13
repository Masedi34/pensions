library(tidyverse)
library(readxl)

unm <- read_excel("./data/Umatched FSD.xlsx",
                  sheet = "Matched and Unmatched",
                  range = "A1576:A3447",
                  col_names = F) %>%
    unlist()

#names(unm) <- "fund.name"

tst <- df.fsd %>%
  filter(fund.name %in% unm &
           (item == "RD_TOTAL ASSETS" | 
                    item == "RF_TOTAL_ASSETS" |
                    item == "RFC_TOTAL_ASSETS")) %>%
  group_by(fund.name, period) %>%
  summarise(total_assets = sum(value,na.rm=T))

write.csv(tst, "unmatched-sum.csv")

tst1 <- df.fsd %>%
  filter(fund.name %in% unm) %>%
  distinct(fund.name) 

write.csv(unmatch.fsd.n$V1, "unmatch-sum.csv")
