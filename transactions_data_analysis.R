suppressWarnings(suppressMessages(library(readxl)))
suppressWarnings(suppressMessages(library(dplyr)))

trades <- read_excel('trade_behavior_analysis.xlsx', sheet = 'transactions')
holdings <- read_excel('trade_behavior_analysis.xlsx', sheet = 'holdings')

behave <- trades %>% full_join(holdings, by = c('date', 'manager', 'igid', 'ticker', 'type')) %>% 
                     filter(!is.na(price)) %>%
                     mutate(shares_sod = shares_eod - quantity) %>% 
                     arrange(manager, igid, ticker, date) %>% 
                     filter(!(shares_eod == 0 & is.na(shares_sod)))

behave$shares_sod[is.na(behave$shares_sod)] <- behave$shares_eod[is.na(behave$shares_sod)]

behave$quantity[is.na(behave$quantity)] <- ''

behave <- behave %>% mutate(side = ifelse(shares_sod < 0 | shares_eod < 0, 'Short', 'Long'),
                            side_by_type = ifelse(type == 'Options - Puts' & side == 'Short', 'Long', ifelse(type == 'Options - Puts' & side == 'Long', 'Short', side))) %>% 
                     select(date, igid, price, manager, side_by_type, shares_sod, shares_eod, type, ticker, quantity)

write.csv(behave, 'delete_soon2.csv')
