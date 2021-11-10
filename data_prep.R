library(tidyr)
library(dplyr)

case <- read.csv('daily_newly_cases_US.csv', stringsAsFactors = F)
case$total <- rowSums(case[ , 13 : 218])
case <- case %>% top_n(500, total) 
case_long <- gather(case, date, new_cases, X2020.01.23:X2020.08.15)
case_long <- case_long %>% 
  select(Admin2, Province_State, Combined_Key, date, new_cases) %>%
  mutate(date = substr(date, 2, nchar(date)) %>% as.Date("%Y.%m.%d"))

death <- read.csv('daily_newly_deaths_US.csv')
death <- filter(death, Combined_Key %in% case$Combined_Key)
death_long <- gather(death, date, new_death, X2020.01.23:X2020.08.15)
death_long <- death_long %>% 
  select(Combined_Key, date, new_death) %>%
  mutate(date = substr(date, 2, nchar(date)) %>% as.Date("%Y.%m.%d"))
case_death <- merge(case_long, death_long, by = c('Combined_Key', 'date'))
intv <- read.csv('us_intv.csv', stringsAsFactors = F)
intv$date <- as.Date(intv$date)
case_death <- merge(case_death, intv, by.x = c('Province_State', 'date'), by.y = c('division_eng_name', 'date'))
case_death <- case_death %>% 
  filter(Province_State != 'Oklahoma') %>%
  group_by(Combined_Key) %>%
  arrange(date) %>%
  mutate(stay_at_home = lead(win7_stay_at_home, 4),
         school_close = lead(win7_school_close, 4),
         childcare_close = lead(win7_childcare_close, 4),
         shop_close = lead(win7_shop_close, 4),
         gathering_10lower = lead(win7_gathering_outside_10lower, 4),
         gathering_10over = lead(win7_gathering_outside_10over, 4)) %>%
  select(Province_State, date, Combined_Key, Admin2, new_cases, new_death,
         state, stay_at_home, school_close,
         childcare_close, shop_close, gathering_10lower, gathering_10over)
write.csv(case_death, 'input_bayes.csv')

# compute Rt ----
library(readr)
library(EpiEstim)
#time window used to estimate Rt
window_wide <- 7  
all_data <- case_long
all_data$add <- all_data$new_cases
all_data$add[all_data$add < 0] <- 0 
all_data$eng <- as.factor(all_data$Combined_Key)  #label each city or region in a country
num_of_city <- length(levels(all_data$eng))  #counting the number of cities of regions
city_name <- levels(all_data$eng)  #extracting city names
all_together <- data.frame()                                                               #constructing data frame
for (i in 1:num_of_city){
  city_data_frame <- subset(all_data, eng == city_name[i] )                                #extracting one city once
  num_of_days <- nrow(city_data_frame)                                                     #days of observation
  city_data_frame$add <- as.numeric(city_data_frame$add)                                   #make sure that the data type is "numeric"
  day_case <- city_data_frame$add                                                          #everyday case of one city or region
  day_case[(day_case < 0) |(is.na(day_case))] <- 0                                         #maker sure that everyday case is nonnegative 
  parameter <- estimate_R(day_case,method="parametric_si",
                          config = make_config(list(mean_si = 7.5, 
                                                    std_si = 3.4,
                                                    t_start = seq(2, num_of_days-window_wide, 1), 
                                                    t_end = seq(2, num_of_days-window_wide, 1) + window_wide
                          )))                                    #Rt estimate
  city_data_frame$Rt_estimate <- NA                                                        #data_frame initialization
  city_data_frame$Rt_estimate[seq(2, num_of_days-window_wide, 1) + window_wide ] <-
    parameter$R$`Mean(R)`
  city_data_frame$Rt_estimate_std <- NA
  city_data_frame$Rt_estimate_std[seq(2, num_of_days-window_wide, 1) + window_wide ] <-
    parameter$R$`Std(R)`                                                                   #reporting mean and std of Rt
  if (i == 1){
    all_together <- city_data_frame
  }else{
    all_together <- rbind(all_together,city_data_frame)
  }
  print(i)
}

#merge with intervention info
intv <- read.csv('us_intv.csv', stringsAsFactors = F)
intv$date <- as.Date(intv$date)
input <- merge(intv, all_together, by.x = c('division_eng_name', 'date'), by.y = c('Province_State', 'date'))
input <- input %>% 
  mutate(cv = Rt_estimate_std / Rt_estimate) %>% 
  mutate(log_Rt_estimate = log(Rt_estimate),
         log_Rt_estimate = ifelse(cv <= 0.3, log_Rt_estimate, NA),
         city_eng_name = Combined_Key) %>%
  select(-X)
write.csv(input, 'input.csv')


