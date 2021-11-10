
library(stringi)

input <- read.csv('input.csv', stringsAsFactors = F)
input$date <- as.Date(input$date)
input <- filter(input, division_eng_name != 'Oklahoma')
gm <- read.csv("2020_US_Region_Mobility_Report.csv",
               stringsAsFactors = F)
gm$date <- as.Date(gm$date)
gm$county <- gm$sub_region_2 %>% as.character() %>%
  str_replace(" County", "") %>%
  str_replace(" Parish", "")
gm <- filter(gm,!(sub_region_1=="Virginia"&sub_region_2=="Richmond County"))
gm <- filter(gm,!(sub_region_1=="Maryland"&sub_region_2=="Baltimore County"))
gm <- filter(gm,!(sub_region_1=="Missouri"&sub_region_2=="St. Louis"))
gm$county[which(gm$sub_region_1=="District of Columbia")] <- "District of Columbia"
input <- merge(input,gm,by.x=c("Admin2","division_eng_name","date"),by.y=c("county","sub_region_1","date"))
input <- input %>% 
  mutate_at(vars(win7_stay_at_home, win7_school_close,
                win7_childcare_close, win7_shop_close,
                win7_gathering_outside_10lower,
                win7_gathering_outside_10over), function(x){lead(x, 4)})
m <- lm(residential_percent_change_from_baseline~
          win7_stay_at_home+win7_school_close+
          win7_childcare_close+win7_shop_close+
          win7_gathering_outside_10lower+win7_gathering_outside_10over,
        input)
m <- lm(residential_percent_change_from_baseline~
            win7_stay_at_home+win7_school_close+
            win7_childcare_close+win7_shop_close+
            win7_gathering_outside_10lower+win7_gathering_outside_10over+
            as.factor(date),input)
summary(m)
tfe <- m$coefficients[grepl('date', names(m$coefficients))]
tfe <- as.data.frame(tfe)
tfe$date <- row.names(tfe) %>% substr(16, nchar(.)) %>% as.Date
input <- merge(input, tfe, by = 'date')
cor.test(input$tfe, input$win7_stay_at_home)
cor.test(input$tfe, input$win7_school_close)
cor.test(input$tfe, input$win7_childcare_close)
cor.test(input$tfe, input$win7_shop_close)
cor.test(input$tfe, input$win7_gathering_outside_10lower)
cor.test(input$tfe, input$win7_gathering_outside_10over)

#plot
p <- ggplot(input) + 
  geom_line(aes(y=residential_percent_change_from_baseline/100, 
                x = date, by = Combined_Key), size = 0.5, alpha = 0.2,
            col = 'grey') +
  geom_line(data = tfe, aes(y = tfe / 100, x = date)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype = 'dotted'), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5, size = 15),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 15))+
  labs(title = "", 
       x = "Time", y = "Time staying at home\ncomparing to baseline") +
  scale_y_continuous(limits = c(-0.1, 0.4), breaks = seq(-0.1, 0.4, 0.1),
                     labels = function(x) paste0(round(x * 100, 1), "%"))
ggsave("plot2.png", plot = p, height = 15, width =25,
       units = "cm", dpi = 200, limitsize = F)

