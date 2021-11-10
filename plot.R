library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

# 3a ----
out <- read.csv("out_3method.csv")
# stay-at-home ----
p1 <- ggplot(filter(out, intervention == 'stay_at_home'), aes(col=type, y=effect, x = type)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u, col = type), 
                size = 0.5, width=0) +
  geom_errorbar(aes(ymin = ci_l5, ymax = ci_u5, col = type), 
                size = 1.5, width=0) +
  scale_color_discrete(labels = c("Bayesian hierarchical model", "Two way fixed effect DID", "Robust DID")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype = 'dotted'), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        #axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 15),
        legend.position = "none") + 
  labs(title = "Stay-at-home order", 
       x = "", y = expression("Percentage change of R"[t]), 
       col = "Method") +
  scale_x_discrete(labels = rep("", 3)) +
  scale_y_continuous(limits = c(-0.31, 0.3), breaks = seq(-0.3, 0.3, 0.1),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(4.92)


# school ----
p2 <- ggplot(filter(out, intervention == 'school_close'), aes(col=type, y=effect, x = type)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u, col = type), 
                size = 0.5, width=0) +
  geom_errorbar(aes(ymin = ci_l5, ymax = ci_u5, col = type), 
                size = 1.5, width=0) +
  scale_color_discrete(labels = c("Bayesian hierarchical model", "Two way fixed effect DID", "Robust DID")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype = 'dotted'), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        #axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 15),
        legend.position = "none") + 
  labs(title = "School closure", 
       x = "", y = expression("Percentage change of R"[t]), 
       col = "Method") +
  scale_x_discrete(labels = rep("", 3)) +
  scale_y_continuous(limits = c(-0.75, 0.5), breaks = seq(-0.75, 0.5, 0.25),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(2.4)


# childcare ----
p3 <- ggplot(filter(out, intervention == 'childcare_close'), aes(col=type, y=effect, x = type)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u, col = type), 
                size = 0.5, width=0) +
  geom_errorbar(aes(ymin = ci_l5, ymax = ci_u5, col = type), 
                size = 1.5, width=0) +
  scale_color_discrete(labels = c("Bayesian hierarchical model", "Two way fixed effect DID", "Robust DID")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype = 'dotted'), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        #axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 15),
        legend.position = "none") + 
  labs(title = "Childcare closure", 
       x = "", y = expression("Percentage change of R"[t]), 
       col = "Method") +
  scale_x_discrete(labels = rep("", 3)) +
  scale_y_continuous(limits = c(-0.3, 0.32), breaks = seq(-0.3, 0.3, 0.1),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(4.84)


# shop ----
p4 <- ggplot(filter(out, intervention == 'shop_close'), aes(col=type, y=effect, x = type)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u, col = type), 
                size = 0.5, width=0) +
  geom_errorbar(aes(ymin = ci_l5, ymax = ci_u5, col = type), 
                size = 1.5, width=0) +
  scale_color_discrete(labels = c("Bayesian hierarchical model", "Two way fixed effect DID", "Robust DID")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype = 'dotted'), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        #axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 15),
        legend.position = "none") + 
  labs(title = "Non-essential retail closure", 
       x = "", y = expression("Percentage change of R"[t]), 
       col = "Method") +
  scale_x_discrete(labels = rep("", 3)) +
  scale_y_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, 0.1),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(5)


# 10lower ----
p5 <- ggplot(filter(out, intervention == 'gathering_10lower'), aes(col=type, y=effect, x = type)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u, col = type), 
                size = 0.5, width=0) +
  geom_errorbar(aes(ymin = ci_l5, ymax = ci_u5, col = type), 
                size = 1.5, width=0) +
  scale_color_discrete(labels = c("Bayesian hierarchical model", "Two way fixed effect DID", "Robust DID")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype = 'dotted'), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        #axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 15),
        legend.position = "none") + 
  labs(title = "Small-size gathering ban", 
       x = "", y = expression("Percentage change of R"[t]), 
       col = "Method") +
  scale_x_discrete(labels = rep("", 3)) +
  scale_y_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, 0.1),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(5)


# 10over ----
p6 <- ggplot(filter(out, intervention == 'gathering_10over'), aes(col=type, y=effect, x = type)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u, col = type), 
                size = 0.5, width=0) +
  geom_errorbar(aes(ymin = ci_l5, ymax = ci_u5, col = type), 
                size = 1.5, width=0) +
  scale_color_discrete(labels = c("Bayesian hierarchical model", "Two way fixed effect DID", "Robust DID")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype = 'dotted'), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        #axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 15))+
        #legend.position = "none") + 
  labs(title = "Large-size gathering ban", 
       x = "", y = expression("Percentage change of R"[t]), 
       col = "Method") +
  scale_x_discrete(labels = rep("", 3)) +
  scale_y_continuous(limits = c(-0.3, 0.62), breaks = seq(-0.3, 0.62, 0.15),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(3.26)

# arrange ----
library(egg)
p <- ggarrange(p1, p2, p3, p4, p5, p6, draw = F, ncol = 3)
ggsave("plot3a.png", plot = p, height = 25, width =60,
       units = "cm", dpi = 200, limitsize = F)

#3b
load("output.RData")
# stay-at-home ----
t <- data.frame(day = 0 : 21, effect = unlist(us_stay_at_home)[seq(16,16+4*21,4)],
                 se = unlist(us_stay_at_home)[seq(17,17+4*21,4)])
t <- t %>% 
  mutate(uci = exp(effect - 1.96 * se) - 1, lci = exp(effect + 1.96 * se) - 1) %>%
  mutate(effect = exp(effect) - 1)
p1 <- ggplot(t, aes(y=effect, x = day)) + 
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                size = 0.5, width=0, col = "#BBBBBB") +
  geom_point(size = 2, col = "#777777") +
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
  labs(title = "Stay-at-home order", 
       x = "Days since intervention", y = expression("Percentage change of R"[t])) +
  scale_y_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.1),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(20)



# school close ----
t <- data.frame(day = 0 : 21, effect = unlist(us_school_close)[seq(16,16+4*21,4)],
                 se = unlist(us_school_close)[seq(17,17+4*21,4)])
t <- t %>% 
  mutate(uci = exp(effect - 1.96 * se) - 1, lci = exp(effect + 1.96 * se) - 1) %>%
  mutate(effect = exp(effect) - 1)
p2 <- ggplot(t, aes(y=effect, x = day)) + 
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                size = 0.5, width=0, col = "#BBBBBB") +
  geom_point(size = 2, col = "#777777") +
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
  labs(title = "School closure", 
       x = "Days since intervention", y = expression("Percentage change of R"[t])) +
  scale_y_continuous(limits = c(-0.5, 2.6), breaks = seq(-0.5, 2.5, 0.5),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(6.45)



# childcare close ----
t <- data.frame(day = 0 : 21, effect = unlist(us_childcare_close)[seq(16,16+4*21,4)],
                se = unlist(us_childcare_close)[seq(17,17+4*21,4)])
t <- t %>% 
  mutate(uci = exp(effect - 1.96 * se) - 1, lci = exp(effect + 1.96 * se) - 1) %>%
  mutate(effect = exp(effect) - 1)
p3 <- ggplot(t, aes(y=effect, x = day)) + 
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                size = 0.5, width=0, col = "#BBBBBB") +
  geom_point(size = 2, col = "#777777") +
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
  labs(title = "Childcare closure", 
       x = "Days since intervention", y = expression("Percentage change of R"[t])) +
  scale_y_continuous(limits = c(-0.5, 0.6), breaks = seq(-0.5, 0.5, 0.1),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(18.2)



# shop close ----
t <- data.frame(day = 0 : 21, effect = unlist(us_shop_close)[seq(16,16+4*21,4)],
                se = unlist(us_shop_close)[seq(17,17+4*21,4)])
t <- t %>% 
  mutate(uci = exp(effect - 1.96 * se) - 1, lci = exp(effect + 1.96 * se) - 1) %>%
  mutate(effect = exp(effect) - 1)
p4 <- ggplot(t, aes(y=effect, x = day)) + 
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                size = 0.5, width=0, col = "#BBBBBB") +
  geom_point(size = 2, col = "#777777") +
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
  labs(title = "Non-essential retail closure", 
       x = "Days since intervention", y = expression("Percentage change of R"[t])) +
  scale_y_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.1),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(20)



# 10lower ----
t <- data.frame(day = 0 : 21, effect = unlist(us_gathering_outside_10lower)[seq(16,16+4*21,4)],
                se = unlist(us_gathering_outside_10lower)[seq(17,17+4*21,4)])
t <- t %>% 
  mutate(uci = exp(effect - 1.96 * se) - 1, lci = exp(effect + 1.96 * se) - 1) %>%
  mutate(effect = exp(effect) - 1)
p5 <- ggplot(t, aes(y=effect, x = day)) + 
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                size = 0.5, width=0, col = "#BBBBBB") +
  geom_point(size = 2, col = "#777777") +
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
  labs(title = "Small-size gathering ban", 
       x = "Days since intervention", y = expression("Percentage change of R"[t])) +
  scale_y_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.1),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(20)



# 10over ----
t <- data.frame(day = 0 : 21, effect = unlist(us_gathering_outside_10over)[seq(16,16+4*21,4)],
                se = unlist(us_gathering_outside_10over)[seq(17,17+4*21,4)])
t <- t %>% 
  mutate(uci = exp(effect - 1.96 * se) - 1, lci = exp(effect + 1.96 * se) - 1) %>%
  mutate(effect = exp(effect) - 1)
p6 <- ggplot(t, aes(y=effect, x = day)) + 
  geom_hline(yintercept = 0, size = 0.5, col = 'grey') +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                size = 0.5, width=0, col = "#BBBBBB") +
  geom_point(size = 2, col = "#777777") +
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
  labs(title = "Large-size gathering ban", 
       x = "Days since intervention", y = expression("Percentage change of R"[t])) +
  scale_y_continuous(limits = c(-0.5, 1.2), breaks = seq(-0.5, 1.2, 0.25),
                     labels = function(x) paste0(round(x * 100, 1), "%")) +
  coord_fixed(11.8)



p <- ggarrange(p1, p2, p3, p4, p5, p6, draw = F, ncol = 3)
ggsave("paper3_plot3b.png", plot = p, height = 25, width =60,
       units = "cm", dpi = 200, limitsize = F)

# fig1
input <- read.csv('input_paper3.csv',
                  stringsAsFactors = F)
input$date <- as.Date(input$date)
input <- filter(input, division_eng_name != 'Oklahoma')
p <- ggplot(input) + 
  geom_line(aes(y = Rt_estimate, 
                x = date, by = Combined_Key), size = 0.5, alpha = 0.2) +
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
       x = "Time", y = expression("R"[t]))+
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) 
ggsave("paper3_plot1a.png", plot = p, height = 15, width =25,
       units = "cm", dpi = 200, limitsize = F)

#plot 1b
intervention_plot <- function(intervention, cols, titles, 
                              city_eng_name, palette){
  for (i in 1:length(intervention)){
    int <- intervention[i]
    t <- input %>% select(date, city_eng_name, int) %>%
      spread(date, int) %>%
      mutate(intervention = do.call(paste, c(.[cols], sep = "-"))) 
    t2 <- as.data.frame(table(t$intervention))
    t <- t %>% select(-city_eng_name) %>% 
      distinct() %>%
      merge(t2, by.x = "intervention", by.y = "Var1") %>%
      gather("date", int, 2:length(cols))
    t$interventionid <- LETTERS[1:nrow(t2)]
    t$date <- as.Date(t$date)
    fig <- ggplot(t) + 
      geom_line(aes(x = date, y = int, group = intervention,
                    lwd = Freq), color = palette[i], alpha = 0.5) +
      scale_size(range = c(0.5, 5), guide = FALSE) +
      theme(panel.background = element_rect(fill = "white",colour = "black",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5,size = 38,margin = margin(0, 0, 10, 0)),
            axis.text = element_text(size = 20),#20 for uk 23for jp us br
            axis.text.y = element_text(size = 25),
            axis.title.y = element_text(size = 35, margin = margin(0, 10, 0, 0)),
            axis.title.x = element_text(size = 35, margin = margin(10, 0, 0, 0)),
            plot.margin = unit(c(1, 1, 1, 1), "cm"))+
      labs(title = titles[i], x = "Time", y = "Status") +
      scale_x_date(date_breaks = "1 month",
                   date_labels = '%b-%d')+
      scale_y_continuous(breaks = c(0, 0.5, 1))
    assign(paste0("fig", i), fig, env = .GlobalEnv)
  }
}
intervention <- c('win7_stay_at_home',"win7_school_close", "win7_childcare_close", 
                  'win7_shop_close', 'win7_gathering_outside_10lower',
                  'win7_gathering_outside_10over')
titles <- c("Stay-at-home order","School closure", "Childcare closure", 
            "Non-essential retail closure", "Small-size gathering ban",
            "Large-size gathering ban")
palette <- brewer.pal(6, 'Set3')
cols <- seq(as.Date("2020/3/13"), as.Date("2020/8/15"), "days") %>% as.character()
intervention_plot(intervention, cols, titles, 
                  'city_eng_name', palette)
fig <- ggarrange(fig1, fig2, fig3, fig4, fig5, fig6,
                 #labels = c("C", rep("", 8)), 
                 #font.label = list(size=50), 
                 ncol = 3, nrow = 2)
ggsave("plot1b.png", fig, width = 80, height = 45, units = "cm", 
       dpi = 100, limitsize = F)
