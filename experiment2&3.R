library(dplyr)
library(stringr)
library(lfe)


input <- read.csv('input.csv', stringsAsFactors = F)
input$date <- as.Date(input$date)
input <- filter(input, division_eng_name != 'Oklahoma')

#two-way fixed effect DID
m <- felm(log_Rt_estimate~win7_stay_at_home+win7_school_close+
            win7_childcare_close+win7_shop_close+
            win7_gathering_outside_10lower+win7_gathering_outside_10over|
            city_eng_name+as.factor(date)|0|city_eng_name,input) #
summary(m)
out_fe <- data.frame(intervention = dimnames(m$beta)[[1]],
                     effect = exp(m$coefficients) - 1, 
                     ci_l = exp(m$coefficients - 1.96 * m$cse) - 1,
                     ci_u = exp(m$coefficients + 1.96 * m$cse) - 1,
                     ci_l5 = exp(m$coefficients - 0.67 * m$cse) - 1,
                     ci_u5 = exp(m$coefficients + 0.67 * m$cse) - 1)
out_fe$intervention <- out_fe$intervention %>% as.character %>%
  sapply(function(x){substr(x, 6, nchar(x))})  
colnames(out_fe) <- c('intervention', 'effect', 'ci_l', 'ci_u', 'ci_l5', 'ci_u5')
out_fe$type <- "twfe"

# robust DID ----
source('didmultiplegt.R')
control <- c('win7_stay_at_home',
             'win7_school_close',
             'win7_childcare_close',
             'win7_shop_close',
             'win7_gathering_outside_10lower',
             'win7_gathering_outside_10over')
for (p in control){
  model <- p %>% str_sub(6, -1)
  model <- paste0("us_", model)
  m <- try(
    did(input, "log_Rt_estimate", "city_eng_name", "date", p,
        controls = control[control != p],
        placebo = 5, dynamic = 21, cluster = "date",
        brep = 100, covariance = TRUE, average_effect = "simple", 
        parallel = TRUE, 
        direction = "both", 
        controlby = "period", period=c("2020-01-01","2020-12-30"),
    controlT = FALSE))
  assign(model,m)
  print(paste("finish", p))
  save.image("output.RData")
}


# post process
load("output.RData")
intervention <- c("stay_at_home", "school_close", "childcare_close",
                  'shop_close', "gathering_outside_10lower", 
                  "gathering_outside_10over")
result <- data.frame(intervention = intervention,
                     effect_average = 0,
                     se_effect_average = 0,
                     size = 0, 
                     stringsAsFactors = F)
b<-grep(ls(),pattern="us_",value=TRUE)
for (i in b){
  int <- substr(i,4,nchar(i))
  c <- get(i)
  d <- which(names(c) == "effect")
  dynamic <- 22
  total <- dynamic * 3 + 19
  dynamic <- dynamic - 1
  f <- 0
  g <- c()
  while (f <= dynamic){
    g <- c(g, min(c[[4 * f + 3 + d]], get(i)[[4 * f + d + 2]] - get(i)[[4 * f + d + 3]]))
    f <- f + 1
  }  
  dynamic <- 21
  start = 1
  end = 1 + dynamic
  v <- c %>% unlist()
  effect_average = mean(v[seq(d, 4 * dynamic + d, 4)], na.rm=TRUE)
  var_effect_average = sum((1 / (dynamic + 1) * v[seq(d + 1, 4 * dynamic + 1 + d, 4)]) ^ 2)
  for (ii in 1:dynamic) {
    if (ii >= dynamic) { break }
    for (jj in (ii + 1):dynamic) {
      name = paste("cov_effect", toString(ii), toString(jj), sep="_")
      var_effect_average = var_effect_average + (1 / (dynamic + 1)) ^ 2 * 2 * unlist(c[name])
    }
  }
  result[which(result$intervention==int), 2] <- effect_average
  result[which(result$intervention==int), 3] <- sqrt(var_effect_average)
  result[which(result$intervention==int), 4] <- sum(g[which(g > 5)])
}
out_r <- data.frame(intervention = result$intervention,
                    effect = exp(result$effect_average) - 1,
                    ci_l = exp(result$effect_average - 1.96 * result$se_effect_average) - 1,
                    ci_u = exp(result$effect_average + 1.96 * result$se_effect_average) - 1,
                    ci_l5 = exp(result$effect_average - 0.67 * result$se_effect_average) - 1,
                    ci_u5 = exp(result$effect_average + 0.67 * result$se_effect_average) - 1)
out_r$type <- 'rdid'
out_b <- read.csv('bayesian result.csv')
out_b <- data.frame(intervention = out_b$X, effect = -out_b$med / 100,
                    ci_l = -out_b$ui / 100, ci_u = -out_b$li / 100,
                    ci_l5 = -out_b$uq / 100, ci_u5 = -out_b$lq / 100)
out_b$type <- 'bayes'
out <- rbind(out_fe, out_r, out_b)
out$intervention <- str_replace(out$intervention, 'outside_', '')
write.csv(out, "out_3method.csv")

