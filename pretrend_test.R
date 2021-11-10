
library(dplyr)
library(lfe)

#two-way fix
input <- read.csv('input_paper3.csv',
                  stringsAsFactors = F)
input$date <- as.Date(input$date)
input <- filter(input, division_eng_name != 'Oklahoma')
input <- arrange(input, city_eng_name, date)

ind_var <- c("win7_stay_at_home", "win7_school_close", "win7_childcare_close",
             "win7_shop_close", 
             "win7_gathering_outside_10lower", "win7_gathering_outside_10over")
var_tocheck <- ind_var
pretrend <- data.frame(var = rep(0, length(var_tocheck)), 
                       beta_pre1 = 0, se_pre1 = 0, p_pre1 = 0, 
                       beta_pre2 = 0, se_pre2 = 0, p_pre2 = 0, 
                       beta_pre3 = 0, se_pre3 = 0, p_pre3 = 0, 
                       beta_pre4 = 0, se_pre4 = 0, p_pre4 = 0, 
                       beta_pre5 = 0, se_pre5 = 0, p_pre5 = 0)
for (k in 1 : length(var_tocheck)){
  i <- var_tocheck[k]
  pretrend$var[k] <- i
  for (j in 1:5){
    create_change_after <- 
      paste0(i, "_a = lead(", i, ", j) -", i)
    create_change_after <- paste("group_by(input, city_eng_name) %>% mutate(", create_change_after, ")")
    input <- eval(parse(text = create_change_after))
    
    create_change_after <- 
      paste0(i, "_a2 = lead(", i, ", 5) - lead(", i, ",j)")
    create_change_after <- paste("group_by(input, city_eng_name) %>% mutate(", create_change_after, ")")
    input <- eval(parse(text = create_change_after))

    change_after_nato0 <- paste0(i, "_a = ifelse(is.na(", i, "_a), 0, ", i, "_a)")
    change_after_nato0 <- paste("mutate(input, ", change_after_nato0, ")")
    input <- eval(parse(text = change_after_nato0))
    
    change_after_nato0 <- paste0(i, "_a2 = ifelse(is.na(", i, "_a2), 0, ", i, "_a2)")
    change_after_nato0 <- paste("mutate(input, ", change_after_nato0, ")")
    input <- eval(parse(text = change_after_nato0))
    formula_part2 <- paste0(i, "_a")
    formula <- paste("log_Rt_estimate ~ ",
                     paste(ind_var, collapse = "+"),
                     '+', formula_part2,
                     "| city_eng_name + as.factor(date) | 0 | city_eng_name") %>%
      as.formula()
    m <- felm(formula = formula, input)
    var <- paste0(i, '_a')
    pretrend[k, j * 3 - 1] <- m$beta[dimnames(m$beta)[[1]] == var]
    pretrend[k, j * 3] <- m$cse[names(m$cse) == var]
    pretrend[k, j * 3 + 1] <- m$cpval[names(m$pval) == var]
  }
}

# output table ####
out <- data.frame(Intervention = 0, '1 day' = 0, '2 days' = 0, '3 days' = 0,
                  '4 days' = 0, '5 days' = 0)
for (i in 1 : nrow(pretrend)){
  out[i, 'Intervention'] <- pretrend$var[i]
  for (j in 1 : 5){
    b <- format(pretrend[i, 3 * j - 1], digits = 3)
    p <- pretrend[i, 3 * j + 1]
    if (p <= 0.05 & p > 0.01){
      b <- paste0(b, '*')
    }
    if (p <= 0.01 & p > 0.001){
      b <- paste0(b, '**')
    }
    if (p <= 0.001){
      b <- paste0(b, '***')
    }
    
    se <- paste0('(', format(pretrend[i, 3 * j], digits = 3), ')')
    out[i, j + 1] <- paste(b, se, sep = '\n')
  }
}
out$Intervention <- c('Stay-at-home', 'School', 'Childcare', 
                      'Retail', 'Small gathering', 'Large gathering')
write.csv(out, 'pretrend_twfe.csv')

#ouput robust did
load("paper3_aerdid.RData")
intervention <- c("stay_at_home", "school_close", "childcare_close",
                  'shop_close', "gathering_outside_10lower", 
                  "gathering_outside_10over")
b<-grep(ls(),pattern="us_",value=TRUE)
out <- data.frame(Intervention = 0, '1 day' = 0, '2 days' = 0, '3 days' = 0,
                  '4 days' = 0, '5 days' = 0)
for (i in 1 : 6){
  out[i, 'Intervention'] <- intervention[i]
  c <- get(paste0('us_', intervention[i]))
  for (j in 1 : 5){
    a <- unlist(c)[which(names(c) == paste('placebo', j, sep = '_'))]
    b <- format(a, digits = 3)
    d <- unlist(c)[which(names(c) == paste('se_placebo', j, sep = '_'))]
    se <- paste0('(', format(d, digits = 3), ')')
    p <- 2*pt(abs(a/d), df = 100, lower=F)
    if (p <= 0.05 & p > 0.01){
      b <- paste0(b, '*')
    }
    if (p <= 0.01 & p > 0.001){
      b <- paste0(b, '**')
    }
    if (p <= 0.001){
      b <- paste0(b, '***')
    }
    out[i, j + 1] <- paste(b, se, sep = '\n')
  }
}
out$Intervention <- c('Stay-at-home', 'School', 'Childcare', 
                      'Retail', 'Small gathering', 'Large gathering')
write.csv(out, 'pretrend_robust.csv')



