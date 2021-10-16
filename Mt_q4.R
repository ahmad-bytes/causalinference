rm(list = ls())

ds <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/Titiunik.txt")
#View(Titiunik)

ds.texas = ds[(ds$texas0_arkansas1 == 0),]
ds.arkansas = ds[(ds$texas0_arkansas1 == 1),]

#texas = mean(ds$bills_introduced[ds$term2year == 1 & ds$texas0_arkansas1 == 0]) - mean(ds$bills_introduced[ds$term2year == 0 & ds$texas0_arkansas1 == 0])
#arkansas = mean(ds$bills_introduced[ds$term2year == 1 & ds$texas0_arkansas1 == 1]) - mean(ds$bills_introduced[ds$term2year == 0 & ds$texas0_arkansas1 == 1])

texas.mean = mean(ds.texas$bills_introduced[ds.texas$term2year == 1]) - mean(ds.texas$bills_introduced[ds.texas$term2year == 0])
texas.count = nrow(ds.texas)
texas.treatment_count = NROW(ds.texas$bills_introduced[ds.texas$term2year == 1])
texas.control_count =  NROW(ds.texas$bills_introduced[ds.texas$term2year == 0])
texas.var_treatment = var(ds.texas$bills_introduced[ds.texas$term2year == 1])
texas.var_control = var(ds.texas$bills_introduced[ds.texas$term2year == 0])
texas.se = sqrt((texas.var_control/texas.control_count) + (texas.var_treatment / texas.treatment_count))

texas.ate = texas.mean * texas.count

arkansas.mean = mean(ds.arkansas$bills_introduced[ds.arkansas$term2year == 1]) - mean(ds.arkansas$bills_introduced[ds.arkansas$term2year == 0])
arkansas.count = nrow(ds.arkansas)
arkansas.treatment_count = NROW(ds.arkansas$bills_introduced[ds.arkansas$term2year == 1])
arkansas.control_count =  NROW(ds.arkansas$bills_introduced[ds.arkansas$term2year == 0])
arkansas.var_treatment = var(ds.arkansas$bills_introduced[ds.arkansas$term2year == 1])
arkansas.var_control = var(ds.arkansas$bills_introduced[ds.arkansas$term2year == 0])
arkansas.se = sqrt(arkansas.var_control/arkansas.control_count + arkansas.var_treatment / arkansas.treatment_count)

arkansas.ate = arkansas.mean * arkansas.count

n = texas.count + arkansas.count

ate = (texas.ate + arkansas.ate)/n

se.ate = sqrt(texas.se  ^ 2 * (texas.count / n)^2 + arkansas.se ^ 2 * (arkansas.count / n)^2)