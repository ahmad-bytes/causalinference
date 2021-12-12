rm(list = ls())
set.seed(51)

age_data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/age_data.txt")

N = NROW(age_data)

fit_1 <- stan_glm(Y ~ Z + age, family=binomial(link="logit"), data=age_data, refresh=0)
print(fit_1)

fit_1$coefficients

all_treatment = rep(1, N)
all_control = rep(0, N)

intercept = fit_1$coefficients[1]
treatment_coeff = fit_1$coefficients[2]
age_coeff = fit_1$coefficients[3]



#prob_treatment <- exp(intercept + treatment_coeff * all_treatment + age_coeff *age_data$age)/(1 + exp(intercept + treatment_coeff * all_treatment + age_coeff *age_data$age))

prob_treatment <- 1/(1 + exp(-(intercept + treatment_coeff * all_treatment + age_coeff *age_data$age)))

numerator = mean(prob_treatment) / mean(1-prob_treatment)

prob_control <- 1/(1 + exp(-(intercept + treatment_coeff * all_control + age_coeff *age_data$age)))

denominator = mean(prob_control) / mean(1-prob_control)

numerator/ denominator

obs_df = data.frame(cbind(Yobs = invlogit(intercept + treatment_coeff*age_data$Z + age_coeff * age_data$age), Z=age_data$Z))

mean(obs_df$Yobs[obs_df$Z == 1 ]) - mean(obs_df$Yobs[obs_df$Z == 0])





