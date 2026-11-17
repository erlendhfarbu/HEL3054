# Analysis IHD data

library(tableone)
library(survminer)
library(survival)

variables <- c("broad_event", "smoking_status", "physical_activity", "education", 
               "birth_year", "bmi_kg_m2", "systolic_bp_mmHg", "total_cholesterol_mmol_l", 
               "diabetes_baseline")
tableone_descr <- CreateTableOne(vars=variables, strata = "sex", data = analysis_ready)

print(tableone_descr)


# create survival_data

km_model <- survfit(Surv(broad_followup_days, broad_event)~sex, data = analysis_ready)

km_model
ggsurvplot(km_model, data = analysis_ready)

cox_model <- coxph(Surv(broad_followup_days, broad_event)~sex + smoking_status, 
                   data=analysis_ready)
summary(cox_model)



#create a hypertension variable

analysis_ready <- analysis_ready |> mutate(
  hypertension = case_when(systolic_bp_mmHg>=140~"Yes",
                           systolic_bp_mmHg<140~"No"))

analysis_ready <- analysis_ready |> mutate(
  hypertension2 = systolic_bp_mmHg>=140)

sum(is.na(analysis_ready$hypertension))

tableone_descr <- CreateTableOne(vars=variables, strata = "hypertension", 
                                 data = analysis_ready)

print(tableone_descr)

# create survival_data

km_model <- survfit(Surv(broad_followup_days, broad_event)~hypertension, data = analysis_ready)

km_model
ggsurvplot(km_model, data = analysis_ready, pval = T, pval.method = T, conf.int = T)

log_rank_test <- survdiff(Surv(broad_followup_days, broad_event)~hypertension, data = analysis_ready)
log_rank_test

cox_model <- coxph(Surv(broad_followup_days, broad_event)~hypertension + smoking_status, 
                   data=analysis_ready)
summary(cox_model)

cox_model_cont <- coxph(Surv(broad_followup_days, broad_event)~systolic_bp_mmHg + smoking_status, 
                   data=analysis_ready)
summary(cox_model_cont)


#### using large dataset ####
analysis_ready_first_ihd <- analysis_ready_first_ihd |> mutate(
  hypertension = case_when(systolic_bp_mmHg>=140~1,
                           systolic_bp_mmHg<140~0))

sum(is.na(analysis_ready_first_ihd$hypertension))

tableone_descr <- CreateTableOne(vars=variables, strata = "hypertension", data = analysis_ready_first_ihd)

print(tableone_descr)


# create survival_data

km_model <- survfit(Surv(broad_followup_days, broad_event)~hypertension, data = analysis_ready_first_ihd)

km_model
ggsurvplot(km_model, data = analysis_ready_first_ihd, 
           pval = T, pval.method = T, conf.int = T)

cox_model <- coxph(Surv(broad_followup_days, broad_event)~hypertension + smoking_status, 
                   data=analysis_ready_first_ihd)
summary(cox_model)

cox_model_cont <- coxph(Surv(broad_followup_days, broad_event)~systolic_bp_mmHg + sex + smoking_status, 
                        data=analysis_ready_first_ihd)
summary(cox_model_cont)

##### test ph-assumption #####
ph_test <- cox.zph(cox_model)
print(ph_test)
plot(ph_test)
