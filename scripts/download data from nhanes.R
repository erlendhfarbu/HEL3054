# ============================================================================
# Week 43: Download NHANES data
#
# Store it in your scripts folder!
# Make sure that you run: install.packages("nhanesA")
#
# DEMO_J contains age and sex
# BMX_J contains height, weight, and BMI
# PAQ_J contains physical activity
# BPX_J : blood pressure (for the BMI vs SBP scatterplot)
# ============================================================================

library(nhanesA)

# Download data from NHANES

demo <- nhanes("DEMO_J")
bmx  <- nhanes("BMX_J")
paq  <- nhanes("PAQ_J")
bpx  <- nhanes("BPX_J")
# Save the data in the data folder

write_csv(demo, "data/demo.csv")
write_csv(bmx,  "data/body_measures.csv")
write_csv(paq,  "data/physical_activity.csv")
write_csv(bpx,  "data/blood_pressure.csv")