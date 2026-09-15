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

demo_j <- nhanes("DEMO_J")
bmx_j  <- nhanes("BMX_J")
paq_j  <- nhanes("PAQ_J")
bpx_j  <- nhanes("BPX_J")
# Save the data in the data folder

write_csv(demo_j, "data/demo_j.csv")
write_csv(bmx_j,  "data/body_measures_j.csv")
write_csv(paq_j,  "data/physical_activity_j.csv")
write_csv(bpx_j,  "data/blood_pressure_j.csv")