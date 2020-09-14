
# Import Libraries
library(tidyverse)
library(ggplot2)
setwd("~/R_Analysis/")

# Read MechaCar_mpg.csv
prototype_data <- read.csv(file='MechaCar_mpg.csv')


#  Generating linear model
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data = prototype_data)

# Summarize linear model
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data = prototype_data))


# Ground Clearance
gc_model <- lm(mpg ~ ground.clearance, prototype_data)
gc_yval <- gc_model$coefficients['ground.clearance']*prototype_data$ground.clearance + gc_model$coefficients['(Intercept)']

# Import datatset into ggplot2
plt <- ggplot(prototype_data,aes(x=ground.clearance,y=mpg))
# Plot scatter and linear model
plt + geom_point() + geom_line(aes(y=gc_yval), color = "red")

#__________________________________________________________________________________

# Vehicle Length
vl_model <- lm(mpg ~ vehicle.length, prototype_data)
vl_yval <- vl_model$coefficients['vehicle.length']*prototype_data$vehicle.length + vl_model$coefficients['(Intercept']

# Import datatset into ggplot2
plt <- ggplot(prototype_data,aes(x=vehicle.length,y=mpg))
# Plot scatter and linear model
plt + geom_point() + geom_line(aes(y=vl_yval), color = "blue")

#__________________________________________________________________________________
#__________________________________________________________________________________



# Suspension Coil

# Read Suspension_Coil.csv
sc_data <-read.csv('Suspension_Coil.csv')

#Summary Statistics
sc_summary <- sc_data %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))
sc_summary

#Summary Statistics to the Lot Production
sc_lot <- sc_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))
sc_lot

# T-test
t.test(sc_data$PSI, mu=1500)
