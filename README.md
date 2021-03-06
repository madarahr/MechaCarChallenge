# MechaCarChallenge

The analysis is to be performed for two types of data.

a) Vehicle specs that impact miles per gallon
b) Consistency of suspension coils among manufacturing lots based on weight capacity


There are two data sets for this challenge.  The first dataset which has 50 prototypes of cars that contain the following information
* Vehicle Length
* Vehicle Weight
* Spoiler Angle
* Ground Clearance
* AWD
* mpg

A statistical analysis has been made to determine the impact of the variables on the mileage.
A linear model was created to check which of the variables have the most impact.
It has been found that Spoiler Angle, Vehicle Weight and AWD have more impact due to p-value > 0.05.

The relationship with the above variables and mpg indicates that the value of
Multiple R-Squared is .7149 and  P-Value is 5.35e-11. This implies a strong positive linear 
relationship and also indicates a low significance level. There is sufficient statistical evidence that the null hypothesis is not true
and hence null hypothesis.
 
From the slope of the model, Ground Clearance does impact mpg however its p-value is significantly lower than 0.05. Hence, this model does not predict mpg of MechaCar prototypes effectively. 



The second dataset is of manufacturing information of vehicle suspension coils from three lots.  The statistical summary is below:


A t-test has been performed on the lots which indicate that the mean psi of the lots is 1500 psi however, LOT3 has a variance of over 100 psi on its suspension coils.
Which means that LOT3 does not have a good quality of manufacturing of suspension coils.  The p value= .5117 however indicates that there is insufficient evidence to reject the null
hypothesis indicating that the means are statistically similar. 

The factors considered in the analysis do not completely address the consumer sentiments when purchasing a vehicle.
The milage being important, there are other factors more importantly price needs to be used in the analysis.  Does improved milage indicate higher cost ? 

A one-way or two-way ANOVA test could be conducted to see if there is a difference between the sample
mean and the population mean of other vehicles in the industry for fuel efficiency and a t-test could be used to test cost. 


![](Suspension_Coil.png)
