Author: Rachel Kulikoff 

Purpose: Add Shocks to Mortality Envelope and Lifetables

Background/Context/Outline: Using inputs of the number of deaths due to mortality shocks (like disasters and war) from Haiti, and the total number of all-cause deaths that don't include mortality shocks, I can produce a with-shock total deaths file as we as a with-shock lifetable. 

In order to do that, I also have a without-shock lifetable file from Haiti. This is created as part of the mortality process at IHME, where 5q0 and 45q15 are estimated through mixed effects regression models and smoothed through space and time using weights and Gaussian Process Regression. These estimated 5q0 and 45q15 inputs are used in a model life table process, where empirical life tables are compared to the estimated 5q0 and 45q15 to produce age specific mx; ax here is produced through a method of ax graduation. These lifetables then get reconciled with HIV death numbers, which are modeled through an ensemble process. That lifetable is the input to this code. 

Because we estimate lifetables up to a terminal age group of 110 plus and only model deaths up to a terminal age group of 95 +, I chose not to recompute mx from total (shock plus non shock) deaths / population; instead, I use a scalar that is total deaths/deaths without shock. The without-shock mx multiplied by this scalar gives with-shock mx for each age group; I apply the scalar for 95 + to the age groups 95-99. 100-104, 105-109, and 110 plus.

I do not change the ax values in the input file which were calculated through graduation. In the future, I would like to look into adding another ax graduation step using the with-shock mx values, as I believe that in years wtih a large mortality shock, like the 2010 Haitian earthquake, ax would be impacted.

I then apply a convenience funtion that I wrote in lifetable_helper_function.R to calulate the rest of the lifetable from mx and ax inputs. This function can be applied broadly to mx and ax values to generate an entire life table. 

Finally, I include some outputs, including the with-shock 5q0 and 45q15 values as well as the entire life table and some graphs of the results. 

 