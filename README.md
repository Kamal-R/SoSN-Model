This repository contains the files used in "Identifying waves of COVID-19 mortality using skew normal curves."

The R scripts assume that the folder that contains the files in this repository are the working directory. 

The main directory contains 5 files and 2 folders. The 5 files are:

- Data_File.RDS: the daily COVID-19 mortality data used for this paper. The data sources are detailed
in the paper, and they are all publicly available. 
- 3_Waves.R: processes data, runs two Stan models ("Wave_3.stan" and "Wave_3_No_DOW.stan"), and saves 
the results to "3_Waves_DOW.RDS" (for "Wave_3.stan") and "3_Waves_No_DOW.RDS" (for "Wave_3_No_DOW.stan").
- 8_Waves.R: processes data, runs three Stan models ("Wave_8.stan" and "Wave_8_Texas.stan"
and "Wave_8_Peru.stan"), and saves the results to "8_Waves.RDS."  
- 3_Waves_Plots.R: creates the plots in Section 5 of the paper and saves them to the Figures folder.
- 8_Waves_Plots.R: creates the plots in Section 4 of the paper and saves them to the Figures folder.

The Models folder contains the following Stan files:
- Wave_3.stan: the N = 3 model
- Wave_3_No_DOW.stan: the N = 3 model with no day-of-the-week effect.
- Wave_8.stan: the N = 8 model. 
- Wave_8_Texas.stan: the N = 8 model for Texas. 
- Wave_8_Peru.stan: the N = 8 model for Peru. 

The Figures folder is empty (besides a brief README file), and the scripts above save the figures they produce to this folder.
