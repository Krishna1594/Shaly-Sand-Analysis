# Setting up New library for this analysis 
myPaths <- .libPaths()
myPaths <- c(myPaths, 'D:/ALL OLEUM/Shaly Sand Analysis/R-research project/R_Library')
.libPaths(myPaths)  # add new path
myPaths <- c(myPaths[3], myPaths[2], myPaths[1])  # switch them to make our temporary path to access our packages

# Our required packages
library(readxl) 
library(crayon) 
library(stringr) 
library(stringi) 
library(openxlsx) 
library(janitor)
library(ggrepel)
library(tidyr)
library(lubridate)
library(leaflet)
library(dplyr) 
library(ggplot2) 
library(plotly) 
library(plotrix) 
library(viridisLite) 
library(viridis)
library(hrbrthemes)
library(scales)
library(formattable)
library(showtext)
library(cowplot)
library(readr)
# Turn on showtext
showtext_auto()

# Analysis
## We will begin our analysis from the bottom of the log; which is frontier formation. The depth range is from 8000ft till bottom for our interest but we will
## do it for whole log.
## From the log headers we have have some more data that needs to be pondered here; Temperature and mud rheology
Rho_mud = 9.45 #ppg
T_bottomhole = 192.0 #°F
Dia_hole = 8.75 #inches
Rho_matrix = 2.65 #g/cc

#SSwell1 <- SSwell1[,-(31:32)]
SSwell1$V_sh_corr_C[is.na(SSwell1$V_sh_corr_C)] <- 0
SSwell1$V_sh_corr_St[is.na(SSwell1$V_sh_corr_St)] <- 0

## Calculating Density Porosity for the whole well
SSwell1$RHOB <- as.numeric(as.character(SSwell1$RHOB))
SSwell1$DPHI_SS <- ifelse(SSwell1$RHOB != -999.25, (Rho_matrix - as.numeric(SSwell1$RHOB))/(Rho_matrix - (Rho_mud*0.12)), SSwell1$RHOB) #density of mud must be converted into g/cc

## When there is a clay content in the formation, the electron density and neutron porosity responses get influence. So, we have to correct for Shale volume
## Before we could correct the responses, we need to establish ????(N) and ????(D) in shale zone, and these are fixed constants. The values picked from logs are
## from depth 12132ft. These values are picked only once:
PHI_D_sh <- 0.05296834 #v/v
PHI_N_sh <- 0.274198 #v/v
SSwell1$DPHI_Corr <- ifelse(SSwell1$DPHI_SS != -999.25, SSwell1$DPHI_SS - ((SSwell1$V_sh_corr_St*0.01) * PHI_D_sh), SSwell1$DPHI_SS) #v/v
SSwell1$NPHI_Corr <- ifelse(SSwell1$CNSS != -999.25, (SSwell1$CNSS*0.01) - ((SSwell1$V_sh_corr_St*0.01) * PHI_N_sh), SSwell1$CNSS) #v/v

## Now, we have shale corrected density and neutron porosities, thus we can now calculate effective porosities based based on cross-over criteria:
## 1. Whether corrected Desnity porosity is greater than corrected Neutron porosity, then this signifies presence of gas
## 2. Whether corrected Desnity porosity is less than corrected Neutron porosity, then this signifies presence of liquid.

SSwell1$PHI_e <- ifelse(SSwell1$DPHI_Corr > SSwell1$NPHI_Corr,
                (((SSwell1$NPHI_Corr^2) + (SSwell1$DPHI_Corr^2))/2)^0.5, (SSwell1$NPHI_Corr + SSwell1$DPHI_Corr)/2)


## Now that we have the required parameters to calculate Sw, we have to decide what method can estimate good Sw.
## we have:
## 1. Archie's equation
## 2. Simandoux Equation
## 3. Indonesia Equation
## All the above methods have pros and cons. We will explore each equation. Before that, we need to export the new well data into .csv file and deduce trends based on zones of interest.
# Exporting the data in csv file
write.csv(SSwell1, "D:/ALL OLEUM/Shaly Sand Analysis/R-research project/US_FRONTIER/SSwell1_Corr.csv")



