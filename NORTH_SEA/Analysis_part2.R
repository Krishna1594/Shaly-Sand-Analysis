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
library(htmlwidgets)
library(mapview)
library(raster)
library(png)
# Turn on showtext
showtext_auto()


# Knowing Well data
## Going through well information, drilling reports, mud reports and survey reports can reveal whole scenario of the operation that went on this well. After, gathering mentioned
## reports, the following data has been taken into account for our calulations:

# Data from Mud Report: Gel-Lignosulphate mud
Rho_m = 9.88 #ppg
Cl_ions = 21000.00 #ppm concentration - Mud Report
Ca_ions = 1080.00 #ppm concentration
SO4_ions = 22.00 # Sulphonate ions - Lignites
OW_ratio = 0 #percentage
T_surface = 75.0 #°F 
T_bottom = 248 #°F
Rho_matrix = 2.65 #g/cc

# Gas Condensate Zone: 3400m till TD
## Lets loda the data:
condensate_dat <- read_csv(file.choose())

summary(condensate_dat)

# Interpretation: SP log
## Establshing Shale Base line and clean sand line for SSP (mV)
SHBL = 85.0 #mV
CSBL = 15.0 #mV
SSP <- CSBL - SHBL #mV - A constant for this zone. No corrections is needed as the bed thinkness is more than 50ft (~17m) here.

## We already know;
R_mf

## Referring to SLB chart and plotting Rmf and projecting on corresponding Temperature

library(png)
dev.new()
Graph3 <- readPNG("D:/ALL OLEUM/Shaly Sand Analysis/R-research project/NORTH_SEA/Analysis/RmfeqRwe.png",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(Graph1,0,0,1,1)

## Now we know;
Rmfeq = 0.084 #ohm-m for R_mf = 0.0938 Ohm-m

## We can use the equation to estimate R(we) from R(mfeq) as:
Rweq = Rmfeq * 10^(SSP/(60 + (0.133 * T_bottom))) #ohm-m

## From the chart again or we can get using the following formula
#Rw <- (Rweq + (0.131 * (10^((1/log(T_bottom/19.9))-2))))/((-0.5 * Rweq) + (10^(0.0426/log(T_bottom/50.8))))

##From SP-01 Chart with T°F
Rw = 0.0225 #ohm-m

dev.off()

# Now we calculate Vsh
## From SP Log:

condensate_dat$Vsh_SP <- (condensate_dat$SP - SHBL)/(CSBL - SHBL) #v/v in decimal


## From GR log
### From GR log, we can establish clean shale formation and Sandstone formation as:
Clean_sh = 150.0 #°API
Clean_ss = 18.0 #°API

condensate_dat$Vsh_Index = (condensate_dat$GR - Clean_ss)/ (Clean_sh - Clean_ss)

### Now for Vsh based on clavier correction
condensate_dat$Vsh_corr_C = (1.7 - (3.38 - ((condensate_dat$Vsh_Index + 0.7)^2))^0.5)

### For, Vsh from Stieber equations
condensate_dat$Vsh_corr_St = (condensate_dat$Vsh_Index/(3 -(2 * condensate_dat$Vsh_Index)))

#condensate_dat$Vsh_SP[is.nan(condensate_dat$Vsh_SP) | condensate_dat$Vsh_SP < 0] <- 0
condensate_dat$Vsh_corr_C[is.nan(condensate_dat$Vsh_corr_C) | condensate_dat$Vsh_corr_C < 0] <- 0
condensate_dat$Vsh_corr_St[is.nan(condensate_dat$Vsh_corr_St) | condensate_dat$Vsh_corr_St < 0] <- 0

## Comparing each Vsh values and choosing the least Vsh:
### Set plot theme
theme_set(theme_minimal(base_family = "Sans"))
theme_update(
  plot.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
  panel.background = element_rect(fill = NA, color = NA),
  panel.border = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 13, margin = margin(r = 10)),
  legend.title = element_text(size = 9),
  plot.caption = element_text(
    family = "Sans",
    size = 10,
    color = "grey70",
    face = "bold",
    hjust = .5,
    margin = margin(5, 0, 20, 0)
  ),
  plot.margin = margin(10, 30, 10, 30)
)

#col2 <- viridisLite::inferno(12)
Vsh_Plot <- plot_ly(marker = list(size=8)) %>% 
  layout(title = '<b>Vsh_comaprison<br> <sup>', 
         plot_bgcolor = "#fafaf5",
         xaxis = list(title = 'Vsh (GR - Index)'), 
         yaxis = list(title = 'Vsh=f(GR Index)'))
Vsh_Plot <- Vsh_Plot %>% add_markers(x = condensate_dat$Vsh_Index, y = condensate_dat$Vsh_Index, name = 'Vsh Index',
                                     symbol = I(17),showlegend = TRUE, xaxis = list(range=c(0,1)), yaxis = list(range=c(0,1)))
Vsh_Plot <- Vsh_Plot %>% add_markers(x = condensate_dat$Vsh_Index, y = condensate_dat$Vsh_SP, name = 'Vsh SP log',
                                     symbol = I(18), showlegend = TRUE, xaxis = list(range=c(0,1)), yaxis = list(range=c(0,1)))
Vsh_Plot <- Vsh_Plot %>% add_markers(x = condensate_dat$Vsh_Index, y = condensate_dat$Vsh_corr_C,  name = 'Vsh Clavier Correc.',
                                     symbol = I(15), showlegend = TRUE, xaxis = list(range=c(0,1)), yaxis = list(range=c(0,1)))
Vsh_Plot <- Vsh_Plot %>% add_markers(x = condensate_dat$Vsh_Index, y = condensate_dat$Vsh_corr_St,  name = 'Vsh Stieber Correc.',
                                     symbol = I(15), showlegend = TRUE, xaxis = list(range=c(0,1)), yaxis = list(range=c(0,1)))
Vsh_Plot

## Thus, we will use Vsh from Stieber's equation. From core data, we have,
a = 1.0
m = 1.7
n = 1.9

## Now for Rsh, we have to plot R(t) versus log calculated V(sh) and then fit best line to reveal the Rsh when V(sh)=1. 
## Both methods gave an approximation of R(sh) for this well as:
Rsh_Plot <- plot_ly(marker = list(size=8)) %>% 
  layout(title = '<b>Estimating Rsh <br> <sup>', 
         plot_bgcolor = "#fafaf5", 
         xaxis = list(type = "log", title = 'R(t) ohm-m'),
         yaxis = list(type = "log", title = 'Vsh'))
Rsh_Plot <- Rsh_Plot %>% add_markers(x = condensate_dat$RDEP, y = condensate_dat$Vsh_corr_St, symbol = I(17),showlegend = TRUE)

Rsh_Plot

## Estimation of Rsh is very difficult in organic rich shales. Moreover, the resistivity of pure shales is influenced by mineral content and presence of formation water with its salinity.
## Usually, as the Vsh increases Resistivity decreases due to the mineral content but not in organic rich shaly-sand plays. Finally,
Rsh = 1.10 #ohm-m

## Calculating DPHI from bulk density and correcting DPHI and NPHI for Vsh:
condensate_dat$RHOB <- as.numeric(as.character(condensate_dat$RHOB))
condensate_dat$DPHI <- ifelse(condensate_dat$RHOB != -999.25, (Rho_matrix - as.numeric(condensate_dat$RHOB))/(Rho_matrix - (Rho_m*0.12)), condensate_dat$RHOB) #v/v

## Estabishing Density-Neutron porosities in clean shale (3401m to 3414.5m)
PHI_D_sh <- 0.279 #v/v an average of DPHI from pure shale zone 
PHI_N_sh <- 0.4063 #v/v an average of DPHI from pure shale zone 

## Clay content must be accounted for the DPHI and NPHI values
condensate_dat$DPHI_Corr <- ifelse(condensate_dat$DPHI != -999.25, condensate_dat$DPHI - ((condensate_dat$Vsh_corr_St) * PHI_D_sh), condensate_dat$DPHI) #v/v
condensate_dat$NPHI_Corr <- ifelse(condensate_dat$NPHI != -999.25, condensate_dat$NPHI - ((condensate_dat$Vsh_corr_St) * PHI_N_sh), Scondensate_dat$NPHI) #v/v

## Now, we have shale corrected density and neutron porosities, thus we can now calculate effective porosities based based on cross-over criteria:
## 1. Whether corrected Desnity porosity is greater than corrected Neutron porosity, then this signifies presence of gas
## 2. Whether corrected Desnity porosity is less than corrected Neutron porosity, then this signifies presence of liquid.

condensate_dat$PHI_e <- ifelse(condensate_dat$DPHI_Corr > condensate_dat$NPHI_Corr,
                        (((condensate_dat$NPHI_Corr^2) + (condensate_dat$DPHI_Corr^2))/2)^0.5, (condensate_dat$NPHI_Corr + condensate_dat$DPHI_Corr)/2)



## Simandoux Equation

condensate_dat$Sw_sim <- (a*Rw/(2*(condensate_dat$PHI_e)^m))*(((((condensate_dat$Vsh_corr_St/Rsh)^2)+((4*condensate_dat$PHI_e^m)/(a*condensate_dat$RDEP*Rw)))^0.5)-(condensate_dat$Vsh_corr_St/Rsh)) 

## Indonesia Equation

condensate_dat$Sw_Ind <- (((1/condensate_dat$RDEP)^0.5)/(((condensate_dat$Vsh_corr_St^(1-(0.5*condensate_dat$Vsh_corr_St)))/(Rsh^0.5))+((condensate_dat$PHI_e^m)/(a*Rw))^0.5))^(2/n)

# This dataset requires final processing as I saw some 'NAN' cells there. I will turn them in to zeros:
condensate_dat$Sw_sim[is.nan(condensate_dat$Sw_sim)] <- 0
condensate_dat$Sw_Ind[is.nan(condensate_dat$Sw_Ind)] <- 0

## Exporting Results finally for comparisons 
write.csv(condensate_dat, "D:/ALL OLEUM/Shaly Sand Analysis/R-research project/NORTH_SEA/Analysis/NSwell1Analysis.csv")
