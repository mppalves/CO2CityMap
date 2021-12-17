library(stringr)
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)

dir <- "C:/Users/pedrosa/github/tomtom"

####################################
#### Preprocessing #################
####################################

data_base <- preprocessing(dir, varia_integr = c("Length"))
# data_base <- preprocessing(dir, varia_integr = c("Length","Segment.Id", "NewSegId", "SpeedLimit", "StreetName", "FRC"))

####################################
#### model configuration ###########
####################################

coeffs <- list(

  # emission factor AVgSpeed < 50
    coeff1 = 0.0928
  , coeff2 = 2
  , coeff3 = 9.2601
  , coeff4 = 358.7

  # emission factor AVgSpeed >= 80
  , coeff5 = 0.0165
  , coeff6 = 2
  , coeff7 = 2.3481
  , coeff8 = 211.68

  # emission factor AVgSpeed >= else
  , coeff9 = 130

  # Emission coefficient
  , coeff10 = 0.1/100/0.1585

  # vehic flux factor
  , flux_veihc = 3.1293
  #, flux_veihc = 8
)

####################################
#### Emissions calculations ########
####################################

x <- emissionsCalc(data_base,coeffs)



####################################
#### plotting emissions ########
####################################

xggplot <- group_by(x,end, order) %>%
  summarise(emissions_mean = mean(emissions),
            emissions_sd = sd(emissions),
            emissions = sum(emissions),
            emissions_low = emissions_mean - 1.96 * emissions_sd,
            emissions_high = emissions_mean + 1.96 * emissions_sd) %>%
  mutate(time = as.POSIXct(paste0(end," ",gsub('^([0-9]+)([0-9]{2})$', '\\1:\\2', order)),format="%Y-%m-%d %H:%M"))

ggplot() +
  geom_area(aes(x=time, y=emissions_low),xggplot, fill = "#ffa600", alpha = 0.3) +
  geom_area(aes(x=time, y=emissions_high),xggplot, fill = "#ffa600", alpha = 0.3) +
  geom_line(aes(x=time, y=emissions_mean),xggplot, fill = "#ffa600") +
  scale_y_continuous(name = bquote("Emissions unit" ~CO^2)) +
  scale_x_datetime(name = "Time") +
  facet_wrap(~end)

ggplot() +
  geom_line(aes(x=time, y=emissions),xggplot, color = "#ffa600", size =1) +
  scale_y_continuous(name = bquote("Emissions unit" ~CO^2)) +
  scale_x_datetime(name = "Time") +
  facet_wrap(~end)
