#-----------------------------------------------------------------------
# Smets and Woulters model of the US Economy - recreating MATLAB example
#-----------------------------------------------------------------------

# In this example, we will recreate the linear version VECM version of the Smetts and Woulters model of the US Economy. 

#-----------------------------------------------------------------------
# librarys needed
#-----------------------------------------------------------------------

library(Quandl)                           # For data
library(tidyverse)                        # For data manipulation / visualsation
library(vars)                             # For modelling
library(urca)                             # For modelling
source("~/GitHub/TST themes/Chart themes.R")

#-----------------------------------------------------------------------
# Using QUANDL FRED data to extract data
#-----------------------------------------------------------------------

Quandl.api_key("x7vhPMRdCbhdzYrvZZHu")

Data.FF <- Quandl("FRED/FEDFUNDS", type = "ts", collapse ="quarterly" )
Data.GDP <- 100*log(Quandl("FRED/GDP",type = "ts"))        # GDP (output)
Data.GDPDEF <- 100*log(Quandl("FRED/GDPDEF",type = "ts"))  # GDP deflator
Data.COE <- 100*log(Quandl("FRED/COE",type = "ts"))        # COE
Data.HOANBS <-  100*log(Quandl("FRED/HOANBS",type = "ts")) # Hours of all persons
Data.PCEC <-  100*log(Quandl("FRED/PCEC",type ="ts"))      # Consumption
Data.GPDI <- 100*log(Quandl("FRED/GPDI", type = "ts"))     # Investment


# Make a chart
Data.SW <- tibble(GDP = Data.GDP,
                  `GDP Deflator` = Data.GDPDEF,
                  COE = Data.COE,
                  `Hours` = Data.HOANBS,
                  `Consumption` = Data.PCEC,
                  `Investment` = Data.GPDI,
                  Date = seq(as.Date("1947-03-01"),as.Date("2018-12-04"), by = "quarter")) %>% 
  filter(Date >= "1954-09-01" &  Date <= "2018-12-01" ) %>% 
  bind_cols(tibble(`Federal Funds` = Data.FF[-259]))


chart1 <- Data.SW %>% 
  dplyr::select(Date, GDP, `GDP Deflator`) %>% 
  gather(Var, Val, -Date) %>% 
  ggplot()+
  geom_line(aes(Date, Val, colour = Var), size =1)+
  tst_theme()+
  scale_colour_tst()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("100x natural log")
  

chart2 <- Data.SW %>% 
  dplyr::select(Date, `Consumption`, `Investment`) %>% 
  gather(Var, Val, -Date) %>% 
  ggplot()+
  geom_line(aes(Date, Val, colour = Var), size =1)+
  tst_theme()+
  scale_colour_tst()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("100x natural log")

chart3 <- Data.SW %>% 
  dplyr::select(Date, COE, `Hours`) %>% 
  gather(Var, Val, -Date) %>% 
  ggplot()+
  geom_line(aes(Date, Val, colour = Var), size =1)+
  tst_theme()+
  scale_colour_tst()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("100x natural log")

chart4 <- Data.SW %>% 
  dplyr::select(Date, `Federal Funds`) %>% 
  gather(Var, Val, -Date) %>% 
  ggplot()+
  geom_line(aes(Date, Val, colour = Var), size =1)+
  tst_theme()+
  scale_colour_tst()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("Percent")

gridExtra::grid.arrange(chart1,chart2,chart3,chart4)

#-----------------------------------------------------------------------
# Determine the cointegration rank 
#-----------------------------------------------------------------------

Data.mod <- Data.SW %>% 
  dplyr::select(-Date) 

ModH1 <- ca.jo(Data.mod, type = "trace", ecdet = "const" , K = 2)

summary(ModH1)

# H1 model suggest r =4
# Note: Add further analysis following matlab example

#-----------------------------------------------------------------------
# Restrict any coefficients to zero if p-val < 2 
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Estimate the VAR model from the VEC 
#-----------------------------------------------------------------------

ModH1 <- vec2var(ModH1, r = 4)

ModH2 <- VAR(Data.mod, p = 3, type = "const")

# Resrict any coefficients to zero if p-val is less than 0

ResModh2 <-  restrict(ModH2, method = "ser", thresh = 2.0)

# Extract cointegration relations and plot

#-----------------------------------------------------------------------
# IRFs 
#-----------------------------------------------------------------------

ResponsesGDP <- ModH1 %>% 
  irf(response = "GDP", n.ahead = 40)


responses <- list()
for(i in names(ResponsesGDP$irf)){
  
  responses[[paste(i)]] <- as.numeric(ResponsesGDP$irf[[i]])
  responses[[paste0(i,"_l")]] <- as.numeric(ResponsesGDP$Lower[[i]])
  responses[[paste0(i,"_u")]] <- as.numeric(ResponsesGDP$Upper[[i]])
  
}

responses <- bind_cols(responses)


#-----------------------------------------------------------------------
# Out of sample forecasting 
#-----------------------------------------------------------------------

ResModh2 %>% predict(h = 20) %>% plot()
