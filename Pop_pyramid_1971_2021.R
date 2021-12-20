# Based on the time series spreadsheet
#-------------------------
library(tidyverse)
library(openxlsx)
library(gganimate)
library(gifski)
library(png)
library(digest)
library(ggthemes)
library(magick)

setwd("C:/Users/atyeo/OneDrive/R data/Population pyramids")

sexcol <- c("#336699", "#C0504D")
col <-"#F8F8F8"

# Source dataset - the ERP data from the API is from 1981 only - so need to use messy time-series spreadsheets

#  Table 59 from 3101.0
xlURL <- "https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/jun-2021/3101059.xlsx"

download.file(xlURL,"3101059.xlsx", mode = "wb" )

sheet2 <- read.xlsx("3101059.xlsx", sheet = 2)
sheet3 <- read.xlsx("3101059.xlsx", sheet = 3)
sheet3 <- sheet3 %>% 
  select(-X1)

data <- sheet2 %>%
  bind_cols(sheet3) %>%
  filter(row_number()>9) %>%
  rename(Year = X1)

#Turn Year into Year
data$Year = seq(1971,2021,1)

# Pivot long
dataL <- data %>%
  pivot_longer(2:304, names_to = "age_sex", values_to = "ERP")


M <-  dataL %>%
  mutate(Sex = str_extract(age_sex, "Male"))

F <-  dataL %>%
  mutate(Sex = str_extract(age_sex, "Female"))

ERP <- M %>%
  bind_rows(F) %>%
  drop_na() %>%
  mutate(Age = rep(seq(0,100,1),102)) %>%
  select(-age_sex)

erp <- ERP %>%
  mutate(ERP = as.numeric(ERP)) %>%
  mutate(ERP = round(ERP/1000,1)) %>%
  mutate(ERP = case_when(Sex == "Male" ~ -1*ERP, TRUE ~ ERP)) %>%  
  mutate(Sex = factor(Sex, levels = c("Male", "Female"), labels = c("Males", "Females"))) %>%
  select(Age, Sex, Year, ERP) %>%
  arrange(Year, Sex, Age)

#==================================
#---Animation---
#==================================

img <- image_graph(1000, 820, res = 99)
pyramid_plot<- function(x){
  p <- erp %>%
    filter(Year == x) %>%
    ggplot(aes(x = Age, y = ERP, fill = Sex)) +  
    geom_col() +  
    scale_y_continuous(breaks = seq(-200,200,50),
                       labels = c(200, 150, 100, 50, 0, 50, 100, 150, 200),
                       limits = c(-200,200))+
    
    scale_x_continuous(breaks = seq(0,100,5))+
    coord_flip() +  
    labs(title=paste0("Australian population, 1971-2021"),
         subtitle = paste0("Year: ",x),
         y = "Population (000s)", x = "Age", caption = "Source: ABS Estimated Resident Population") +
    scale_fill_manual(values = sexcol)+
    theme_classic(base_size = 18)+
    theme(plot.background = element_rect(fill = col),
          panel.background = element_rect(fill = col,
                                          colour = col))
  p
}

map(c(1971, 1972, 1973, 1974, 1975,
      1976, 1977, 1978, 1979, 1980,
      1981, 1982, 1983, 1984, 1985,
      1986, 1987, 1988, 1989, 1990,
      1991, 1992, 1993, 1994, 1995,
      1996, 1997, 1998, 1999, 2000,
      2001, 2002, 2003, 2004, 2005,
      2006, 2007, 2008, 2009, 2010,
      2011, 2012, 2013, 2014, 2015,
      2016, 2017, 2018, 2019, 2020, 2021), pyramid_plot)
dev.off()

animation <- image_animate(img, fps = 2)
image_write(animation, "Pyramid 1971-2021.gif")


