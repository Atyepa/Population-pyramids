library(tidyverse)
library(readxl)   
library(plotly)   
library(yaml)     
library(progress) 
library(labeling) 
library(fansi)    
library(utf8)     
library(openxlsx) 
library(caTools)  
library(gganimate)
library(gifski)   
library(png)      
library(digest)   
library(crosstalk)
library(ggthemes) 
library(magick)   
library(rematch)  


sexcol <- c("#336699", "#C0504D")

#col <- "#E5FFF8"
col <-"#F8F8F8"

# Source dataset:
path <- "//Corp/Peopledfs/atyepa/An R folder/ATSI_ERP_1971_2016.xlsx"

#--- Read in data ---
daturl <- "https://github.com/Atyepa/YBFS/raw/main/Assembled_YBFS_28072021.xlsx"
download.file(daturl,"Assembled_YBFS_28072021.xlsx", mode = "wb" )

#--- Read in assembled data --
data1 <- read.xlsx("Assembled_YBFS_28072021.xlsx")

data <- read.xlsx(path)

#--- Finesse data ---
erp <- data %>% 
  mutate(Sex = factor(Sex, ordered = T, levels = c("Males", "Females"))) %>% 
  mutate(Age = factor(Age, levels = unique(Age), ordered = T)) %>% 
  mutate(ERP = round(ERP/1000,1)) %>% 
  mutate(ERP = case_when(Sex == "Males" ~ -1*ERP, TRUE ~ ERP)) %>% 
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
    scale_y_continuous(breaks = seq(-40,40,10), 
                       labels = c(40, 30, 20, 10, 0, 10, 20, 30, 40),
                       limits = c(-40,40))+
    coord_flip() +  
    labs(title=paste0("Australian Aboriginal and Torres Strait Islander population"),
         subtitle = paste0("Census year: ",x),
         y = "Population (000s)", x = "Age", caption = "Source: ABS Estimated Resident Population") +
          scale_fill_manual(values = sexcol)+
          theme_classic(base_size = 18)+
    theme(plot.background = element_rect(fill = col), 
          panel.background = element_rect(fill = col,
                                          colour = col))
  p
}

map(c(1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016), pyramid_plot)
dev.off()
animation <- image_animate(img, fps = 1)
image_write(animation, "Pyramid_1971_2016.gif")

## DARK 

img2 <- image_graph(1000, 820, res = 99)
pyramid_plot2<- function(x){
  p <- erp %>%
    filter(Year == x) %>%
    ggplot(aes(x = Age, y = ERP, fill = Sex)) +   
    geom_col() +   
    scale_y_continuous(breaks = seq(-40,40,10), 
                       labels = c(40, 30, 20, 10, 0, 10, 20, 30, 40),
                       limits = c(-40,40))+
    coord_flip() +  
    labs(title=paste0("Australian Aboriginal and Torres Strait Islander population"),
         subtitle = paste0("Census year: ",x),
         y = "Population (000s)", x = "Age", caption = "Source: ABS Estimated Resident Population") +
    scale_fill_manual(values = sexcol)+
    theme_classic(base_size = 18)+
    theme_hc(bgcolor = "darkunica") 
  p
}

map(c(1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016), pyramid_plot2)
dev.off()
animation2 <- image_animate(img2, fps = 1)
image_write(animation2, "Pop Pyramid 1971-2016.gif")


