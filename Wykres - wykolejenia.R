# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Usuni?cie wszystkich bibliotek, zmiennych oraz ustalenie seed, by stan by? odtwarzalny za ka?dym razem ####

#restartowanie sesji
#.rs.restartR()

#od??czeni wszytkich pakiet?w
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo?anie funkcji 

#usuni?cie zmiennych
gc(reset = TRUE)
rm(list = ls())

#ustalenie seed
set.seed(1)

# bedy po angileku
Sys.setenv(LANGUAGE='en')

#nie notacja naukowa
options("scipen"=100, "digits"=4)

# dodaj możliwość drukowania polsich liter w ggplot2
Sys.setlocale("LC_ALL", "Polish")
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Deklaracja bibliotek ####

# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("scales")
# install.packages("tidyrverse")
# install.packages("reshape2")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(reshape2)



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Globalne ####
color = c(
  "Wrocław" = "black",
  "Aglomeracja śląska" = "#993404",
  "Gdańsk" = "#d95f0e",
  "Kraków" = "#fe9929",
  "Łódź" = "#fed98e",
  "Warszawa" = "#ffffd4"
  )

X_LABELS <- c("2010", "'11", "'12", "'13", "'14", "'15", "'16", "'17", "'18", "'19", "'20", "'21", "2022")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### theme wykresu #### 

FILL_COL <- "#dedfe0"
TEXT_COL <- "#4e4d47"

Theme <-   theme(
  axis.line = element_blank(),
  axis.text.x = element_text(family = "Ubuntu", size = 8,  hjust = 0.5,  color = TEXT_COL),
  axis.text.y = element_text(family = "Ubuntu", size = 8,  hjust = 0.0,  color = TEXT_COL),
  axis.ticks = element_blank(),
  axis.title.x = element_text(family = "Ubuntu", size = 10, hjust = 0.5,  color = TEXT_COL),
  axis.title.y = element_text(family = "Ubuntu", size = 10, hjust = 0.5,  color = TEXT_COL),
  
  
  panel.border = element_blank(),
  # panel.grid.major=element_blank(),
  panel.grid.minor = element_blank(),
  
  #tlo
  plot.background  = element_rect(fill = FILL_COL,  color = NA), 
  panel.background = element_rect(fill = FILL_COL,  color = NA),
  text = element_text(family = "Ubuntu", size = 10, color = TEXT_COL),
  
  # legenda
  legend.position = "bottom",# "none",
  legend.key.width = unit(0.9, "cm"),
  legend.key.height = unit(0.3, "cm"),
  legend.title.align = 0.5,
  legend.title = element_text(family = "Ubuntu", size = 7, hjust = 0, color = TEXT_COL),#element_blank(),
  legend.background = element_rect(fill = FILL_COL, color = NA),
  legend.key = element_rect(fill = FILL_COL),
  legend.text       = element_text(family = "Ubuntu", size = 7, hjust = 0, color = TEXT_COL),
  legend.direction = "horizontal",
  
  # tytuy
  plot.title    = element_text(family = "Ubuntu", size = 10, hjust = 0.0,  color = TEXT_COL, face="bold"),
  plot.subtitle = element_text(family = "Ubuntu", size = 7,  hjust = 0.01, face = "italic", color = TEXT_COL),
  plot.caption  = element_text(family = "Ubuntu", size = 7,  hjust = 0.99, color = TEXT_COL),
)  

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### ZMIENNE ####
setwd()

Dane <- read.csv(paste0(getwd(), "/DANE", ".csv"), encoding = "UTF-8")
names(Dane)[1] <- "label"

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Obróbka ####
Dane_long = Dane %>%
  rename(
    "Aglomeracja śląska" = "Śląsk" 
  ) %>%
  melt(
    id.vars = c("label", "data", "rok_caly"),
    variable.name = "Metryka", 
    value.name = "Wartosc") %>%
  # changing order
  mutate(
    data = as.Date(data, format = "%d/%m/%Y"),
    Metryka = factor(
      Metryka, 
      levels = c("Wrocław", "Aglomeracja śląska", "Gdańsk", "Kraków", "Łódź", "Warszawa")
      ),
    alpha = ifelse(Metryka == "Wrocław", 1, 0.99)
  ) 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Wykresy ####

w <- 
  ggplot(Dane_long, aes(x = data, y = Wartosc, color = Metryka, alpha = alpha))  +
  geom_line(size = 1.1) +
  # costum color
  scale_colour_manual(name = "",
                      labels = 
                        c("Wrocław", "Aglomeracja śląska", "Gdańsk", "Kraków", "Łódź", "Warszawa"),
                      values = 
                        c(
                          color["Wrocław"],
                          color["Aglomeracja śląska"],
                          color["Gdańsk"],
                          color["Kraków"],
                          color["Łódź"],
                          color["Warszawa"]
                          ),
                      guide = guide_legend(override.aes = list(size=10))) + 
  # adding alpha
  scale_alpha_continuous(range = c(0.5, 1.0), guide=FALSE) +
  # x axis brakes
  scale_x_continuous(
    breaks = as.Date(c("01-01-2010", "01-01-2011", "01-01-2012", "01-01-2013", "01-01-2014", "01-01-2015", "01-01-2016", "01-01-2017", "01-01-2018", "01-01-2019",
                       "01-01-2020", "01-01-2021", "01-01-2022"), format = "%d-%m-%Y"), 
    labels = X_LABELS
  ) +
  # Opisy
  labs(title = "Dzięki TORYwolucji wrocławskie tramwaje wróciły na właściwe tory",
       subtitle = "w 2022 wykolejenia zdażały się podobnie często jak w innych miastach",
       y = "Liczba wykolejeń na 1mln wozokilometrów",
       x = "Rok",
       caption = "Autor: WroData | Na podstawie danych pozyskanych w trybie dostępu do informacji publicznej",
       color = "")   +
  Theme + 
  theme(legend.position="bottom") + 
  guides(colour = guide_legend(nrow = 1, override.aes = list(size=10))) 




plot(w)


png(filename = paste0("Wykres - wykolejenia - ", 
                      Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 
