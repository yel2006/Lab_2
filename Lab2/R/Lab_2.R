##load packages
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(knitr)
library(kableExtra)

#read in data
data("DRG_data")

##wrangling
Wrangled_DRG_Data <- DRG_data %>%
  mutate(DRG.Code = substr(DRG.Definition, 0, 3)) %>%  ##creates new column with just the numeric DRG code
  group_by(DRG.Code)  ##groups by DRG code

names(Wrangled_DRG_Data) <- gsub("\\.", " ", names(Wrangled_DRG_Data))
##Replaces periods in column names with a space

##boxplot function
#' Boxplot Function
#'This function produces a boxplot
#'
#' @param df a dataframe
#' @param Category a category / column of interest in df
#'
#' @return a boxplot of DRG code vs Category
#'
#'
#' @import readr
#' @import dplyr
#' @import tidyverse
#' @import ggplot2
#' @import stringr
#' @import lubridate
#' @import knitr
#' @import kableExtra
#'
boxplot_function <- function(df,Category) {
  plots <-  ggplot (df, aes(x = Wrangled_DRG_Data$'DRG Code', y = Category)) + ##initiate plot
    geom_boxplot() + ##produce boxplot
    ylab('Amount in Dollars') + ##rename y-axis
    xlab('DRG Code') +  ##rename x-axis
    ggtitle(paste0('Category')) +  ##attempt to personalize plot title
    theme(axis.text.x = element_text(angle = 90, hjust=1)) ##rotates x-values to make it easier to read
  return(plots) #returns plots
}

boxplot_function(Wrangled_DRG_Data, Wrangled_DRG_Data$'Average Medicare Payments')

boxplot_function(Wrangled_DRG_Data, Wrangled_DRG_Data$'Average Total Payments')

boxplot_function(Wrangled_DRG_Data, Wrangled_DRG_Data$'Average Covered Charges')


## Function
#' Calculate function
#' This function produces a table showing DRG Code vs statistical function
#'
#' @param df a dataframe
#' @param statistic an already built-in statistical function
#'
#' @return a table of DRG Code vs statistical function
#'
#'
#' @import readr
#' @import dplyr
#' @import tidyverse
#' @import ggplot2
#' @import stringr
#' @import lubridate
#' @import knitr
#' @import kableExtra
#'
Calculate <- function(df, statistic) {
  Avg_Medicare_Pmnt <- Wrangled_DRG_Data%>%
    select('DRG Code', 'Average Medicare Payments') %>% #selects columns of interest
    summarise_all(statistic) %>% ##summarize columns of interest by statistic
    knitr::kable (caption = 'Calculation by DRG Code') %>% ##table title
    kable_styling(bootstrap_options = "striped", font_size = 14) ##striped style makes each row more discernable
  return(Avg_Medicare_Pmnt) ##returns Avg_Medicare_Pmnt
}

Calculate(Avg_Medicare_Pmnt, mean)

Calculate(Avg_Medicare_Pmnt, median)

Calculate(Avg_Medicare_Pmnt, sd)






