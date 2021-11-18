#Title: Data Science: R Basics
#Date: 17-12-2021
#Author: Joseph Baafi

# Clear workspace
rm(list = ls())

# Load packages 
library(tidyverse)
library(dslabs)
data(murders)
library(ggplot2)

# Plot a categorical variable against a contonous variable. This was a lab8 exercise that 
# Michelle gave to me to figure out. 

# Dataframe
df <- data.frame(dimension=rep(c('1mm', '2mm'), 3), 
                  trees=c('Alder', 'Alder', 'Birch', 'Birch', 'CP', 'CP'), 
                  record=c(5.15, 6.78, 4.82, 5.85, 3.73, 5.18))

# Plot 
ggplot(data=df, aes(x=trees, y=record, group=dimension, color=dimension))+ 
  geom_line()+
  xlab("Tree species")+
  ylab("Decomposition Rate")+
  ylim(0, 8)+ 
  theme_light()


