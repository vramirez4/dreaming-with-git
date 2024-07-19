# Project 1:
# The goal of this code is to generate a figure plotting percent overlap vs. mixture discriminability

# Checks if pacman package is installed, otherwise installs it, then installs/loads several packages
if (!require("pacman")) install.packages("pacman")
#Load packages
pacman::p_load(tidyverse)

df <- read.csv("MixturesWithFeatures.csv")

# You can load from Dropbox if you are having issues loading from the workspace:
# df <- read.csv("https://www.dropbox.com/scl/fi/f75j07xedtsv3774con0k/MixturesWithFeatures.csv?rlkey=lyfw541vdb6byhpj1341etvnu&dl=1", row.names=NULL)

#Starter plot of Discrimination vs. Number of different compounds
df %>%
  ggplot(aes(y = num_of_different_molecules, x = experimental_values)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(x="Percentage discrimination",y="Number of different compounds")

#Starter plot of Discrimination vs. Percentage overlap
df %>%
  ggplot(aes(y = overlap_percent, x = experimental_values)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(x="Percentage discrimination",y="Percentage overlap")

#Starter plot of Discrimination vs. Angle Distance
df %>%
  ggplot(aes(y = angle_dist, x = experimental_values)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(x="Percentage discrimination",y="Angle Distance")



