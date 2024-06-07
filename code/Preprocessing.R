#Load packages
# Checks if pacman package is installed, otherwise installs it, then installs/loads several packages
if (!require("pacman")) install.packages("pacman")
#Load packages
pacman::p_load(tidyverse, janitor, readxl)


# Load Files
## Load Challenge

# Read the select Excel sheet that corresponds to behavioral sheet in R
#df <- read_excel("Training Data.xls", sheet = "Training Data") %>%
df <- read_excel("data/raw/Training Data.xls", sheet = "Training Data") %>%
  clean_names()

# Read the select Excel sheet that corresponds to stimuli sheet in R
df_lookup <- read_excel("data/raw/Training Data.xls", sheet = "Mixture definition") %>%
  # collapse CIDs into one column for later use in fuction
  unite("CIDs", "CID...3":ncol(.), sep = ";", remove = TRUE, na.rm = TRUE) %>%
  # remove columns that were collapsed with no CID
  mutate(CIDs = str_replace_all(CIDs, ";0", "")) %>%
  clean_names() %>%
  rename(CIDs = ci_ds)

#Calculate the difference in mixture size
diff_mix_size <- df %>%
  # Get CIDs of of mixture 1
  left_join(
    df_lookup,
    by = c("dataset", "mixture_1" = "mixture_label")
  ) %>%
  # Get CIDs of of mixture 2
  rename(a = CIDs) %>%
  left_join(
    df_lookup,
    by = c("dataset", "mixture_2" = "mixture_label")
  ) %>%
  rename(b = CIDs) %>%
  mutate(num_compound_a = str_count(a, ";")+1, num_compound_b = str_count(b, ";")+1, diff_mixture_size = abs(num_compound_a - num_compound_b))

#Calculate % overlap
overlap_percentage <- function(a, b) {
  numbers_a <- str_split(a, ";")[[1]]
  numbers_b <- str_split(b, ";")[[1]]
  
  # Find matches using regex
  matches <- sum(sapply(numbers_a, function(x) as.integer(str_detect(b, paste0("\\b", x, "\\b")))))
  
  # Combine both lists and find unique numbers to get the total set size
  total_unique_numbers <- length(unique(c(numbers_a, numbers_b)))
  
  # Calculate the percentage of overlap
  if (total_unique_numbers > 0) {
    (matches / total_unique_numbers) * 100
  } else {
    0 # Avoid division by zero in cases where there are no numbers at all
  }
}

diff_mix_size <- diff_mix_size %>%
  rowwise() %>%
  mutate(overlap_percent = overlap_percentage(a, b)) %>%
  ungroup()
  
#Calculate angle distance #####
# Read in features
dragon_ravia <- read.csv("data/raw/DREAM_Mix_Dragon.csv",header=TRUE) %>% # Dragon 6 descriptors
  rename(CIDs = NAME) %>% 
  select(CIDs, nCIR, ZM1, GNar, S1K, piPC08, MATS1v, MATS7v, GATS1v, 'Eig08_AEA.bo.', 'SM02_AEA.bo.', 'SM03_AEA.dm.', 'SM10_AEA.dm.', 'SM13_AEA.dm.', 'SpMin3_Bh.v.', RDF035v, G1m, G1v, G1e, G3s, 'R8u.', nRCOSR) # Select Ravia et al. 2020 Features


# ----------------------------------------DISTANCE FUNCTIONS-----------------------------------#
# Vectorized function that sums component Dragon then compares them via angle distance
#this is a measure of perceptual distance according to Ravia et al. 2020
angle_dist_ravia <- function(a,b){
  
  x <- dragon_ravia %>% 
    filter(CIDs %in% unlist(str_split(a, ';'))) %>% 
    select(-c(CIDs)) %>%
    summarise(across(everything(), ~ sum(., na.rm = TRUE)))
  
  
  if(nrow(x) == 0 | any(is.nan(unlist(x)))){
    return(888)
  }
  
  y <- dragon_ravia %>% 
    filter(CIDs %in% unlist(str_split(b, ';'))) %>% 
    select(-c(CIDs)) %>%
    summarise(across(everything(), ~ sum(., na.rm = TRUE)))
  
  
  
  if(nrow(y) == 0 | any(is.nan(unlist(y)))){
    return(999)
  }
  
  # calculate angle instead of euc distance
  ## Calculate the dot product of two vectors
  dot<-sum(x*y, na.rm=TRUE) 
  ## Calculate the norm of a vectors
  x.norm<-sqrt(sum(x^2, na.rm=TRUE))
  y.norm<-sqrt(sum(y^2, na.rm=TRUE))
  ## Calculate angle between two vectors
  acos(dot/(x.norm*y.norm))
  
}


# Takes list of mixtures (CIDs separated by semicolon, e.g. CID;CID), gets all binary mixtures then calculates both distances with their functions
df_share <- diff_mix_size %>%
  # Get CIDs of of mixture 1
  left_join(
    df_lookup,
    by = c("dataset", "mixture_1" = "mixture_label")
  ) %>%
  # Get CIDs of of mixture 2
  rename(a_CIDs = CIDs) %>%
  left_join(
    df_lookup,
    by = c("dataset", "mixture_2" = "mixture_label")
  ) %>%
  rename(b_CIDs = CIDs) %>%
  mutate(angle_dist = map2_dbl(a_CIDs, b_CIDs, angle_dist_ravia)) %>% 
  select(-c(a_CIDs,b_CIDs))
#write.csv("prediction.csv")




# Plots #####

df_share %>%
  ggplot(aes(y = diff_mixture_size, x = experimental_values)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(x="Percentage discrimination",y="Number of different compounds")

df_share %>%
  ggplot(aes(y = overlap_percent, x = experimental_values)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(x="Percentage discrimination",y="Percentage overlap")

df_share %>%
  ggplot(aes(y = angle_dist, x = experimental_values)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(x="Percentage discrimination",y="Angle Distance")

write.csv(file = "data/processed/MixturesWithFeatures.csv",df_share)
