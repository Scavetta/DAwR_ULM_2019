# SILAC Analysis (Stable Isotope Labelling in Active Cells)
# Rick Scavetta
# 12 Sept 2019
# Case study for workshop

# clear workspace
rm(list = ls())

# Load packages
library(tidyverse)

# Read in the data:
protein <- read.delim("Protein.txt")

# Examine the data:
summary(protein.df)
glimpse(protein.df)

# Convert to a tibble:
protein <- as_tibble(protein)

# Identify and remove contaminants
# Count
sum(protein$Contaminant == "")

# The proportion or percent:
sum(protein$Contaminant == "+") / nrow(protein) * 100

# Get only real values
protein %>% 
  filter(Contaminant != "+") -> protein

# Exercise 8.2 (Clean-up and Transform)
# log10 intensities
protein$Intensity.L <- log10(protein$Intensity.L)
protein$Intensity.M <- log10(protein$Intensity.M)
protein$Intensity.H <- log10(protein$Intensity.H)

# Add intensities
protein$Intensity.H.M <- protein$Intensity.H + protein$Intensity.M
protein$Intensity.M.L <- protein$Intensity.M + protein$Intensity.L

# log2 ratios
protein$Ratio.M.L <- log2(protein$Ratio.M.L)
protein$Ratio.H.M <- log2(protein$Ratio.H.M)

# check output
protein
head(protein$Ratio.M.L, 30)

# Examine data:
# Exercise 9.2 (Find protein values)
protein %>% 
  filter(Uniprot %in% paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE")) %>% 
  select(Uniprot, Ratio.H.M, Ratio.M.L)

# Exercise 9.3 (Find significant hits) - 104 hits
# For the H/M ratio
protein %>% 
  filter(Ratio.H.M.Sig < 0.05) %>% 
  select(Uniprot, Ratio.H.M)

# Exercise 10.2 (Find significant hits) using [] - 479 hits (because of NAs)
protein[protein$Ratio.H.M.Sig < 0.05, c("Uniprot", "Ratio.H.M")]

# For these next exercises see section 10.5 and 10.6 in the book:
# Exercise 10.4 (Find top 20 values)
# H/M
protein %>% 
  top_n(20, Ratio.H.M) %>% 
  select(Uniprot) -> topHM

# M/L
protein %>% 
  top_n(20, Ratio.M.L) %>% 
  select(Uniprot) -> topML

# Exercise 10.5 (Find intersections)
intersect(topHM, topML)

# Removing the _MOUSE label on Uniprot IDs
# classic: use sub(), or gsub()
gsub("_MOUSE", "", protein$Uniprot)

# tidyverse: use the stringr package str_remove(), or str_remove_all()
str_remove_all(protein$Uniprot, "_MOUSE")



