library(tidyverse)
library(janitor)

tri <- read.csv('data/tri_2022_us.csv', sep = ",", check.names = FALSE)

# Check data types
col_types <-data.frame(sapply(tri, class))
col_types

# Change some columns incorrectly of type numeric
char_cols <- c(1, 3, 9,19, 28:33,38)
for (i in char_cols){
  tri[,i] <- as.character(tri[,i])
}

# Check data types again
str(tri)

# Subset to columns of interest
tri_sub <- tri[,c(2,6,7,8,9,11,12,13,18,19,20,34,35,39:47,62,85,91,94,101,103,104:117)]
str(tri_sub)

# Peek at data
tri_sub %>% select(where(is.character)) %>% 
  summary()

tri_sub %>% select(where(is.numeric)) %>% 
  glimpse()

# Clean up colnames a bit
tri_sub2 <- tri_sub |> janitor::clean_names()
colnames(tri_sub2)
colnames(tri_sub2) <- sub(".*?_", "", colnames(tri_sub2))
str(tri_sub2)

# Industry sectors ----
industry_sectors <- tri_sub2 |> group_by(industry_sector) |> count(sort = TRUE)
industry_sectors$industry_sector <- factor(industry_sectors$industry_sector, levels = industry_sectors$industry_sector)

ggplot(industry_sectors, aes(y = industry_sector, x = n)) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'white') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "Number of entries", y = "Industry sector", title = "All")

# Chemical types ----
# Select some chemicals of interest
chem_of_interest <- colnames(tri_sub2[c(13,14,16,18:20)])
chem_of_interest

# Investigate which of those chemical types co-occur
chem_combo_counts <- tri_sub2 |> group_by(metal, pbt,elemental_metal_included, clean_air_act_chemical, pfas, carcinogen ) |> count(sort = TRUE)
chem_combo_counts$proportion <- chem_combo_counts$n/sum(chem_combo_counts$n)

# Create a dataframe with proportion of entries
#  with certain chemical types present
proportions_df <- data.frame(
  present = as.character(), 
  n = as.numeric(), 
  percent=as.numeric(), 
  feature = as.character()
  )

for (nm in chem_of_interest){
  # get table of n and % samples each category
  temp_df <- tri_sub2 %>% janitor::tabyl(nm)

  colnames(temp_df)[1] <- "present"
  
  # Add chemical type to column
  temp_df$feature <- nm
  
  proportions_df <- rbind(proportions_df, temp_df)
}

# Factor features in order of percent present for plotting
proportions_df <- proportions_df |> dplyr::arrange(present, percent)
proportions_df$feature <- factor(proportions_df$feature, levels = unique(proportions_df$feature)) 

# Plot percents
ggplot(proportions_df, aes(y = feature, x = percent, fill = present)) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_brewer(type = 'qual', palette = 3)

# Plot number of entries
ggplot(proportions_df, aes(y = feature, x = n, fill = present)) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_brewer(type = 'qual', palette = 3)

# Grams v pounds -----
# Remember releases have different measures!
table(tri_sub2$unit_of_measure)

# Subset to entries in pounds
in_pounds <- tri_sub2[tri_sub2$unit_of_measure == "Pounds",]

# Dioxins ----
# Subset to just entries in grams
in_grams <- tri_sub2[tri_sub2$unit_of_measure == "Grams",]

# Are all in_grams entries for dioxin?
table(in_grams$classification)

# Dioxin by state
per_state_dioxin <- in_grams |> group_by(st) |> count(sort = T)

table(in_grams$industry_sector)

# By state -----
# Investigate entries per state
per_state <- tri_sub2 |> group_by(st) |> count(sort = T)

chem_combo_releases <- in_pounds |> 
  group_by(metal, pbt,elemental_metal_included, clean_air_act_chemical, pfas, carcinogen ) |> 
  summarize(tot_rel = sum(total_releases),
            on_site = sum(on_site_release_total),
            off_site = sum(off_site_release_total))

# Which states have highest total releases?
per_state_totals <- in_pounds |> 
  group_by(st) |> 
  summarize(tot_releases = sum(total_releases)) |>
  dplyr::arrange(desc(tot_releases))

# Factor for plotting
per_state_totals$st <- factor(per_state_totals$st, levels = per_state_totals$st)

ggplot(per_state_totals, aes(y = st, x = tot_releases)) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'white') +
  theme_classic() +
  scale_x_log10(expand = c(0,0)) +
  labs(x = "Total releases (pounds)", y = "State/Territory", title = "All")

# Which states have highest metal releases?
per_state_metal <- in_pounds |> 
  dplyr::filter(metal == 'YES') |> 
  group_by(st) |> 
  summarize(tot_releases = sum(total_releases)) |>
  dplyr::arrange(desc(tot_releases))

# Factor for plotting
per_state_metal$st <- factor(per_state_metal$st, levels = per_state_metal$st)

ggplot(per_state_metal, aes(y = st, x = tot_releases)) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'white') +
  theme_classic() +
  scale_x_log10(expand = c(0,0)) +
  labs(x = "Total releases (pounds)", y = "State/Territory", title = "Metal")
