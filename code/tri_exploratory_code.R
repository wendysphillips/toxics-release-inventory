library(tidyverse)
library(janitor)

tri <- read.csv('data/tri_2022_us.csv', sep = ",", check.names = FALSE)

# Check data types
str(tri)
str(tri[,99:119])

# Change some columns incorrectly in numeric
char_cols <- c(1, 3, 9,19, 28:33,38)
tri[,char_cols] <- as.character(tri[,char_cols])

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

# Create a dataframe with proportion of entries
#  with certain chemical types present
proportions_df <- data.frame(present = as.character(), n = as.numeric(), percent=as.numeric(), feature = as.character())
for (nm in colnames(tri_sub2[c(13,14,16,18:20)])){
  temp_df<-tri_sub2 %>% janitor::tabyl(nm)
  colnames(temp_df)[1] <- "present"
  temp_df$feature <- nm
  proportions_df <- rbind(proportions_df, temp_df)
}

# Factor features in order of percent present
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
