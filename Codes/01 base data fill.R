# Pre post-invasion meta analysis

setwd("C:/Users/Propietario/Desktop/Pre_Post_invasion")

folder_name <- "Plots"
dir.create(folder_name)


setwd("C:/Users/Propietario/Desktop/Escritorio/full databse/DATA")
df <- read_xlsx("Global_dataset.xlsx", sheet = "Sheet1")


# ts with invaders

df1<- df %>% filter(Alien =="Y")

unique(df1$taxon)

str(df1)

# First record invader
first_records <- df1 %>%
  filter(Alien == "Y") %>%
  group_by(site_id, taxon) %>%
  summarize(first_year = min(year)) %>%
  ungroup()

# Identify instances where two or more invaders arrive in the same year within the same time series
multiple_invasives <- first_records %>%
  group_by(site_id, first_year) %>%
  tally() %>%
  filter(n > 1) 

earliest_multiple_invasives <- multiple_invasives %>%
  group_by(site_id) %>%
  slice_min(order_by = first_year, n = 1) %>%
  ungroup()

# Number of alien species per time series
df2<- df1 %>% group_by(site_id, taxon) %>% summarise(Richness =n_distinct(taxon))  %>%
  group_by(site_id) %>% summarise(Richness = n() )

head(df2)



df3<- df2 %>% filter(Richness ==1)


# Extract unique site_ids
unique_site_ids1 <- unique(earliest_multiple_invasives$site_id) # more than one invader
unique_site_ids1 <- unique_site_ids1 %>% as.data.frame()
unique_site_ids1$n_invaders <- ">1"

unique_site_ids2 <- unique(df3$site_id) # just one invader
unique_site_ids2 <- unique_site_ids2 %>% as.data.frame()
unique_site_ids2$n_invaders <- "1"

db_invaders <- rbind(unique_site_ids1,unique_site_ids2)
colnames(db_invaders)[1] <- "site_id"
db_invaders <- db_invaders[!duplicated(db_invaders$site_id), ]


############# Time series with only 1 invader ################

db_invaders1 <- db_invaders %>% filter(n_invaders==1)

d1<- df %>% filter(site_id %in% unique(db_invaders1$site_id))
d1<- df %>% filter(site_id %in% unique(df2$site_id))



############################            Inspect data           ####################################
invader_data <- d1 %>% filter(Alien == 'Y')

# Calculate the length of the time series for each site_id
time_series_length <- d1 %>%
  group_by(site_id) %>%
  summarise(min_year = min(year), max_year = max(year)) %>%
  mutate(length = max_year - min_year + 1)

first_record_invader <- invader_data %>%
  group_by(site_id) %>%
  summarise(first_record_year = min(year))

# Join the data
plot_data <- left_join(time_series_length, first_record_invader, by = "site_id")

ggplot(plot_data, aes(x = as.factor(site_id))) +
  geom_bar(aes(y = length), stat = "identity", fill = "blue", alpha = 0.6) +
  geom_point(aes(y = first_record_year - min_year), color = "red", size = 2) +
  labs(y = "Year", title = "Length of Time Series and First Record of Invader") +
  theme_minimal() + coord_flip()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Part 2:
invader_data <- d1 %>% filter(Alien == 'Y')

time_series_length <- d1 %>%
  group_by(site_id) %>%
  summarise(min_year = min(year), max_year = max(year)) %>%
  mutate(length = max_year - min_year + 1)

first_record_invader <- invader_data %>%
  group_by(site_id) %>%
  summarise(first_record_year = min(year))

# Join the data and filter out time series with invaders in the first year
plot_data <- left_join(time_series_length, first_record_invader, by = "site_id") %>%
  filter(min_year != first_record_year | is.na(first_record_year))

df_summary <- df %>%
  group_by(site_id) %>%
  summarize(country = first(country))

plot_data_combined <- left_join(plot_data, df_summary, by = "site_id")
plot_data_combined$country <- as.factor(plot_data_combined$country)

my_colors <- c("France" = "blue", "Spain" = "green", "Germany" = "red",
               "Sweden" = "blue", "Luxembourg" = "green", "Bulgaria" = "red",
               "Denmark" = "blue", "UK" = "green", "Ireland" = "red",
               "Norway" = "blue", "Hungary" = "green", "Belgium" = "red") 

my_colors <- brewer.pal(12, "Set3")


ggplot(plot_data_combined, aes(x = reorder(site_id, -length), y = length, fill = country)) +
  geom_bar(stat = "identity", alpha = 1.1) +
  geom_point(aes(y = first_record_year - min_year), color = "red", size = 2) +
  labs(y = "Year", x="") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by = 5)) + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size=12) #
  ) +
  scale_fill_manual(values = my_colors)  # 

unique(plot_data_combined$site_id) # 180 time series

#######################################################################################################

plot_data

plot_data_combined$min_yearss <- plot_data_combined$first_record_year- plot_data_combined$min_year

plot_data_combined<- plot_data_combined %>% filter(min_yearss >1)

unique(plot_data_combined$site_id) # 160 time series


## Full database with 1 invader
data<- df %>% filter(site_id %in% unique(plot_data$site_id))
data1 <- data1[,c(1:4)]

time_series <- unique(data$site_id)
combined_df <- data.frame()

for(i in 1:length(time_series)){
  
  subset_df <- data1 %>% filter(site_id == time_series[i]) 
    
    # Extract the unique species from the subset data frame
    species <- unique(subset_df$taxon)
    
    for(j in 1:length(species)){
      
      # Subset the data frame to include only the current species
      subset_species_df <- subset_df %>% filter(taxon == species[j])
      
      # check if the species has less than 2 values
      if(sum(!is.na(subset_species_df$abundance[subset_species_df$taxon == species[j]])) < 2) {
        # if true, skip the loop for this species
        next
      }
      
      subset_species_df <- subset_species_df %>%  group_by(site_id)  %>% 
        complete(year = seq(min(year), max(year), period = 1))   
      
      sampling_years <- unique(subset_df$year)
      
      subset_species_df2 <- subset_species_df %>% filter(year %in% sampling_years)
      
      subset_species_df2$taxon <- species[j]
      subset_species_df2[is.na(subset_species_df2)] = 0
      
      subset_species_df3 <- subset_species_df2 %>%  group_by(site_id)  %>% 
        complete(year = seq(min(year), max(year), period = 1)) 
      
      subset_species_df3$taxon <- species[j]
      
      subset_species_df3$abundance<- as.numeric(subset_species_df3$abundance)
      Paride <- complete(mice(subset_species_df3, m = 5, maxit =50, method = "pmm", seed = 123))
      
      # Bind the result to the combined dataframe
      combined_df <- rbind(combined_df, Paride)
      
      cat("\n The loop for-->",time_series[i])
    }
  }


write_xlsx(result1,"Time_series_all_invaders_filled.xlsx")


setwd("C:/Users/Propietario/Desktop/Pre_Post_invasion")

combined_df <- read_xlsx("Time_series_all_invaders_filled.xlsx")
result1 <- combined_df
join_data <- unique(df[, c("site_id","country")])
join_data1 <- unique(df[, c("taxon","country","Alien")])

result <- left_join(combined_df, join_data, by = "site_id")
result1 <- left_join(result, join_data1, by = c("taxon", "country"))

unique(result1$site_id) # 231 ts ---> 42ts are dropped becasue there are only 1 occ of invader

## all the full data is here:
plot_data_combined2 <- read_xlsx("Abudance_full_ts.xlsx")
result1<- plot_data_combined2







