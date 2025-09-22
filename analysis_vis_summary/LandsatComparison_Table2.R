library(data.table)
library(tidyverse)
####### ####### 
### READ IN AND POST-PROCESS DATA
####### ####### 
data <- fread(file=".../Landsat_analysis_data.csv")

####### ####### 
### Process data
####### ####### 
## add rank year column
df1 <- data %>%   group_by(region, year) %>%
                  summarise(S2_total = sum(S2max)) %>%
                  mutate(rankyear = dense_rank(S2_total),
                         rankyear = as.factor(rankyear))

df2 <- df1 %>% left_join(data, by = c("region", "year")) %>% 
              mutate(highlow = recode(rankyear,
                                        `1` = "dry",
                                        `2` = "dry",
                                        `3` = "dry",
                                        `4` = "wet",
                                        `5` = "wet",
                                        `6` = "wet" )) 
## sum lake area by year and region
df <- df2 %>% group_by(region,year ) %>% 
          summarize( Landsat_Pekel = sum(Landsat_Pekel),
                     Landsat_Pickens = sum(Landsat_Pickens),
                     S2max = sum (S2max),
                     highlow = first(highlow))
####### ####### 
### Create summary table
####### ####### 
wetdry_table <- df %>% group_by(region, highlow) %>%
                    summarise(Landsat_Pekel_mean = mean(Landsat_Pekel),
                              Landsat_Pekel_sd = sd(Landsat_Pekel),
                              Landsat_Pickens_mean =mean(Landsat_Pickens),
                              Landsat_Pickens_sd = sd(Landsat_Pickens),
                              S2max_mean = mean(S2max),
                              S2max_sd = sd(S2max))

allyrs_table <- df %>% group_by(region) %>%
                      summarise(Landsat_Pekel_mean = mean(Landsat_Pekel),
                                Landsat_Pekel_sd = sd(Landsat_Pekel),
                                Landsat_Pickens_mean =mean(Landsat_Pickens),
                                Landsat_Pickens_sd = sd(Landsat_Pickens),
                                S2max_mean = mean(S2max),
                                S2max_sd = sd(S2max)) %>%
                      mutate(highlow='all')


allregions_allyears <-  df2 %>% ungroup () %>% group_by(year) %>%
                                summarize( Landsat_Pekel = sum(Landsat_Pekel),
                                   Landsat_Pickens = sum(Landsat_Pickens),
                                   S2max = sum (S2max))%>%
                                ungroup () %>%
                            summarise(Landsat_Pekel_mean = mean(Landsat_Pekel),
                                      Landsat_Pekel_sd = sd(Landsat_Pekel),
                                      Landsat_Pickens_mean =mean(Landsat_Pickens),
                                      Landsat_Pickens_sd = sd(Landsat_Pickens),
                                      S2max_mean = mean(S2max),
                                      S2max_sd = sd(S2max))%>%
                                  mutate(highlow='all',
                                         region='entire study domain')

combined_table <- wetdry_table %>% bind_rows(allyrs_table) %>%
                 bind_rows(allregions_allyears) %>%
                mutate(highlow = factor(highlow, levels = c("dry", "wet", "all")))%>%
                pivot_wider(names_from = highlow,
                            values_from = -c(region, highlow),
                            names_sep = "_") %>% 
                  mutate(across(where(is.numeric), round))

# Function to merge mean and sd columns 
# Function to merge mean and sd columns with seasonal patterns
# Function to merge mean and sd columns with seasonal patterns
# Function to merge mean and sd columns with seasonal patterns
merge_mean_sd <- function(df) {
  # Get column names
  cols <- colnames(df)
  
  # Find all unique suffixes (dry, wet, all)
  suffixes <- unique(gsub(".*_(mean|sd)_", "", grep("_(mean|sd)_", cols, value = TRUE)))
  
  # Create result dataframe with region column if it exists
  result <- if("region" %in% cols) data.frame(region = df$region) else data.frame()
  
  # For each suffix (dry, wet, all)
  for(suffix in suffixes) {
    # Find mean columns for this suffix
    mean_cols <- grep(paste0("mean_", suffix, "$"), cols, value = TRUE)
    
    # Find corresponding sd columns
    sd_cols <- gsub("mean_", "sd_", mean_cols)
    
    # Check if all sd columns exist
    if(!all(sd_cols %in% cols)) {
      warning(paste("Some sd columns missing for suffix:", suffix))
      next
    }
    
    # For each pair of mean/sd columns
    for(i in seq_along(mean_cols)) {
      # Get base name (removing mean/sd and suffix)
      base_name <- gsub(paste0("_mean_", suffix), "", mean_cols[i])
      
      # Create merged column with NA handling
      result[[paste0(base_name, "_", suffix)]] <- ifelse(
        is.na(df[[mean_cols[i]]]) | is.na(df[[sd_cols[i]]]),
        "",  # Return empty string if either value is NA
        paste0(
          round(df[[mean_cols[i]]], 2),
          " ± ",
          round(df[[sd_cols[i]]], 2)
        )
      )
    }
  }
  
  if(ncol(result) <= 1) {
    stop("No matching mean/sd column pairs found")
  }
  
  return(result)
}
merged_df <- merge_mean_sd(combined_table)

# Reorder the columns
desired_order <- c( "region",
  "Landsat_Pekel_dry", "Landsat_Pekel_wet", "Landsat_Pekel_all", 
  "Landsat_Pickens_dry", "Landsat_Pickens_wet", "Landsat_Pickens_all", 
  "S2max_dry", "S2max_wet","S2max_all")

# Function to rename and reorder regions
rename_reorder_regions <- function(df) {
  # Create named vector for renaming
  region_names <- c(
    "AKCP" = "Arctic Coastal Plain",
    "YKF" = "Yukon Flats",
    "YKD" = "Yukon Kuskokwim Delta",
    "MRD" = "Mackenzie River Delta",
    "TUK" = "Tuktoyaktuk Peninsula",
    "AND" = "Anderson Plain",
    "entire study domain" = "Entire study domain"
  )
  
  # Rename regions
  df$region <- factor(df$region,
                      levels = names(region_names),
                      labels = region_names
  )
  
  # Reorder the dataframe by the new factor levels
  df <- df[order(df$region), ]
  
  return(df)
}

# Apply to your merged dataframe
merged_df2 <- rename_reorder_regions(merged_df)
summary_table <- merged_df2 %>%  select(all_of(desired_order))





####### ####### 
### Save 
####### ####### 
write.csv(summary_table, row.names = FALSE,fileEncoding = "latin1",
          ".../Landsat_comparison_table2.csv")


####### ####### 
### OVER/UNDERESTIMATES IN DRY/WET YEARS
####### ####### 
regionaloverunder<-combined_table %>% ungroup() %>%
  mutate(Pekel_percent_dry = (Landsat_Pekel_mean_dry-S2max_mean_dry)/S2max_mean_dry,
         Pekel_percent_wet = (Landsat_Pekel_mean_wet-S2max_mean_wet)/S2max_mean_wet,
         Pickens_percent_dry = (Landsat_Pickens_mean_dry-S2max_mean_dry)/S2max_mean_dry,
         Pickens_percent_wet = (Landsat_Pickens_mean_wet-S2max_mean_wet)/S2max_mean_wet)

overunder<-regionaloverunder %>%
        drop_na() %>%
        summarize(Pekel_dry_mean = mean(Pekel_percent_dry)*100,
                  Pekel_dry_sd = sd(Pekel_percent_dry)*100,
                  Pekel_wet_mean = mean(Pekel_percent_wet)*100,
                  Pekel_wet_sd = sd(Pekel_percent_wet)*100,
                  Pickens_dry_mean = mean(Pickens_percent_dry)*100,
                  Pickens_dry_sd = sd(Pickens_percent_dry)*100,
                  Pickens_wet_mean = mean(Pickens_percent_wet)*100,
                  Pickens_wet_sd = sd(Pickens_percent_wet)*100,)
percentdiff <-regionaloverunder %>% 
              drop_na() %>%
              rowwise() %>%
              mutate(diff_pek = (abs(Pekel_percent_dry-Pekel_percent_wet))/abs(Pekel_percent_wet),
                    diff_pick = (abs(Pickens_percent_dry-Pickens_percent_wet))/abs(Pickens_percent_wet)) %>%
              ungroup() %>%
              summarize(diffpek_mean = mean(diff_pek)*100,
                        diffpek_sd = sd(diff_pek)*100, 
                        diffpick_mean = mean(diff_pick)*100,
                        diffpick_sd = sd(diff_pick*100)*100)
percentdiff


regionaloverunder2<-combined_table %>% ungroup() %>%
  mutate(Pekel_pickens_dry = (Landsat_Pekel_mean_dry-Landsat_Pickens_mean_dry)/Landsat_Pickens_mean_dry,
         Pekel_pickens_wet = (Landsat_Pekel_mean_wet-Landsat_Pickens_mean_wet)/Landsat_Pickens_mean_wet)

precentdiff_2<-regionaloverunder2 %>%  
  drop_na() %>% 
  summarize(Pekel_pickens_dry_mean = mean(Pekel_pickens_dry)*100,
            Pekel_pickens_dry_sd = sd(Pekel_pickens_dry)*100,
            Pickens_pekel_wet_mean = mean(Pekel_pickens_wet)*100,
            Pekel_pickens_wet_sd = sd(Pekel_pickens_wet)*100) %>%ungroup()


