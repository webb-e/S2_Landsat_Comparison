library(sf)
library(tidyverse)
library(stringr)
library(data.table)
library(scales)

#####
### Read in lake shapefiles (i.e., database)
#####

# Define the folder path, list files in folder
#shp_folder <- "..../Lakes_clipped/"
shp_folder <-'..../Lake_extraction_shps//'
#shp_folder <- "..../Lake_extraction_shps//"

shp_files <- list.files(shp_folder, pattern = "\\.shp$", full.names = TRUE)
first_shp <- st_read(shp_files[1], quiet = TRUE)
target_crs <- st_crs(first_shp)
# Function to read a shapefile and extract the region from the filename
read_shp_with_region <- function(file) {
                          file_name <- basename(file)
                         region <- str_extract(file_name, "^[^_]+")
                         # region <- str_extract(file_name, "(?<=_)[^.]+(?=\\.shp)")
                          shp_data <- st_read(file, quiet = TRUE) %>%
                          mutate(region = region)  # Add the region column
                          # If target_crs is not set, use the first file's CRS
                          if (is.null(target_crs)) {
                            target_crs <<- st_crs(shp_data)
                          }
                          # Transform CRS if different from the target
                          if (st_crs(shp_data) != target_crs) {
                            shp_data <- st_transform(shp_data, target_crs)
                          }
                          return(shp_data)
                          }

# Read all shapefiles and combine into one dataframe
shp_df <- bind_rows(lapply(shp_files, read_shp_with_region))

# Convert to a dataframe 
shp_df <- as.data.frame(shp_df) %>% 
              dplyr::select(area_km2, region, n_lakes) %>%
              mutate(type='database')

#####
### Read in csv with post-processed lakes (i.e., analysis)
#####

df <- fread(file='.../Lakewise_csvs/Landsat_analysis_data.csv')

#####
### combine data and get ready for plotting
#####
oneyrdf<-df  %>% filter(year==2018)%>% dplyr::select(n_lakes, lake_id)

csv_df<- df %>% group_by(lake_id, region) %>% 
              summarize(area_km2 = mean(S2max)) %>%
              right_join(oneyrdf, by= 'lake_id') %>%
              dplyr::select(region, area_km2, n_lakes) %>%
              mutate(type='analysis') %>% ungroup() %>% dplyr::select(-lake_id)

rename_reorder_regions <- function(df) {
                  region_names <- c(
                    "AKCP" = "Alaskan Coastal Plain",
                    "YKF" = "Yukon Flats",
                    "YKD" = "Yukon-Kuskokwim Delta",
                    "MRD" = "Mackenzie River Delta",
                    "TUK" = "Tuktoyaktuk Peninsula",
                    "AND" = "Anderson Plain" )
                  df$region <- factor(df$region,
                                      levels = names(region_names),
                                      labels = region_names)
                  df <- df[order(df$region), ]
                  return(df)
}

data1<-rbind(csv_df, shp_df) %>% 
            mutate(type=fct_relevel(type,c("database", "analysis")))

data<-rename_reorder_regions(data1)


#####
### Answer some questions about size distributions
#####
## how many lakes?
data %>% filter(type=='database') %>% summarize(lake_instances=sum(n_lakes),
                                                lake_objects = n())
data %>% filter(type=='analysis') %>% summarize(lake_instances =sum(n_lakes),
                                                lake_objects = n())
### how many lakes by region
data %>% filter(type=='database') %>% group_by(region) %>% 
                                    summarize(lake_instances=sum(n_lakes),
                                                lake_objects = n())

## percentage of lakes included in the analysis
precentage_regional<-data %>%   group_by(region, type) %>% 
                          summarise(nlakes = n()) %>%
                          pivot_wider(names_from = type, values_from = nlakes) %>%
                          mutate(percent_analysis = (analysis / database) * 100)
percentage_total <- precentage_regional %>% ungroup() %>% 
                        summarize(database = sum(database),
                                  analysis = sum(analysis)) %>%
                        mutate(percent_analysis = (analysis / database) * 100)

### percentage of lake objects below the Landsat detection limit
precentage_landsat<-data %>% filter(type=='database')  %>% 
                                summarise(percentage = mean(area_km2 >= 0.001) * 100)



### how much data do we miss by cutting off data >0,5 km2?
nrow(data[data$area_km2>0.5,])/nrow(data)*100 ##3.5

#####
### Make plot
#####

custom_labels <- function(x) {
  ifelse(x < 0.1, sprintf("%.3f", x),
         ifelse(x<1, sprintf("%.1f", x),
                sprintf("%.0f", x)))
}
customylabel <- function(x) {
  x <- ifelse(x == 0, 0, x)  
  format(x, nsmall = 2, trim = TRUE)  # Keep 2 decimal places but remove trailing .00
}


finalplot <-
ggplot(data , aes(x = area_km2, fill = type, color = type)) + 
  geom_density(aes(alpha = type, y =after_stat(scaled)), adjust = 1.5) +
  scale_fill_manual(values = c("database" = "#481126", "analysis" = "#eeb870")) +
  scale_color_manual(values = c("database" = "#481126", "analysis" = "#eeb870")) +
  scale_alpha_manual(values = c("database" = 1, "analysis" = 0.7)) +
  facet_wrap(~region, nrow = 2) +
  scale_x_log10(labels = custom_labels)+
  scale_y_continuous(labels = customylabel) +
  theme_bw(base_size = 25) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 25),
        axis.text.x=element_text(angle=45, hjust=1),
        legend.key.size = unit(1, "cm")) +
  labs(x = "Lake area" ~(km^2), y = "Relative Density")
#####
### Save plot
#####
ggsave(finalplot, filename ='Landsat_comparison_Fig2.jpg', 
       width=200, height=130,units="mm", dpi=500, scale=2,
       path='..../Figures')

