library(tidyverse)
library(data.table)
library(cowplot)
####### ####### 
### READ IN  DATA
####### ####### 
df <- fread(file="..../Landsat_analysis_data.csv")


## group by region and year
dat<- df %>% group_by(region, year) %>% 
              summarize(S2_total = sum(S2max),
                        Pekel_total = sum(Landsat_Pekel),
                        Pickens_total = sum(Landsat_Pickens))%>% 
              pivot_longer(cols=ends_with("_total"),
                        names_to = "product", 
                        values_to = "area")%>%
                        mutate(product = recode(product, 
                                                "S2_total" = 'Sentinel-2',
                                                "Pekel_total" = "GSWO",
                                                "Pickens_total" = 'GLAD')) %>%
                        mutate( product = fct_relevel(product,c("GSWO", "GLAD"))) 
### rename and reorder regions
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

data <- rename_reorder_regions(dat)
####### ####### 
### PLOTS
####### ####### 


plot<-ggplot(data, aes(y=area, x = year, color = product )) +
  geom_point(size=3.5) +geom_line(linetype='dashed', linewidth=0.5,show.legend = FALSE) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                          axis.text.x = element_text(angle = 45, hjust = 1),
                                   #legend.position='top',
                          legend.title = element_blank())+
  scale_color_manual(values = c("#feb204","#313178", '#d53600'))+
  guides(color = guide_legend(override.aes = list(size = 5))) +
  facet_wrap(~region, nrow=2) +
  ylab( expression("Regional lake area" ~ (km^2)))
  
  
  

ggsave(plot, filename ='Landsat_comparison_Fig3.jpg',
       width=150, height=100,units="mm", dpi=500, scale=2,
       path=".../Figures")


