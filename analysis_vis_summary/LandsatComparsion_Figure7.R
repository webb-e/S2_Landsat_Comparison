library(data.table)
library(tidyverse)
library(tidyr)
library(pracma)
library(ggdist)
library(broom)
library(cowplot)
library(emmeans)
####### ####### 
### READ IN DATA
####### ####### 
df <- fread(file=".../Landsat_analysis_data.csv")

####### ####### 
### SOME INTIAL DATA PROCESSING
####### ####### 
# helper to get relative difference as (A - B)/mean(A,B) * 100
#rel_diff <- function(a, b) {100 * (a - b) / b}
rel_diff <- function(a, b) {100 * (a - b) / ((a + b) / 2)}
data2 <- df %>% group_by(region, year) %>% 
          summarize(S2_total = sum(S2max),
                    Pekel_total = sum(Landsat_Pekel),
                    Pickens_total = sum(Landsat_Pickens)) %>%
          group_by(region) %>%
          mutate( Pekel_S2_abs = Pekel_total - S2_total,
                 Pickens_S2_abs = Pickens_total - S2_total,
                 GSWO_GLAD_abs = Pickens_total - Pekel_total,
                 rankyear_descending =  dense_rank(desc(S2_total)),
                  rankyear = dense_rank(S2_total),
                prop_max = S2_total/max(S2_total))  %>% 
            ungroup() 


#####
### CALCULATE DRYNESS SENSITIVITY
#####
### first, convert df to long
differnce_total<-   data2 %>%  pivot_longer(cols=ends_with("_abs"),
                                       names_to = "product", 
                                       values_to = "absolute_difference") %>%
                              mutate(product = recode(product, 
                                      "Pekel_S2_abs" = "GSWO-S2",
                                      "Pickens_S2_abs" = 'GLAD-S2',
                                      "GSWO_GLAD_abs" = 'GLAD-GSWO'),
                                    region = recode(region, "AKCP" = "ACP"),
                                    relative_difference = case_when(
                                              product == "GSWO-S2" ~ rel_diff(Pekel_total, S2_total),
                                              product == "GLAD-S2" ~ rel_diff(Pickens_total, S2_total),
                                              product == "GLAD-GSWO" ~ rel_diff(Pickens_total, Pekel_total)))

# Calculate dryness sensitivity for each product in each region
dryness_sensitivity <- differnce_total %>%
                            group_by(region, product) %>%
                            do(tidy(lm(relative_difference ~ prop_max, ,data = .))) %>%
                            ungroup() %>% filter(term=='prop_max')%>%
                            mutate( label = case_when(
                              p.value > 0.1 ~ "",
                              p.value > 0.05 ~ "*",
                              p.value > 0.01 ~ "**",
                              p.value > 0.001 ~ "***",
                              TRUE ~ NA_character_      ) ) %>%
                            rename(dryness_sensitivity=estimate) %>%
                            mutate(region = as.factor(region))

#####
### DETERMINE TRENDS
#####

### make dataframe with total lake area and product as columns
longtotal <- data2 %>% pivot_longer(cols=ends_with("_total"),
                                    names_to = "product", 
                                    values_to = "total_lake_area") %>%
                        mutate(product = recode(product,
                                                "S2_total" = "Sentinel-2",
                                                "Pekel_total" = "GSWO",
                                                "Pickens_total" = 'GLAD'))
### make dataframe with the 2016 regional km2 in a column
longtotal2<- data2 %>%  group_by(region) %>%
                  mutate(S2_meanwater = S2_total[year == 2016][1],
                         Pickens_meanwater = Pickens_total[year == 2016][1],
                         Pekel_meanwater = Pekel_total[year == 2016][1]) %>%
                  ungroup() %>%
                  pivot_longer(cols=ends_with("_meanwater"),
                               names_to = "product", 
                               values_to = "mean_water") %>%
                  mutate(product = recode(product,
                                          "S2_meanwater" = "Sentinel-2",
                                          "Pekel_meanwater" = "GSWO",
                                          "Pickens_meanwater" = 'GLAD')) %>%
                  dplyr::select(product, mean_water, region) %>% distinct()



# Calculate trends for each product in each region
trends <- longtotal %>%
              group_by(region, product) %>%
              do(tidy(lm(total_lake_area ~year, na.action=na.omit,data = .))) %>%
              ungroup() %>% filter(term== 'year')


### join with df with info about low water years and convert from km2/yr to %/yr

trend_data <- trends %>% left_join(longtotal2, by=c("region", "product")) %>% 
            distinct() %>% 
            mutate(percent_slope = estimate/mean_water*100,
                   percent_slope_error = std.error/mean_water*100) %>%
            mutate(region = recode(region, "AKCP" = "ACP"),
                   region = factor(region, levels=c('ACP', 'YKF', 'YKD','MRD', 'TUK', 'AND')),
                   product = factor(product, levels=c('GSWO', 'GLAD', "Sentinel-2")))


## calculate relative difference of trends (trend(Landsat)-trend(s2)/trend(s2))
rel_trends<- trend_data %>% dplyr::select(region, product, percent_slope) %>%
          pivot_wider(names_from = product, values_from = percent_slope) %>%
          mutate(`GLAD-GSWO` = rel_diff(GLAD, GSWO), 
                 `GSWO-S2`   = rel_diff(GSWO, `Sentinel-2`),
                 `GLAD-S2`   = rel_diff(GLAD, `Sentinel-2`)) %>%
          dplyr::select(region, `GLAD-GSWO`, `GSWO-S2`, `GLAD-S2` ) %>%
          pivot_longer( cols = -region,names_to = "product",  values_to = "relative_trend_difference")

# calculate absolute difference of trends
abs_trends<- trend_data %>% dplyr::select(region, product, percent_slope) %>%
                pivot_wider(names_from = product, values_from = percent_slope) %>%
                mutate(`GLAD-GSWO` = GLAD-GSWO, 
                       `GSWO-S2`   = GSWO-`Sentinel-2`,
                       `GLAD-S2`   = GLAD- `Sentinel-2`) %>%
                dplyr::select(region, `GLAD-GSWO`, `GSWO-S2`, `GLAD-S2` ) %>%
                pivot_longer( cols = -region,names_to = "product",  values_to = "absolute_trend_difference")

#####
### COMBINE DATAFRAMES FOR PLOTTING 
#####

### combine dataframes for plotting
trend_diff_df <- dryness_sensitivity %>% dplyr::select(region, product, dryness_sensitivity) %>%
                  left_join(rel_trends %>% dplyr::select(region, product, relative_trend_difference),
                            by=c('region', 'product'))  %>%
                    left_join(abs_trends%>% dplyr::select(region, product, absolute_trend_difference),
                              by=c('region', 'product'))

####### ####### 
### SOME BASIC REPORTING NUMBERS
####### ####### 

## average relative difference in trend
trend_diff_df %>% group_by(product)%>%
                    summarize(mean =mean(relative_trend_difference, na.rm=TRUE),
                              sd = sd(relative_trend_difference, na.rm=TRUE))

## average absolute difference in trend
trend_diff_df %>% group_by(product)%>%
  summarize(mean =mean(absolute_trend_difference, na.rm=TRUE),
            sd = sd(absolute_trend_difference, na.rm=TRUE))
### do different products give different trend estimates?
aov_model <- aov(percent_slope ~ product + Error(region/product), data = trend_data)
summary(aov_model)

model_foremm <- aov(percent_slope ~ product + region, data = trend_data)
# Pairwise comparisons of product means, to get p-val
emm <- emmeans(model_foremm, ~ product)
pairs(emm, adjust = "tukey")

####### ####### 
### DETERMINE HOW MUCH VARIANCE IS EXPLAINED BY DRYNESS SENSITIVITY
####### ####### 

# fit model
model <- lm(absolute_trend_difference~dryness_sensitivity, data=trend_diff_df)

## get R2
summary(model)$r.squared
summary(model)$coefficients[,4]


####### ####### 
### PLOTS
####### ####### 

### plot trends by region and product
left_plot<-ggplot(trend_data, 
                  aes(x=region, color=product, y=percent_slope)) + 
  geom_point(size=8, position=position_dodge(width=0.5)) +
  geom_errorbar(size = 1.5, aes(x=region, ymin=percent_slope-percent_slope_error, 
                                ymax=percent_slope+percent_slope_error),
                width=0.4, position=position_dodge(width=0.5))+
  scale_color_manual(values = c("#feb204","#313178", '#d53600'), name='Product')+
  theme_light(base_size=40) +
  ylab(expression ("Trend (%"~yr^-1~")")) + xlab (' ') +
  geom_abline(intercept = 0, size=0.75, color='grey', slope=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1, "cm"))

#### graph lake area trends vs. error sensitivity
right_plot<-ggplot(trend_diff_df, 
                   aes(x=dryness_sensitivity, y=absolute_trend_difference)) + 
  geom_point( aes(shape=region, color=product), size=8) +
  scale_color_manual(values = c("#E5A46E","#64323e", "#0B5C55"), name = 'Comparison')+
  labs(x= 'Dryness sensitivity', y =expression ("Difference in trend (%"~yr^-1~")"), shape = 'Region') + 
  theme_light(base_size=40) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key.size = unit(1, "cm"))+
  geom_smooth(se=F, method = "lm", color='grey', size=1, linetype='dashed')


combined_plot <- plot_grid(left_plot ,  right_plot , nrow=2, align="v")
combined_plot
####### ####### 
### SAVE
####### #######                 
ggsave(combined_plot, filename ='Landsat_comparison_Fig7.jpg', 
       width=200, height=200,units="mm", dpi=500, scale=2,
       path=".../Figures")


