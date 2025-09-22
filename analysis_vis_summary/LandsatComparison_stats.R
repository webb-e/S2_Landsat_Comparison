library(data.table)
library(tidyverse)
library(broom)
####### ####### 
### READ IN  DATA
####### ####### 
df <- fread(file=".../Landsat_analysis_data.csv")

## get differences between Landsat products
df <- df %>% mutate(GSWO_GLAD_abs = Landsat_Pekel-Landsat_Pickens,
                    GSWO_GLAD_per = (Landsat_Pekel-Landsat_Pickens)/Landsat_Pickens)
data2 <- df %>% group_by(region, year) %>% 
            summarize(S2_total = sum(S2max),
                      Pekel_total = sum(Landsat_Pekel),
                      Pickens_total = sum(Landsat_Pickens)) %>%
            group_by(region) %>%
            mutate( Pekel_S2_abs = Pekel_total - S2_total,
                    Pickens_S2_abs = Pickens_total - S2_total,
                    GSWO_GLAD_abs = Pekel_total - Pickens_total,
                    rankyear_descending =  dense_rank(desc(S2_total)),
                    rankyear = dense_rank(S2_total),
                    prop_max = S2_total/max(S2_total))  %>% 
            ungroup() 

## DRY/WET YEARS
wetdry<- data2 %>%
  mutate(highlow = recode(rankyear,
                          `1` = "dry",
                          `2` = "dry",
                          `3` = "dry",
                          `4` = "wet",
                          `5` = "wet",
                          `6` = "wet" ))

## cloudiness
cloudiness<-data %>% group_by(region) %>% 
  summarize(cloudinessregional = 100*mean(cloudiness, na.rm= T)) %>%
  ungroup() %>%
  summarize(cloudiness_domain = mean(cloudinessregional, na.rm= T),
            stdev = sd(cloudinessregional))


####### ####### 
### ARE DRY YEARS DIFFERENT FROM WET YEARS?
####### ####### 
rel_diff <- function(a, b) {
  100 * (a - b) / ((a + b) / 2)
}
# : Compute relative % differences per region and year
region_diffs <- wetdry %>%
  mutate(Pekel_vs_S2 = rel_diff(Pekel_total, S2_total),
    Pickens_vs_S2 = rel_diff(Pickens_total, S2_total),
    Pickens_vs_Pekel = rel_diff(Pickens_total, Pekel_total)) %>%
  select(region, highlow, Pekel_vs_S2, Pickens_vs_S2, Pickens_vs_Pekel)

# Reshape to long format
long_diffs <- region_diffs %>% pivot_longer( cols = starts_with("Pekel") | starts_with("Pickens"),
    names_to = "comparison",
    values_to = "percent_diff" )

# Compute mean ± SD across regions within each comparison and highlow group
summary_across_regions <- long_diffs %>%
  group_by(highlow, comparison) %>%
  summarise( mean_across_regions = mean(percent_diff, na.rm = TRUE),
              sd_across_regions = sd(percent_diff, na.rm = TRUE),
              .groups = "drop")

# Print results
print(summary_across_regions)


### pekel model
pekmodel <- lm(Pekel_error_abs~ region + highlow, data=wetdry)
anova(pekmodel)
### verify assumptions 
plot(pekmodel)

### pickens model
pickmodel <- lm(Pickens_error_abs~S2_total + region, data=data2)
summary(pickmodel)
### verify assumptions 
plot(pickmodel)




####### ####### 
### ABSOLUTE DIFFERENCES BETWEEN LANDSAT AND S2
####### ####### 

df %>% summarise(pek_mean = mean(Pekel_error_abs),
                 pek_sd = sd(Pekel_error_abs),
                 pek_median = median(Pekel_error_abs),
                 pick_mean = mean(Pickens_error_abs),
                 pick_sd = sd(Pickens_error_abs),
                 pick_median = median(Pickens_error_abs))
####### ####### 
### PERCENT DIFFERENCES BETWEEN LANDSAT AND S2
####### ####### 

df %>% group_by(year) %>% 
  summarize(sumpek = sum(Landsat_Pekel),
            sumpick = sum(Landsat_Pickens),
            sumS2 = sum(S2max),
            GSWO_S2 = rel_diff(sumpek, sumS2),
            GLAD_S2 = rel_diff(sumpick, sumS2),
            GLAD_GSWO = rel_diff(sumpick, sumpek)) %>%
  summarize(GSWO_S2_mean = mean(GSWO_S2),
            GSWO_S2_sd = sd(GSWO_S2),
            GLAD_S2_mean = mean(GLAD_S2),
            GLAD_S2_sd = sd(GLAD_S2),
            GLAD_GWSO_mean = mean(GLAD_GSWO),
            GLAD_GSWO_sd = sd(GLAD_GSWO))
####### ####### 
### COMPARE PEKEL TO PICKENS
####### ####### 

df %>% group_by(year) %>% 
  summarize(sumpek = sum(Landsat_Pekel),
            sumpick = sum(Landsat_Pickens),
            oversest = (sumpick-sumpek)/sumpek) %>%
  summarize(over_est = 100*mean(oversest),
             over_sd = 100*sd(oversest))
