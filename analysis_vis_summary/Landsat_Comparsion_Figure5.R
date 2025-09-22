library(data.table)
library(tidyverse)
library(tidyr)
library(cowplot)
library(tibble)
library(broom)
library(ggpubr)
library(patchwork)
library(emmeans)
####### ####### 
### READ IN AND WRANGLE DATA
####### ####### 
df <- fread(file=".../Landsat_analysis_data.csv")

## calculate sum of lake area and (Landsat - S2) in each year
pptotal<- df %>% group_by(region, year) %>% 
                    summarize(S2_total = sum(S2max),
                              Pekel_total = sum(Landsat_Pekel),
                              Pickens_total = sum(Landsat_Pickens)) %>%
                    group_by(region) %>%
                    mutate(Pekel_S2_abs = Pekel_total - S2_total,
                           Pickens_S2_abs = Pickens_total - S2_total,
                           Pekel_Pickens_abs =  Pickens_total - Pekel_total,
                           rankyear = dense_rank(S2_total),
                           prop_max = S2_total/max(S2_total),) %>%
                    ungroup()


# helper to get relative difference 
#rel_diff <- function(a, b) {100 * (a - b) / b}
rel_diff <- function(a, b) {100 * (a - b) / ((a + b) / 2)}
## convert error to long
errortotal<-   pptotal %>%  pivot_longer(cols=ends_with("_abs"),
                                           names_to = "product", 
                                           values_to = "absolute_difference") %>%
                              mutate(product = recode(product, 
                                                      "Pekel_S2_abs" = "GSWO-S2",
                                                      "Pickens_S2_abs" = 'GLAD-S2',
                                                      "Pekel_Pickens_abs" = 'GLAD-GSWO'),
                                     region = recode(region, "AKCP" = "ACP"),
                                   #  product = fct_relevel(product,c("GSWO-S2", "GLAD-S2",'GLAD-GSWO')) ,
                                     relative_difference = case_when(
                                       product == "GSWO-S2" ~ rel_diff(Pekel_total, S2_total),
                                       product == "GLAD-S2" ~ rel_diff(Pickens_total, S2_total),
                                       product == "GLAD-GSWO" ~ rel_diff(Pickens_total, Pekel_total)))

####### ####### 
### PREPARE FOR PLOTTING
####### ####### 

### rename and reorder regions
rename_reorder_regions <- function(df) {
  region_names <- c(
    "ACP" = "Alaskan Coastal Plain",
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
### make dataframe with slopes and IDs
## get slopes
slope_fits <- errortotal %>%
                group_by(product, region) %>%
                do(tidy(lm(relative_difference ~ prop_max,data = .))) %>%
                ungroup() %>% filter(term=='prop_max')%>%
                mutate( significance = case_when(
                  p.value > 0.1 ~ "",
                  p.value > 0.05 ~ "*",
                  p.value > 0.01 ~ "**",
                  p.value > 0.00001 ~ "***",
                  TRUE ~ NA_character_      ),
                  estimate = round(estimate,2)) %>%
                dplyr::select(product, region, estimate, significance) %>%
                arrange(region)  %>%   
                rename('dryness sensitivity' =estimate,
                       comparison = product) %>%
                rename_reorder_regions()


data<-rename_reorder_regions(errortotal)

# Filter significant fits (p <= 0.1)
significant_regions <- slope_fits %>% 
  filter(!is.na(`dryness sensitivity`)) %>%
  filter(significance != "") %>%
  distinct(comparison, region)

data_sig <- data %>% 
  semi_join(significant_regions, by = c("product" = "comparison", "region"))
####### ####### 
### MAKE PLOT
####### ####### 
plotfinal<-ggplot(data, aes(y=relative_difference, x=prop_max, color=product)) +
  geom_point(size=3) +
  geom_smooth(data = data, method = "lm", size = 0.4, se = FALSE, show.legend = FALSE, linetype = "dashed") +
  geom_smooth(data = data_sig, method = "lm", size = 0.5, se = FALSE, show.legend = FALSE) +
  theme_bw(base_size = 20) +
  geom_abline(intercept = 0, size=0.5, color='grey', slope=0) +
  ylim(-45,70)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position='top',
        legend.title = element_blank())+
  scale_color_manual(values = c("#E5A46E","#64323e", "#0B5C55"), name = 'Comparison')+
  guides(color = guide_legend(override.aes = list(size = 5))) +
  facet_wrap(~region, nrow=2) +
  xlab(expression("Proportion of maximum S2"[regional]))+
  ylab("Relative difference between products (%)")

plotfinal
ggsave(plotfinal, filename ='Landsat_comparison_Fig5.jpg', 
       width=120, height=100,units="mm", dpi=500, scale=2,
       path=".../Figures")

####### ####### 
### IS DRYNESS SENSITIVITY DIFFERENT BETWEEN COMPARISONS?
####### ####### 

lm1 <- lm(`dryness sensitivity` ~ comparison + region, data = slope_fits)
summary(lm1)

# ANOVA to test for comparison effects
anova(lm1)
lm1 <- lm(`dryness sensitivity` ~ comparison + region, data = slope_fits)
em <- emmeans(lm1, ~ comparison)
pairs(em)
# Get pairwise comparisons
pairwise_comparisons <- pairs(em)
summary(pairwise_comparisons)
