library(data.table)
library(tidyverse)
library(tidyr)
library(pracma)
library(ggdist)
library(broom)
library(patchwork)
library(cocor)

####### ####### 
### READ IN AND WRANGLE DATA
####### ####### 
df <- fread(file=".../Landsat_analysis_data.csv")
rel_diff <- function(a, b) {100 * (a - b) / mean(c(a, b))}
## get differences between Landsat products
df <- df %>% mutate(    GSWO_GLAD_abs = Landsat_Pickens - Landsat_Pekel,
                        GSWO_GLAD_per = mapply(rel_diff, Landsat_Pickens, Landsat_Pekel),
                        GSWO_S2_per   = mapply(rel_diff, Landsat_Pekel, S2max),
                        GLAD_S2_per   = mapply(rel_diff, Landsat_Pickens, S2max))
#### tidy up and add the rank year column
df_sums <- df %>%
            group_by(region, year) %>%
            summarise(S2_total = sum(S2max)) %>%
            mutate(rankyear = dense_rank(S2_total),
                   prop_max = S2_total/max(S2_total),
                   rankyear = as.factor(rankyear)) %>%
            ungroup() 

### make lake-wise data and add column for high/low years
lakewise_dat <-df %>% left_join(df_sums, by = c("region", "year")) %>% 
                    mutate(highlow = recode(rankyear,
                                            `1` = "dry",
                                            `2` = "dry",
                                            `3` = "dry",
                                            `4` = "wet",
                                            `5` = "wet",
                                             `6` = "wet" ))


#### tidy up and add the rank year column
### define quantiles 
n_quantiles <- 5
quintile_bounds <- quantile(lakewise_dat$S2max, probs = seq(0, 1, length.out = n_quantiles + 1))

#### convert to long 
lakewise_long_abs <- lakewise_dat %>%  pivot_longer(cols=ends_with("_abs"),
                                    names_to = "product", 
                                    values_to = "error") %>%
                  mutate(product = recode(product, 
                                          "Pekel_error_abs" = "GSWO-S2",
                                          "Pickens_error_abs" = 'GLAD-S2',
                                          "GSWO_GLAD_abs" = 'GLAD-GSWO'),
                         type = 'absolute')%>%
                      mutate(
                        quintile = ntile(S2max, n_quantiles),  # Assign to quantiles
                        sizegroup = case_when(
                          quintile == 1 ~ paste0("[", round(quintile_bounds[1], 3), ", ", round(quintile_bounds[2], 3), "]"),
                          quintile == 2 ~ paste0("[", round(quintile_bounds[2], 3), ", ", round(quintile_bounds[3], 3), "]"),
                          quintile == 3 ~ paste0("[", round(quintile_bounds[3], 3), ", ", round(quintile_bounds[4], 3), "]"),
                          quintile == 4 ~ paste0("[", round(quintile_bounds[4], 3), ", ", round(quintile_bounds[5], 3), "]"),
                          quintile == 5 ~ paste0("[", round(quintile_bounds[5], 3), ", ", round(quintile_bounds[6], 3), "]")))


lakewise_long_relative <- lakewise_dat %>%  pivot_longer(cols=ends_with("_per"),
                                                    names_to = "product", 
                                                    values_to = "error") %>%
                                          filter(!(product %in% c("Pekel_error_per", "Pickens_error_per"))) %>%
                                            mutate(product = recode(product, 
                                                                  "GSWO_S2_per" = "GSWO-S2",
                                                                  "GLAD_S2_per" = 'GLAD-S2', 
                                                                  "GSWO_GLAD_per" = 'GLAD-GSWO'),
                                                 type = 'relative') %>%
                                          drop_na(highlow) %>%
                                          mutate(
                                            quintile = ntile(S2max, n_quantiles),  # Assign to quantiles
                                            sizegroup = case_when(
                                              quintile == 1 ~ paste0("[", round(quintile_bounds[1], 3), ", ", round(quintile_bounds[2], 3), "]"),
                                              quintile == 2 ~ paste0("[", round(quintile_bounds[2], 3), ", ", round(quintile_bounds[3], 3), "]"),
                                              quintile == 3 ~ paste0("[", round(quintile_bounds[3], 3), ", ", round(quintile_bounds[4], 3), "]"),
                                              quintile == 4 ~ paste0("[", round(quintile_bounds[4], 3), ", ", round(quintile_bounds[5], 3), "]"),
                                              quintile == 5 ~ paste0("[", round(quintile_bounds[5], 3), ", ", round(quintile_bounds[6], 3), "]")))
                                          

####### ####### 
### FIGURES
####### ####### 
lakewise_filtered_rel <- lakewise_long_relative %>% group_by(product) %>%
                filter( error >= quantile(error, 0.05, na.rm = TRUE),
                        error <= quantile(error, 0.95, na.rm = TRUE)) %>% ungroup() %>%
                mutate(product = fct_relevel(product, c('GSWO-GLAD', 'GSWO-S2', 'GLAD-S2')))

lakewise_filtered_abs <- lakewise_long_abs %>% group_by(product) %>%
                  filter( error >= quantile(error, 0.05, na.rm = TRUE),
                          error <= quantile(error, 0.95, na.rm = TRUE)) %>% ungroup() %>%
                  mutate(product = fct_relevel(product, c('GSWO-GLAD', 'GSWO-S2', 'GLAD-S2')))
                


top_plot<-ggplot(lakewise_filtered_abs,
                    aes(x=quintile, y=error, fill=highlow)) +
                    geom_hline(yintercept =0, color='grey', size=0.5)  +
                      geom_boxplot(aes(group = interaction(sizegroup, highlow)),
                                   outlier.shape = NA,
                                   position = position_dodge(width = 0.8)) +
                      scale_fill_manual(values = c("dry" = "#F6E3BA", "wet" = "#01869E"))  +
                      facet_wrap(~product, scales='free_y') + 
                      guides(color=guide_legend(override.aes=list(fill=NA))) +
                      theme_bw(base_size = 25) +
                      theme(legend.title = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            strip.text = element_text(size=20),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            legend.text = element_text(size = 20),       # Increase legend text size
                            legend.key.size = unit(2, "cm"))  +
                      labs(y="Absolute difference" ~(km^2), x = " ")

top_plot
bottom_plot<-ggplot(lakewise_filtered_rel,
                          aes(x=quintile, y=error, fill=highlow)) +
                geom_hline(yintercept =0, color='grey', size=0.5)  +
                geom_boxplot(aes(group = interaction(sizegroup, highlow)),
                             outlier.shape = NA,
                             position = position_dodge(width = 0.8)) +
                scale_fill_manual(values = c("dry" = "#F6E3BA", "wet" = "#01869E"))  +
                 facet_wrap(~product, scales='free_y') + 
                 guides(color=guide_legend(override.aes=list(fill=NA))) +
                theme_bw(base_size = 25) +
                theme(strip.text = element_blank(),
                      legend.title = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.text = element_text(size = 20),       # Increase legend text size
                      legend.key.size = unit(2, "cm"))+        # Increase legend symbol size
                labs(y=expression('Relative difference (%)'), 
                     x=expression('Lake size'~(km^2))) 

# Combine labels and plots
final_plot <- (top_plot / bottom_plot)  +
              plot_layout(guides = "collect")


####### ####### 
### SAVE
####### #######                 
ggsave(final_plot, filename ='Landsat_comparison_Fig4.jpg', 
       width=200, height=130,units="mm", dpi=500, scale=2,
       path=".../Figures")



####### ####### 
### SPEARMAN'S CORRELATION
####### #######  

compare_spearman_correlation <- function(data, x_col, y_col, group_col) {
  # Function to compute Fisher Z-transformation
  fisher_z <- function(r) {
    return(0.5 * log((1 + r) / (1 - r)))
  }
  
  # Compute overall (ungrouped) Spearman's correlation and its p-value
  overall_test <- cor.test(data[[x_col]], data[[y_col]], method = "spearman")
  overall_corr <- overall_test$estimate  # Spearman's rho
  overall_p_value <-  formatC(overall_test$p.value, format = "e", digits = 50)  # p-value for significance of overall correlation
  
  # Compute Spearman's correlation for each group
  cor_results <- data %>%
    group_by(!!sym(group_col)) %>%
    summarize(
      spearman_corr = cor(!!sym(x_col), !!sym(y_col), method = "spearman")
    )
  
  # Extract correlation coefficients
  r1 <- cor_results$spearman_corr[1]
  r2 <- cor_results$spearman_corr[2]
  
  # Calculate sample sizes for each group
  n1 <- sum(data[[group_col]] == unique(data[[group_col]])[1])
  n2 <- sum(data[[group_col]] == unique(data[[group_col]])[2])
  
  # Fisher Z-transformation for both correlations
  z1 <- fisher_z(r1)
  z2 <- fisher_z(r2)
  
  # Compute Z-statistic
  z_diff <- (z1 - z2) / sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
  
  # Compute p-value (two-tailed test)
  p_value <- 2 * (1 - pnorm(abs(z_diff)))
  p_value_precise <- formatC(p_value, format = "e", digits = 50)
  
  # Return results as a list
  return(list(
    overall_correlation = list(correlation = overall_corr, p_value = overall_p_value),
    group1 = list(correlation = r1, n = n1),
    group2 = list(correlation = r2, n = n2),
    z_statistic = z_diff,
    p_value = p_value_precise
  ))
}

### ### ### ### 
### GSWO - S2
### ### ### ### 
### absolute difference
pekabs<-lakewise_long_abs %>% filter(product=='GSWO-S2')

compare_spearman_correlation(data = pekabs,
                             x_col = 'S2max',y_col = 'error',group_col ='highlow')

### relative difference
pekrel<-lakewise_long_relative %>% filter(product=='GSWO-S2')

compare_spearman_correlation(data = pekrel,
                             x_col = 'S2max',y_col = 'error',group_col ='highlow')


### ### ### ### 
### GLAD - S2
### ### ### ### 

### absolute difference
pickabs<-lakewise_long_abs %>% filter(product=='GLAD-S2')
compare_spearman_correlation(data = pickabs,
                             x_col = 'S2max',y_col = 'error',group_col ='highlow')
### relative difference
pickrel<-lakewise_long_relative %>% filter(product=='GLAD-S2')
compare_spearman_correlation(data = pickrel,
                             x_col = 'S2max',y_col = 'error',group_col ='highlow')
### ### ### ### 
### GSWO - GLAD
### ### ### ### 
### absolute difference
gswglad_abs<-lakewise_long_abs %>% filter(product=='GLAD-GSWO')
compare_spearman_correlation(data = gswglad_abs,
                             x_col = 'S2max',y_col = 'error',group_col ='highlow')
### relative difference
gswglad_rel<-lakewise_long_relative %>% filter(product=='GSWO-GLAD')
compare_spearman_correlation(data = gswglad_rel,
                             x_col = 'S2max',y_col = 'error',group_col ='highlow')
