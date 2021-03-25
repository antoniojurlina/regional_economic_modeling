#------------------------------------------------#
# Script for analyzing and plotting AS2 data,    #
# by calculating location quotients,             #
# multipliers and other relevant data            #
#------------------------------------------------#

#-------- packages --------
library(tidyverse)
library(tidylog)
library(ggthemes)
library(scales)
library(broom)
library(lmtest)
library(car)
library(GGally)
library(formattable)

#-------- data and directory --------
directory <- paste0(here::here(), "/data")
setwd(directory)

load("growth_data.RData")
load("multiplier_data.RData")

directory <- paste0(here::here(), "/output")
setwd(directory)

#-------- functions --------
linear_reg <- function(data, formula){
  lm(formula, data)
}

#-------- indexing --------
growth_data_indexed <- growth_data %>%
  group_by(region) %>%
  mutate(gdp        = 100 * gdp        / first(gdp),
         labor      = 100 * labor      / first(labor),
         capital    = 100 * capital    / first(capital),
         technology = 100 * technology / first(technology)) %>%
  ungroup()

#-------- testing --------
regions <- unique(growth_data_indexed$region)

breusch_pagan <- map(regions, function(x) {
  bind_rows(
    bptest(log(gdp) ~ log(labor) + log(capital) + log(technology), 
         data = growth_data_indexed %>%
           filter(region == x)) %>%
    tidy() %>%
    mutate(region = x,
           model = "general") %>% 
    dplyr::select(region, names(.)),
    
    bptest(log(y) ~ log(k) + log(technology), 
           data = growth_data_indexed %>%
             mutate(y = gdp/labor,
                    k = capital/labor) %>% 
             filter(region == x)) %>%
      tidy() %>%
      mutate(region = x,
             model = "per capita") %>% 
      dplyr::select(region, names(.))
  )
}) %>% bind_rows()

variance_inflation <- map(regions, function(x) {
  bind_rows(
    variance <- vif(
      growth_data_indexed %>%
        filter(region == x) %>%
        linear_reg(log(gdp) ~ log(labor) + log(capital) + log(technology))
    ) %>%
      tidy() %>%
      rename(vif = x) %>%
      mutate(region = x,
             model = "general") %>%
      dplyr::select(region, names(.)),
    
    variance <- vif(
      growth_data_indexed %>%
        mutate(y = gdp/labor,
               k = capital/labor) %>% 
        filter(region == x) %>%
        linear_reg(log(y) ~ log(technology) + log(k))
    ) %>%
      tidy() %>%
      rename(vif = x) %>%
      mutate(region = x,
             model = "per capita") %>%
      dplyr::select(region, names(.))
 )
}) %>% bind_rows()

corr_matrix_1 <- map(regions, function(x){
  bind_rows(cor(growth_data_indexed %>%
        filter(region == x) %>%
        dplyr::select(-region, -year)) %>%
    tidy() %>%
      mutate(region = x) %>%
      dplyr::select(region, names(.)),
  tibble(region = NA,
         .rownames = NA,
         labor = NA,
         capital = NA,
         technology = NA,
         gdp = NA))
}) %>% bind_rows()

corr_matrix_2 <- map(regions, function(x){
  bind_rows(cor(growth_data_indexed %>%
                  mutate(y = gdp/labor,
                         k = capital/labor) %>% 
                  filter(region == x) %>%
                  dplyr::select(y, k, technology)) %>%
              tidy() %>%
              mutate(region = x) %>%
              dplyr::select(region, names(.)),
            tibble(region = NA,
                   .rownames = NA,
                   y = NA,
                   k = NA,
                   technology = NA))
}) %>% bind_rows()

walk(regions, function(x) {
  plot <- ggcorr(growth_data_indexed %>%
                   filter(region == x) %>%
                   dplyr::select(-region, -year)) +
    labs(title = paste0(x, "(general)")) 
  
  ggsave(paste0(x, "-correlogram1.png"), plot, height = 5, width = 5)
})

walk(regions, function(x) {
  plot <- ggcorr(growth_data_indexed %>%
                   mutate(y = gdp/labor,
                          k = capital/labor) %>% 
                   filter(region == x) %>%
                   dplyr::select(y, k, technology)) +
    labs(title = paste0(x, "(per capita)"))
  
  ggsave(paste0(x, "-correlogram2.png"), plot, height = 5, width = 5)
})
  
walk(regions, function(x) {
  model <- growth_data_indexed %>%
    filter(region == x) %>%
    linear_reg(log(gdp) ~ log(labor) + log(capital) + log(technology))
  
  plot <- ggplot(model) +
    geom_point(aes(x=.fitted, y=.resid)) +
    theme_linedraw() +
    geom_hline(aes(yintercept = 0)) +
    labs(title = paste0(x, "(general)"))
  
  ggsave(paste0(x, "-residuals vs fitted 1.png"), plot, height = 5, width = 5)
})

walk(regions, function(x) {
  model <- growth_data_indexed %>%
    mutate(y = gdp/labor,
           k = capital/labor) %>% 
    filter(region == x) %>%
    linear_reg(log(y) ~ log(technology) + log(k))
  
  plot <- ggplot(model) +
    geom_point(aes(x=.fitted, y=.resid)) +
    theme_linedraw() +
    geom_hline(aes(yintercept = 0)) +
    labs(title = paste0(x, "(per capita)"))
  
  ggsave(paste0(x, "-residuals vs fitted 2.png"), plot, height = 5, width = 5)
})

#-------- growth models --------
estimates_1 <- map(regions, function(x) {
  growth_data_indexed %>%
    filter(region == x) %>%
    linear_reg(log(gdp) ~ log(labor) + log(capital) + technology) %>%
    tidy() %>%
    mutate(region = x) %>%
    dplyr::select(region, names(.))
}) %>% bind_rows()

coefficients_1 <- map(regions, function(x) {
  growth_data_indexed %>%
    filter(region == x) %>%
    linear_reg(log(gdp) ~ log(labor) + log(capital) + technology) %>%
    glance() %>%
    mutate(region = x) %>%
    dplyr::select(region, names(.))
}) %>% bind_rows()

estimates_2 <- map(regions, function(x) {
  growth_data_indexed %>%
    mutate(y = gdp/labor,
           k = capital/labor) %>% 
    filter(region == x) %>%
    linear_reg(log(y) ~ technology + log(k)) %>%
    tidy() %>%
    mutate(region = x) %>%
    dplyr::select(region, names(.))
}) %>% bind_rows()

coefficients_2 <- map(regions, function(x) {
  growth_data_indexed %>%
    mutate(y = gdp/labor,
           k = capital/labor) %>% 
    filter(region == x) %>%
    linear_reg(log(y) ~ technology + log(k)) %>%
    glance() %>%
    mutate(region = x) %>%
    dplyr::select(region, names(.))
}) %>% bind_rows()

#-------- multipliers --------
basic_industries <- c("Manufacturing", "Mining, quarrying, and oil and gas extraction", "Finance and insurance")
multiplier_data$ad_hoc <- "Non-Basic"
multiplier_data$ad_hoc[which(multiplier_data$description %in% basic_industries)] <- "Basic"

multiplier_data_analysis <- multiplier_data %>%
  filter(code != 510) %>%
  group_by(year,region) %>%
  mutate(employment_us_total = sum(employment_us)) %>% 
  ungroup() %>%
  group_by(year, region) %>%
  mutate(employment_state = sum(employment)) %>%
  ungroup() %>%
  group_by(year, region, description) %>%
  mutate(sri = employment    / employment_state,
         si  = employment_us / employment_us_total,
         lq  =  sri /si) %>%
  ungroup() %>%
  group_by(region, description) %>%
  mutate(qi_change      = 100 * (last(lq) - first(lq)) / first(lq),
         growth_rate    = 100 * (last(employment) - first(employment)) / first(employment),
         birch          = (last(employment) - first(employment)) * (last(employment) / first(employment) ),
         national_share = first(employment) * (last(employment_us_total) - first(employment_us_total)) / first(employment_us_total),
         industrial_mix = first(employment) * ((last(employment_us) - first(employment_us)) / first(employment_us) - 
                                                (last(employment_us_total) - first(employment_us_total)) / first(employment_us_total)),
         local_share    = first(employment) * ((last(employment) - first(employment)) / first(employment) - 
                                                (last(employment_us) - first(employment_us)) / first(employment_us))) %>%
  ungroup() %>%
  group_by(year, region, description) %>%
  mutate(shift = national_share + industrial_mix + local_share) %>%
  ungroup()

multipliers_prelim <- left_join(left_join(
  multiplier_data_analysis %>%
    filter(lq > 1) %>% 
    group_by(region, year, description) %>%
    summarise(export_employment = (1 - 1/lq) * employment) %>%
    ungroup() %>%
    group_by(region, year) %>%
    summarise(export_employment_1 = sum(export_employment)) %>% 
    ungroup(),
  multiplier_data_analysis %>%
    filter(ad_hoc == "Basic") %>% 
    group_by(region, year) %>% 
    summarise(export_employment_2 = sum(employment)) %>% 
    ungroup()),
  multiplier_data_analysis %>%
    group_by(region, year) %>%
    summarise(employment_state = mean(employment_state)) %>%
    ungroup(), 
  by = c("region", "year"))

multipliers <- multipliers_prelim %>%
  ungroup() %>%
  group_by(region) %>%
  summarize(multiplier1_sd = sd(employment_state / export_employment_1),
            multiplier1 = (last(employment_state)-first(employment_state)) / (last(export_employment_1) - first(export_employment_1)),
            multiplier2_sd = sd(employment_state / export_employment_2),
            multiplier2 = (last(employment_state)-first(employment_state)) / (last(export_employment_2) - first(export_employment_2)))

multipliers$multiplier3_sd <- map(regions, function(x) {
   multipliers_prelim %>% filter(region == x) %>%
    linear_reg(employment_state ~ export_employment_1) %>%
    tidy() %>% .[2,3]}) %>% unlist()

multipliers$multiplier3 <- map(regions, function(x) {  
  multipliers_prelim %>% filter(region == x) %>%
    linear_reg(employment_state ~ export_employment_1) %>%
    tidy() %>% .[2,2]})  %>% unlist()

multipliers$multiplier4_sd <- map(regions, function(x) {    
  multipliers_prelim %>% filter(region == x) %>%
    linear_reg(employment_state ~ export_employment_2) %>%
    tidy() %>% .[2,3]}) %>% unlist()

multipliers$multiplier4 <- map(regions, function(x) {   
  multipliers_prelim %>% filter(region == x) %>%
    linear_reg(employment_state ~ export_employment_2) %>%
    tidy() %>% .[2,2]}) %>% unlist()

colnames(multipliers) <- c("region", "sd_1", "multiplier_1", "sd_2", "mulitplier_2", "sd_3", "multiplier_3", "sd_4", "mulitplier_4")

multipliers <- multipliers %>%
  pivot_longer(cols = 2:9, names_to = "type", values_to = "value")


#-------- plots --------
multipliers_plot_data <- multipliers_prelim %>%
  pivot_longer(cols = 3:4, names_to = "method", values_to = "export_employment") %>%
  mutate(method = ifelse(method == "export_employment_1", "LQ", "Ad-Hoc"))

plot <- ggplot(multipliers_plot_data, aes(x = export_employment, y = employment_state)) +
  geom_point() +
  facet_wrap(method ~ region, scales = "free", nrow = 2, ncol = 4) +
  stat_smooth(method = "lm", se = F, color = "#BB5566") +
  theme_par() +
  xlab("Basic sector size") +
  ylab("Total region employment") +
  labs(title = "Regional Multiplier OLS (LQ and Ad-Hoc)")

ggsave("Regional Mulitplier OLS.png", plot, height = 9, width = 17)

complete_plot <- multiplier_data_analysis %>%
  filter(year == 1998) %>%
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change)) %>%
  ggplot(aes(x = code, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=code, xend=code, y=0, yend=qi_change), show.legend = FALSE) +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_wsj() +
  scale_size_continuous("1998 employment", labels = comma) +
  ylab("1998 - 2016 % Change LQ") +
  labs(title = "Industry Employment Location Quotient") +
  theme_linedraw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())

ggsave("complete plot LQ.png", complete_plot, height = 7, width = 7)

complete_plot <- multiplier_data_analysis %>%
  pivot_longer(16:18, names_to = "type", values_to = "share") %>% 
  filter(year == 1998) %>%
  mutate(type = dplyr::recode(type, 
                       "industrial_mix" = "industrial mix",
                       "local_share" = "local share",
                       "national_share" = "national share")) %>% 
  select(region, description, share, type) %>%
  group_by(region, type) %>%
  summarize(share = round(sum(share, na.rm = TRUE), 1)) %>% 
  ggplot(aes(x = region, y = share, color = type, fill = type)) + 
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_ptol() +
  scale_fill_ptol() +
  ylab("1998 - 2016 Employment Shift") +
  labs(title = "Industry Employment Shift Share Analysis") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("complete plot shift share.png", complete_plot, height = 7, width = 7)

walk(regions, function(x){
  plot <- multiplier_data_analysis %>%
    pivot_longer(16:18, names_to = "type", values_to = "share") %>%
    filter(year == 1998) %>%
    filter(region == x) %>%
    mutate(type = dplyr::recode(type, 
                                "industrial_mix" = "industrial mix",
                                "local_share" = "local share",
                                "national_share" = "national share")) %>%
    ggplot(aes(x = description, y = share, color = type, fill = type)) + 
    geom_col(position = "dodge") +
    coord_flip() +
    scale_color_ptol() +
    scale_fill_ptol() +
    ylab("1998 - 2016 % Employment Shift") +
    labs(title = paste0(x, " Industry Employment Shift Share Analysis")) +
    theme_linedraw() + 
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.y = element_blank())
  
  ggsave(paste0(x, " Shift Share.png"), plot, height = 7, width = 13)
})

plot <- growth_data_indexed %>%
  pivot_longer(cols = 3:6, names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = year, y = log(value), group = variable, color = variable)) +
  geom_line() +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  theme_par() +
  scale_color_ptol() +
  theme(axis.title.x = element_blank())

ggsave("plot 1.png", plot, height = 7, width = 5)

plot <- growth_data_indexed %>%
  mutate(y = gdp/labor,
         k = capital/labor) %>% 
  dplyr::select(region, y, k) %>% 
  ggplot(aes(x = k, y = y)) +
  geom_line() +
  facet_wrap(. ~ region, scales = "free", ncol = 1) +
  theme_par() +
  ylab("Y/L") +
  xlab("K/L")

ggsave("plot 2.png", plot, height = 8, width = 5)

plot <- growth_data %>% 
  group_by(region, year) %>%
  mutate(gdp_per_capita = gdp / labor) %>% 
  ggplot(aes(x = year, y = gdp_per_capita, group = region, colour = region)) +
  geom_line() +
  scale_color_ptol() +
  theme_linedraw()

ggsave("plot 3.png", plot, height = 5, width = 7)

plot <- growth_data %>% 
  ggplot(aes(x = year, y = gdp, group = region, colour = region)) +
  geom_line() +
  scale_color_ptol() +
  theme_linedraw()

ggsave("plot 4.png", plot, height = 5, width = 7)

plot <- growth_data %>% 
  mutate(ratio = capital/labor) %>%
  ggplot(aes(x = year, y = ratio, group = region, colour = region)) +
  geom_line() +
  scale_color_ptol() +
  ylab("capital-to-labor ratio") +
  theme_linedraw()

ggsave("plot 5.png", plot, height = 5, width = 7)

#-------- tables --------
table_employment <- multiplier_data_analysis %>%
  filter(year %in% c(1998, 2016)) %>%
  select(region, year, employment_state) %>% 
  group_by(region, year) %>%
  summarize(employment = employment_state[1]) %>% 
  pivot_wider(names_from = "year", values_from = "employment") %>% 
  rename("Region" = "region") %>%
  formattable(align = c("r", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2016`= formatter("span", style = ~ style(color = ifelse(`2016` <`1998`, "red", "green")),
                                  ~ icontext(ifelse(`2016` <`1998`,"arrow-down", "arrow-up"), `2016`))
              ))

table_complete <- left_join(
  multiplier_data_analysis %>% 
    filter(year %in% c(1998, 2016)) %>%
    select(region, year, description, lq, employment, employment_state) %>% 
    group_by(region, year, description) %>% 
    summarize(lq = round(mean(lq, na.rm = T), 5),
              p_employment = round(100 * employment / employment_state, 1)) %>%
    pivot_wider(names_from = "year", values_from = c("lq", "p_employment")),
  multiplier_data_analysis %>%
    filter(year == 2016) %>%
    select(region, description, growth_rate, birch, national_share, industrial_mix, local_share) %>% 
    group_by(region, description) %>% 
    summarize(growth_rate = round(growth_rate[1], 1),
              birch = round(birch[1], 1),
              national_share = round(national_share[1], 1),
              industrial_mix = round(industrial_mix[1], 1),
              local_share = round(local_share[1], 1)),
  by = c("region", "description")) %>% 
  rename("Region" = "region",
         "Industry" = "description",
         "1998 LQ" = "lq_1998",
         "2016 LQ" = "lq_2016",
         "1998 % Employment" = "p_employment_1998",
         "2016 % Employment" = "p_employment_2016",
         "Growth rate" = "growth_rate",
         "Birch Index" = "birch",
         "National share" = "national_share",
         "Industrial mix" = "industrial_mix",
         "Local share" = "local_share") %>% 
  ungroup() %>%
  formattable(align = c("r", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2016 LQ`= formatter("span", style = ~ style(color = ifelse(`2016 LQ` <`1998 LQ`, "red", "green")),
                                     ~ icontext(ifelse(`2016 LQ` <`1998 LQ`,"arrow-down", "arrow-up"), `2016 LQ`)),
                `2016 % Employment`= formatter("span", style = ~ style(color = ifelse(`2016 % Employment` <`1998 % Employment`, "red", "green")),
                                               ~ icontext(ifelse(`2016 % Employment` <`1998 % Employment`,"arrow-down", "arrow-up"), `2016 % Employment`)),
                `Growth rate` = formatter("span", style = ~style(color = ifelse(`Growth rate` < 0, "red", "green"))),
                `Birch Index` = formatter("span", style = ~style(color = ifelse(`Birch Index` < 0, "red", "green"))),
                `National share` = formatter("span", style = ~style(color = ifelse(`National share` < 0, "red", "green"))),
                `Industrial mix` = formatter("span", style = ~style(color = ifelse(`Industrial mix` < 0, "red", "green"))),
                `Local share` = formatter("span", style = ~style(color = ifelse(`Local share` < 0, "red", "green")))
              ))


table_top_employment <- left_join(
  multiplier_data_analysis %>% 
    filter(year %in% c(1998, 2016)) %>%
    select(region, year, description, lq, employment, employment_state) %>%
    group_by(region, year, description) %>% 
    summarize(lq = round(mean(lq, na.rm = T), 1),
              p_employment = round(100 * employment / employment_state, 1)[1]) %>% 
    pivot_wider(names_from = "year", values_from = c("lq", "p_employment")) %>% 
    rename("1998 % Employment" = "p_employment_1998",
           "2016 % Employment" = "p_employment_2016",
           "1998 LQ" = "lq_1998",
           "2016 LQ" = "lq_2016") %>%
    arrange(region, desc(`2016 % Employment`)) %>%
    top_n(5),
  multiplier_data_analysis %>%
    filter(year == 2016) %>%
    select(region, description, growth_rate, birch, national_share, industrial_mix, local_share) %>% 
    group_by(region, description) %>% 
    summarize(growth_rate = round(growth_rate[1], 1),
              birch = round(birch[1], 1),
              national_share = round(national_share[1], 1),
              industrial_mix = round(industrial_mix[1], 1),
              local_share = round(local_share[1], 1)),
  by = c("region", "description")) %>% 
  rename("Region" = "region",
         "Industry" = "description",
         "Growth rate" = "growth_rate",
         "Birch Index" = "birch",
         "National share" = "national_share",
         "Industrial mix" = "industrial_mix",
         "Local share" = "local_share") %>% 
  ungroup() %>%
  formattable(align = c("r", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2016 LQ`= formatter("span", style = ~ style(color = ifelse(`2016 LQ` <`1998 LQ`, "red", "green")),
                                     ~ icontext(ifelse(`2016 LQ` <`1998 LQ`,"arrow-down", "arrow-up"), `2016 LQ`)),
                `2016 % Employment`= formatter("span", style = ~ style(color = ifelse(`2016 % Employment` <`1998 % Employment`, "red", "green")),
                                               ~ icontext(ifelse(`2016 % Employment` <`1998 % Employment`,"arrow-down", "arrow-up"), `2016 % Employment`)),
                `Growth rate` = formatter("span", style = ~style(color = ifelse(`Growth rate` < 0, "red", "green"))),
                `Birch Index` = formatter("span", style = ~style(color = ifelse(`Birch Index` < 0, "red", "green"))),
                `National share` = formatter("span", style = ~style(color = ifelse(`National share` < 0, "red", "green"))),
                `Industrial mix` = formatter("span", style = ~style(color = ifelse(`Industrial mix` < 0, "red", "green"))),
                `Local share` = formatter("span", style = ~style(color = ifelse(`Local share` < 0, "red", "green")))
              ))

table_top_lq <- left_join(
  multiplier_data_analysis %>% 
    filter(year %in% c(1998, 2016)) %>%
    select(region, year, description, lq, employment, employment_state) %>%
    group_by(region, year, description) %>% 
    summarize(lq = round(mean(lq, na.rm = T), 1),
              p_employment = round(100 * employment / employment_state, 1)[1]) %>% 
    pivot_wider(names_from = "year", values_from = c("lq", "p_employment")) %>% 
    rename("1998 % Employment" = "p_employment_1998",
           "2016 % Employment" = "p_employment_2016",
           "1998 LQ" = "lq_1998",
           "2016 LQ" = "lq_2016") %>% 
    arrange(region, desc(`2016 LQ`)) %>%
    top_n(5, `2016 LQ`),
  multiplier_data_analysis %>%
    filter(year == 2016) %>%
    select(region, description, growth_rate, birch, national_share, industrial_mix, local_share) %>% 
    group_by(region, description) %>% 
    summarize(growth_rate = round(growth_rate[1], 1),
              birch = round(birch[1], 1),
              national_share = round(national_share[1], 1),
              industrial_mix = round(industrial_mix[1], 1),
              local_share = round(local_share[1], 1)),
  by = c("region", "description")) %>% 
  rename("Region" = "region",
         "Industry" = "description",
         "Growth rate" = "growth_rate",
         "Birch Index" = "birch",
         "National share" = "national_share",
         "Industrial mix" = "industrial_mix",
         "Local share" = "local_share") %>% 
  ungroup() %>%
  formattable(align = c("r", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2016 LQ`= formatter("span", style = ~ style(color = ifelse(`2016 LQ` <`1998 LQ`, "red", "green")),
                                     ~ icontext(ifelse(`2016 LQ` <`1998 LQ`,"arrow-down", "arrow-up"), `2016 LQ`)),
                `2016 % Employment`= formatter("span", style = ~ style(color = ifelse(`2016 % Employment` <`1998 % Employment`, "red", "green")),
                                               ~ icontext(ifelse(`2016 % Employment` <`1998 % Employment`,"arrow-down", "arrow-up"), `2016 % Employment`)),
                `Growth rate` = formatter("span", style = ~style(color = ifelse(`Growth rate` < 0, "red", "green"))),
                `Birch Index` = formatter("span", style = ~style(color = ifelse(`Birch Index` < 0, "red", "green"))),
                `National share` = formatter("span", style = ~style(color = ifelse(`National share` < 0, "red", "green"))),
                `Industrial mix` = formatter("span", style = ~style(color = ifelse(`Industrial mix` < 0, "red", "green"))),
                `Local share` = formatter("span", style = ~style(color = ifelse(`Local share` < 0, "red", "green")))
              ))

table_growth_birch <- multiplier_data_analysis %>%
  filter(year == 2016) %>%
  select(region, year, growth_rate, birch) %>% 
  group_by(region) %>%
  summarize(growth_rate = round(mean(growth_rate, na.rm = T), 5),
            birch = round(mean(birch, na.rm = T), 5)) %>% 
  rename("Region" = "region",
         "Growth rate" = "growth_rate",
         "Birch Index" = "birch") %>%
  formattable(align = c("r", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `Growth rate` = formatter("span", style = ~style(color = ifelse(`Growth rate` < 0, "red", "green"))),
                `Birch Index` = formatter("span", style = ~style(color = ifelse(`Birch Index` < 0, "red", "green")))
              ))

  





  




