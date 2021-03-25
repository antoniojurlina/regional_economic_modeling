#------------------------------------------------#
# Script for analyzing and plotting AS1 data,    #
# by calculating location quotients,             #
# gini coefficients and other neccessary values  #
#------------------------------------------------#

#-------- packages --------
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(ggrepel)
library(tidyr)
library(formattable)
library(htmltools)
library(webshot)  

#-------- data and directory --------
paste0(here::here(), "/data") %>% setwd()

load("project 01.RData")
options(scipen=10000)

paste0(here::here(), "/output") %>% setwd()

#-------- analysis --------
# calculate necessary values for lab purposes
project_1_analysis <- project_1 %>%
  group_by(year, region, level, sector) %>%
  mutate(sri = employment / state_employment,
         si = us_employment / us_employment_total,
         qi = sri /si,
         diff_sq = (sri - si) ^ 2) %>%
  ungroup() %>%
  group_by(year, region, level) %>%
  mutate(gini = sum(diff_sq)) %>%
  select(-diff_sq) %>%
  ungroup() %>%
  group_by(region, level, sector) %>%
  mutate(qi_change = 100 * (first(qi) - last(qi)) / last(qi),
         growth_rate = 100 * (first(employment) - last(employment)) / last(employment),
         birch = (first(employment) - last(employment)) * (first(employment) / last(employment) ),
         national_share = last(employment) * (first(us_employment_total) - last(us_employment_total)) / last(us_employment_total),
         industrial_mix = last(employment) * ((first(us_employment) - last(us_employment)) / last(us_employment) - 
                                              (first(us_employment_total) - last(us_employment_total)) / last(us_employment_total)),
         local_share = last(employment) * ((first(employment) - last(employment)) / last(employment) - 
                                             (first(us_employment) - last(us_employment)) / last(us_employment))) %>%
  ungroup() %>%
  group_by(year, region, level, sector) %>%
  mutate(shift = national_share + industrial_mix + local_share) %>%
  ungroup()

# add state border lat and long data 
geo_data <- ggplot2::map_data("state") %>%
  select(-subregion) %>%
  filter(region %in% unique(project_1_analysis$region))

project_1_analysis_geo <- left_join(project_1_analysis, geo_data, by = "region")

#-------- plotting LQs --------
complete_plot <- project_1_analysis %>%
  filter(level == 3, year == 2009) %>%
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change)) %>%
ggplot(aes(x = code, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=code, xend=code, y=0, yend=qi_change), show.legend = FALSE) +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "Industry Employment Location Quotient") +
  theme_linedraw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())

under_100_plot <- project_1_analysis %>%
  filter(level == 3, year == 2009) %>%
  filter(qi_change < 80) %>%
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change)) %>%
  ggplot(aes(x = code, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=code, xend=code, y=0, yend=qi_change), show.legend = FALSE) +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "Industry Employment Location Quotient",
       subtitle = "changes under 100%") +
  theme_linedraw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())

new_hampshire <- project_1_analysis %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "new hampshire") %>%
  filter(qi_change >= 80 | qi_change <= -80) %>% 
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change),
         level = factor(level)) %>% 
  ggplot(aes(x = sector, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=sector, xend=sector, y=0, yend=qi_change), show.legend = FALSE) +
  coord_flip() +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "New Hampshire Industry Employment Location Quotient",
       subtitle = "changes over 80%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

new_hampshire_tight <- project_1_analysis %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "new hampshire") %>%
  filter(qi_change < 80) %>% 
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change),
         level = factor(level)) %>% 
  ggplot(aes(x = sector, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=sector, xend=sector, y=0, yend=qi_change), show.legend = FALSE) +
  coord_flip() +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "New Hampshire Industry Employment Location Quotient",
       subtitle = "changes under 100%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

maine <- project_1_analysis %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "maine") %>%
  filter(qi_change >= 80 | qi_change <= -80) %>% 
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change),
         level = factor(level)) %>% 
  ggplot(aes(x = sector, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=sector, xend=sector, y=0, yend=qi_change), show.legend = FALSE) +
  coord_flip() +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "Maine Industry Employment Location Quotient",
       subtitle = "changes over 80%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

maine_tight <- project_1_analysis %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "maine") %>%
  filter(qi_change < 80) %>% 
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change),
         level = factor(level)) %>% 
  ggplot(aes(x = sector, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=sector, xend=sector, y=0, yend=qi_change), show.legend = FALSE) +
  coord_flip() +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "Maine Industry Employment Location Quotient",
       subtitle = "changes under 100%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

mass <- project_1_analysis %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "massachusetts") %>%
  filter(qi_change >= 80 | qi_change <= -80) %>% 
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change),
         level = factor(level)) %>% 
  ggplot(aes(x = sector, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=sector, xend=sector, y=0, yend=qi_change), show.legend = FALSE) +
  coord_flip() +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "Massachusetts Industry Employment Location Quotient",
       subtitle = "changes over 80%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

mass_tight <- project_1_analysis %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "massachusetts") %>%
  filter(qi_change < 80) %>% 
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change),
         level = factor(level)) %>% 
  ggplot(aes(x = sector, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=sector, xend=sector, y=0, yend=qi_change), show.legend = FALSE) +
  coord_flip() +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "Massachusetts Industry Employment Location Quotient",
       subtitle = "changes under 100%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

penn <- project_1_analysis %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "pennsylvania") %>%
  filter(qi_change >= 80 | qi_change <= -80) %>% 
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change),
         level = factor(level)) %>% 
  ggplot(aes(x = sector, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=sector, xend=sector, y=0, yend=qi_change), show.legend = FALSE) +
  coord_flip() +
  scale_color_ptol() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "Pennsylvania Industry Employment Location Quotient",
       subtitle = "changes over 80%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

penn_tight <- project_1_analysis %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "pennsylvania") %>%
  filter(qi_change < 80) %>% 
  mutate(sign = ifelse(qi_change >= 0, "positive", "negative"),
         qi_change = abs(qi_change),
         level = factor(level)) %>% 
  ggplot(aes(x = sector, y = qi_change, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=sector, xend=sector, y=0, yend=qi_change), show.legend = FALSE) +
  coord_flip() +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Change LQ") +
  labs(title = "Pennsylvania Industry Employment Location Quotient",
       subtitle = "changes under 100%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

ggsave("complete plot.png", complete_plot, height = 7, width = 10)
ggsave("under 100 plot.png", under_100_plot, height = 7, width = 10)
ggsave("new hampshire.png", new_hampshire, height = 7, width = 8)
ggsave("maine.png", maine, height = 7, width = 8)
ggsave("mass.png", mass, height = 7, width = 8)
ggsave("penn.png", penn, height = 7, width = 8)
ggsave("new hampshire tight.png", new_hampshire_tight, height = 10, width = 8)
ggsave("maine tight.png", maine_tight, height = 10, width = 8)
ggsave("mass tight.png", mass_tight, height = 10, width = 8)
ggsave("penn tight.png", penn_tight, height = 10, width = 8)

#-------- plotting gini map --------
map_plot <- project_1_analysis_geo %>%
  filter(year == 2017) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(data = map_data("usa"), 
               aes(x = long, y = lat, group = group), 
               colour = "black", 
               fill = "transparent",
               size = 0.09,
               inherit.aes = FALSE) +
  geom_polygon(aes(colour = gini, fill = gini)) +
  coord_map(xlim = c(min(project_1_analysis_geo$long) - 1,
                    max(project_1_analysis_geo$long) + 1), 
           ylim = c(min(project_1_analysis_geo$lat) - 1,
                    max(project_1_analysis_geo$lat) + 0.5)) +
  scale_color_gradient2("Gini Coefficient") +
  scale_fill_gradient2("Gini Coefficient") +
  theme_map() +
  theme(legend.position = "top",
        legend.key.width=unit(2,"cm"))

ggsave("gini map.png", map_plot, height = 7, width = 8)

#-------- plotting growth rates --------

complete_plot <- project_1_analysis %>%
  filter(level == 3, year == 2009) %>%
  mutate(sign = ifelse(growth_rate >= 0, "positive", "negative"),
         growth_rate = abs(growth_rate)) %>%
  ggplot(aes(x = code, y = growth_rate, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=code, xend=code, y=0, yend=growth_rate), show.legend = FALSE) +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Growth Rate") +
  labs(title = "Industry Employment Growth Rate") +
  theme_linedraw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())

under_100_plot <- project_1_analysis %>%
  filter(level == 3, year == 2009) %>%
  filter(growth_rate < 80) %>%
  mutate(sign = ifelse(growth_rate >= 0, "positive", "negative"),
         growth_rate = abs(growth_rate)) %>%
  ggplot(aes(x = code, y = growth_rate, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=code, xend=code, y=0, yend=growth_rate), show.legend = FALSE) +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Growth Rate") +
  labs(title = "Industry Employment Growth Rate",
       subtitle = "changes under 100%") +
  theme_linedraw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())

ggsave("complete plot growth.png", complete_plot, height = 7, width = 9)
ggsave("under 100 plot growth.png", under_100_plot, height = 7, width = 9)

#-------- plotting birch index --------

complete_plot <- project_1_analysis %>%
  filter(level == 3, year == 2009) %>%
  mutate(sign = ifelse(birch >= 0, "positive", "negative"),
         birch = abs(birch)) %>%
  ggplot(aes(x = code, y = birch, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=code, xend=code, y=0, yend=birch), show.legend = FALSE) +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Birch Index") +
  labs(title = "Industry Employment Birch Index") +
  theme_linedraw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())

under_100_plot <- project_1_analysis %>%
  filter(level == 3, year == 2009) %>%
  filter(birch < 2500, birch > -2500) %>% 
  mutate(sign = ifelse(birch >= 0, "positive", "negative"),
         birch = abs(birch)) %>%
  ggplot(aes(x = code, y = birch, color = sign)) + 
  geom_point(aes(size = employment)) + 
  geom_segment(aes(x=code, xend=code, y=0, yend=birch), show.legend = FALSE) +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_wsj() +
  scale_size_continuous("2009 employment", labels = comma) +
  ylab("2009 - 2017 % Birch Index") +
  labs(title = "Industry Employment Birch Index",
       subtitle = "values under 2500") +
  theme_linedraw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())

ggsave("complete plot birch.png", complete_plot, height = 7, width = 9)
ggsave("under 100 plot birch.png", under_100_plot, height = 7, width = 9)

#-------- plotting shift share --------

maine <- project_1_analysis %>%
  pivot_longer(18:20, names_to = "type", values_to = "share") %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "maine") %>%
  filter(qi_change >= 80 | qi_change <= -80) %>% 
  mutate(type = recode(type, 
                       "industrial_mix" = "industrial mix",
                       "local_share" = "local share",
                       "national_share" = "national share")) %>%
  ggplot(aes(x = sector, y = share, color = type, fill = type)) + 
  geom_col(position = "dodge") +
  coord_flip() +
  scale_color_ptol() +
  scale_fill_ptol() +
  ylab("2009 - 2017 % Employment Shift") +
  labs(title = "Maine Industry Employment Shift Share Analysis",
       subtitle = "changes over 80%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

mass <- project_1_analysis %>%
  pivot_longer(18:20, names_to = "type", values_to = "share") %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "massachusetts") %>%
  filter(qi_change >= 80 | qi_change <= -80) %>% 
  mutate(type = recode(type, 
                       "industrial_mix" = "industrial mix",
                       "local_share" = "local share",
                       "national_share" = "national share")) %>%
  ggplot(aes(x = sector, y = share, color = type, fill = type)) + 
  geom_col(position = "dodge") +
  coord_flip() +
  scale_color_ptol() +
  scale_fill_ptol() +
  ylab("2009 - 2017 % Employment Shift") +
  labs(title = "Massachusetts Industry Employment Shift Share Analysis",
       subtitle = "changes over 80%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

new_hampshire <- project_1_analysis %>%
  pivot_longer(18:20, names_to = "type", values_to = "share") %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "new hampshire") %>%
  filter(qi_change >= 80 | qi_change <= -80) %>% 
  mutate(type = recode(type, 
                       "industrial_mix" = "industrial mix",
                       "local_share" = "local share",
                       "national_share" = "national share")) %>%
  ggplot(aes(x = sector, y = share, color = type, fill = type)) + 
  geom_col(position = "dodge") +
  coord_flip() +
  scale_color_ptol() +
  scale_fill_ptol() +
  ylab("2009 - 2017 % Employment Shift") +
  labs(title = "New Hampshire Industry Employment Shift Share Analysis",
       subtitle = "changes over 80%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

penn <- project_1_analysis %>%
  pivot_longer(18:20, names_to = "type", values_to = "share") %>%
  filter(year == 2009 & level == 3) %>%
  filter(region == "pennsylvania") %>%
  filter(qi_change >= 80 | qi_change <= -80) %>% 
  mutate(type = recode(type, 
                       "industrial_mix" = "industrial mix",
                       "local_share" = "local share",
                       "national_share" = "national share")) %>%
  ggplot(aes(x = sector, y = share, color = type, fill = type)) + 
  geom_col(position = "dodge") +
  coord_flip() +
  scale_color_ptol() +
  scale_fill_ptol() +
  ylab("2009 - 2017 % Employment Shift") +
  labs(title = "Pennsylvania Industry Employment Shift Share Analysis",
       subtitle = "changes over 80%") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())

ggsave("new hampshire shift share.png", new_hampshire, height = 7, width = 8)
ggsave("maine shift share.png", maine, height = 7, width = 8)
ggsave("mass shift share.png", mass, height = 7, width = 8)
ggsave("penn shift share.png", penn, height = 7, width = 8)

complete_plot <- project_1_analysis %>%
  pivot_longer(18:20, names_to = "type", values_to = "share") %>% 
  filter(year == 2009 & level == 3) %>%
  mutate(type = recode(type, 
                       "industrial_mix" = "industrial mix",
                       "local_share" = "local share",
                       "national_share" = "national share")) %>% 
  select(region, sector, share, type) %>%
  group_by(region, type) %>%
  summarize(share = round(sum(share, na.rm = TRUE), 1)) %>% 
  ggplot(aes(x = region, y = share, color = type, fill = type)) + 
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_ptol() +
  scale_fill_ptol() +
  ylab("2009 - 2017 % Employment Shift") +
  labs(title = "Industry Employment Shift Share Analysis") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

under_100_plot <- project_1_analysis %>%
  pivot_longer(18:20, names_to = "type", values_to = "share") %>% 
  filter(year == 2009 & level == 3) %>%
  filter(share < 25000, share > -25000) %>%
  mutate(type = recode(type, 
                       "industrial_mix" = "industrial mix",
                       "local_share" = "local share",
                       "national_share" = "national share")) %>% 
  select(region, sector, share, type) %>%
  group_by(region, type) %>%
  summarize(share = round(sum(share, na.rm = TRUE), 1)) %>% 
  ggplot(aes(x = region, y = share, color = type, fill = type)) + 
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(. ~ region, scales = "free_y", ncol = 1) +
  scale_color_ptol() +
  scale_fill_ptol() +
  ylab("2009 - 2017 % Employment Shift") +
  labs(title = "Industry Employment Shift Share Analysis",
       subtitle = "shifts under 25,000") +
  theme_linedraw() + 
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("complete shift share.png", complete_plot, height = 7, width = 7)
ggsave("under 100 shift share.png", under_100_plot, height = 7, width = 7)

#-------- tables --------
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

table_shift_share_1 <- project_1_analysis %>%
  pivot_longer(18:20, names_to = "type", values_to = "share") %>% 
  filter(year == 2009 & level == 3) %>%
  mutate(type = recode(type, 
                       "industrial_mix" = "industrial mix",
                       "local_share" = "local share",
                       "national_share" = "national share")) %>% 
  select(region, sector, share, type) %>%
  group_by(region, type) %>%
  summarize(share = round(sum(share, na.rm = TRUE), 1)) %>%
  rename("State" = "region",
         "Shift type" = "type",
         "Shift share" = "share") %>%
  formattable(align = c("l", "c", "r"),
              list(
                State = formatter("span", style = ~style(color = "dark grey", font.weight = "bold")),
                `Shift share` = formatter("span", style = ~style(color = ifelse(`Shift share` < 0, "red", "green")))
              ))

table_shift_share_2 <- project_1_analysis %>%
  pivot_longer(18:20, names_to = "type", values_to = "share") %>% 
  filter(year == 2009 & level == 3) %>%
  filter(share < 25000, share > -25000) %>%
  mutate(type = recode(type, 
                       "industrial_mix" = "industrial mix",
                       "local_share" = "local share",
                       "national_share" = "national share")) %>% 
  select(region, sector, share, type) %>%
  group_by(region, type) %>%
  summarize(share = round(sum(share, na.rm = TRUE), 1)) %>%
  rename("State" = "region",
         "Shift type" = "type",
         "Shift share" = "share") %>%
  formattable(align = c("l", "c", "r"),
              list(
                State = formatter("span", style = ~style(color = "dark grey", font.weight = "bold")),
                `Shift share` = formatter("span", style = ~style(color = ifelse(`Shift share` < 0, "red", "green")))
              ))

table_gini <- project_1_analysis %>%
  select(region, year, gini) %>% 
  group_by(region, year) %>%
  summarize(gini = round(gini[1], 5)) %>% 
  pivot_wider(names_from = "year", values_from = "gini") %>% 
  rename("State" = "region") %>%
  formattable(align = c("r", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2017`= formatter("span", style = ~ style(color = ifelse(`2017` <`2009`, "red", "green")),
                                  ~ icontext(ifelse(`2017` <`2009`,"arrow-down", "arrow-up"), `2017`))
              ))

table_lq <- project_1_analysis %>%
  select(region, year, qi) %>% 
  group_by(region, year) %>%
  summarize(qi = round(mean(qi, na.rm = T), 5)) %>% 
  pivot_wider(names_from = "year", values_from = "qi") %>% 
  rename("State" = "region") %>%
  formattable(align = c("r", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2017`= formatter("span", style = ~ style(color = ifelse(`2017` <`2009`, "red", "green")),
                                  ~ icontext(ifelse(`2017` <`2009`,"arrow-down", "arrow-up"), `2017`))
              ))

table_growth_birch <- project_1_analysis %>%
  select(region, year, growth_rate, birch) %>% 
  group_by(region) %>%
  summarize(growth_rate = round(mean(growth_rate, na.rm = T), 5),
            birch = round(mean(birch, na.rm = T), 5)) %>% 
  rename("State" = "region",
         "Growth rate" = "growth_rate",
         "Birch Index" = "birch") %>%
  formattable(align = c("r", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `Growth rate` = formatter("span", style = ~style(color = ifelse(`Growth rate` < 0, "red", "green"))),
                `Birch Index` = formatter("span", style = ~style(color = ifelse(`Birch Index` < 0, "red", "green")))
              ))

table_employment <- project_1_analysis %>%
  select(region, year, state_employment) %>% 
  group_by(region, year) %>%
  summarize(employment = state_employment[1]) %>% 
  pivot_wider(names_from = "year", values_from = "employment") %>% 
  rename("State" = "region") %>%
  formattable(align = c("r", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2017`= formatter("span", style = ~ style(color = ifelse(`2017` <`2009`, "red", "green")),
                                  ~ icontext(ifelse(`2017` <`2009`,"arrow-down", "arrow-up"), `2017`))
              ))

table_complete <- left_join(
  project_1_analysis %>%
    filter(qi_change >= 80 | qi_change <= -80) %>% 
    select(region, year, sector, qi, employment, state_employment) %>% 
    group_by(region, year, sector) %>% 
    summarize(qi = round(mean(qi, na.rm = T), 5),
              p_employment = round(100 * employment / state_employment, 1)) %>%
    pivot_wider(names_from = "year", values_from = c("qi", "p_employment")),
  project_1_analysis %>%
    filter(qi_change >= 80 | qi_change <= -80) %>% 
    filter(year == 2017) %>%
    select(region, sector, growth_rate, birch, national_share, industrial_mix, local_share) %>% 
    group_by(region, sector) %>% 
    summarize(growth_rate = round(growth_rate[1], 1),
              birch = round(birch[1], 1),
              national_share = round(national_share[1], 1),
              industrial_mix = round(industrial_mix[1], 1),
              local_share = round(local_share[1], 1)),
    by = c("region", "sector")) %>% 
  rename("State" = "region",
         "Industry" = "sector",
         "2009 LQ" = "qi_2009",
         "2017 LQ" = "qi_2017",
         "2009 % Employment" = "p_employment_2009",
         "2017 % Employment" = "p_employment_2017",
         "Growth rate" = "growth_rate",
         "Birch Index" = "birch",
         "National share" = "national_share",
         "Industrial mix" = "industrial_mix",
         "Local share" = "local_share") %>% 
  ungroup() %>%
  mutate(State = recode(State, 
                        "maine" = "Maine",
                        "massachusetts" = "Massachusetts",
                        "pennsylvania" = "Pennsylvania",
                        "new hampshire" = "New Hampshire")) %>% 
  formattable(align = c("r", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2017 LQ`= formatter("span", style = ~ style(color = ifelse(`2017 LQ` <`2009 LQ`, "red", "green")),
                                  ~ icontext(ifelse(`2017 LQ` <`2009 LQ`,"arrow-down", "arrow-up"), `2017 LQ`)),
                `2017 % Employment`= formatter("span", style = ~ style(color = ifelse(`2017 % Employment` <`2009 % Employment`, "red", "green")),
                                     ~ icontext(ifelse(`2017 % Employment` <`2009 % Employment`,"arrow-down", "arrow-up"), `2017 % Employment`)),
                `Growth rate` = formatter("span", style = ~style(color = ifelse(`Growth rate` < 0, "red", "green"))),
                `Birch Index` = formatter("span", style = ~style(color = ifelse(`Birch Index` < 0, "red", "green"))),
                `National share` = formatter("span", style = ~style(color = ifelse(`National share` < 0, "red", "green"))),
                `Industrial mix` = formatter("span", style = ~style(color = ifelse(`Industrial mix` < 0, "red", "green"))),
                `Local share` = formatter("span", style = ~style(color = ifelse(`Local share` < 0, "red", "green")))
              ))


table_top_employment <- left_join(
  project_1_analysis %>% 
    select(region, year, sector, qi, employment, state_employment) %>%
    group_by(region, year, sector) %>% 
    summarize(qi = round(mean(qi, na.rm = T), 1),
              p_employment = round(100 * employment / state_employment, 1)[1]) %>% 
    pivot_wider(names_from = "year", values_from = c("qi", "p_employment")) %>% 
    rename("2009 % Employment" = "p_employment_2009",
           "2017 % Employment" = "p_employment_2017",
           "2009 LQ" = "qi_2009",
           "2017 LQ" = "qi_2017") %>%
    arrange(region, desc(`2017 % Employment`)) %>%
    top_n(5),
  project_1_analysis %>%
    filter(year == 2017) %>%
    select(region, sector, growth_rate, birch, national_share, industrial_mix, local_share) %>% 
    group_by(region, sector) %>% 
    summarize(growth_rate = round(growth_rate[1], 1),
              birch = round(birch[1], 1),
              national_share = round(national_share[1], 1),
              industrial_mix = round(industrial_mix[1], 1),
              local_share = round(local_share[1], 1)),
  by = c("region", "sector")) %>% 
  rename("State" = "region",
         "Industry" = "sector",
         "Growth rate" = "growth_rate",
         "Birch Index" = "birch",
         "National share" = "national_share",
         "Industrial mix" = "industrial_mix",
         "Local share" = "local_share") %>% 
  ungroup() %>%
  mutate(State = recode(State, 
                        "maine" = "Maine",
                        "massachusetts" = "Massachusetts",
                        "pennsylvania" = "Pennsylvania",
                        "new hampshire" = "New Hampshire")) %>% 
  formattable(align = c("r", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2017 LQ`= formatter("span", style = ~ style(color = ifelse(`2017 LQ` <`2009 LQ`, "red", "green")),
                                     ~ icontext(ifelse(`2017 LQ` <`2009 LQ`,"arrow-down", "arrow-up"), `2017 LQ`)),
                `2017 % Employment`= formatter("span", style = ~ style(color = ifelse(`2017 % Employment` <`2009 % Employment`, "red", "green")),
                                               ~ icontext(ifelse(`2017 % Employment` <`2009 % Employment`,"arrow-down", "arrow-up"), `2017 % Employment`)),
                `Growth rate` = formatter("span", style = ~style(color = ifelse(`Growth rate` < 0, "red", "green"))),
                `Birch Index` = formatter("span", style = ~style(color = ifelse(`Birch Index` < 0, "red", "green"))),
                `National share` = formatter("span", style = ~style(color = ifelse(`National share` < 0, "red", "green"))),
                `Industrial mix` = formatter("span", style = ~style(color = ifelse(`Industrial mix` < 0, "red", "green"))),
                `Local share` = formatter("span", style = ~style(color = ifelse(`Local share` < 0, "red", "green")))
              ))


table_top_lq <- left_join(
project_1_analysis %>% 
  select(region, year, sector, qi, employment, state_employment) %>%
  group_by(region, year, sector) %>% 
  summarize(qi = round(mean(qi, na.rm = T), 1),
            p_employment = round(100 * employment / state_employment, 1)[1]) %>% 
  pivot_wider(names_from = "year", values_from = c("qi", "p_employment")) %>% 
  rename("2009 % Employment" = "p_employment_2009",
         "2017 % Employment" = "p_employment_2017",
         "2009 LQ" = "qi_2009",
         "2017 LQ" = "qi_2017") %>% 
  arrange(region, desc(`2017 LQ`)) %>%
  top_n(5, `2017 LQ`),
project_1_analysis %>%
  filter(year == 2017) %>%
  select(region, sector, growth_rate, birch, national_share, industrial_mix, local_share) %>% 
  group_by(region, sector) %>% 
  summarize(growth_rate = round(growth_rate[1], 1),
            birch = round(birch[1], 1),
            national_share = round(national_share[1], 1),
            industrial_mix = round(industrial_mix[1], 1),
            local_share = round(local_share[1], 1)),
by = c("region", "sector")) %>% 
  rename("State" = "region",
         "Industry" = "sector",
         "Growth rate" = "growth_rate",
         "Birch Index" = "birch",
         "National share" = "national_share",
         "Industrial mix" = "industrial_mix",
         "Local share" = "local_share") %>% 
  ungroup() %>%
  mutate(State = recode(State, 
                        "maine" = "Maine",
                        "massachusetts" = "Massachusetts",
                        "pennsylvania" = "Pennsylvania",
                        "new hampshire" = "New Hampshire")) %>%
  formattable(align = c("r", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
              list(
                State = formatter("span", 
                                  style = ~style(color = "dark grey", font.weight = "bold")),
                `2017 LQ`= formatter("span", style = ~ style(color = ifelse(`2017 LQ` <`2009 LQ`, "red", "green")),
                                     ~ icontext(ifelse(`2017 LQ` <`2009 LQ`,"arrow-down", "arrow-up"), `2017 LQ`)),
                `2017 % Employment`= formatter("span", style = ~ style(color = ifelse(`2017 % Employment` <`2009 % Employment`, "red", "green")),
                                               ~ icontext(ifelse(`2017 % Employment` <`2009 % Employment`,"arrow-down", "arrow-up"), `2017 % Employment`)),
                `Growth rate` = formatter("span", style = ~style(color = ifelse(`Growth rate` < 0, "red", "green"))),
                `Birch Index` = formatter("span", style = ~style(color = ifelse(`Birch Index` < 0, "red", "green"))),
                `National share` = formatter("span", style = ~style(color = ifelse(`National share` < 0, "red", "green"))),
                `Industrial mix` = formatter("span", style = ~style(color = ifelse(`Industrial mix` < 0, "red", "green"))),
                `Local share` = formatter("span", style = ~style(color = ifelse(`Local share` < 0, "red", "green")))
              ))
