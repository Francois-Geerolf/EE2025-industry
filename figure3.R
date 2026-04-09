rm(list = ls())
library(dplyr)
library(eurostat)
library(ggplot2)
source("~/iCloud/website/code/R-markdown/init_min.R")

load_data("wdi/NV.IND.MANF.CD.RData")
load_data("wdi/iso2c_long2.RData")
load_data("macro.RData")
load_data("flags/colors.RData")

figure3_data <- NV.IND.MANF.CD %>%
  right_join(iso2c, by = "iso2c") %>%
  filter(iso2c %in% c("1W", "US", "CN", "EU")) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  filter(date >= as.Date("1997-01-01")) %>%
  group_by(date) %>%
  mutate(value = value/value[iso2c == "1W"]) %>%
  filter(!(iso2c == "1W")) %>%
  mutate(Iso2c = case_when(iso2c == "EU" ~ "Europe",
                           iso2c == "CN" ~ "China",
                           iso2c == "US" ~ "United States")) %>%
  filter(date >= as.Date("2005-01-01")) %>%
  left_join(colors, by = c("Iso2c" = "country")) %>%
  mutate(color = ifelse(iso2c == "US", color2, color)) %>%
  arrange(date) %>%
  select(date, Iso2c, value)

write_csv2(figure3_data, file = "figure3.csv")

figure3 <- figure3_data %>%
  ggplot(.) + theme_minimal() + 
  # Chine: #FFDC00; USA: #B22234; Europe: #003399
  #scale_color_manual(values = c("#FFDC00", "#B22234", "#003399")) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
  geom_line(aes(x = date, y = value, linetype = Iso2c)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.3, 0.9),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.direction = "horizontal") +
  scale_x_date(breaks = seq(1950, 2100, 1) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(0, 70, 2),
                     labels = scales::percent_format(accuracy = 1)) + 
  xlab("") + ylab("")


figure3
save(figure3, file = "figure3.RData")
ggsave(figure3, file = "figure3.pdf", bg = "white", width = 7, height = 4)
ggsave(figure3, file = "figure3.png", bg = "white", width = 7, height = 4)
ggsave(figure3, file = "figure3.svg", bg = "white", width = 7, height = 4)

