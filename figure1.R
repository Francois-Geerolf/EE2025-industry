library(dplyr)
library(eurostat)
library(ggplot2)
library(scales)
source("~/iCloud/website/code/R-markdown/init_min.R")
load_data("eurostat/nama_10_a10.RData")
load_data("eurostat/geo.RData")

figure1_data <- nama_10_a10 %>%
  filter(na_item == "B1G",
         nace_r2 %in% c("C", "TOTAL", "B-E"),
         geo %in% c("FR", "EU27_2020", "IT", "DE"),
         unit == "CP_MNAC") %>%
  mutate(date = as.Date(paste0(time, "-01-01"))) %>%
  filter(date >= as.Date("1995-01-01")) %>%
  left_join(geo, by = "geo") %>%
  mutate(Geo = ifelse(geo == "EU27_2020", "Europe", Geo)) %>%
  select(geo,  Geo, nace_r2, date, values) %>%
  spread(nace_r2, values) %>%
  transmute(date, Geo, values = C/TOTAL, values_B_E = `B-E`/TOTAL)

write_csv2(figure1_data, file = "figure1.csv")

figure1 <- figure1_data %>%
  ggplot(.) + geom_line(aes(x = date, y = values, linetype = Geo)) + 
  theme_minimal() + xlab("") + ylab("") +
  scale_x_date(breaks = seq(1960, 2100, 1) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.3, 0.07),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.direction = "horizontal") +
  scale_y_continuous(breaks = 0.01*seq(-500, 200, 1),
                     labels = percent_format(accuracy = 1))

figure1
save(figure1, file = "figure1.RData")
ggsave(figure1, file = "figure1.pdf", bg = "white", width = 7, height = 4)
ggsave(figure1, file = "figure1.png", bg = "white", width = 7, height = 4)
ggsave(figure1, file = "figure1.svg", bg = "white", width = 7, height = 4)

