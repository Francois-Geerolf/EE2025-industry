rm(list = ls())
library(tidyverse)
library(scales)
source("~/iCloud/website/code/R-markdown/init_min.R")
load_data("oecd/INDSERV.RData")
load_data("oecd/INDSERV_var.RData")
metadata_load_fr <- function(code, CL_code, data = INDSERV_var){
  assign(code, as.data.frame(data@codelists, codelistId = CL_code) %>%
           select(id, label.fr) %>%
           setNames(c(code, stringr::str_to_title(code))),
         envir = .GlobalEnv)
}
metadata_load <- function(code, CL_code, data = INDSERV_var){
  assign(code, as.data.frame(data@codelists, codelistId = CL_code) %>%
           select(id, label.en) %>%
           setNames(c(code, stringr::str_to_title(code))),
         envir = .GlobalEnv)
}

metadata_load("REF_AREA", "CL_AREA")

figure2_data <- INDSERV %>%
  filter(MEASURE == "PRVM",
         ACTIVITY == "C",
         FREQ == "M",
         ADJUSTMENT == "Y",
         REF_AREA %in% c("USA", "EU27_2020", "ITA", "FRA", "DEU")) %>%
  left_join(REF_AREA, by = "REF_AREA") %>%
  mutate(date = paste0(obsTime, "-01") %>% as.Date) %>%
  select(-obsTime) %>%
  select(date, everything()) %>%
  filter(date >= as.Date("1992-01-01")) %>%
  mutate(obsValue = ifelse(year(date) == 2020 & month(date) %in% 2:5, NA, obsValue)) %>%
  group_by(Ref_area) %>%
  arrange(date) %>%
  mutate(obsValue = 100*obsValue/obsValue[1]) %>%
  ungroup %>%
  mutate(Ref_area = ifelse(REF_AREA == "EU27_2020", "Europe", Ref_area)) %>%
  mutate(REF_AREA = ifelse(REF_AREA == "EU27_2020", "EU27", REF_AREA)) %>%
  select(Ref_area, REF_AREA, date, obsValue)


write_csv2(figure2_data, file = "figure2.csv")

figure2 <- figure2_data %>%
  ggplot(.) + geom_line(aes(x = date, y = obsValue, linetype = Ref_area)) +
  theme_minimal() + xlab("") + ylab("") +
  scale_x_date(breaks = c(seq(1992, 2100, 1)) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(-10, 300, 10),
                labels = dollar_format(accuracy = 1, prefix = "")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.4, 0.07),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.direction = "horizontal") +
  geom_text(data = . %>% group_by(Ref_area) %>% filter(date == max(date)),
            aes(x = date, y = obsValue, label = REF_AREA))
        


figure2
save(figure2, file = "figure2.RData")
ggsave(figure2, file = "figure2.pdf", bg = "white", width = 7, height = 4)
ggsave(figure2, file = "figure2.png", bg = "white", width = 7, height = 4)
ggsave(figure2, file = "figure2.svg", bg = "white", width = 7, height = 4)

