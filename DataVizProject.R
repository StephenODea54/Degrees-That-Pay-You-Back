                    ### IMPORT LIBRARIES ###
library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggthemes)

                    ### IMPORT DATA ###
salary_by_major <- read_csv(
  "Data/degrees-that-pay-back.csv",
  col_types = "fnnnnnnn"
)

salary_by_type <- read_csv(
  "Data/salaries-by-college-type.csv",
  col_types = "ffnnnnnn"
)

salary_by_region <- read_csv(
  "Data/salaries-by-region.csv",
  col_types = "ffnnnnnn"
)

                    ### SET PROJECT THEME ###
theme_set(theme_bw())

                    ### STACKED BAR CHART ###
salary_by_major %>%
  ggplot() +
  geom_bar(stat = "identity", aes(x = reorder(`Undergraduate Major`, `Mid-Career 90th Percentile Salary`), y = `Mid-Career 90th Percentile Salary`, fill = "90th Percentile")) +
  geom_bar(stat = "identity", aes(x = `Undergraduate Major`, y = `Mid-Career 75th Percentile Salary`, fill = "75th Percentile")) +
  geom_bar(stat = "identity", aes(x = `Undergraduate Major`, y = `Mid-Career Median Salary`, fill = "50th Percentile")) +
  geom_bar(stat = "identity", aes(x = `Undergraduate Major`, y = `Mid-Career 25th Percentile Salary`, fill = "25th Percentile")) +
  geom_bar(stat = "identity", aes(x = `Undergraduate Major`, y = `Mid-Career 10th Percentile Salary`, fill = "10th Percentile")) +
  geom_point(aes(x = `Undergraduate Major`, y = `Starting Median Salary`, color = "Black")) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Degrees That Pay Back",
       x = "",
       y = "Salary (USD)",
       fill = "Salary Percentile") +
  scale_color_identity(name = "",
                       labels = "Entry Level Median Salary",
                       guide = "legend") +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(0,0,15,0)),
        axis.text.y = element_text(hjust = 0.95, vjust = 0.2),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

                    ### BOX PLOTS ###
salary_by_type %>%
  arrange(desc(`Mid-Career Median Salary`)) %>%
  mutate(School = str_to_title(`School Name`),
         School = fct_reorder(School, `Mid-Career Median Salary`)) %>%
  drop_na() %>%
  head(20) %>%
  ggplot(aes(x = School,
             ymin = `Mid-Career 10th Percentile Salary`,
             lower = `Mid-Career 25th Percentile Salary`,
             middle = `Mid-Career Median Salary`,
             upper = `Mid-Career 75th Percentile Salary`,
             ymax = `Mid-Career 90th Percentile Salary`,
             fill = `School Type`,
             color = `School Type`)) +
  geom_boxplot(stat = "identity", alpha = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  expand_limits(y = 50000) +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() +
  labs(title = "Where It Pays To Attend School",
       x = "",
       y = "Salary (USD)") +
  theme(axis.text.y = element_text(hjust = 0.95, vjust = 0.2),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")

                    ### SCATTERPLOT ###
# EXTRACT MAP DATA FROM GGPLOT
us_map <- map_data("state") %>% as_tibble()

# CREATE MAPPING FOR STATE AND REGION
tmp <- data.frame(state = c("california", "illinois", "indiana", "iowa", "kansas", "michigan", "minnesota", "missouri", "nebraska",
                            "north dakota", "ohio", "south dakota", "wisconsin", "connecticut", "delaware", "maine",
                            "massachusetts", "maryland", "district of columbia", "new hampshire", "new jersey", "new york", "pennsylvania",
                            "rhode island", "vermont", "alabama", "arkansas", "florida", "georgia", "kentucky", "louisiana",
                            "mississippi", "north carolina", "oklahoma", "south carolina", "tennessee", "texas",
                            "virginia", "west virginia", "arizona", "colorado", "idaho", "montana", "nevada",
                            "new mexico", "oregon", "utah", "washington", "wyoming"),
                  Region = c("California", "Midwestern", "Midwestern", "Midwestern", "Midwestern", "Midwestern", "Midwestern",
                             "Midwestern", "Midwestern", "Midwestern", "Midwestern", "Midwestern", "Midwestern", "Northeastern", "Northeastern",
                             "Northeastern", "Northeastern", "Northeastern", "Northeastern", "Northeastern", "Northeastern", "Northeastern", "Northeastern",
                             "Northeastern", "Northeastern", "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", "Southern",
                             "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", "Western", "Western",
                             "Western", "Western", "Western", "Western", "Western", "Western", "Western", "Western")) %>% as_tibble()

# WRANGLE SALARY_BY_REGION AND MERGE WITH TMP
salary_by_region1 <- salary_by_region %>%
  group_by(Region) %>%
  summarise(mid_career_median_salary = median(`Mid-Career Median Salary`))

salary_by_region1 <- tmp %>%
  left_join(salary_by_region1, by = "Region")

# MERGE SALARY_BY_REGION AND US_MAP
salary_by_region_map <- us_map %>%
  left_join(salary_by_region1, by = c("region" = "state"))

salary_by_region_map %>%
  ggplot(aes(long, lat, group = subregion)) +
  coord_map("ortho",
            orientation = c(39, -98, 0)) +
  geom_polygon(aes(group = group, fill = mid_career_median_salary, color = Region)) +
  scale_fill_distiller(name = "Median Salary", palette = "Blues", labels = dollar_format(),
                       breaks = c(77800, 80000, 88000, 91550)) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(fill = "white")),
         fill=guide_legend(byrow=TRUE)) +
  labs(x = "",
       y = "",
       fill = "") +
  theme_map() +
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.margin=margin(),
        legend.justification = "center")

                    ### GROUPED BAR CHART ###
salary_by_region %>%
  left_join(salary_by_type) %>%
  drop_na() %>%
  group_by(Region, `School Type`) %>%
  summarise(mean = mean(`Mid-Career Median Salary`)) %>%
  ggplot(aes(`School Type`, mean, fill = Region)) + 
  geom_col(alpha = 0.8, position = 'dodge', color = "#000000") +
  scale_fill_brewer(palette = 'Pastel1') +
  coord_flip() +
  scale_y_continuous(labels = dollar_format()) +
  labs(subtitle = "Best Region for Each School Type",
       y = "Salary (USD)") +
  theme(axis.text.y = element_text(hjust = 0.95, vjust = 0.2),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
