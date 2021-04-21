                    ### IMPORT LIBRARIES ###
library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(maps)
library(mapproj)

                    ### IMPORT DATA ###
salary_by_major <- read_csv("C:/Users/campb/Desktop/degrees-that-pay-back.csv")
salary_by_type <- read_csv("C:/Users/campb/Desktop/salaries-by-college-type.csv", 
                                     col_types = cols(`Mid-Career 10th Percentile Salary` = col_double(), 
                                                      `Mid-Career 90th Percentile Salary` = col_double()))
salary_by_region <- read_csv("C:/Users/campb/Desktop/salaries-by-region.csv")

                    ### SET PROJECT THEME ###
theme_set(theme_economist_white())

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
  labs(title = "College Degrees with the Highest Earnings Potential",
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

                    ### ERROR BAR CHART ###
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
  labs(title = "Where It Pays to Attend School",
       subtitle = "Top 20 Schools Aggregated by School Type",
       x = "",
       y = "Salary (USD)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, margin = margin(0,0,15,0)),
        axis.text.y = element_text(hjust = 0.95, vjust = 0.2),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

                    ### CHOROPLETH ###
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
salary_by_region <- salary_by_region %>%
  group_by(Region) %>%
  summarise(mid_career_median_salary = median(`Mid-Career Median Salary`))

salary_by_region <- tmp %>%
  left_join(salary_by_region, by = "Region")

rm(tmp)

# MERGE SALARY_BY_REGION AND US_MAP
salary_by_region_map <- us_map %>%
  left_join(salary_by_region, by = c("region" = "state"))

salary_by_region_map %>%
  ggplot(aes(long, lat, group = subregion)) +
  coord_map("ortho",
            orientation = c(39, -98, 0)) +
  geom_polygon(aes(group = group, fill = mid_career_median_salary), color = "black") +
  scale_color_brewer(palette = "Blues") +
  theme_minimal() +
  labs(title = "Mid-Career Salaries",
       x = "",
       y = "",
       fill = "") +
  theme_map() +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5))
