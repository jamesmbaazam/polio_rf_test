library(tidyverse)

# Read data
covid_data <- read_csv("kyushu_covid03.csv")

# 1. Incidence per 1000 person-years during omicron (per perfecture)
inc_1000_pys_omicron <- covid_data %>%
    filter(period == "omicron") %>%
    mutate(
        person_years = population * (n / 365),
        inc_1000_pys = (cases / person_years) * 1000
    )

# 2. Relationship between incidence and population density
inc_density_relationship <- inc_1000_pys_omicron %>%
    ggplot(aes(x = population_density_km2, y = inc_1000_pys)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    labs(
        title = "Incidence per 1000 Person-Years vs Population Density",
        x = "Population Density (people/km²)",
        y = "Incidence per 1000 Person-Years"
    ) +
    theme_minimal()

inc_density_relationship

# 4. Community testing comparison metrics

# Inputs
population <- 811442
infected <- 141
uninfected <- population - infected
quarantine_cost_per_person <- 200 * 5

# Test parameters
tests <- data.frame(
    test = c("pcr", "rapid_antigen"),
    sensitivity = c(0.94, 0.97),
    specificity = c(1.00, 0.98),
    test_cost = c(2.00, 5.00),
    infections_prevented_per_case = c(0.5, 3)
)

# Calculations
metrics <- tests %>%
    rowwise() %>%
    mutate(
        TP = sensitivity * infected,
        FP = (1 - specificity) * uninfected,
        positives = round(TP + FP),
        test_total_cost = test_cost * population,
        quarantine_cost = positives * quarantine_cost_per_person,
        infections_prevented = TP * infections_prevented_per_case,
        total_cost = test_total_cost + quarantine_cost,
        cost_per_infection_prevented = total_cost / infections_prevented
    ) %>%
    select(test, positives, test_total_cost, quarantine_cost,
           infections_prevented, cost_per_infection_prevented)

metrics
