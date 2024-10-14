## Life expectancy
# UN WPP
# Life expectancy at Exact Age x (ex) - Both Sexes
# The average number of remaining years of life expected by a hypothetical
# cohort of individuals alive at age x who would be subject during the remaining
# of their lives to the mortality rates of a given year. It is expressed as years.
# https://population.un.org/wpp/Download/Standard/Mortality/.

life_expectancy_africa <- read.csv("data-raw/UN_WPP_life_expectancy_SSA.csv")
usethis::use_data(life_expectancy_africa, overwrite = TRUE, internal = TRUE)
