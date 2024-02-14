## Life expectancy
# UN WPP
# Life expectancy at Exact Age x (ex) - Both Sexes
# The average number of remaining years of life expected by a hypothetical
# cohort of individuals alive at age x who would be subject during the remaining
# of their lives to the mortality rates of a given year. It is expressed as years.
# https://population.un.org/wpp/Download/Standard/Mortality/.
# As this source is for Africa, DALY functionality should not be used when modelling countries outside of Africa.
# Also keep in mind that the choice of average life expectancy may produce negative DALY values in countries with a life expectancy
# that is greater than average. Consider using the greatest observed life expectancy in a region if this is an issue for your use case.
# Year 2022

life_expectancy_africa <- read.csv("data-raw/UN_WPP_2022_life_expectancy_Africa.csv")
usethis::use_data(life_expectancy_africa, overwrite = TRUE, internal = TRUE)
