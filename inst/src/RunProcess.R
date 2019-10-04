fd::initialize("brain")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

fd::update_weather()

#fs::dir_create(fd::path("results","flumomo"))

# Set up data
model <- amort$new()
model$run_all()


