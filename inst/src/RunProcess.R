fd::initialize("brain")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

fd::update_weather()

# Set up data
model <- amort$new()
model$run_all()
