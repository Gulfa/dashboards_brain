fd::initialize("brain")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

weather_download$new()$run_all()

amort$new()$run_all()
