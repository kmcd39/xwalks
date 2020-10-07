## code to prepare `state2div` xwalk.

# Uses transcription from census bureau
# pdf here:
# https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv
library(dplyr)
state2div <- read.csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")

colnames(state2div) <- tolower(colnames(state2div))

colnames(state2div)[2] <- "abv"

state2div
usethis::use_data(state2div)
