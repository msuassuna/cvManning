## @knitr setup

# Define knitr options
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)

# Set missing data in tables to a blank space
options(knitr.kable.NA = "")

# Load the packages needed to running the code
library(fivethirtyeight)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggthemes)
library(stringr)

## @knitr loadData

# Load stake survey dataset
data("weather_check", package="fivethirtyeight")

# Create a subset, no missing data
# Pick a specific regions
sdat <- weather_check %>%
  filter(region==params$region)