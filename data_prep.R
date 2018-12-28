#Data prep
library(tidyverse)
library(readr)
library(reshape2)

area_by_district <- read_csv("area_by_district_1908.csv")

tidy <- gather(area_by_district, key = "condition", value, -district)
