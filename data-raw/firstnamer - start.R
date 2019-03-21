#
# firstnamer
# getting started
#



# Install packages and developing tools ----

install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

# Install and check developing tools
library(devtools)
# has_devel() # Doit retourner TRUE (ou rien ?) si l'installation est correcte ?



# Libraries ----

library(dplyr)
library(magrittr)
library(stringr)



# Data ----



# > FRANCE : Fichier des prénoms ----

# Load and unzip data file
temp <- tempfile()
download.file("https://www.insee.fr/fr/statistiques/fichier/2540004/nat2017_txt.zip",temp)
fn_fr <- read.table(unz(temp, "nat2017.txt"), stringsAsFactors = FALSE, encoding = "UTF-8")
unlink(temp)

# Clean data
names(fn_fr) <- c("sex", "fn", "year", "count")

# Remove bad data
fn_fr <- fn_fr[fn_fr$sex %in% c("1", "2"), ]

# Variable class
fn_fr$sex <- factor(fn_fr$sex)
fn_fr$year <- as.integer(fn_fr$year)
fn_fr$count <- as.integer(fn_fr$count)

# Clean encoding
fn_fr$firstname <- str_replace_all(fn_fr$fn, c(
  "Ã‚" = "A",
  "Ã€" = "A",
  "Ã„" = "A",
  "Ã†" = "AE",
  "Ã‡" = "C",
  "Ãˆ" = "C",
  "Ã‹" = "E",
  "ÃŠ" = "E",
  "Ã‰" = "E",
  "ÃŽ" = "I",
  "Ã\u008f" = "I",
  "Ã”" = "O",
  "Ã–" = "O",
  "Ãœ" = "OE",
  "Ã›" = "U",
  "Ã™" = "U"
))

# Save data
devtools::use_data(fn_fr, internal = TRUE, overwrite = TRUE)

# Recode source data
devtools::use_data_raw()


# > INTERNATIONAL : behindthenames.com

library(rvest)
url <- "https://www.behindthename.com/names/1"
page <- url %>% read_html %>% html_text


# Test package ----

# General test
library(devtools)
devtools::install_github("pmerckle/firstnamer")
library(firstnamer)
gender_unique("Pierre")
gender("Armando")
gender(c("Jacques", "Bernadette", "Nicolas", "Carla", "François", "Julie", "Emmanuel", "Brigitte"))
gender("Camille", year_max = 1950)
gender("Camille", year_min = 1950)
year("Pierre")

# Help pages
package?firstnamer
?unaccent
?gender_unique

# Functions
unaccent("Jérémie")
gender("Henry")
gender(c("Patrick", "Michelle"))
is_female("Marcelle")
year("Théo")
year("Anouk", "Lilia")

# Create new vignette
devtools::use_vignette("firstnamer")

# Dev
library(gender)


