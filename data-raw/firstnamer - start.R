#
# firstnamer
# getting started
#


# Install packages and developing tools ----

install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

# Install and check developing tools
library(devtools)
has_devel() # Doit retourner TRUE (ou rien ?) si l'installation est correcte ?


# Libraries ----

library(dplyr)
library(magrittr)
library(stringr)



# Data ----

# Load and unzip data file
temp <- tempfile()
download.file("https://www.insee.fr/fr/statistiques/fichier/2540004/nat2017_txt.zip",temp)
data <- read.table(unz(temp, "nat2017.txt"), stringsAsFactors = FALSE, encoding = "UTF-8")
unlink(temp)

# Clean data
fn_fr <- data
names(fn_fr) <- c("sex", "firstname", "year", "count")

# Remove bad data
fn_fr <- fn_fr[fn_fr$sex %in% c("1", "2"), ]

# Variable class
fn_fr$sex <- factor(fn_fr$sex)
fn_fr$year <- as.integer(fn_fr$year)
fn_fr$count <- as.integer(fn_fr$count)




# Clean encoding
fn_fr$firstname <- str_replace_all(fn_fr$firstname, c(
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
devtools::use_data(fn_fr, internal = TRUE)

# Recode source data
devtools::use_data_raw()



# Test package

# General test
library(devtools)
devtools::install_github("pmerckle/firstnamer")
library(firstnamer)
gender(c("Jacques", "Bernadette", "Nicolas", "Carla", "François", "Julie", "Emmanuel", "Brigitte"))

# Help pages
package?firstnamer
?unaccent
?gender_unique

# Functions
unaccent("Jérémie")
gender("Henry")
is.female("Marcelle")

# Create new vignette
devtools::use_vignette("firstnamer")



