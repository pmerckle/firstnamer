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
data <- read.table(unz(temp, "nat2017.txt"), stringsAsFactors = FALSE)
unlink(temp)

# Clean data
df <- data
names(df) <- c("sex", "firstname", "year", "count")

# Remove bad data
df <- df[df$sex %in% c("1", "2"), ]

# Variable class
df$sex <- factor(df$sex)
df$year <- as.integer(df$year)
df$count <- as.integer(df$count)

# Clean encoding
df$firstname <- str_replace_all(df$firstname, c(
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
devtools::use_data(df)

# Recode source data
devtools::use_data_raw()


# Documentation ----
devtools::document()

# Test package

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
