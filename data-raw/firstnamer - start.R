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


# > INTERNATIONAL : behindthename.com ----

library(rvest)
# Get number of pages to scrape from homepage
url <- "https://www.behindthename.com/names"
page <- url %>% read_html %>% html_nodes(xpath = '//*[@id="div_pagination"]/div/a')
page <- url %>% read_html %>% html_nodes(css = '#div_pagination > div > a')
n <- page[length(page)-1] %>% html_text %>% as.integer
# Scrape pages
df <- NULL
for (i in 1:n) {
  print(i)
  page <- paste0(url, "/", i) %>% read_html
  name <- page %>% html_nodes("span.listname") %>% html_text
  gender <- page %>% html_nodes("span.listgender") %>% html_text
  usage <- page %>% html_nodes("span.listusage") %>% html_text
  df <- rbind(df, cbind(name, gender, usage))
}
bhtn <- as.data.frame(df)



# Test package ----


# General test
.libPaths("C:/R")
library(devtools)
devtools::install_github("pmerckle/firstnamer")
library(firstnamer)
gender_unique("Pierre")
gender("Armando")
gender(c("Jacques", "Bernadette", "Nicolas", "Carla", "François", "Julie", "Emmanuel", "Brigitte"))
gender("Camille", year_max = 1950)
gender("Camille", year_min = 1950)
year("Théoxane")

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
year(c("Anouk", "Lilia"))

# Create new vignette
devtools::use_vignette("firstnamer")

# Dev
library(gender)


