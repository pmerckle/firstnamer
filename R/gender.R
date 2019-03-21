# firstnamer::gender()
#
# This is the function named 'gender' and its variations,
# which get information about gender from first name
#



## FUNCTIONS ----


# unaccent ----

#' Remove accents
#'
#' This function removes accents from character strings.
#'
#' @param string input character vector.
#' @return An unaccented character vector.
#' @examples
#' unaccent("Jérémie")
#' unaccent("Héloïse")
#' @export

unaccent <- function(string) {
  text <- gsub("['`^~\"]", " ", string)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  return(text)
}


# gender_unique ----

#' Predict gender from first name
#'
#' This function predicts the gender of a first name.
#'
#' @param first_name first name as a character string.
#' @param year_min starting year of the period over which the prediction is computed.
#' @param year_max ending year of the period over which the prediction is computed.
#' @param freq return the probability of the first name being male
#' @return
#' The predicted gender based on the proportions of males and females with the input first name. Possible values are either "male" or "female". NA is returned when the input first name is unknown in the database.
#' If freq is set to TRUE, the function returns the probability of the first name being male.
#' @seealso
#' \code{\link{gender}}, \code{\link{is_male}}, \code{\link{is_female}}
#' @examples
#' gender_unique("Baptiste")
#' gender_unique("Henriette")
#' @import dplyr
#' @import magrittr

gender_unique <- function(first_name, year_min = 1900, year_max = 2017, freq = FALSE) {
  temp <- fn_fr %>% filter(fn == toupper(unaccent(first_name)) & year >= year_min & year <= year_max) %>%
    group_by(fn, sex) %>%
    summarise(nb =sum(count)) %>%
    mutate(pourcentage = nb / sum(nb) * 100) %>%
    filter(pourcentage > 50)
  if(freq) res <- ifelse(temp$sex == 1, temp$pourcentage/100, 1-temp$pourcentage/100) else res <- ifelse(temp$sex == 1, "male", "female")
  return(res)
}



# gender ----

#' Predict genders from first names
#'
#' This function predicts the genders of a vector of first names.
#'
#' @inheritParams gender_unique
#' @return
#' The predicted genders based on the proportions of males and females with the input first names. Possible values are either "male" or "female". NAs are returned when the input first names are unknown in the database.
#' If freq is set to TRUE, the function returns the probabilities of the first names being male.
#' @seealso
#' \code{\link{gender_unique}}, \code{\link{is_male}}, \code{\link{is_female}}
#' @examples
#' gender(c("Baptiste", "Henriette")
#' @export

gender <- function(firstname, year_min = 1900, year_max = 2017, freq = FALSE) as.vector(sapply(firstname, gender_unique, year_min = year_min, year_max = year_max, freq = freq))

# is_male ----

#' Predict whether the genders of first names are male.
#'
#' This function predicts whether the genders of first names are male.
#'
#' @param firstname First names as a character vector.
#' @return
#' Logical. TRUE if the input first name is male, FALSE otherwise. NA is returned when the first name is unknown in the database.
#' @seealso
#' \code{\link{gender_unique}}, \code{\link{gender}}, \code{\link{is_female}}
#' @examples
#' is_male(c("Baptiste", "Annick")
#' @export

is_male <- function(firstname, year_min = 1900, year_max = 2017) gender(firstname, year_min = year_min, year_max = year_max) == "male"

# is_female ----

#' Predict whether the genders of first names are male.
#'
#' This function predicts whether the genders of first names are female.
#'
#' @inheritParams is_male
#' @return
#' Logical. TRUE if the input first name is female, FALSE otherwise. NA is returned when the first name is unknown in the database.
#' @seealso
#' \code{\link{gender_unique}}, \code{\link{gender}}, \code{\link{is_male}}
#' @examples
#' is_female(c("Baptiste", "Annick")
#' @export

is_female <- function(firstname, year_min = 1900, year_max = 2017) gender(firstname, year_min = year_min, year_max = year_max) == "female"


# To do next :

# * Add probs or freq logical parameter to is_male and is_female to return
# probability instead of logical.




# AGE AND YEAR OF BIRTH ----



# year_unique ----

#' Predict year from first name
#'
#' This function predicts the year of birth from a first name.
#'
#' @param first_name first name as a character string.
#' @param year_min starting year of the period over which the prediction is computed.
#' @param year_max ending year of the period over which the prediction is computed.
#' @return
#' The predicted year of birth based on the proportions of individuals with the input first name born each year. The function returns the mode of the distribution, i.e. the year with the highest number of individuals born by the input firstname. NA is returned when the input first name is unknown in the database.
#' @examples
#' year("Baptiste")
#' year("Henriette")
#' @import dplyr
#' @import magrittr

year_unique <- function(first_name, year_min = 1946, year_max = 2017) {
  temp <- fn_fr %>% filter(fn == toupper(unaccent(first_name)) & year >= year_min & year <= year_max) %>%
    group_by(firstname, year) %>%
    summarise(nb =sum(count)) %>%
    filter(nb == max(nb))
  res <- temp$year
  return(res)
}


# year ----

#' Predict years of birth from first names
#'
#' This function predicts the years of birth of individuals from a vector of first names.
#'
#' @inheritParams year_unique
#' @return
#' The predicted years of birth based on the proportions of individuals with the input first names born each year. The function returns the modes of the distributions, i.e. the years with the highest numbers of individuals born by the input firstnames. NAs are returned when the input first names are unknown in the database.
#' @examples
#' year(c("Baptiste", "Henriette")
#' @export

year <- function(firstname, year_min = 1946, year_max = 2017) as.vector(sapply(firstname, year_unique, year_min = year_min, year_max = year_max))

# Quand deux modes parfaits, lequel choisir ?



