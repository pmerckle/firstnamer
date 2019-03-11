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
  text <- gsub("['`^~\"]", "", string)
  return(text)
}


# gender.unique ----

#' Predict gender from first name
#'
#' This function predicts the gender of a first name.
#'
#' @param firstname first name as a character string.
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

gender_unique <- function(fn, freq = FALSE) {
  temp <- fn_fr %>% filter(firstname == toupper(unaccent(fn))) %>%
    group_by(firstname, sex) %>%
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
#' @param firstname First names as a character vector.
#' @param freq Return the probability of the first names being males.
#' @return
#' The predicted genders based on the proportions of males and females with the input first names. Possible values are either "male" or "female". NAs are returned when the input first names are unknown in the database.
#' If freq is set to TRUE, the function returns the probabilities of the first names being male.
#' @seealso
#' \code{\link{gender_unique}}, \code{\link{is_male}}, \code{\link{is_female}}
#' @examples
#' gender(c("Baptiste", "Henriette")
#' @export

gender <- function(firstname, freq = FALSE) as.vector(sapply(firstname, gender_unique, freq = freq))

# is.male ----

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

is_male <- function(firstname) gender(firstname) == "male"

# is.female ----

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

is_female <- function(firstname) gender(firstname) == "female"


# To do next :

# * Add probs or freq logical parameter to is_male and is_female to return
# probability instead of logical.



