# firstnamer: gender()
#
# This is the function named 'gender' and its variations
# which get information about gender from first name
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



## DATA ----

# df

#' French first names 1900-2016
#'
#' A dataset containing the first name, sex and count of individuals born in
#' France for each year between 2000 and 2016 attributes of almost 54,000
#'
#' @format A data frame with 620994 rows and 4 variables: \describe{
#'   \item{sex}{Sex, with two levels, 1 or 2}
#'   \item{firstname}{First name}
#'   \item{year}{Year of birth}
#'   \item{count}{Number of individuals born that year}
#'   }
#' @source \url{https://www.insee.fr/fr/statistiques/2540004}
"df"

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
#' \code{\link{gender}}, \code{\link{is.male}}, \code{\link{is.female}}
#' @examples
#' gender_unique("Baptiste")
#' gender_unique("Henriette")

gender_unique <- function(fn, freq = FALSE) {
  temp <- df %>% filter(firstname == toupper(unaccent(fn))) %>%
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
#' \code{\link{gender_unique}}, \code{\link{is.male}}, \code{\link{is.female}}
#' @examples
#' gender(c("Baptiste", "Henriette")

gender <- function(firstname, freq = FALSE) sapply(firstname, gender_unique, freq = freq) %>% as.vector

# is.male ----

#' Predict whether the genders of first names are male.
#'
#' This function predicts whether the genders of first names are male.
#'
#' @param firstname First names as a character vector.
#' @return
#' Logical. TRUE if the input first name is male, FALSE otherwise. NA is returned when the first name is unknown in the database.
#' @seealso
#' \code{\link{gender_unique}}, \code{\link{gender}}, \code{\link{is.female}}
#' @examples
#' is_male(c("Baptiste", "Annick")

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
#' \code{\link{gender_unique}}, \code{\link{gender}}, \code{\link{is.male}}
#' @examples
#' is_female(c("Baptiste", "Annick")

is_female <- function(firstname) gender(firstname) == "female"



# To do :
# Add probs or freq logical parameter to is.male and is.female to return probability instead of logical



