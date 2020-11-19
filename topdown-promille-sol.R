library(tidyverse)
library(checkmate)

# Auxiliary functions ----------------------------------------------------------

# Function that calculates absorbed mass of alcohol ----
calculate_alcohol_mass <- function(drinks) {
  0.8 * (1000 * drinks[["massn"]] * 0.06 +
         500 * drinks[["hoibe"]] * 0.06 +
         200 * drinks[["wein"]] * 0.11 +
         40 * drinks[["schnaps"]] * 0.4)
}

# Function that calculates total body water according to Whatson
calculate_body_water <- function(sex, height, weight, age) {
  if (sex == "male") {
    return(2.447 - 0.09516 * age + 0.1074 * height + 0.33612 * weight)
  }
  0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
}

# Function that calculates blood alcohol concentration
calculate_blood_alcohol <- function(alcohol_mass, body_water) {
  0.8 * alcohol_mass / (1.055 * body_water)
}

# Function that calculates time difference in hours
calculate_time_difference <- function(drinking_time) {
  difftime(drinking_time[[2]], drinking_time[[1]], units = "hours") %>% 
    as.numeric()
}

# Function that calculates the reduction of blood alcohol level during the
# drinking time
calculate_reduction <- function(drinking_time, blood_alcohol) {
  
  if (calculate_time_difference(drinking_time) < 1) {
    return(blood_alcohol)
  }
  
  blood_alcohol - ((calculate_time_difference(drinking_time) - 1) * 0.15)
}

--------------------------------------------------------------------------------

# Function that calculates promille
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight, 
                              drinking_time, drinks) {
  
  sex <- str_to_lower(sex)
  sex <- match.arg(sex)
  
  # For consistent data
  drinks <- as.list(unlist(drinks))
  
  # Checks
  assert_number(age, lower = 0, upper = 150, finite = TRUE)
  assert_number(height, lower = 0, upper = 300, finite = TRUE)
  assert_number(weight, lower = 0, upper = 600, finite = TRUE)
  assert_true(sex %in% c("male", "female"))
  assert_posixct(drinking_time, any.missing = FALSE, len = 2, sorted = TRUE)
  assert(check_list(drinks, types = "numeric", min.len = 1, max.len = 4),
         check_numeric(drinks, min.len = 1, max.len = 4), combine = "or")
  assert_names(names(drinks), subset.of = c("massn", "hoibe", "wein", "schnaps"))
  
  
  # Fill up missing drink types
  for (i in c("massn", "hoibe", "wein", "schnaps")) {
    if (is.null(drinks[[i]])) {
      drinks[[i]] <- 0
    }
  }
  
  # Not the smoothest way to solve the duplicates problem :(
  if (sum(duplicated(names(drinks))) > 0) {
    drinks_unlisted <- unlist(drinks)
    drinks <- list(
      "massn" = sum(drinks_unlisted[names(drinks_unlisted) == "massn"]),
      "hoibe" = sum(drinks_unlisted[names(drinks_unlisted) == "hoibe"]),
      "wein" = sum(drinks_unlisted[names(drinks_unlisted) == "wein"]),
      "schnaps" = sum(drinks_unlisted[names(drinks_unlisted) == "schnaps"]))
  }
  
  # Illegal?
  if (age < 16) {
    warning("illegal")
  }
  if (age < 18 & drinks[["schnaps"]] != 0) {
    warning("illegal")
  }
  
  # Calculate absorbed mass of alcohol
  alcohol_mass <- calculate_alcohol_mass(drinks)
  
  # Calculate total body water according to Whatson
  body_water <- calculate_body_water(sex, height, weight, age)
  
  # Calculate blood alcohol concentration
  blood_alcohol <- 
    calculate_blood_alcohol(alcohol_mass, body_water)

  # Calculate reduction of blood alcohol level during the drinking time
  reduction <- calculate_reduction(drinking_time, blood_alcohol)
  
  if (reduction < 0) {
    return(0)
  }
  
  reduction
  
}
