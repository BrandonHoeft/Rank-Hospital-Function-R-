# Function to search Hospitals based on state, disease mortality rate, and hospital rank.
# Author: Brandon Hoeft
# Date: August 30, 2015

#Dataset - 
setwd("~/Documents/R Files/R Programming (Coursera 2015)/Programming Assignment 3")

# Dataset background: The data for this assignment come from the Hospital Compare web site 
# (http://hospitalcompare.hhs.gov) run by the U.S. Department of Health and Human Services.

# Objective:
# Write a function called rankhospital that takes 3 arguments: the 2-character 
# abbreviated name of a state (state), an outcome (outcome), and the ranking of a hospital 
# in that state for that outcome (num). The function reads the outcome-of-care-measures.csv
# file and returns a character vector with the name of the hospital that has the ranking 
# specified by the num argument.

    

# rankhospital function ####

rankhospital <- function (state, outcome, num) {
    # read the HHS outcomes data file and columns needed.
    temp <- read.csv("outcome-of-care-measures.csv", colClass = "character")[, c(2, 7, 11, 17, 23)]
    names(temp)[3:5] <- c("heart attack", "heart failure", "pneumonia") # rename disease columns.
        
    # Check argument validity. Stop function if argument inputs invalid.
    if (all(temp$State != state)) {
        stop("invalid state")
    } else if (all(names(temp)[3:5] != outcome)) {
        stop("invalid outcome")
    } 
        
    # Suppress the warning for coercing non-numerics to NAs during as.numeric
    temp[, outcome] <- suppressWarnings(as.numeric(temp[, outcome])) #coerce char to number
        
    # Filter rows to "State" and remove diseases with unnavailable data. Keep name and outcome rate columns
    temp <- temp[temp$State == state & !(is.na(temp[, outcome])),  c("Hospital.Name", outcome)] 
    # order the functions first, by outcome, then by ASCENDING hospital.name to break any ties.
    ranking <- order(order(temp[, outcome], temp$Hospital.Name))
    temp <- cbind(temp, ranking)
    
    # allows the num argument in the function to take non-numeric inputs of "best" and "worst"
    if(num == "best") { num <- 1 }
    if (num == "worst") { num <- nrow(temp)}
    
    # If the value entered in num is larger than the number of hospitals in that state, 
    # then the function should return NA to indicate that is an invalid ranking.
    if (num <= nrow(temp)) {
    temp[temp$ranking == num, "Hospital.Name"]
    } else {
        return(NA)
    }
    
}

