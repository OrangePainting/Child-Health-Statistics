# Helper functions for Child Health Statistics

clean_percentage = function(percent_string) {
  result = gsub("[^0-9.]", "", percent_string)
  as.numeric(result)
}

format_percentage = function(value) {
  ifelse(is.na(value) | value >= 777, "N/A", paste0(round(value, 1), "%"))
}

categorize_group = function(group_name) {
  
  if (group_name %in% c("Total", "Male", "Female")) {
    return("Sex")
  }
  age_categories = c("0 years", "1 year", "2 years", "3 years", "4 years", "5-9 years",
                     "10-14 years", "15-17 years", "0-4 years", "5-11 years", "12-17 years")
  if (group_name %in% age_categories) {
    return("Age Group")
  }
  race_categories = c("White only", "Black only", "Asian only", "Black and White",
                      "Hispanic", "Non-Hispanic", "Mexican or Mexican American",
                      "White only, non-Hispanic", "Black only, non-Hispanic",
                      "Other race, non-Hispanic", "American Indian or Alaska Native only",
                      "Native Hawaiian or Other Pacific Islander only",
                      "American Indian and Alaska Native and White")
  if (group_name %in% race_categories) {
    return("Race/Ethnicity")
  }
  
  family_categories = c("Two parents", "Single parent", "Cohabiting parents", "Adult guardian",
                        "Single parent (never married)", "Single parent (ever married)", 
                        "Married parents", "Other family structure")
  if (group_name %in% family_categories) {
    return("Family Structure")
  }
  
  employment_categories = c("Working", "Not working", "Part-time working")
  if (group_name %in% employment_categories) {
    return("Parental Employment")
  }
  
  education_categories = c("High school diploma", "GED", "Some college", "Associate degree",
                           "Bachelor degree", "Graduate degree", "Less than high school diploma",
                           "High school diploma or GED", "College degree or higher"
  )
  if (group_name %in% education_categories) {
    return("Parental Education")
  }
  
  poverty_categories = c("Below FPL", "100-199% FPL", "200%+ FPL",
                         "Less than 100% FPL", "100% to less than 200% FPL", "200% and greater FPL")
  if (group_name %in% poverty_categories) {
    return("Poverty Level")
  }
  
  insurance_categories = c("Private", "Medicaid", "Government", "Uninsured",
                           "Medicaid or other state programs", "Other government coverage")
  if (group_name %in% insurance_categories) {
    return("Insurance Type")
  }
  
  disability_categories = c("Has disability", "No disability", "Difficulty walking",
                            "With disability", "Without disability")
  if (group_name %in% disability_categories) {
    return("Disability Status")
  }
  
  geographic_categories = c("MSA", "Metro", "Northeast", "Midwest", "South", "West")
  if (group_name %in% geographic_categories) {
    return("Geographic")
  }
  
  social_categories = c("High vulnerability", "Moderate vulnerability", "Low vulnerability")
  if (group_name %in% social_categories) {
    return("Social Vulnerability")
  }
  
  "Other"
}
order_groups_by_category = function(groups, category) {
  if (category == "Age Group") {
    age_order = c("0-4 years", "5-11 years", "12-17 years")
    return(factor(groups, levels = age_order[age_order %in% groups]))
  } else if (category == "Sex") {
    sex_order = c("Total", "Male", "Female")
    return(factor(groups, levels = sex_order[sex_order %in% groups]))
  } else {
    return(factor(groups))
  }
}
