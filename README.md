## Purpose

This is an interactive application designed to explore and analyze child health data from the National Health Interview Survey. The app contains a set of health outcomes including asthma, ADHD, learning disabilities, mental health indicators, as well as multiple demographic categories such as sex, age, family structure, and more. This app also visualizaes health survey data, all of which comes from the public domain.

## How to use
- Extract this repo into a local folder
- Ensure you have the shiny library installed
- Ensure you have the ggplot2 library installed
- Ensure you have the dplyr library installed
- Run the app.R file 

## Data

The data used in this application comes from the National Health Interview Survey, which is one of the principal health data collection programs of the National Center for Health Statistics, part of the Centers for Disease Control and Prevention.

### Details:

- **Source:** National Health Interview Survey, National Center for Health Statistics, Centers for Disease Control and Prevention
- **Population:** Children less than 18 years in the U.S.
- **Data Year:** Mostly 2019 data

### Variables in the Dataset:

- **Outcome (or Indicator):**
  - Ever having asthma
  - Current asthma
  - Ever having attention-deficit/hyperactivity disorder (ADHD)
  - Ever having a learning disability
  - Fair or poor health status
  - Has a usual place of care
  - Receipt of influenza vaccination
  - Delayed getting medical care due to cost
  - Prescription medication use
  - Two or more hospital emergency department visits
  - Missing 11 or more school days due to illness or injury

- **Group:**
  - **Sex:** Total, Male, Female
  - **Age Group:** 0-4 years, 5-11 years, 12-17 years
  - **Race/Ethnicity:** White only, Black only, Asian only, Hispanic, Non-Hispanic, Mexican or Mexican American, and etc.
  - **Family Structure:** Single parent (never married), Single parent (ever married), Married parents, Cohabiting parents, etc.
  - **Parental Employment:** No working parents, Single parent working, Two parents both working, etc.
  - **Parental Education:** Less than high school diploma, High school diploma or GED, Some college, College degree or higher
  - **Poverty Level:** Based on Federal Poverty Level (FPL) - Less than 100% FPL, 100% to less than 200% FPL, 200% and greater FPL
  - **Insurance Type:** Private, Medicaid or other state programs, Other government coverage, Uninsured
  - **Disability Status:** With disability, Without disability
  - **Geographic Region:** Northeast, Midwest, South, West, plus urban/rural classifications

- **Percentage:** The percentage of children in the specified group who have the health outcome or characteristic
- **Confidence Interval:** The 95% confidence interval for the percentage estimate
- **Title:** Title of each statistic
- **Description:** description of how the data was collected
- **Year:** Year of data collection

## References

- National Center for Health Statistics. NHIS Child Summary Health Statistics. Data accessed December 16, 2025. Available from https://data.cdc.gov/d/wxz7-ekz9.
