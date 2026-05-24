# DS Salary Analytics

An interactive R Shiny dashboard exploring the US data science job market using 742 real Glassdoor postings. Built to answer: *What role, skills, location, and company should I target to maximize my data science salary?*

---

## Overview

The dashboard is organized into five tabs:

| Tab | What It Shows |
|---|---|
| **Dashboard** | Salary landscape, role breakdown, seniority distribution |
| **Location** | Job density & salary by US state; local vs out-of-state pay |
| **Skills** | Skill demand %, salary premium, co-occurrence patterns |
| **Company** | How size, ownership type, rating & age affect pay |
| **Career Fit** | Personalised salary estimate + skill gap analysis |

---

## Dataset

**Jobs Dataset from Glassdoor** — thedevastator (Kaggle, 2022)  
742 postings · 38 US states · 24 sectors · 2017–2018  
[kaggle.com/datasets/thedevastator/jobs-dataset-from-glassdoor](https://www.kaggle.com/datasets/thedevastator/jobs-dataset-from-glassdoor)

Place the downloaded file as `salary_data_cleaned.csv` in the same folder as `app.R`.

---

## Requirements

Install the required R packages before running:

```r
install.packages(c(
  "shiny", "bslib", "bsicons",
  "ggplot2", "plotly", "dplyr",
  "stringr", "tidyr", "scales"
))
```

---

## Running the App

1. Clone or download this repository
2. Place `salary_data_cleaned.csv` in the project folder
3. Open `app.R` in RStudio
4. Click **Run App**, or run in the console:

```r
shiny::runApp()
```

## Shinyapps Link 

https://nutsthree.shinyapps.io/DS-Salary-LE/

---

## Notes

- Developed for academic purposes — Data Analytics with R
- Supports light and dark mode via the toggle in the top navbar
- Filters on Dashboard, Location, and Skills tabs are synced
- Company tab has its own independent filters
