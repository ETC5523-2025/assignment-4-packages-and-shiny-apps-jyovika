
# haiInsight

Tools and an interactive app to explore **healthcare-associated
infection (HAI)** data derived from the BHAI package.  
This package was developed as part of *ETC5523: Communicating with Data*
at Monash University.

## `haiInsight` provides simple, reproducible tools for summarising and visualising infection data, including an interactive Shiny dashboard to explore infection trends by **infection type**, **age group**, and **sex**.

## Installation

You can install the development version from GitHub:

``` r
# Install dependencies if needed
install.packages(c("devtools", "shiny", "ggplot2", "dplyr", "bslib", "kableExtra"))

# Install from GitHub
devtools::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-jyovika")
```

Load the package:

``` r
library(haiInsight)
```

## Example usage

``` r
# Load included data
data("hai_data_clean")

# Summarise total cases by infection type
summarise_hai(hai_data_clean, by = "Infection_Type")

# Visualise totals by age group
plot_hai_totals(hai_data_clean, by = "Age_Group")
```

These functions let you quickly compare infection distributions across
groups - with built-in styling and optional proportional views.

## Launch the Shiny app

``` r
run_hai_app()
```

This opens an interactive interface to:

- Filter by **infection type** and **sex**

- View totals by **infection type**, **age group**, or **sex**

- Display both plots and summary tables with clear, styled output

## Learn more

For detailed examples and explanation, check the vignettes:

[Summarising HAI data](articles/summarising-hai.html) [Visualising
totals with `plot_hai_totals()`](articles/visualising-hai-totals.html)

## Acknowledgements

- Data adapted from the BHAI dataset (Germany, 2011).
- Zacher, Benedikt; Haller, Sebastian; Willrich, Niklas; Walter, Jan;
  Abu Sin, Muna; Cassini, Alessandro; Plachouras, Diamantis; Suetens,
  Carl; Behnke, Michael; Gastmeier, Petra; Wieler, Lothar H.; Eckmanns,
  Tim (2019). *Application of a new methodology and R package reveals a
  high burden of healthcare-associated infections (HAI) in Germany
  compared to the average in the European Union/European Economic Area,
  2011 to 2012.*  
  **Euro Surveillance**,
  24(46):1900135.<https://doi.org/10.2807/1560-7917.ES.2019.24.46.1900135>
- Developed by Jyovika Aswale for educational and demonstration
  purposes.

## 🔗 Useful links

- Website:
  <https://ETC5523-2025.github.io/assignment-4-packages-and-shiny-apps-jyovika>

- Source code: [GitHub
  Repository](https://github.com/ETC5523-2025/assignment-4-packages-and-shiny-apps-jyovika)

- Report an issue: [Submit
  here](https://github.com/ETC5523-2025/assignment-4-packages-and-shiny-apps-jyovika/issues)
