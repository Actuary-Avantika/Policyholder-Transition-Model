# Policyholder Transition Model

### Discrete-Time Markov Chain | Insurance Portfolio

An interactive R Shiny application that demonstrates how a discrete-time Markov Chain can be used to model and project movements within an insurance policyholder portfolio.

## Live Demo

Coming soon — this project will be deployed with Shinylive and GitHub Pages.

## Project Overview

The model represents a policyholder portfolio using four states:

- **Active** — policy remains in force
- **Claim** — policyholder is in a claim-related state
- **Lapsed** — policy has lapsed
- **Closed** — absorbing state in the current model

The application uses a transition matrix to calculate how the portfolio evolves over time.

### Core mathematical framework

The model uses the n-step transition matrix:

**Pⁿ = P × P × ... × P**

and the portfolio projection:

**πₙ = π₀Pⁿ**

The application also demonstrates stationary distributions, absorbing states, and sensitivity analysis around lapse assumptions.

## Features

- Overview of the actuarial problem and Markov Chain framework
- Model Inputs and transition matrix display
- Portfolio projection over a selectable horizon
- n-step transition matrix analysis
- Interpretation of Active → Active and Active → Closed probabilities
- Stationary distribution analysis
- Absorbing-state identification
- Scenario analysis for high, base and low lapse assumptions
- Portfolio-distribution visualisations using `ggplot2`
- Methodology, assumptions and limitations section
- Professional dashboard-style Shiny interface

## Technology

- **R**
- **Shiny**
- **ggplot2**
- **Markov Chains / Discrete-Time Markov Chains**
- **Actuarial modelling concepts**
- **Data visualisation**
- **Shinylive** (deployment)
- **GitHub Pages** (planned live hosting)

## Important modelling note

The transition probabilities in this project are **hypothetical** and are not presented as actual insurer experience data. In a real actuarial application, transition probabilities could be estimated from historical policyholder experience and segmented by relevant characteristics and time periods.

## Running locally

Install the required R packages:

```r
install.packages(c("shiny", "ggplot2"))
```

Then open `app.R` in RStudio and run:

```r
shiny::runApp()
```

## Project purpose

This project was developed as an actuarial/data-analytics portfolio project to demonstrate:

1. Markov Chain understanding
2. Actuarial and insurance business thinking
3. R programming
4. Shiny application development
5. Mathematical modelling
6. Data visualisation
7. Scenario analysis
8. Professional communication of quantitative results

## Author

**Avantika Vashisht**

Actuarial Science | Mathematical Modelling | R & Data Analytics
