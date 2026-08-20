# Policyholder Transition Model

An interactive R Shiny application that models an insurance policyholder portfolio as a **discrete-time Markov Chain (DTMC)**, built as a portfolio project alongside actuarial exam preparation (CS2 covers Markov chains directly).

## What it does

Policyholders move between four states each period — **Active, Claim, Lapsed, Closed** — with `Closed` treated as an absorbing state. The app lets you:

- Inspect the assumed transition matrix and initial distribution
- See how such a matrix would actually be **estimated from data**: simulate a synthetic policyholder panel under the assumed matrix, then re-estimate it via the maximum-likelihood estimator for a time-homogeneous Markov chain, `p_hat(i,j) = N(i,j) / N(i,.)`
- **Project the portfolio** forward `n` years (`pi_n = pi_0 * P^n`) and download the results
- Inspect **n-step transition probabilities** (`P^n`)
- Study **long-run behaviour**: the stationary distribution (`pi*P = pi`), which states are absorbing, and the **expected time to absorption** via the fundamental matrix `N = (I - Q)^-1`
- Run **scenario/sensitivity analysis** on the Active → Lapsed assumption (high / base / low lapse), with input validation, a one-click reset, and CSV export
- See an **illustrative financial impact** figure (expected claim-state exposure × an assumed cost per claim period) — explicitly flagged as schematic, not a reserving calculation

## Project structure

```
.
├── global.R   # shared data + all helper functions (matrix power, stationary
│              # distribution, absorbing states, fundamental matrix / expected
│              # time to absorption, policyholder simulation, MLE estimation)
├── ui.R       # app layout, styling, and inputs/outputs for every tab
├── server.R   # reactive logic, validation, plots, tables, download handlers
└── README.md
```

Shiny automatically sources `global.R`, `ui.R` and `server.R` when the app is run from this folder — no separate `app.R` is needed.

## Running locally

```r
install.packages(c("shiny", "ggplot2"))
shiny::runApp("path/to/this/folder")
```

## Key assumptions & limitations

- The base transition matrix is hypothetical, not fitted to real insurer data (the Data & Estimation tab demonstrates the *method* using simulated data instead)
- Transition probabilities are assumed constant over the projection period (time-homogeneous chain)
- New business, economic conditions, and policyholder-level characteristics are not modelled
- The financial impact figure is a simplified illustration, not an actuarial reserve calculation

## Possible next steps

- Fit the transition matrix to a real or more realistic dataset
- Add policyholder segmentation (e.g. by product line or age band) as additional states or a mixture model
- Add confidence intervals around the estimated transition probabilities
- Deploy to [shinyapps.io](https://www.shinyapps.io/) for a live, no-install demo link

---
© 2026 Avantika Vashisht — Actuarial Markov Chain Project
