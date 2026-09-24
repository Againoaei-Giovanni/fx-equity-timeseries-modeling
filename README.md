# Daily Time-Series & Volatility Modeling: BET Index vs. EUR/RON (2006–2025)

An empirical financial time-series investigation analyzing the short- and long-term dynamic interactions, volatility clustering, and shock transmission between the Romanian equity market (BET Index) and the foreign exchange market (EUR/RON).

---

## Project Overview & Dataset
* **Data Frequency:** Daily trading data covering February 2006 – February 2025 (~4,760 observations) sourced from Bloomberg.
* **Core Series:** Bucharest Stock Exchange Index (`BET`) and the Romanian Leu vs. Euro exchange rate (`EUR/RON`).
* **Transformations:** Logarithmic transformation and first-differencing to derive stationary daily log-returns.

---

## Methodological Workflow

### 1. Exploratory Diagnostics & STL Decomposition
* Evaluated macro trends, seasonality, and irregular components via Robust STL Decomposition.
* Identified significant leptokurtosis (Kurtosis: 15.73 for BET, 23.09 for FX) and pronounced *volatility clustering* during major crisis regimes (2008–2009 Global Financial Crisis, 2020 COVID-19 shock).

### 2. Stationarity Profiling
* Conducted unit root and stationarity tests: Augmented Dickey-Fuller (ADF), Phillips-Perron (PP), and KPSS tests.
* Confirmed both price series are integrated of order one, $I(1)$ in levels, and strictly stationary $I(0)$ in logarithmic returns.

### 3. Mean Modeling (ARIMA Architecture)
* Evaluated candidate specifications across training (first ~4,500 obs) and out-of-sample test sets (252 obs).
* **Selected Best Models:**
  - `BET Index`: **ARIMA(2,0,2)** (AIC: -25,802.58; outperforming candidate specs with lowest out-of-sample MAPE).
  - `EUR/RON`: **ARIMA(0,0,1)** (AIC: -39,184.23; lowest RMSE/MAE on test partition).
* Validated absence of weekly calendar seasonality (SARIMA refuted via ACF inspection and Kruskal-Wallis tests).

### 4. Volatility & Risk Modeling (ARMA-GARCH)
* Confirmed heteroscedasticity and autoregressive conditional volatility via ARCH-LM diagnostics ($p < 0.001$).
* Fitted **ARMA(2,2)-GARCH(1,1)** for BET and **ARMA(0,1)-GARCH(1,1)** for FX under a Student's $t$ innovation distribution.
* **Findings:** Extreme volatility persistence ($\alpha_1 + \beta_1 \approx 0.988$ for BET, $\approx 0.999$ for FX), capturing prolonged volatility half-life and fat-tail risk.

### 5. Cointegration & Vector Autoregression (VAR)
* **Johansen Cointegration:** Both Trace and Maximum Eigenvalue tests failed to reject the null hypothesis of $r = 0$, confirming **no long-run equilibrium cointegrating vector** between equity and currency levels.
* **VAR(10) System Modeling:**
  - Optimal lag $p = 10$ established via AIC/FPE criteria.
  - Impulse Response Functions (IRF; 95% bootstrap CI, 20-day horizon) identified a statistically significant **unidirectional transmission channel**: unexpected exchange rate depreciation shocks induce a persistent, negative contraction in BET equity returns, whereas equity market shocks exert zero impact on exchange rate stability.

---

## Tech Stack & Libraries
* **Language:** R
* **Core Libraries:** `tidyverse`, `forecast`, `tseries`, `urca`, `rugarch`, `vars`, `FinTS`, `ggplot2`
