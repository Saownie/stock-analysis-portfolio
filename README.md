# 🌑 Obsidian Analytics: Institutional Portfolio Suite

![R](https://img.shields.io/badge/R-4.x-blue.svg?style=flat&logo=r)
![Shiny](https://img.shields.io/badge/Shiny-1.8-blue.svg?style=flat&logo=shiny)
![Status](https://img.shields.io/badge/Status-Live-success.svg)
![License](https://img.shields.io/badge/License-MIT-green.svg)

**Obsidian Analytics** is a high-performance quantitative finance terminal designed to bridge the gap between retail trading tools and institutional analytics. 

Engineered with a **Reactive Data Pipeline**, it integrates **Stochastic Modeling**, **Mean-Variance Optimization**, and **Systemic Risk Analysis** to provide actionable wealth management insights in a professional "Dark Mode" environment.

[**🚀 Launch Live Terminal**](https://saownie.shinyapps.io/stock_analysis_project/)

---

## 📸 Terminal Preview

![Dashboard Screenshot](https://via.placeholder.com/800x450/1a1a1a/2ecc71?text=Obsidian+Analytics+Dashboard+Preview)


---

## ✨ Core Modules

### 1. 📈 Historical Backtesting Engine
Visualizes the comparative growth of assets against institutional benchmarks.
* **Metric:** Cumulative Wealth Index (CWI) normalized to initial investment.
* **Benchmark:** Real-time comparison against the S&P 500 (SPY).
* **Tech:** Interactive `plotly` time-series with dynamic scaling.

### 2. 🧠 AI Optimizer (The Efficient Frontier)
Solves the asset allocation problem using **Modern Portfolio Theory (MPT)**.
* **Simulation:** Generates **2,000 random portfolios** via Monte Carlo approximation to visualize the solution space.
* **Optimization Target:** Identifies the **Max Sharpe Ratio** portfolio (The "White Diamond").
* **Output:** Delivers precise weight recommendations (e.g., *40% NVDA, 60% GLD*) to maximize risk-adjusted returns.

### 3. ⚠️ Risk Lab (Correlation Matrix)
Identifies hidden concentration risks and systemic exposure.
* **Metric:** Pearson Correlation Coefficient ($\rho$).
* **Visualization:** Color-coded Heatmap.
    * 🟩 **Bright:** High Correlation (Systemic Risk).
    * 🟥 **Dark/Red:** Negative Correlation (Effective Hedging).

### 4. 🔮 Future Forecast (Stochastic Modeling)
Projects probable future wealth ranges using **Geometric Brownian Motion (GBM)**.
* **Methodology:** Simulates **500 parallel market universes** over a user-defined horizon (1–10 years).
* **Drift & Shock:** Derived dynamically from the user's optimized portfolio stats ($\mu, \sigma$).
* **Confidence Intervals:**
    * **95th Percentile:** Best Case Scenario.
    * **Median (50%):** Expected Outcome.
    * **5th Percentile:** Worst Case (Value at Risk).

---

## 🧮 Mathematical Framework

### The "Log-Return" Transformation
Raw prices ($P_t$) are converted into **Logarithmic Returns ($r_t$)** for statistical normality and time-additivity:
$$r_t = \ln\left(\frac{P_t}{P_{t-1}}\right)$$

### Geometric Brownian Motion (The Forecast Engine)
Future price paths are simulated using the stochastic differential equation solution:

$$S_{t} = S_{t-1} \times \exp\left( (\mu - \frac{1}{2}\sigma^2)\Delta t + \sigma \sqrt{\Delta t} Z_t \right)$$

**Where:**
* $\mu$ = Expected daily return (Drift).
* $\frac{1}{2}\sigma^2$ = Itô's Lemma adjustment for volatility drag.
* $Z_t$ = Random shock from a Standard Normal Distribution ($Z \sim N(0,1)$).

---

## 🛠️ Installation & Local Setup

### Prerequisites
* **R** (Version 4.0 or higher)
* **RStudio**

### 1. Clone the Repository
```bash
git clone [https://github.com/yourusername/obsidian-analytics.git](https://github.com/yourusername/obsidian-analytics.git)
