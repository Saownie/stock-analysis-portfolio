# 📈 QuantDash: Institutional-Grade Portfolio Analytics

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-007BC2?style=for-the-badge&logo=rstudio&logoColor=white)
![Status](https://img.shields.io/badge/Status-Live-success?style=for-the-badge)

> **A high-performance financial dashboard for real-time portfolio backtesting, risk analysis, and wealth trajectory modeling.**

---

## 🚀 Live Demo
**[Launch the App](https://saownie.shinyapps.io/stock_analysis_project/)** *(Click to view the interactive dashboard)*

---

## 📸 Screenshots
| **Dark Mode Dashboard** | **Interactive Analytics** |
|:---:|:---:|
| ![Dashboard View](LINK_TO_YOUR_DASHBOARD_IMAGE.png) | ![Risk Metrics](LINK_TO_YOUR_METRICS_IMAGE.png) |

---

## 💼 Project Overview
**QuantDash** is a professional-grade financial analysis tool built to bridge the gap between static reports and interactive decision-making. Designed with a **"Wall Street First"** aesthetic, it leverages the `tidyquant` engine to ingest real-time market data and calculate institutional risk metrics instantly.

Unlike standard scripts, this application features a **Cyberpunk/Bloomberg-style UI** with glassmorphism effects, neon-accented data visualization, and a fully responsive layout for mobile analysis.

## ✨ Key Features
* **Real-Time Data Ingestion:** Fetches live OHLCV data from Yahoo Finance API.
* **Institutional Metrics:** Automatically calculates Annualized Return, Volatility (Std Dev), and Sharpe Ratio.
* **Interactive Visualization:** High-performance `plotly` charts with zoom, pan, and hover capabilities.
* **Multi-Asset Comparison:** Compare performance across Equities (AAPL), ETFs (SPY), and Crypto (BTC-USD) simultaneously.
* **Pro UI/UX:** Custom CSS-styled "Dark Mode" with glassmorphism cards and responsive layouts.

## 🛠️ Tech Stack
* **Core Logic:** R (Statistical Computing)
* **Frontend:** R Shiny, `bslib` (Modern Theming)
* **Financial Engine:** `tidyquant`, `PerformanceAnalytics`
* **Visualization:** `plotly`, `ggplot2`
* **Deployment:** ShinyApps.io

## 💻 How to Run Locally

```r
# 1. Clone the repository
git clone [https://github.com/yourusername/QuantDash.git](https://github.com/yourusername/QuantDash.git)

# 2. Open 'app.R' in RStudio

# 3. Install dependencies
install.packages(c("shiny", "tidyquant", "plotly", "bslib", "tidyverse"))

# 4. Run the App
shiny::runApp()
