# Quantitative Equity Research Terminal

A professional dark-theme equity analytics dashboard built in **R Shiny**, powered by **Yahoo Finance** data via `quantmod`.  
This tool enables users to select a stock, a date range, and a rolling window to analyze key performance metrics with interactive charts and tables.

---

## 🚀 Features
- **Data Source**: Yahoo Finance via `quantmod`
- **Metrics**: Annualised return, volatility, Sharpe ratio, max drawdown, total return
- **Risk Analysis**: Value-at-Risk (VaR), Expected Shortfall
- **Distribution Stats**: Skewness, Kurtosis
- **Visualizations**:
  - Price series
  - Daily log returns
  - Rolling volatility & Sharpe ratio
  - Cumulative returns
  - Drawdowns
  - Return distribution & Q-Q plot
- **Performance Table** summarizing key statistics

---

## 📊 Example Dashboard
Live demo: [Quantitative Terminal](https://datamexlabs.shinyapps.io/QuantitativeTerminal/)  

Example metrics for **Apple Inc. (AAPL)**:
- Last Price: $254.23  
- Annualised Return: +17.12%  
- Annualised Volatility: +28.77%  
- Sharpe Ratio: 0.595  
- Max Drawdown: -40.37%  
- Total Return: +484.9%  

---

## ⚙️ Installation
Clone the repository:
```bash
git clone https://github.com/humbertohr/QuantitativeResearchTerminal.git
cd QuantitativeResearchTerminal
