# Value at Risk and Expected Shortfall Estimation for Amazon Stock (2002–2024)  

## Project Description  
This project focuses on estimating **99% Value at Risk (VaR)** and **99% Expected Shortfall (ES)** for Amazon.com Inc. (AMZN.US) daily stock returns using historical data from **2002-01-02 to 2024-12-31**.  
The analysis applies three approaches — **Historical Method**, **Weighted Historical Method**, and **EWMA (Exponentially Weighted Moving Average)** — and evaluates their performance through standard **backtesting procedures**.  

Dataset:  
- Source: [stooq.pl](https://stooq.pl)  
- Variables: Date, Open, High, Low, Close, Volume  
- Observations: 5786 trading days  

## Methodology  
1. **Data Processing**  
   - Computed log returns.  
   - Defined losses as negative log returns.  
   - Used 500-day rolling window for risk estimation.  

2. **Risk Estimation Methods**  
   - Historical Simulation  
   - Weighted Historical Simulation  
   - EWMA (Exponentially Weighted Moving Average)  

3. **Backtesting**  
   - **Kupiec Test** → evaluates unconditional coverage of VaR.  
   - **Traffic Light Test (Basel II)** → assesses number of VaR breaches.  
   - **Christoffersen Test** → checks independence of exceedances.  

## Results & Insights  
- **Historical Method** performed best in Kupiec and Traffic Light tests, showing stability but limited flexibility.  
- **EWMA** performed best in the Christoffersen test, capturing market dynamics but showing higher instability.  
- **Weighted Historical Method** ranked second across all tests, offering the best balance between stability and adaptability.  

**Conclusion:** The **Weighted Historical Method** is the most suitable for Amazon’s stock data, as it better reflects changing market conditions while maintaining strong backtesting results.
