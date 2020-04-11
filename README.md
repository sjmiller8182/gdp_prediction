# Predicting the next recession

## Overview

Economic recessions are periods of time when an economy shinks.
These periods of time generally costly to businesses and the populace alike.
Deep recessions can be particularly costly to the populace as business downsizing and business failures during recessions generally result in a decrease in available jobs (increasing unemployment).
However, if it was possible to predict a coming recession with some confidence, then it may be possible for business and the populace to prepare and mitigate losses.

We propose to model the change in GDP for the United States to attempt to predict recessions.
A working definition of a recession is two consecutive quarters of decrease in GDP.
Thus, we will use a 2-step ahead forecast in evaluating models.
50 quarters of historical data will be used for training models to predict the next 2 quarters.

## Conclusion
* Various ARMA, VAR and Neural Network models were developed.
* GDP data was very noisy and overall, models were not able to capture variance in this data.
* The univariate model AR(2) performed better than VAR and MLP models.
* An ensemble model appeared to improve the forecasts, but further analysis should be performed (with sliding ASE) since our test dataset consisted of only 2 data points (unfortunately, we did not have enough data to perform this analysis).
* Addition of other exogenous variables with even stronger cross correlations may improve performance of multivariate models.
* In addition, it may be worthwhile to to look for some leading indicators of recession since GDP tends to be a lagging indicator.

## References

 1. Jim Chappelow, Recession,  Investopedia. Accessed March 6, 2020. https://www.investopedia.com/terms/r/recession.asp
 2. U.S. Bureau of Economic Analysis, Gross Domestic Product [A191RP1Q027SBEA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/A191RP1Q027SBEA, March 6, 2020.
 3. U.S. Bureau of Labor Statistics, All Employees, Total Nonfarm [PAYEMS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/PAYEMS, March 6, 2020.
 4. Board of Governors of the Federal Reserve System (US), 10-Year Treasury Constant Maturity Rate [DGS10], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DGS10, March 6, 2020.
 5. Board of Governors of the Federal Reserve System (US), Effective Federal Funds Rate [FEDFUNDS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/FEDFUNDS, March 6, 2020.
 6. U.S. Bureau of Economic Analysis, Real Disposable Personal Income [A067RO1Q156NBEA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/A067RO1Q156NBEA, March 6, 2020.
 7. U.S. Bureau of Labor Statistics, Consumer Price Index for All Urban Consumers: All Items in U.S. City Average [CPIAUCNS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/CPIAUCNS, March 6, 2020.
 8. U.S. Bureau of Economic Analysis, Population [POPTHM], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/POPTHM, March 6, 2020.
 9. U.S. Bureau of Economic Analysis, Corporate Profits After Tax (without IVA and CCAdj) [CP], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/CP, March 6, 2020.
 10. Federal Reserve Bank of St. Louis, Spot Crude Oil Price: West Texas Intermediate (WTI) [WTISPLC], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/WTISPLC, March 6, 2020.
 11. ICE Benchmark Administration Limited (IBA), Gold Fixing Price 10:30 A.M. (London time) in London Bullion Market, based in U.S. Dollars [GOLDAMGBD228NLBM], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/GOLDAMGBD228NLBM, March 6, 2020.
 12. Board of Governors of the Federal Reserve System (US), Japan / U.S. Foreign Exchange Rate [EXJPUS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/EXJPUS, March 6, 2020.
 13. Board of Governors of the Federal Reserve System (US), U.S. / U.K. Foreign Exchange Rate [EXUSUK], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/EXUSUK, March 6, 2020.
 14. Wilshire Associates, Wilshire 5000 Total Market Full Cap Index [WILL5000INDFC], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/WILL5000INDFC, March 26, 2020.
 15. Federal Reserve Bank of St. Louis, Real Manufacturing and Trade Inventories [INVCMRMTSPL], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/INVCMRMTSPL, March 26, 2020.
 16. U.S. Census Bureau and U.S. Department of Housing and Urban Development, New Private Housing Units Authorized by Building Permits [PERMIT], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/PERMIT, March 26, 2020.
 17. U.S. Census Bureau, Homeownership Rate for the United States [RHORUSQ156N], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/RHORUSQ156N, March 26, 2020.
 18. Board of Governors of the Federal Reserve System (US), Industrial Production Index [INDPRO], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/INDPRO, March 26, 2020.
 19. Federal Reserve Bank of St. Louis, 10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity [T10Y3M], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/T10Y3M, March 26, 2020.
