# Data

The data used in this analysis was collected from 
[Federal Reserve Economic Data (FRED) Repository](https://fred.stlouisfed.org/),
which is provided by the Federal Reserve Bank of Saint Louis.

Quarterly observations of change in GDP and 18 exogenous variables were collected from 1982 Q1 to 2019 Q3.

The variables are summerized in the table below.

## Data Description

| Variable | Description | FRED ID |
|----------|-------------|---------|
| `date`     | Date of observation | N/A |
| `gdp_change` | Change in GDP from the previous observation | A191RP1Q027SBEA |
| `unrate`   | Unemployment rate | UNRATE |
| `nfjobs`   | Non-farming jobs  | PAYEMS | 
| `treas10yr` | 10 Year US treasury constant maturity rate | DGS10 |
| `fedintrate` | US federal interest rate | FEDFUNDS |
| `personincomechg` | Change in real disposable personal income | A067RO1Q156NBEA |
| `cpichg` | Change in Consumer Price Index for all urban consumers: all ttems in U.S. city average | CPIAUCNS |
| `popchg` | Change in Population | POPTHM |
| `corpprofitchg` | Change in Corporate profits after tax (converted to percent change) | CP |
| `crude_wtichg` | Change in Spot Crude Oil Price: West Texas Intermediate | WTISPLC |
| `goldchg` | Change in Gold Fixing Price 10:30 A.M. (London time) in london bullion market, based in U.S. Dollars | GOLDAMGBD228NLBM |
| `ppichg` | Change in Producer price index | PPIACO |
| `japanchg` | Change in US/Japan exchange rate | EXJPUS | 
| `ukchg` | Change in US/UK exchange rate | EXUSUK |
| `wilshirechg` | Change in Wilshire 5000 Total Market Full Cap Index | WILL5000INDFC |
| `ipichg` | Change in Industrial Production Index | INDPRO |
| `inventorieschg` | Change in Real Manufacturing and Trade Inventories | INVCMRMTSPL |
| `homeownership` | Cahnge in Homeownership Rate for the United States | RHORUSQ156N |
| `housingpermitschg` | Change in New Private Housing Units Authorized by Building Permits | PERMIT |
| `treas10yr3mo` | 10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity | T10Y3M |

## Data References

 * U.S. Bureau of Economic Analysis, Gross Domestic Product [A191RP1Q027SBEA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/A191RP1Q027SBEA, March 6, 2020.
 * U.S. Bureau of Labor Statistics, All Employees, Total Nonfarm [PAYEMS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/PAYEMS, March 6, 2020.
 * Board of Governors of the Federal Reserve System (US), 10-Year Treasury Constant Maturity Rate [DGS10], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DGS10, March 6, 2020.
 * Board of Governors of the Federal Reserve System (US), Effective Federal Funds Rate [FEDFUNDS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/FEDFUNDS, March 6, 2020.
 * U.S. Bureau of Economic Analysis, Real Disposable Personal Income [A067RO1Q156NBEA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/A067RO1Q156NBEA, March 6, 2020.
 * U.S. Bureau of Labor Statistics, Consumer Price Index for All Urban Consumers: All Items in U.S. City Average [CPIAUCNS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/CPIAUCNS, March 6, 2020.
 * U.S. Bureau of Economic Analysis, Population [POPTHM], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/POPTHM, March 6, 2020.
 * U.S. Bureau of Economic Analysis, Corporate Profits After Tax (without IVA and CCAdj) [CP], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/CP, March 6, 2020.
 * Federal Reserve Bank of St. Louis, Spot Crude Oil Price: West Texas Intermediate (WTI) [WTISPLC], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/WTISPLC, March 6, 2020.
 * ICE Benchmark Administration Limited (IBA), Gold Fixing Price 10:30 A.M. (London time) in London Bullion Market, based in U.S. Dollars [GOLDAMGBD228NLBM], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/GOLDAMGBD228NLBM, March 6, 2020.
 * Board of Governors of the Federal Reserve System (US), Japan / U.S. Foreign Exchange Rate [EXJPUS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/EXJPUS, March 6, 2020.
 * Board of Governors of the Federal Reserve System (US), U.S. / U.K. Foreign Exchange Rate [EXUSUK], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/EXUSUK, March 6, 2020.
 * Wilshire Associates, Wilshire 5000 Total Market Full Cap Index [WILL5000INDFC], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/WILL5000INDFC, March 26, 2020.
 * Federal Reserve Bank of St. Louis, Real Manufacturing and Trade Inventories [INVCMRMTSPL], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/INVCMRMTSPL, March 26, 2020.
 * U.S. Census Bureau and U.S. Department of Housing and Urban Development, New Private Housing Units Authorized by Building Permits [PERMIT], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/PERMIT, March 26, 2020.
 * U.S. Census Bureau, Homeownership Rate for the United States [RHORUSQ156N], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/RHORUSQ156N, March 26, 2020.
 * Board of Governors of the Federal Reserve System (US), Industrial Production Index [INDPRO], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/INDPRO, March 26, 2020.
 * Federal Reserve Bank of St. Louis, 10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity [T10Y3M], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/T10Y3M, March 26, 2020.

