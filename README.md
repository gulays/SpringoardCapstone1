# StockPricePrediction
Stock Price Prediction with Machine Learning Algorithms
This study uses machine learning techniques to predict the stock price pattern of a single stock. 
The goal is to predict whether the stock price will increase or decrease sometime in the future compared to its value on a given day. 
Algorithms that will be discussed are Linear Regression, Logistic Regression, LASSO, Random Forest Gradient Boosting Method and XGBOOST. 

Data Source: 
To apply different models and to compare their efficiencies, only one stock, AT&T (T) is chosen  from 1/1/2010 to 4/25/2017. 
The first data set contains daily stock information (Open, Low, High, Close, Volume, Adjusted Close, Dividend Date). 
It is extracted from Yahoo!Finance using pandas_datareader library. The dividends information is also extracted from Yahoo!Finance and 
the earning data is extracted from busystock.com.  

