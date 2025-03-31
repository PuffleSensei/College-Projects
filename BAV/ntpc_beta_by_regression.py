# -*- coding: utf-8 -*-
"""NTPC Beta By Regression.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/11XCyGr8G2As215im0-cFF4j7qUz4ul6v
"""

#Install required libraries
!pip install yfinance
import matplotlib.pyplot as plt
import statsmodels.api as sm
import yfinance as yf
import pandas as pd
import numpy as np
import seaborn as sns
import scipy.stats as stats

#Download stock prices for the security monthly data last 5 years
NTPC_df = yf.download('NTPC.NS',
                      start='2019-03-01',
                      end='2024-03-01',
                      progress=False,interval ='1mo'
)
NTPC_df.head()

#Download Market index closing prices
nifty_df = yf.download('^NSEI',start='2019-03-01',
                      end='2024-03-01',
                      progress=False,interval ='1mo',
)
nifty_df.head()

#Calculate Log returns
nifty_df['Return_NIFTY'] = np.log(nifty_df['Adj Close']/nifty_df['Adj Close'].shift(1))
print(nifty_df['Return_NIFTY'])

nifty_df['Return_NIFTY'].plot(figsize=(8,5))
plt.show()

nifty_df['Adj Close'].plot(figsize=(8,5))
plt.show()

nifty_df1 = nifty_df.dropna(axis=0)
nifty_df1.head()

#Caclulate log returns
NTPC_df['Return_NTPC'] = np.log(NTPC_df['Adj Close']/NTPC_df['Adj Close'].shift(1))
print(NTPC_df['Return_NTPC'])

NTPC_df1 = NTPC_df.dropna(axis=0)
NTPC_df1.head()

NTPC_df['Return_NTPC'].plot(figsize=(8,5))
plt.show()

NTPC_df['Adj Close'].plot(figsize=(8,5))
plt.show()

NTPC_df1_returns = NTPC_df1['Return_NTPC']
NTPC_df1_returns.head()

nifty_df1_returns = nifty_df1['Return_NIFTY']
nifty_df1_returns.head()

returns = pd.merge(nifty_df1_returns,NTPC_df1_returns,how='inner',on='Date')
returns.head()
returns.describe()

sns.pairplot(returns)
plt.show()

sns.heatmap(returns.corr(), annot = True, cmap="YlGnBu")
plt.show()

x = returns['Return_NIFTY']
y = returns['Return_NTPC']
x_sm = sm.add_constant(x)

"""#Assumptions of OLS regression
##A linear relationship between the dependent and independent variables - The linear regression model is linear in parameters
##The independent variables are not highly correlated with each other - no multicollinearity
##The variance of the residuals is constant - no heteroskedasticity
##Random sampling of observation and no autocorrelation
##Assumption of Normality of Errors

##Ri = αi + βi(RM) + εi.........Market model

• Ri = Return of security i

• αi = The return from the asset that is not related to the market’s return. This is “alpha” return from the security

• βi = Beta or the return from the security explained by the market index’s return

• RM = The market index’s return

• εi = Error term for past returns not explained by the regression equation
"""

#Regression - Market model
model = sm.OLS(y,x_sm)

"""#OLS estimation or Ordinary Least square Minimizes the sum of squared errors"""

results = model.fit()

#Regression result
results.summary()

"""#Beta for NTPC = 0.888 and significant at 5%

"""

#Regression - Security Characteristic line
returns.info()
sns.regplot(x = 'Return_NIFTY', y = 'Return_NTPC', data = returns)

print('resid', results.resid)

#Histogram of residuals
sns.distplot(results.resid)
results.resid.describe()

#Checking for Homoskedasticity of errors (Assumption requirement of linear regression)
#perform White's test
from statsmodels.stats.diagnostic import het_white
white_test = het_white(results.resid,  results.model.exog)

#define labels to use for output of White's test
labels = ['Test Statistic', 'Test Statistic p-value', 'F-Statistic', 'F-Test p-value']

#print results of White's test
print(dict(zip(labels, white_test)))

"""#The errors are homoskedastic as per the White test

"""

#Testing for Normality of errors (Assumption requirement of linear regression)
sm.qqplot(results.resid,line='45',fit=True,dist=stats.norm)

stats.normaltest(results.resid)

"""We can see that since we are plotting the data with the theoretical quantiles of a normal distribution, we are getting almost a straight line, though some points are not exactly on the line.
The D’Agostino’s K-squared test result suggests that the null hypothesis that the distribution is normal cannot be rejected at 1% significance level.
"""