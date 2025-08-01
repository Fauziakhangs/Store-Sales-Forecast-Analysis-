# Time-Series and Forecasting

**📌 Overview**
This project focused on forecasting store sales for a grocery chain in Ecuador using historical data from 2013 to 2017. The goal was to predict future sales trends and analyze the impact of promotions to support inventory management and marketing decisions. By uncovering sales patterns and identifying top-performing stores and product categories, the analysis provided valuable business insights.

**📊 Data Exploration**
The project began with exploratory data analysis, where our team visualized trends using bar charts, scatter plots, and QQ plots to understand data distribution and seasonality. After selecting a more consistent and informative dataset, we analyzed promotion effects and sales behavior across various stores and product types.

**💻 Forecasting Approach**
I led the modeling phase using R, applying advanced time series techniques. To ensure data stationarity and readiness for forecasting, I conducted ADF and KPSS tests, examined ACF/PACF plots, and used box plots to assess white noise. I applied log transformations and differencing where necessary and developed both manual and auto ARIMA models. To enhance seasonal accuracy, I implemented harmonic regression with Fourier terms, which effectively captured both long-term trends and regular fluctuations in sales.

![image](https://github.com/Fauziakhangs/Time-Series/blob/e5e27ba1f9e98bfb466c77dec830856da2265897/Time%20Series.PNG)

**🔍 Model Evaluation & Forecasting**
Using residual diagnostics and backtesting, I refined model performance to ensure reliability. The final forecast plot showed strong alignment with actual sales trends, highlighting predictable seasonal variations and a clear upward trajectory. The model provides a solid foundation for future sales forecasting, enabling more accurate planning and decision-making.

![image](https://github.com/Fauziakhangs/Time-Series/blob/c617a88cde907c51e67212d6089c7c89f40f0ad7/Forecasting.PNG)

**✅ Outcome**

This project significantly strengthened my skills in the time forecasting, model diagnostics, and collaborative project management I gained hands-on experience with transforming non-stationary data, constructing and validating ARIMA models, and generating forecasts that are both interpretable and actionable. The insights from this project can help optimize sales strategies and enhance data-driven decision-making fro retail businesses. 
