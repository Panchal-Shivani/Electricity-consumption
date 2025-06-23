# Time Series Forecasting: Germany's Weekly Electricity Consumption

## Project Overview

This project focuses on forecasting Germany's weekly electricity consumption using various time series modeling techniques. The analysis utilizes publicly available data from Open Power System Data (OPSD) spanning from 2006 to 2017, implementing multiple forecasting approaches to identify the most accurate model for energy planning and policy decision-making.[1]

## Dataset Information

### Data Source
- **Provider**: Open Power System Data (OPSD) via Kaggle[1]
- **Time Period**: 2006-2017
- **Original Format**: Daily electricity consumption data
- **Transformed Format**: Weekly aggregated consumption totals

### Data Preprocessing
The original daily consumption data was aggregated into weekly totals using Excel by:
- Grouping each day by the starting date of the week
- Summing weekly consumption values
- This transformation provided a smoother time series more suitable for modeling[1]

## Project Structure

```
├── Complete.R                 # Main analysis script
├── Book1.csv                 # Weekly electricity consumption data
└── README.md                 # This file
```

## Methodology

### 1. Exploratory Data Analysis
- **Time Series Visualization**: Plot of observed weekly electricity consumption (2006-2017)[1]
- **Component Decomposition**: STL decomposition to identify trend, seasonality, and irregular components[1]
- **Autocorrelation Analysis**: Assessment of predictability using ACF plots[1]
- **Random Walk Test**: AR(1) model fitting with z-test to confirm data is not a random walk[1]

### 2. Data Partitioning
- **Training Set**: 2006 to specified end period[1]
- **Validation Set**: 208 weeks for model validation[1]
- **Total Observations**: 624 weekly observations (52 weeks × 12 years)[1]

### 3. Forecasting Models Implemented

#### Model 1: Holt-Winters Exponential Smoothing
- Triple exponential smoothing capturing trend and seasonality[1]
- Automatic parameter optimization
- Forecast horizon: 52 weeks

#### Model 2: Moving Average on Residuals
- Trailing moving average (k=4) applied to residuals[1]
- Combined with base forecasting methods
- Enhanced accuracy through residual modeling

#### Model 3: Seasonal Regression Model
- Linear regression with seasonal dummy variables[1]
- `tslm(train.ts ~ season)` implementation
- Captures systematic seasonal patterns

#### Model 4: Two-Level Combined Forecasting
- Integration of seasonal regression with trailing MA on residuals[1]
- `tot.fst.2level` combining multiple forecast components
- Enhanced accuracy through ensemble approach

#### Model 5: ARIMA Models
- **Auto ARIMA**: Automatic model selection using `auto.arima()`[1]
- **Seasonal ARIMA**: ARIMA(0,1,2)(0,1,1) model for full dataset[1]
- Residual diagnostics using ACF plots

## Key Features

### Technical Implementation
- **Libraries Used**: `forecast`, `zoo`, `ggplot2`[1]
- **Time Series Object**: 52-week frequency ts object (2006-2017)[1]
- **Visualization**: Both base R and ggplot2 implementations[1]
- **Model Comparison**: Comprehensive accuracy metrics

### Model Evaluation Metrics
- **RMSE** (Root Mean Square Error)
- **MAPE** (Mean Absolute Percentage Error)
- **MAE** (Mean Absolute Error)
- Comparison against naive and seasonal naive benchmarks[1]

## Code Highlights

### Time Series Creation
```r
elec.ts <- ts(data$Consumption,
             start = c(2006, 1), end = c(2017, 52), freq = 52)
```

### STL Decomposition
```r
elec.stl <- stl(elec.ts, s.window = "periodic")
autoplot(elec.stl, main = "Electricity Consumption Time Series Components")
```

### Combined Forecasting
```r
tot.fst.2level.elec <- elec.season.pred$mean + elec.ma.trail.res.pred$mean
```

## Results and Model Performance

The project implements comprehensive accuracy testing across all models using the `accuracy()` function, comparing:
- Holt-Winters exponential smoothing performance[1]
- Seasonal regression model accuracy[1]
- ARIMA model effectiveness[1]
- Combined forecasting approach results[1]
- Benchmark comparisons with naive methods[1]

## Visualizations

The project includes multiple visualization types:
- **Historical Data Plots**: Complete time series with custom formatting[1]
- **Decomposition Charts**: STL component analysis[1]
- **Forecast Plots**: Model predictions with confidence intervals[1]
- **Residual Analysis**: Autocorrelation diagnostics[1]
- **Model Comparison**: Overlay plots of different forecasting approaches[1]

## Requirements

### R Packages
```r
install.packages("forecast")
library(forecast)
library(zoo)
library(ggplot2)
```

### Data Requirements
- CSV file named `Book1.csv` with weekly consumption data[1]
- Column named `Consumption` containing numerical values[1]

## Usage

1. **Setup Environment**:
   ```r
   install.packages("forecast")
   library(forecast)
   library(zoo)
   library(ggplot2)
   ```

2. **Load Data**:
   ```r
   data <- read.csv("Book1.csv")
   ```

3. **Run Analysis**:
   Execute the complete R script to perform all analyses and generate forecasts[1]

4. **View Results**:
   The script generates multiple plots and accuracy metrics for model comparison[1]

## Key Findings

The analysis demonstrates that Germany's electricity consumption exhibits:
- **Strong Seasonality**: Clear weekly and annual patterns[1]
- **Predictable Structure**: Non-random walk behavior confirmed through statistical testing[1]
- **Forecast Accuracy**: Multiple models provide reliable predictions for energy planning[1]

## Applications

This forecasting model supports:
- **Energy Planning**: Strategic capacity planning for electricity providers[1]
- **Policy Decision-Making**: Data-driven insights for energy policy[1]
- **Resource Management**: Optimized electricity distribution planning[1]

## Future Enhancements

Potential improvements include:
- Integration of external variables (weather, economic indicators)
- Real-time forecasting capabilities
- Extended forecast horizons
- Machine learning ensemble methods

## License

This project is available for educational and research purposes. Please cite appropriately when using this work.

## Contact

For questions or collaboration opportunities, please contact the project authors through their respective institutional emails.

[1] https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/46181816/1461fafd-c77e-4d48-a002-8f83992ffd32/Complete.R
[2] https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/46181816/8a9c2421-9e0f-4168-8ff9-f5890104476a/Final-Project-Abstract.docx
