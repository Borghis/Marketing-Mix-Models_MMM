# Marketing-Mix-Models-MMM-
To complete my Bachelor’s degree in Statistics for Economics and Business, I undertook an internship at a company where I learned the fundamental concepts of Marketing Mix Models and gained insight into the importance of optimising advertising expenditure to support data-driven decision-making.

To compute the models, I used the Robyn library.

Starting from the script Data_campaigns.R, I processed the dataset containing the company’s advertising expenditures across different platforms and advertisement types.
I performed data cleaning with the goal of making the process as automated as possible. Since the original data was provided in a format where each row represented a single advertisement, I aggregated the data into a tabular structure suitable for modeling, with the following format:

(date | plt1_advtype1 | plt1_advtype2 | plt2_advtype1 | plt2_advtype2).

The Data_preparation.R script was subsequently developed to integrate contextual variables with the campaign data produced in Data_campaigns.R. Considering the automotive market context, external macroeconomic and sector-specific indicators were included, such as interest rates, inflation rates, fuel prices, unemployment rates, vehicle registration time series, and vehicle trade volumes.
Additionally, this script was used to assess the correlation between the model features and the target variables (leads and revenue), supporting informed feature selection.

The MMM.R script was developed to estimate the Marketing Mix Models according to the Robyn framework. Model selection was carried out through an iterative tuning process, exploring multiple configurations of adstock transformations and saturation functions.
Model diagnostics were assessed using the plots provided by the Robyn package, complemented by custom visual analyses implemented in the Plots.R script.
