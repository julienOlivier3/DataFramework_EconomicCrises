# DataFramework_EconomicCrises
Data analysis of corporate website data, business survey data and corporate credit ratings supporting the findings in the paper:

Julian Oliver Dörr, Jan Kinne, David Lenz, Georg Licht and Peter Winker (2021). An Integrated Data Framework for Policy Guidance in Times of Dynamic Economic Shocks.

<code> setup.R </code> serves to set up working directory, visualization layouts, ...
<code> webdata.R </code>, <code> survey.R </code> and <code> ratings.R </code> read and clean the respective sources of firm-level data. 
<code> firm_characteristics.R </code> prepares firm characteristics such as industry affiliation, size and age from the [Mannheim Enterprise Panel](https://www.zew.de/en/research-at-zew/the-mannheim-enterprise-panel) for the relevant firm IDs.
<code> impact_analysis.R </code> conducts estimations and prepares visualizations as shown in the paper.

Credit rating data, survey responses and classified webdata are deposited in the ZEW Research Data Center (ZEW-FDZ) and will be made available upon reasonable request from the corresponding author.
