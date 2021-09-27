# DataFramework_EconomicCrises
Data analysis of corporate website data, business survey data and corporate credit ratings supporting the findings in the paper **"An Integrated Data Framework for Policy Guidance in Times of Dynamic Economic Shocks"**, Dörr et. al ([2021](https://ftp.zew.de/pub/zew-docs/dp/dp21062.pdf)).

**Abstract**<br/>
Usually, official and survey-based statistics guide policy makers in their choice of response
instruments to economic crises. However, in an early phase, after a sudden and unforeseen
shock has caused incalculable and fast-changing dynamics, data from traditional statistics
are only available with non-negligible time delays. This leaves policy makers uncertain
about how to most effectively manage their economic countermeasures to support businesses,
especially when they need to respond quickly, as in the COVID-19 pandemic. Given this
information deficit, we propose a framework that guides policy makers throughout all stages
of an unforeseen economic shock by providing timely and reliable data as a basis to make
informed decisions. We do so by combining early stage ‘ad hoc’ web analyses, ‘follow-up’
business surveys, and ‘retrospective’ analyses of firm outcomes. A particular focus of our
framework is on assessing the early effects of the pandemic, using highly dynamic and largescale data from corporate websites. Most notably, we show that textual references to the
coronavirus pandemic published on a large sample of company websites and state-of-the-art
text analysis methods allow to capture the heterogeneity of the crisis’ effects at a very early
stage and entail a leading indication on later movements in firm credit ratings.

**Files**<br/>
<code> setup.R</code> serves to set up working directory, visualization layouts, ... .
<code> webdata.R</code>, <code> survey.R</code> and <code> ratings.R</code> read and clean the respective sources of firm-level data. 
<code> firm_characteristics.R</code> prepares firm characteristics such as industry affiliation, size and age from the [Mannheim Enterprise Panel](https://www.zew.de/en/research-at-zew/the-mannheim-enterprise-panel) for the relevant firm IDs.
<code> impact_analysis.R</code> conducts estimations and prepares visualizations as shown in the paper.

**Data**<br/>
Credit rating data, survey responses and classified webdata are deposited in the ZEW Research Data Center (ZEW-FDZ) and will be made available upon reasonable request from the corresponding author.
