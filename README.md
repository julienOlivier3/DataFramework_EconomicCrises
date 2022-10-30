# Integrated Data Framework for Policy Makers in Times of Economic Crises
Data analysis of corporate website data, business survey data and corporate credit ratings supporting the findings in the PLoS ONE publication **"An integrated data framework for policy guidance during the coronavirus pandemic: Towards real-time decision support for economic policymakers"**, Dörr et al. ([2022](https://doi.org/10.1371/journal.pone.0263898)).

**Abstract**:<br/>
Usually, official and survey-based statistics guide policymakers in their choice of response instruments to economic crises. However, in an early phase, after a sudden and unforeseen shock has caused unexpected and fast-changing dynamics, data from traditional statistics are only available with non-negligible time delays. This leaves policymakers uncertain about how to most effectively manage their economic countermeasures to support businesses, especially when they need to respond quickly, as in the COVID-19 pandemic. Given this information deficit, we propose a framework that guided policymakers throughout all stages of this unforeseen economic shock by providing timely and reliable sources of firm-level data as a basis to make informed policy decisions. We do so by combining early stage 'ad hoc' web analyses, 'follow-up' business surveys, and `retrospective' analyses of firm outcomes. A particular focus of our framework is on assessing the early effects of the pandemic, using highly dynamic and large-scale data from corporate websites. Most notably, we show that textual references to the coronavirus pandemic published on a large sample of company websites and state-of-the-art text analysis methods allowed to capture the heterogeneity of the pandemic's effects at a very early stage and entailed a leading indication on later movements in firm credit ratings. While the proposed framework is specific to the COVID-19 pandemic, the integration of results obtained from real-time online sources in the design of subsequent surveys and their value in forecasting firm-level outcomes typically targeted by policy measures, is a first step towards a more timely and holistic approach for policy guidance in times of economic shocks.

**Files**:<br/>
<code> setup.R</code> serves to set up working directory, visualization layouts, ... .
<code> webdata.R</code>, <code> survey.R</code> and <code> ratings.R</code> read and clean the respective sources of firm-level data. 
<code> firm_characteristics.R</code> prepares firm characteristics such as industry affiliation, size and age from the [Mannheim Enterprise Panel](https://www.zew.de/en/research-at-zew/the-mannheim-enterprise-panel) for the relevant firm IDs.
<code> impact_analysis.R</code> conducts estimations and prepares visualizations as shown in the working paper.
<code> classifier_performance.ipynb</code> analyzes the performance of the text classification on a test set of co-labeled text references.

**Data**:<br/>
Credit rating data, survey responses and classified webdata are deposited in the ZEW Research Data Center (ZEW-FDZ) and will be made available upon reasonable request from the corresponding author.

**Presentation**:<br/>
Presentation slides of the paper can be found [here](https://raw.githack.com/julienOlivier3/DataFramework_EconomicCrises/main/pres.pdf).
