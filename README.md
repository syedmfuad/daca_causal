# daca_causal 
Run difference-in-differences models for immigrants near the criteria cutoffs for DACA eligibility and investigate the causal effect of DACA on labor market outcomes of residents in the US. We find that DACA increases labor market outcome of the eligible immigrants, most notably reflected by increases in annual income and weekly wage both in the entire US sample and state subsamples. 

We then use an instrumental variable approach to test the effect the policy had on labor market outcomes for those who were ineligible. We show that DACA has a detrimental effect on the labor market outcomes of the DACA-ineligible population, including ineligible immigrants and the native-born citizens; this impact is often larger in absolute magnitude relative to the positive impact on DACA-eligible immigrants.

## DACA_data.R 
Cleaning individual-level data from ACS, filtering information, feature manipulation, identifying "potential" DACA-eligible population, and disaggregating by industry of employment. Running difference-in-differences models on entire population and state subsamples. 

## DACA_Plot.R 
Difference-in-differences plots

## DACA_eligible_IV.R 
IV models on DACA-ineligible population  

## DACA_IV.R 
IV models on entire population 

