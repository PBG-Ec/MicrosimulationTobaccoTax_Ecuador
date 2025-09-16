# MicrosimulationTobaccoTax_Ecuador

R implementation of Maldonado (2022) model, evaluating the effect of tobacco tax increase in Ecuador on multiple Sustainable Development Goals (SDGs).
Based on Extended Cost-Effectiveness Analysis for Health Policy Assessment (ECEA, see Vergeut 2016) and microsimluations, this model builds and analyses synthetic population data to explore the effect of tax increase on tobacco.

## Microsimulation Approach

- **Artificial Society Construction:**  
Build a synthetic population that mirrors demographic and socioeconomic characteristics.

- **Tax Scenario Simulated:**  
  A range of tobacco tax increase is modeled based on different excise tax.

- **Behavioral Response Modeling:**  
  The simulation incorporated tobacco price elasticity (Mena 2024) to predict how different income groups would respond.

- **Elasticity Split:**  
  - 50% of the response would be smokers quitting (extensive margin)
  - 50% would be reduced cigarette consumption among continuing smokers (intensive margin)


## Data Sources & Assumptions

- **Synthetic Dataset vs. Real Survey Data:**  
  The study used a synthetic dataset from Ecuador Survey.

- **Healthcare costs and use **  
  HC costs and use were compiled or borrowed from prior studies and.

##  Impact Analysis

The simulation tracked effects across multiple domains, including:
- Health
- Poverty
- Gender Equality
- Reduced Inequalities


### Key Outputs Modeled

- Reduction in smoking consumption behaviours 
- Lower healthcare costs and catastrophic expenditures
- Improved equity and domestic resource mobilization
