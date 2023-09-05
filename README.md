# Estimating a two-way fixed effect models using Canadian matched employer-employee data

## About the repository

These programs can be used to estimate an AKM-style two-way fixed effects model using Canadian matched employer-employee data. Simulated data to test the code is provided, since the Canadian matched employer-employee data is not available to the public. 

## Background information

### AKM models

The AKM model assumes that the log of an individual's earnings can be decomposed into a the sum of a person effect, a firm effect, a time-varying index of individual characteristics, and a residual. (These models are called 'AKM' models thanks to the influential work by Abowd, Kramarz, and Margolis 1999.) 

Formally, the AKM model posits that the log earnings of individual $i$ at firm $j$ at time $t$ can be written as:

$y_{ijt} = \alpha_i + \psi_j + \beta'X_{it} + \varepsilon_{ijt},$

where $\alpha_i$ is the person effect for individual $i$, $\psi_j$ is the firm effect for firm $j$, $X_{it}$ is a vector of time-varying characteristics for individual $i$ at time $t,$ $\beta$ is a conformable vector of coefficients, and $\varepsilon_{ijt}$ is the residual. Included in the vector $X_{it}$ are variables for marital status, province of residence, year effects, and controls for age (more details below).

### Identification

The firm effects are identified for firms that are "connected". Two firms are "connected" if there exists a worker who moves between them. In addition, for the estimator of the firm effects to be unbiased, firm-to-firm mobility must be uncorrelated with time-varying unobservables. 

To estimate the AKM model, I use the largest connected set of workers and firms in the data. 

To control for age effects, I include a quartic polynomial in age in the vector of controls $X_{it}$. However, since I also include year effects in $X_{it}$, the linear term of the polynomial in age is not identified (age is a linear function of year and birth year, and the person effects are colinear with birth year). 

## Data

### The Canadian Employer-Employee Database (CEEDD)

The code was originally used with Canadian matched employer-employee data, called the Canadian Employer-Employee Dynamics Database (CEEDD). The underlying linkable files of the CEEDD are derived from administrative data sources maintained by Statistics Canada. Several key components of the CEEDD are:
- the T1 Personal Master File (T1PMF), which contains individual-level data (e.g. gender, age, marital status, ...) 
- the T4 Record of Employment (T4ROE), which contains job-level data (e.g. earnings, industry, start/end date, ...)
- the National Accounts Longitudinal Microdata File (NALMF), which contains firm-level data (e.g. revenue, value added, ...)

The CEEDD contains info on the *universe* of individuals, firms, and jobs in Canada from 2001 to 2020.

Since the CEEDD data is not available for public use, I provide simulated data to run the code in this repository. The simulated data matches the structure of the CEEDD (i.e., similar variable names etc.), but other than that, the data is entirely fabricated and is not related to the true Canadian data in any way.


## Overview of the code
#### 0_simulate_data.R
 - Creates simulated data. I simulate worker level data, job level data, and firm level data, following the structure of the CEEDD.

#### 1_prepare_data.R 
- Cleans and merges the different datasets to create the matched employer-employee dataset used to estimate the AKM model. I apply the same filters to the simulated data that were originally applied to the CEEDD:
    - Restrict the sample of jobs to "primary jobs" only, which are those that pay each individual the most in each year
    - Restrict the sample of jobs that are "full-time equivalent", i.e. ,those that pay at least $18,377 (in 2012 dollars)
    - Restrict the sample to individuals who are not missing marital status, birth year, or gender
    - Remove small firms with low value added or revenue
    - Restrict the sample of firms to those in the business sector only

