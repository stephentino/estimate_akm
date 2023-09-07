# Estimating a two-way fixed effect models using Canadian matched employer-employee data

## About the repository

The purpose of this project is to estimate an AKM-style two-way fixed effects model using Canadian matched employer-employee data. Simulated data and some public use programs are provided, since the Canadian matched employer-employee data are not available to the public. The original code used to estimate the AKM-style model with the restricted-access Canadian data are also provided. 

**Contents:**

- [Background information](#Background-information)

- [Data](#Data)

- [Overview of the code: public use](#Overview-of-the-code)

- [Overview of the code: restricted access](#Overview-of-the-code)

## Background information

### AKM models

The AKM model assumes that the log of an individual's earnings can be decomposed into a the sum of a person effect, a firm effect, a time-varying index of individual characteristics, and a residual. (These models are called 'AKM' models thanks to the influential work by Abowd, Kramarz, and Margolis 1999.) 

Formally, the AKM model posits that the log earnings of individual $i$ at firm $j$ at time $t$ can be written as:

$y_{ijt} = \alpha_i + \psi_j + \beta'X_{it} + \varepsilon_{ijt},$

where $\alpha_i$ is the person effect for individual $i$, $\psi_j$ is the firm effect for firm $j$, $X_{it}$ is a vector of time-varying characteristics for individual $i$ at time $t,$ $\beta$ is a conformable vector of coefficients, and $\varepsilon_{ijt}$ is the residual. Included in the vector $X_{it}$ are variables for marital status, province of residence, year effects, and controls for age (more details below).

The fixed effects are high-dimensional, since there are many firms and individuals in the typical matched employer-employee dataset. This makes AKM-style models difficult to estimate in practice. To overcome this challenge, I use the *lfe* package in *R* to estimate the model with high-dimensional fixed effects. 

### Identification

The firm effects are not identified for all firms in the matched employer-employee data. They are only identified for firms that are "connected". Two firms are "connected" if there exists a worker who moves between them. Thus, an important step prior to estimating the AKM-style model involves extracting the "largest connected set" of workers and firms from the matched employer-employee data. To extract the largest connected set of workers and firms, I use the *igraph* package. The employer-employee data can be viewed as a graph where the firms are nodes and the edges are worker flows between firms. The largest connected set is equivalent to the maximal connected component of the worker-firm graph.

For the estimator of the firm effects to be unbiased, firm-to-firm mobility must be uncorrelated with time-varying unobservables. This is an untestable assumption, although some evidence in support of this assumption can be found in the literature.

Another implicit assumption of the AKM-style model is that the worker and firm effects are additively separable. Other methods, such as the method by Bonhomme, Lamadon, and Manresa (referred to as "BLM") should be used if there are reasons to believe that the assumption of additive separability does not hold.

To control for age effects, I include a quartic polynomial in age in the vector of controls $X_{it}$. However, since I also include year effects in $X_{it}$, the linear term of the polynomial in age is not identified (age is a linear function of year and birth year, and the person effects are colinear with birth year). Therefore, I omit the linear term of the polynomial in age and apply a normalization to age, following the literature.  


## Data

### The Canadian Employer-Employee Database (CEEDD)

The code was originally used with Canadian matched employer-employee data, called the Canadian Employer-Employee Dynamics Database (CEEDD). The underlying linkable files of the CEEDD are derived from administrative data sources maintained by Statistics Canada. Several key components of the CEEDD are:
- the T1 Personal Master File (T1PMF), which contains individual-level data (e.g. gender, age, marital status, ...) 
- the T4 Record of Employment (T4ROE), which contains job-level data (e.g. earnings, industry, start/end date, ...)
- the National Accounts Longitudinal Microdata File (NALMF), which contains firm-level data (e.g. revenue, value added, ...)

The CEEDD contains info on the universe of individuals, firms, and jobs in Canada from 2001 to 2020.

Since the CEEDD data is not available for public use, I provide simulated data that is entirely fabricated and is not related to the true Canadian data in any way. This simulated data can be used to test the programs in the "*estimate_akm/public_use*" folder. The programs in the "*estimate_akm/public_use*" folder are a simplified version of the original code.  

## Overview of the code

### Public use

#### estimate_akm/public_use/code/1_simulate_data.R
 - Creates simulated data that can be used to test the programs. I simulate a matched employer-employee dataset that follows workers and firms over a period of 20 years. 



#### estimate_akm/public_use/code/2_estimate_akm.R
- First, this code applies some additional filters to the data that are required before akm estimation:
    - Restricts the sample to workers and firms observed at least twice
    - Restricts the sample to the largest connected set of workers and firms. Note: the firms and workers in the matched employer-employee data form a "graph" where the nodes are firms and the edges are workers' firm-to-firm transitions. The "largest connected set" of workers and firms is the maximal connected subgraph. The maximal connected subgraph is extracted using the *igraph* package.
    - Normalizes "age" using the age associated with the highest "residualized earnings", where the "residualized earnings" are calculated from a regression of log earnings on time-varying individual covariates and year effects.  
- Next, the code estimates the AKM-style two-way fixed effects model using the final sample of workers and firms. All of the coefficients and fixed effects are estimated jointly (in contrast to the "two-step procedure" that is discussed below.) To estimate the model, the *lfe* package is used.

#### estimate_akm/public_use/code/3_estimate_two_step_akm.R 
- This code applies some additional filters to the data and then estimates the AKM-style model, similar to *2_estimate_akm.R*. However, the estimation of the model is done in "two steps". In the first step, log earnings are regressed on time-varying individual characteristics and year effects. In the second step, the two-way fixed effects model is estimated using the residualized earnings. This "two-step" procedure produces nearly identical results to the one-step procedure, an the computational run time is significantly reduced.  

### Restricted access

#### estimate_akm/original/code/ 
- Cleans and merges the different datasets to create the matched employer-employee dataset used to estimate the AKM model. I apply the following filters to the CEEDD to produce the final dataset used in the estimation:
    - Restrict the sample of jobs to "primary jobs" only, which are those that pay each individual the most in each year
    - Restrict the sample of jobs that are "full-time equivalent", i.e. ,those that pay at least $18,377 (in 2012 dollars)
    - Restrict the sample to individuals who are not missing marital status, birth year, or gender
    - Remove small firms with low value added or revenue
    - Restrict the sample of firms to those in the business sector only
