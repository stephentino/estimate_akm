# Estimating a two-way fixed effect model using Canadian matched employer-employee data

## About the repository

The purpose of this project is to estimate an AKM-style two-way fixed effects model using Canadian matched employer-employee data. Simulated data and some public use programs are provided, since the Canadian matched employer-employee data are not available to the public. The original code used to estimate the AKM-style model with the restricted-access Canadian data are also provided. 

**Contents:**

- [Background information](#Background-information)

- [Data](#Data)

- [Overview of the code: public use](#Overview-of-the-code)

- [Overview of the code: restricted access](#Overview-of-the-code)

## Background information

### AKM models

The AKM model [(Abowd, Kramarz, and Margolis 1999)](https://doi.org/10.1111/1468-0262.00020) can be used to quantify the contribution of workers and firms to earnings inequality. The model assumes that the log of an individual's earnings can be decomposed into a the sum of a person effect, a firm effect, a time-varying index of individual characteristics, and a residual. 

Formally, the AKM model posits that the log earnings of individual $i$ at firm $j$ at time $t$ can be written as:

$y_{ijt} = \alpha_i + \psi_j + \beta'X_{it} + \varepsilon_{ijt},$

where $\alpha_i$ is the person effect for individual $i$, $\psi_j$ is the firm effect for firm $j$, $X_{it}$ is a vector of time-varying characteristics for individual $i$ at time $t,$ $\beta$ is a conformable vector of coefficients, and $\varepsilon_{ijt}$ is the residual. Included in the vector $X_{it}$ are variables for marital status, province of residence, year effects, and controls for age (more details below).

A common approach to understand the contribution of worker and firm heterogeneity to overall earnings inequality is to conduct the variance decomposition:

$Var(y_{ijt}) =$

$Var(\alpha_i) + Var(\psi_j) + Var(\beta'X_{it}) + Var(\varepsilon_{ijt})$

$+ 2Cov(\alpha_i, \psi_j) + 2Cov(\alpha_i, \beta'X_{it}) + 2 Cov(\psi_j, \beta'X_{it})$


The contribution of worker heterogenity is captured by $Var(\alpha_i)$, the contribution of firm heterogeneity is captured by $Var(\psi_j)$, and the contribution of worker sorting is captured by $2 Cov(\alpha_i, \psi_j)$. If high wage workers tend to work at high-paying firms, then $Cov(\alpha_i,\psi_j)$ will be high.

An assumption of the AKM-style model is that the worker and firm effects are additively separable. Other methods, such as the BLM method [(Bonhomme, Lamadon, and Manresa, 2019)](https://doi.org/10.3982/ECTA15722) should be used if there are reasons to believe that the assumption of additive separability does not hold.


### Identification

The firm effects are not identified for all firms in the matched employer-employee data. They are only identified for firms that are "connected". Two firms are "connected" if there exists a worker who moves between them. Thus, an important step prior to estimating the AKM-style model involves extracting the "largest connected set" of workers and firms from the matched employer-employee data. To extract the largest connected set of workers and firms, I use the *igraph* package. The employer-employee data can be viewed as a graph where the firms are nodes and the edges are worker flows between firms. The largest connected set is equivalent to the maximal connected component of the worker-firm graph.

To control for age effects, I include a quartic polynomial in age in the vector of controls $X_{it}$. However, since I also include year effects in $X_{it}$, the linear term of the polynomial in age is not identified (age is a linear function of year and birth year, and the person effects are colinear with birth year). Therefore, I omit the linear term of the polynomial in age and also normalize age by subtracting and dividing by the age at which the earnings profile is at a maximum. This approach follows [Card et al. (2018)](https://doi.org/10.1086/694153) and  [Card et al. (2016)](https://doi.org/10.1093/qje/qjv038). As explained by [Card et al. (2018)](https://doi.org/10.1086/694153), omitting the linear term of the polynomial without including the additional normalization of age can bias the person effects upward. 

For the estimator of the firm effects to be unbiased, firm-to-firm mobility must be uncorrelated with time-varying unobservables. This is often referred to as the "exogenous mobility assumption" (see, for example, [Card et al., 2018](https://doi.org/10.1086/694153)). This is an untestable assumption, although some evidence in support of this assumption can be found in the literature. For example, [Card et al. (2013)](https://doi.org/10.1093/qje/qjt006) show that wage gains and losses from a firm-to-firm transition are typically symmetric, and that earnings growth tends to be relatively flat before and after a job move. 

[Bonhomme et al. (2023)](https://doi.org/10.1086/720009) explain that firm effects will be biased upward, and the covariance between worker effects and firm effects will be biased downward, if there are many firms in the data that are "weakly connected" by a relatively small number of workers moving between them. This is referred to as "limited mobility bias", and it can be substantial in practice. In this project, I do not correct for limited mobility bias. I will add code that corrects for limited mobility bias in the future. 


## Data

### The Canadian Employer-Employee Database (CEEDD)

The code was originally used with Canadian matched employer-employee data, called the [Canadian Employer-Employee Dynamics Database (CEEDD)](https://www.statcan.gc.ca/en/statistical-programs/document/5228_D1_V1). The underlying linkable files of the CEEDD are derived from administrative data sources maintained by Statistics Canada. Several key components of the CEEDD are:
- the T1 Personal Master File (T1PMF), which contains individual-level data (e.g. gender, age, marital status, ...) 
- the T4 Record of Employment (T4ROE), which contains job-level data (e.g. earnings, industry, start/end date, ...)
- the National Accounts Longitudinal Microdata File (NALMF), which contains firm-level data (e.g. revenue, value added, ...)

The CEEDD contains info on the universe of individuals, firms, and jobs in Canada from 2001 to 2020.

Since the CEEDD data is not available for public use, I provide simulated data that is entirely fabricated and is not related to the true Canadian data in any way. This simulated data can be used to test the programs in the "*estimate_akm/public_use*" folder. The programs in the "*estimate_akm/public_use*" folder are a simplified version of the original code (see below).

## Overview of the code

### Public use code in [estimate_akm/public_use/code](https://github.com/stephentino/estimate_akm/blob/main/public_use/code/)

#### [1_simulate_data.R](https://github.com/stephentino/estimate_akm/blob/main/public_use/code/1_simulate_data.R)
 - Creates simulated data that can be used to test the programs. I simulate a matched employer-employee dataset that follows workers and firms over a period of 20 years. 

#### [2_estimate_akm.R](https://github.com/stephentino/estimate_akm/blob/main/public_use/code/2_estimate_akm.R)
- First, this code applies some filters to the data that are required before akm estimation:
    - Restricts the sample to workers and firms observed at least twice
    - Restricts the sample to the largest connected set of workers and firms. Note: the firms and workers in the matched employer-employee data form a "graph" where the nodes are firms and the edges are workers' firm-to-firm transitions. The "largest connected set" of workers and firms is the maximal connected subgraph. The maximal connected subgraph is extracted using the *igraph* package.  
- Next, the code estimates the AKM-style two-way fixed effects model using the final sample of workers and firms. All of the coefficients and fixed effects are estimated jointly (in contrast to the "two-step procedure" that is discussed below.) To estimate the model, the *lfe* package is used.
- Finally, a variance decomposition is conducted. This variance decomposition shows the fraction of the variance in log earnings that is attributable to person effects, firm effects, and the covariance between the two.

#### [3_estimate_two_step_akm.R](https://github.com/stephentino/estimate_akm/blob/main/public_use/code/3_estimate_two_step_akm.R)
- This code is similar to *2_estimate_akm.R*. However, the estimation of the model is done in "two steps". In the first step, log earnings are regressed on time-varying individual characteristics and year effects. In the second step, the two-way fixed effects model is estimated using the residualized earnings. This "two-step" procedure produces nearly identical results to the one-step procedure, an the computational run time is significantly reduced. When working with real matched employer-employee datasets that require a lot of memory, the two-step procedure is preferred.

#### [utils](https://github.com/stephentino/estimate_akm/tree/main/public_use/code/utils)

- This folder contains R scripts with important functions that are used by the main scripts described above. 

### Original code (used with restricted access data) in [estimate_akm/original/code/](https://github.com/stephentino/estimate_akm/tree/main/original)

#### [1A_clean_merge.R](https://github.com/stephentino/estimate_akm/blob/main/original/1A_merge_and_clean.R)
- Cleans and merges the different datasets to create the matched employer-employee dataset used to estimate the AKM model. The CEEDD is a linkable database with different files for individual, firm, and job level information. These different files need to be merged (using individual and firm ID's), and the data must also be cleaned, before estimating the AKM model. Due to limitations in computational power and memory, certain filters must be applied to the data to reduce the dataset size before merging the different files together.
- I apply the following filters to produce the final dataset used in the estimation:
    - Restrict the sample of jobs to "primary jobs" only, which are those that pay each individual the most in each year
    - Restrict the sample of jobs that are "full-time equivalent", i.e. ,those that pay at least $18,377 (in 2012 dollars)
    - Restrict the sample to individuals who are not missing marital status, birth year, or gender
    - Remove small firms with low value added or revenue
    - Restrict the sample of firms to those in the business sector only
 
#### [1B_add_spells.R](https://github.com/stephentino/estimate_akm/blob/main/original/1B_add_spells.R)
- A subset of jobs in the ROE file in the CEEDD are associated with start/end dates for the job spell. However, the start/end dates are only non-missing in the data in the year that the worker separates from the employer. This code "fills out" the start/end dates so that each "job-year" in the data with start/end dates available is associated with the start/end date of the spell.

#### [2_compare_different_models.R](https://github.com/stephentino/estimate_akm/blob/main/original/2_compare_different_models.R)
- This code tests that the *lfe package* estimates fixed effects models correctly using a subset of the full matched employer-employee data. I checked, for example, that the variance decomposition is nearly identical whether fixed effects are "partialled out" or not. I also checked that the "two step" procedure for estimating AKM produces nearly identical results as the "one step" procedure (see the discussion above). 

#### [3_estimate_two_step_akm.R](https://github.com/stephentino/estimate_akm/blob/main/original/3_estimate_two_step_akm.R)
- First, this code applies some filters to the data that are required before akm estimation:
    - Restricts the sample to workers and firms observed at least twice
    - Restricts the sample to the largest connected set of workers and firms. Note: the firms and workers in the matched employer-employee data form a "graph" where the nodes are firms and the edges are workers' firm-to-firm transitions. The "largest connected set" of workers and firms is the maximal connected subgraph. The maximal connected subgraph is extracted using the *igraph* package.  
- Next, the code estimates the AKM-style two-way fixed effects model using the final sample of workers and firms. the estimation of the model is done in "two steps". In the first step, log earnings are regressed on time-varying individual characteristics (province of residence, marital status, a quartic polynomial in normalized age excluding the linear term) and year effects. In the second step, the two-way fixed effects model is estimated using the residualized earnings. This "two-step" procedure produces nearly identical results to the one-step procedure, an the computational run time is significantly reduced. When working with real matched employer-employee datasets that require a lot of memory, the two-step procedure is preferred. To estimate the AKM model, the *lfe* package is used.
- For my project, I needed to estimate the AKM model many times using different subsets of the data. For example, I estimate the model once using the full sample, once using data from 2001 to 2005 only, once using data from 2010 to 2015 only, etc. The code contains "switches" to decide which subset of the data is used to estimate the AKM model. 
- Finally, a variance decomposition is conducted. This variance decomposition shows the fraction of the variance in log earnings that is attributable to person effects, firm effects, and the covariance between the two. 

#### [utils](https://github.com/stephentino/estimate_akm/tree/main/original/utils)

- This folder contains R scripts with important functions that are used by the main scripts described above. 


# Author
Stephen Tino, PhD Candidate in Economics, University of Toronto, s.tino@mail.utoronto.ca
