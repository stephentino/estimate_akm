# Estimating a two-way fixed effect models using Canadian matched employer-employee data

## About the repository

This repo contains code to estimate a two-way fixed effects model using Canadian matched employer-employee data. Simulated data to test the code is provided, since the Canadian matched employer-employee data is not available to the public. 

I wrote this code while conducting research on monopsony power in Canada for my PhD thesis in economics at the University of Toronto.

## Background information

### AKM models

Two-way fixed effects models are often called 'AKM' models thanks to the influential work by Abowd, Kramarz, and Margolis (1999). The AKM model assumes that the log of an individual's earnings can be decomposed into a the sum of a person effect, a firm effect, a time-varying index of individual characteristics, and a residual. Formally, the AKM model posits that the log earnings of individual $i$ at firm $j$ at time $t$ can be written as:

$y_{ijt} = \alpha_i + \psi_j + \beta'X_{it} + \varepsilon_{ijt},$

where $\alpha_i$ is the person effect for individual $i$, $\psi_j$ is the firm effect for firm $j$, $X_{it}$ is a vector of time-varying characteristics for individual $i$ at time $t,$ $\beta$ is a conformable vector of coefficients, and $\varepsilon_{ijt}$ is the residual.

### Data

This code was originally tested using Canadian matched employer-employee data, called the Canadian Employer-Employee Dynamics Database (CEEDD). The underlying linkable files of the CEEDD are derived from administrative data sources maintained by Statistics Canada. Several key components of the CEEDD are:
- the T1 Personal Master File (T1PMF), which contains individual-level data (e.g. gender, age, marital status, ...) 
- the T4 Record of Employment (T4ROE), which contains job-level data (e.g. earnings, industry, start/end date, ...)
- the National Accounts Longitudinal Microdata File (NALMF), which contains firm-level data (e.g. revenue, value added, ...)

The CEEDD contains info on the *universe* of individuals, firms, and jobs in Canada from 2001 to 2020, meaning that it is a very large dataset.

Since the CEEDD data is not available for public use, I provide simulated data to run the code in this repository. 

## Overview of the code
