# Paper Replication Assignment 5: Make Table 2A

In this assignment, you will learn to understand, run, and interpret
regression results in Table 2 Panel A in *Defensive Investments and the
Demand for Air Quality: Evidence from the NOx Budget Program* by
Deschênes et al. (2017).

## **Part 1. Understanding the Methodology**

In the first part of this assignment, you will need to read **Section
IV. Econometric Model** (pages 2969 - 2970) in the paper by Deschênes et
al. (2017). You can find a copy of the paper in
[here](https://pubs.aeaweb.org/doi/pdfplus/10.1257/aer.20131002).
Afterward, you will need to answer the questions below.

We will focus on the regression equation

<img src="equation.png" data-fig-align="center" width="500" />

where $cst$ represents the unit of each observation in the data.

#### **Question 1: What is the unit of each observation in the data? (3 points)**

#### Answer:

The variable $NBPOperating_{cst}$ is a product of three dummy variables,
$post_t \times nbp_c \times smm_s$.

$nbp_c$ is a dummy variable for NBP participating counties.

$smm_s$ is a dummy variable for the summer season.

#### **Question 2: What does the variable *post* represent? Why is *post* equal to 0.5 in the year 2003? (2 points)**

#### Answer:

#### **Question 3: What does the parameter of interest** $\gamma_1$ **represent? (1 point)**

#### Answer:

#### **Question 4: The variable** $\mu_{ct}$ **represents county-year fixed effects. Why do we have to include this factor in our analysis? (1 point)**

#### Answer:

#### **Question 5: The variable** $\eta_{st}$ **represents season-year fixed effects. Why do we have to include this factor in our analysis? (1 point)**

#### Answer:

#### **Question 6: The variable** $\nu_{cs}$ **represents county-season fixed effects. Why do we have to include this factor in our analysis? (1 point)**

#### Answer:

## Part 2. Preparing the Data

Step 1: Declare that you will use the **tidyverse** and **lfe** package.
**(4 points)**

Step 2: Upload **data.csv** into the Environment. **(2 points)**

Note: This is the same dataframe that Deschenes et al. (2017) used to
run their regressions. It is download from the [replication package
released by the
authors](https://www.openicpsr.org/openicpsr/project/112938/version/V1/view).

Step 3: Create a new variable called **post** that is equal to 0.5 in
the year 2003 and equal to 1 in the years 2004 through 2007. Otherwise,
**post** is equal to 0. **(6 points)**

Step 4: The variable **fips** are county indicators. The script below
creates a unique identifier for each county-season which will be used to
represent $\nu_{cs}$ in the regressions.

You need to create another unique identifier for each season-year which
will be used to represent $\eta_{ct}$ in the regressions. Name the new
variable **smmXy**. **(2 points)**

``` r
df3<-df2 %>%
  mutate(smmXc = smm*as.numeric(fips)) 
```

Step 5: The script below creates a unique identifier for each state and
year. Use the same method to create a unique identifier for each
county-year. Name the new variable **cXy**. **(2 points)**

``` r
df4<-df3 %>%
  mutate(stXy = paste(state, "-", as.character(year)))
```

Step 6: Create a new variable that represents the variable
$NBPOperating_{cst}$. Name the new variable **NBPOperating**. **(3
points)**

Hint: We discussed this in Part 1 Question 1.

## Part 3: Running Regressions

Answer Questions 1 to 3 based on the regression results below.

``` r
summary(m1<-lm(nox_emit ~ NBPOperating, data=df5))
```

#### **Question 1: According to the regression, the NOx Budget Program was able to reduce NOx emissions by \_\_\_\_\_\_ thousand tons per county-season. (1 point)**

#### **Question 2: According to the regression, the value of** $\gamma_1$ **is \_\_\_\_\_\_. (1 point)**

#### **Question 3: Can the regression produce an accurate estimate of the NBP’s impact? If not, why? (2 points)**

#### Answer:

The script below adds weather controls to the regression. Answer
Questions 4 to 6 based on the regression results below.

``` r
summary(m2<-felm(nox_emit ~ NBPOperating + 
                   dptp_xtile_1 + dptp_xtile_2 + dptp_xtile_4 + dptp_xtile_5 +
                   dptp_xtile_6 + dptp_xtile_7 + dptp_xtile_8 + dptp_xtile_9 +
                   dptp_xtile_10 + dptp_xtile_11 + dptp_xtile_12 + dptp_xtile_13 +
                   dptp_xtile_14 + dptp_xtile_15 + dptp_xtile_16 + dptp_xtile_17 +
                   dptp_xtile_18 + dptp_xtile_19 + dptp_xtile_20 + tmean_xtile_1 +
                   tmean_xtile_2 + tmean_xtile_4 + tmean_xtile_5 + tmean_xtile_6 +
                   tmean_xtile_7 + tmean_xtile_8 + tmean_xtile_9 + tmean_xtile_10 +
                   tmean_xtile_11 + tmean_xtile_12 + tmean_xtile_13 + 
                   tmean_xtile_14 + tmean_xtile_15 + tmean_xtile_16 + 
                   tmean_xtile_17 + tmean_xtile_18 +
                   tmean_xtile_19 + tmean_xtile_20 + mean_prcp, 
                 data=df5))
```

#### **Question 4: According to the regression, the NOx Budget Program was able to reduce NOx emissions by \_\_\_\_\_\_ thousand tons per county-season. (1 point)**

#### **Question 5: According to the regression, the value of** $\gamma_1$ **is \_\_\_\_\_\_. (1 point)**

The part below replicates regression results in the paper by Deschênes
et al. (2017).

<img src="table2a.png" data-fig-align="center" width="600" />

### Column 1

The following regression will replicate the results in Table 2 Panel A
Column 1. Because multiple fixed effects will be included, we will use
the function *felm* instead of *lm*. Answer Questions 7 and 8 based on
the regression results below.

``` r
summary(c1<-felm(nox_emit ~ NBPOperating | 
                   smmXc + smmXy + stXy , 
                 data=df5))
```

#### **Question 6: According to the regression, the NOx Budget Program was able to reduce NOx emissions by \_\_\_\_\_\_ thousand tons per county-season. (1 point)**

#### **Question 7: What fixed effects were included in the regression? (3 point)**

#### Answer:

### Column 2

The following regression will replicate the results in Table 2 Panel A
Column 1. Modify the script below to replicate the results in Column 2.
**(3 points)**

``` r
summary(c2<-felm(nox_emit ~ NBPOperating + 
                   smmXc + smmXy + stXy , 
                 data=df5))
```

### Column 3

The following regression will replicate the results in Table 2 Panel A
Column 5. Modify the script below to replicate the results in column 3.
**(3 points)**

``` r
df2001<-df5 %>%
  filter(year>=2001)

summary(c3<-felm(nox_emit ~ NBPOperating + 
                   dptp_xtile_1 + dptp_xtile_2 + dptp_xtile_4 + dptp_xtile_5 +
                   dptp_xtile_6 + dptp_xtile_7 + dptp_xtile_8 + dptp_xtile_9 +
                   dptp_xtile_10 + dptp_xtile_11 + dptp_xtile_12 + dptp_xtile_13 +
                   dptp_xtile_14 + dptp_xtile_15 + dptp_xtile_16 + dptp_xtile_17 +
                   dptp_xtile_18 + dptp_xtile_19 + dptp_xtile_20 + tmean_xtile_1 +
                   tmean_xtile_2 + tmean_xtile_4 + tmean_xtile_5 + tmean_xtile_6 +
                   tmean_xtile_7 + tmean_xtile_8 + tmean_xtile_9 + tmean_xtile_10 +
                   tmean_xtile_11 + tmean_xtile_12 + tmean_xtile_13 + 
                   tmean_xtile_14 + tmean_xtile_15 + tmean_xtile_16 + 
                   tmean_xtile_17 + tmean_xtile_18 +
                   tmean_xtile_19 + tmean_xtile_20 + mean_prcp | 
                   smmXc + smmXy + cXy, 
                 weights = df2001$pop_all,
                 data=df2001))
```

### Column 4

The following regression will replicate the results in Table 2 Panel A
Column 5. Modify the script below to replicate the results in column 4.
**(3 points)**

``` r
df2001<-df5 %>%
  filter(year>=2001)

summary(c4<-felm(nox_emit ~ NBPOperating + 
                   dptp_xtile_1 + dptp_xtile_2 + dptp_xtile_4 + dptp_xtile_5 +
                   dptp_xtile_6 + dptp_xtile_7 + dptp_xtile_8 + dptp_xtile_9 +
                   dptp_xtile_10 + dptp_xtile_11 + dptp_xtile_12 + dptp_xtile_13 +
                   dptp_xtile_14 + dptp_xtile_15 + dptp_xtile_16 + dptp_xtile_17 +
                   dptp_xtile_18 + dptp_xtile_19 + dptp_xtile_20 + tmean_xtile_1 +
                   tmean_xtile_2 + tmean_xtile_4 + tmean_xtile_5 + tmean_xtile_6 +
                   tmean_xtile_7 + tmean_xtile_8 + tmean_xtile_9 + tmean_xtile_10 +
                   tmean_xtile_11 + tmean_xtile_12 + tmean_xtile_13 + 
                   tmean_xtile_14 + tmean_xtile_15 + tmean_xtile_16 + 
                   tmean_xtile_17 + tmean_xtile_18 +
                   tmean_xtile_19 + tmean_xtile_20 + mean_prcp | 
                   smmXc + smmXy + cXy, 
                 weights = df2001$pop_all,
                 data=df2001))
```

### Column 5

You just have to run the script below.

``` r
df2001<-df5 %>%
  filter(year>=2001)

summary(c5<-felm(nox_emit ~ NBPOperating + 
                   dptp_xtile_1 + dptp_xtile_2 + dptp_xtile_4 + dptp_xtile_5 +
                   dptp_xtile_6 + dptp_xtile_7 + dptp_xtile_8 + dptp_xtile_9 +
                   dptp_xtile_10 + dptp_xtile_11 + dptp_xtile_12 + dptp_xtile_13 +
                   dptp_xtile_14 + dptp_xtile_15 + dptp_xtile_16 + dptp_xtile_17 +
                   dptp_xtile_18 + dptp_xtile_19 + dptp_xtile_20 + tmean_xtile_1 +
                   tmean_xtile_2 + tmean_xtile_4 + tmean_xtile_5 + tmean_xtile_6 +
                   tmean_xtile_7 + tmean_xtile_8 + tmean_xtile_9 + tmean_xtile_10 +
                   tmean_xtile_11 + tmean_xtile_12 + tmean_xtile_13 + 
                   tmean_xtile_14 + tmean_xtile_15 + tmean_xtile_16 + 
                   tmean_xtile_17 + tmean_xtile_18 +
                   tmean_xtile_19 + tmean_xtile_20 + mean_prcp | 
                   smmXc + smmXy + cXy, 
                 weights = df2001$pop_all,
                 data=df2001))

res_tab<-as.data.frame(cbind(C1 = coef(c1)[[1]], 
                             C2 = coef(c2)[[1]], 
                             C3 = coef(c3)[[1]], 
                             C4 = coef(c4)[[1]], 
                             C5 = coef(c5)[[1]]))
kable(res_tab, digits=2)
```

#### **Question 8: Based on the regression results in Column 1 through 5, the NOx Budget Program decreased NOx emissions in the average county by \_\_\_\_\_\_\_\_(minimum value) to \_\_\_\_\_\_(maximum value) tons of NOx per summer. (2 points)**

#### **Question 9: Because there are 1185 counties regulated by the NOx Budget Program, our results correspond to a total decrease of between \_\_\_\_\_\_\_\_(minimum value) to \_\_\_\_\_\_(maximum value) tons of NOx per summer. (4 points)**

Remove all lines of script with #\| eval: false which prevents the line
from running when rendered. Afterward, click Render to generate an html
file of this document. Click Render to generate an html file of this
document.

You have reached the end of the assignment. Render the Quarto document
and push the completed assignment back into the GitHub repository. **(2
points)**
