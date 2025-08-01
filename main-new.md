---
abstract: 
  We examine the spending response to the end of federal student-loan
  forbearance in the fall of 2023, when millions of Americans were
  required to resume monthly student loan payments after a three-year
  pause. Using a novel data set combining ZIP-code-level data on
  consumer spending and student loan debt, we find a significant
  contraction in spending due to the policy change, particularly in
  low-income ZIP codes. Our estimates imply that the resumption of
  student loan payments led to a \[\$40-\$76 billion\] reduction in
  consumer spending on an annual basis. These findings provide causal
  evidence that debt-relief policies like the student loan forbearance
  program can provide meaningful stimulus to aggregate demand.
author:
- Aditya Aladangady
- Edmund Crawley
- William Gamber
- Patrick Moran
- Jose Nino
bibliography: bib.bib
date: |
  \
  **Preliminary and Incomplete.\
  Please do not cite or circulate.**
title: ' Debt Payments and Spending: Evidence from the 2023 Student Loan
  Payment Restart'
format:
  pdf: default
  docx: default
execute:
  echo: false                    # Hide code chunks by default
  warning: false
  message: false
---

Introduction [\[sec:intro\]]{#sec:intro label="sec:intro"}
==========================================================

In October 2023, roughly 40 million Americans faced a new monthly bill
as federal student loan payments resumed after a three-year
pandemic-induced pause.[^2] The restart of loan payments effectively
reduced disposable income for borrowers, raising a critical question:
How do debt payments affect household spending? Despite a growing
academic literature studying the economic effects of student debt, there
is surprisingly little empirical evidence on the link between student
debt and household spending, largely due to data limitations. And yet,
understanding the effects of student debt on household spending is
crucial given that spending plays a central role in any measure of
household well-being and is a key channel through which student loan
policy may affect the macroeconomy.

In this paper, we exploit a unique natural experiment and granular, ZIP
code level data on consumer spending to better understand how changes in
student loan payments and interest accrual affect household spending.
Following the resumption of student loan payments in October 2023, we
find that households significantly curtailed spending in areas with
higher exposure to student debt relative to those with lower exposure,
all else equal, indicating that the payment pause had supported
consumption in those areas. We also find that the pullback in spending
was much larger in lower-income areas, consistent with these households
facing tighter credit constraints. Our results imply that the end of
forbearance amounted to a noticeable drag on aggregate demand of roughly
\[\$40 billion-\$76 billion\] at an annual rate.

*A priori*, the effects of the resumption of student loan payments on
consumer spending are unclear. Given the size and prevalence of student
debt---there was about \$1.25 trillion in federal student debt in
mid-2023---there is scope for the resumption of payments to reduce
aggregate spending, but the extent depends on the marginal propensity to
consume of the debt holders. On one hand, individuals with student debt
are usually young and have low wealth, meaning that they are likely to
be more affected by binding credit constraints. On the other hand,
student debtors tend to have higher income than others in their age
group, and may hold higher stocks of liquid savings. As a result, these
households may respond less to the monthly payment resumption. The
relative importance of these different factors is ultimately an
empirical question and one we seek to answer in the data. To better
understand the effects of student debt on consumer spending, we exploit
ZIP code level data on consumer spending and evaluate the evolution of
spending in different ZIP codes before and after the resumption of
student loan payments in October 2023. In our empirical analysis, we
compare the change in spending across ZIP codes with different levels of
student debt but similar age, income, and education. To do so, we create
a novel dataset that combines third-party reported debit- and
credit-card spending at a weekly frequency, derived from 55 million
individuals and roughly 89 million credit and debit cards, obtained from
Verisk Commerce Signals Spend Tracker (Verisk 2024), with ZIP code level
data on student loan balances per capita using anonymized credit records
from the New York Fed Consumer Credit Panel (2023).

Identification rests upon the assumption that, absent the resumption of
student loan payments in October 2023, spending would have evolved
similarly across ZIP codes with different levels of student debt, but
similar variation in age, income, and education (i.e. conditional
parallel trends). To validate our research design, we explicitly test
for parallel trends in spending prior to the resumption of student loan
payments in October 2023 using a dynamic difference-in-differences
estimator. We find no relationship between student debt and spending
growth prior to the resumption of student loan payments, which helps
support our causal interpretation of the empirical results. We further
validate our empirical design by performing a placebo test where we
evaluate the relationship between consumer spending and auto loan debt,
which was unaffected by the payment moratorium. Reassuringly, and
consistent with the fact that there were no changes in auto loan
repayment during this period, we find no relationship between auto loans
and spending. This supports our interpretation that consumer spending is
responding to the restart of student loan payments, rather than a more
general trend related to other forms of debt.

We have three main sets of results. First, we find that consumer
spending declined gradually but meaningfully following the resumption of
student loan payments. For every \$10,000 in student debt per capita, we
estimate that consumer spending fell by an average of \[\$6 per week\]
over the \[7\] months following payment resumption, with a peak
reduction of \[\$12\] per week by April of 2024. Given that the median
borrower holds roughly $\$25,000$ in student debt, these estimates imply
that the median borrower reduced spending by \[\$800\] at an annual rate
on average, and \[\$1600\] at the peak. Using these two estimates as
rough bounds on the effect of the policy, an aggregate
back-of-the-envelope partial equilibrium exercise implies that the end
of student loan forbearance reduced aggregate spending by \[\$40-\$76
billion\] at an annual rate, equal to \[0.2-0.4 percent\] of total
personal consumption expenditures. Based on the relationship between
student loan balances and required payments, these results imply an
average marginal propensity to consume out of forbearance liquidity of
between 50-100 percent, in line with other estimates from the
literature.

Second, we evaluate the timing of the response and find that consumer
spending is more responsive to changes in current required payments than
news about future payments or the interest rate accruing on student
debt. More specifically, we find only a small, statistically
insignificant change in spending in June 2023 when Congress enacted a
law to prevent further extensions of student loan forbearance,
effectively forcing payments to resume in October 2023. Further, we find
only a small, statistically insignificant change in spending in
September 2023, when interest began to accumulate on student debt, one
month before mandatory payments resumed. These results suggest that
consumer spending is more responsive to current required payments than
news about future required payments or the interest rate on debt. In
addition, because interest continues to accrue on loans, our result
suggests well-targeted forbearance programs can be a relatively low cost
form of stimulus for the government.

Third, we see economically meaningful heterogeneity in the response of
spending across locations, consistent with binding credit constraints
for a subset of the population. In particular, we show the cutback in
spending per dollar of debt is three times larger than in the lowest
income quintile ZIP codes than in the highest income quintile ZIP
codes.[^3] We view this finding--along with the lack of announcement
effects--as consistent with binding credit constraints leading the
cutback in spending to be higher for lower-income households. Our
findings are consistent with recent research arguing for the importance
of credit constraints when evaluating student loan repayment plans
(Boutros, Clara, and Gomes 2024) and echoes the conclusions of (Ganong
and Noel 2020) who find that current budget constraints are of primary
importance for understanding the behavior of indebted households.

Related literature {#related-literature .unnumbered}
------------------

A growing literature evaluates the consequences of student debt for
other aspects of household behavior.[^4] Numerous papers document
relationships between student debt and economic outcomes, such as home
ownership (Mezza et al. 2020), job match quality (Field 2009; Rothstein
and Rouse 2011; Luo and Mongey 2019), entrepreneurship (Krishnan and
Wang 2019), the likelihood of default (Armona, Chakrabarti, and
Lovenheim 2022; Mueller and Yannelis 2019), borrowing (Dinerstein,
Yannelis, and Chen 2024), graduate school enrollment (Chakrabarti, Fos,
et al. 2023), and job mobility (Jacob, Jones, and Keys 2024). However,
we know relatively little about the effects of student debt payments on
consumer spending. This is despite the fact that a large literature has
argued that consumer spending is of first-order importance for
understanding the welfare and distributional effects of public policy,
given its central place in the utility function (see e.g., (Blundell and
Preston 1998; Blundell, Pistaferri, and Preston 2008)). We contribute to
this literature by being the first paper to evaluate the effects of a
change in student loan repayment on a direct measure of consumer
spending.

We also contribute to a growing literature that evaluates the potential
stimulative effects of 'household liquidity policy' as an alternative to
traditional fiscal stimulus (Schneider and Moran 2024). Household
liquidity policy seeks to stimulate the economy by relaxing household
liquidity constraints, rather than providing direct fiscal transfers,
and one increasingly popular approach is through counter-cyclical debt
forbearance. While a handful of papers evaluate the stimulative effects
of mortgage forbearance, our paper is one of the first to evaluate the
effects of student loan forbearance on consumer spending. On the
mortgage side, (Ganong and Noel 2020) find that mortgage maturity
extensions have large effects on default and consumption. (Albuquerque
and Varadi 2022) find that mortgage holidays in the UK during the
pandemic played a significant role in supporting spending, especially
for liquidity constrained households. (Lee and Maghzian 2023) study the
employment response to pandemic-era mortgage forbearance in the US and
use a model to translate the estimates into the marginal propensity to
spend. Like us, they find relatively high marginal propensities to spend
out of debt forbearance.

Turning to student loans, (Chakrabarti, Mangrum, et al. 2023) conduct a
survey in August 2023, immediately prior to the resumption of student
loan payments, and find that American consumers expected modest
reductions in spending when payments resumed. Using actual spending
data, we find that Americans did indeed cut spending, by about as much
as the responses to their survey suggested. (Katz 2023) studies how
households allocated the liquidity provided by the onset of student loan
forbearance, finding that households used much of this additional
liquidity to pre-pay student loans, despite not being required to do so.
(Dinerstein, Yannelis, and Chen 2024) use credit bureau data to study
how student loan forbearance affected borrowing on mortgages, auto
loans, and credit cards. The authors find that borrowers used liquidity
to take out additional auto, credit, and mortgage debt. Our paper
differs from theirs in three important dimensions. First, we directly
observe consumer spending based on debit and credit card transactions,
which complements their data on credit card borrowing. Second, we
develop a new empirical strategy that allows us to estimate the effects
of student debt on spending behavior in the entire population of federal
student loan borrowers, whereas the identification scheme in
(Dinerstein, Yannelis, and Chen 2024) requires restricting their sample
to borrowers who took out loans before 2010.[^5] Third, while they study
the start of loan forbearance, we study the end of loan forbearance,
which for many borrowers represents an anticipated reduction in
disposable income.[^6] Finally, we provide new evidence on income
heterogeneity, evaluating which segments of the income distribution
drove the cutback in spending following the resumption of repayment.

Our results have important implications for our understanding of the
distributional consequences of student debt. To the best of our
knowledge, we are the first to show that student loan repayment leads
low income Americans to cut their spending more sharply than their high
income counterparts. Our study thus contributes evidence to the growing
debate about the distributional consequences of student loan forgiveness
(Di Maggio, Kalda, and Yao 2019; Perry 2021; Catherine and Yannelis
2023) and alternative income-driven-repayment plans (Mueller and
Yannelis 2019, 2022; Herbst 2023; Boutros, Clara, and Gomes 2024). Our
finding that low income Americans cut their spending more sharply than
high income Americans is consistent with the importance of credit
constraints for understanding households' responses to debt payments
(see e.g. (Boutros, Clara, and Gomes 2024).) Given the centrality of
consumption in the utility function, we believe that our empirical
results may be useful in the future for disciplining models of student
loan repayment, which can be used to evaluate the welfare and
distributional consequences of student debt forgiveness or alternative
income driven repayment plans.

Data and Summary Statistics [\[sec:dataandsummarystats\]]{#sec:dataandsummarystats label="sec:dataandsummarystats"}
===================================================================================================================

Data sources
------------

We merge four data sources to construct a novel panel data set
containing student loan balances, demographic characteristics, and
weekly spending at a ZIP code level. We obtain spending data from Verisk
Commerce Signals Spend Tracker (Verisk) and student loan data from the
Federal Reserve Bank of New York/Equifax Consumer Credit Panel (CCP).
The CCP does not contain detailed information on income, education
level, or other demographic characteristics beyond age. As such, we use
the 2015-2019 American Community Survey (ACS) ZCTA-level extract and the
Internal Revenue Service's Statistics of Income (SOI) to provide
additional information about household income, education, and
demographics in each ZIP code.

Our data from Verisk consist of a ZIP code by week panel of total
spending on debit and credit cards.[^7] Specifically, Verisk collects
and aggregates transactions based on issuer-side records covering $55$
million individuals, which covers $89$ million credit and debit cards.
$65$ percent of the sample is based on credit card transactions. In
total, the data capture $\$800$ billion in annual sales. Raw data were
processed by Verisk, who then aggregated and scaled the data to provide
us with estimates of the total number and value of credit and debit card
transactions in every ZIP code at a weekly frequency.[^8] Importantly
for our use, the transaction for an individual and their card is
attached to the ZIP code of the card owner's billing address, not where
the transaction was made. This feature allows us to link spending with
student debt and demographic information which are also measured at the
ZIP code of residence.

The CCP provides a $5$ percent random sample of anonymized,
individual-level US credit records that, when aggregated, allows us to
construct a measure of outstanding federal student loan balances by ZIP
code as of September 2023. While we observe loan-level records for each
individual in the data, these are not categorized as either "federal" or
"private" student loans. So, one challenge we face is identifying which
loans in the data are federal loans---and therefore subject to the
statutory pause and resumption in required payments---as opposed to
private loans.[^9]

As described in further detail in Appendix
[\[sec:data\_appendix\_ccp\]](#sec:data_appendix_ccp){reference-type="ref"
reference="sec:data_appendix_ccp"}, to overcome this challenge we
utilize loan servicer portfolio identifiers to classify loans in an
approach similar to (Goss, Mangrum, and Scally 2024). In particular,
every loan in the data is associated with a servicer sub-portfolio. Even
though a single company may service both federal and non-federal loans,
these identifiers appear to separate loans into different sub-portfolios
by federal status. Since terms of federal forbearance required servicers
to stop reporting delinquencies begining in 2020:Q3, we classify every
loan in a given sub-portfolio as federal if the sub-portfolio shows
past-due balances drop to zero in 2020:Q3 and remain at at zero
thereafter (see Appendix
[\[sec:data\_appendix\_ccp\]](#sec:data_appendix_ccp){reference-type="ref"
reference="sec:data_appendix_ccp"} for additional details). Our approach
leads us to classify a total of \[\$1.25 trillion\] of the total
\[\$1.45 trillion\] in student loan balances in 2023:Q3 as "federal"
balances, and the aggregate series aligns well over time with data from
the Department of Education (see Appendix Figure
[\[fig:appendix\_sl\_comparison\]](#fig:appendix_sl_comparison){reference-type="ref"
reference="fig:appendix_sl_comparison"}).

We supplement our data with local demographic information from the ACS
and SOI. In particular, we use the total population, share of college
graduates, the share of individuals in various ages bins, \[the share of
households who own their homes or have a mortgage\], and the share of
white and black individuals at a ZIP code level (Manson et al.
2024).[^10] From the SOI, we use ZIP code level data on mean adjusted
gross income (AGI) and the distribution of households across six AGI
bins for tax year 2020.

One limitation of the CCP is that while we observe student loan balances
in each quarter, due to the fact legislation mandated student loan
payments and delinquencies do not show up on credit reports for the
first year of repayment, we do not observe actual or required payments
on these loans. As a result, we evaluate the aggregate effects of
student debt on consumer spending, which is the main policy relevant
question when thinking about the stimulative effects of student loan
forbearance. Further, if we impose the assumption that locations with
higher debts do not have systematically different ratios of required
payments to debt levels, the elasticities we recover are a simple
scaling of the elasticity of spending with respect to required
payments.[^11]

We link Verisk data on zip-code level spending to our student loan
measures, ACS demographic and income data, as well as tax data from SOI.
We drop ZIP codes with a total ACS population below $2,000$ as well as
areas like college towns and military bases where the ACS and CCP
populations do not align due to differences in billing addresses and
residences. In total, our data cleaning process drops $13,366$ ZIP codes
out of a total of $29,953$ in the Verisk data. However, because most of
the ZIP codes we drop have a low population, we maintain over $96$
percent of the total ACS population after filtering. Further details on
our data cleaning procedure are outlined in
[\[sec:data\_appendix\_verisk\]](#sec:data_appendix_verisk){reference-type="ref"
reference="sec:data_appendix_verisk"}.

Summary statistics
------------------

[\[tab:summary\_stats\]](#tab:summary_stats){reference-type="ref"
reference="tab:summary_stats"} shows key ZIP code-level summary
statistics in the filtered and unfiltered data sets. As the table shows,
there is considerable variation across ZIP codes in their student loan
balances per capita, AGI, education, and racial makeup. Comparing the
left and right columns of the table shows that our data cleaning process
does not meaningfully change these statistics.

[\[fig:binscatters\]](#fig:binscatters){reference-type="ref"
reference="fig:binscatters"} shows binscatter relationships between
student loan balances and four key demographic characteristics. Areas
with higher student loan balances per capita tend to have higher
fractions of college educated workers
([\[fig:binscatter-college\_share\]](#fig:binscatter-college_share){reference-type="ref"
reference="fig:binscatter-college_share"}), higher fractions of young
workers
([\[fig:binscatter-65\_population\_share\]](#fig:binscatter-65_population_share){reference-type="ref"
reference="fig:binscatter-65_population_share"}, and vice-versa for
older workers in
[\[fig:binscatter-65\_population\_share\]](#fig:binscatter-65_population_share){reference-type="ref"
reference="fig:binscatter-65_population_share"}), and higher average
incomes
([\[fig:binscatter-income\_share\]](#fig:binscatter-income_share){reference-type="ref"
reference="fig:binscatter-income_share"}). These correlations present a
challenge to identifying the effect of student debt repayment on
spending, since there may be differences in behavior between these
demographic groups that may not necessarily relate to student debt.

1ex

1ex

*Note:* This figure shows the relationship at a ZIP code level of
student loan per capita deciles against mean income, the share of
college degrees, share of individuals aged 25-34, and share of
individuals aged 65 or older.\
*Sources*: CCP, ACS, and IRS SOI.

Because our identification relies on comparing spending in areas
differing in student loan balances, we must ensure that our
specification appropriately addresses differences in spending growth
over the period that may be driven by variation in the college share,
income distribution, and age distribution across ZIP codes. This
motivates the specification we employ in
[\[sec:methodology\]](#sec:methodology){reference-type="ref"
reference="sec:methodology"}, which allows spending growth to vary
flexibly based on local demographic composition and income distribution.
Notably, we are able to do this because our data provide a panel of
weekly measures of spending both before and after the change in student
loan payments, allowing us to disentangle longer-running differences in
spending growth from those driven by the impact of the policy change in
the Fall of 2023.

Empirical Methodology [\[sec:methodology\]]{#sec:methodology label="sec:methodology"}
=====================================================================================

We estimate a two-way fixed effects model to evaluate how the change in
ZIP code spending varied with student loan balances per adult over the
sample period January 2023 through April 2024. Specifically, we estimate
the model below, which relates the 52-week change in per capita spending
in ZIP code $i$ in week $t$ ($\Delta_{52} SpendPC_{it}$) to its per
capita student loan balances as of September 2023 ($SLBalPC_{i}$):

Static Difference-in-Differences: $$\begin{aligned}
\label{eq:static-did}
    \Delta_{52} SpendPC_{it} 
    &= \beta_\text{post} \mathbf{1}_{\left\{ t > Oct2023 \right\}}SLBalPC_{i} \nonumber \\
    &+  \sum_{\tau} \gamma _\tau \mathbf{1}_{\left\{ t = \tau \right\}} \mathbf{X}_i + \delta_{s(i)t} + \alpha_{i} + \varepsilon_{it}.\end{aligned}$$
just changing the one coefficient that captures post-treatment. and
cluster at the individual level.

Dynamic Difference-in-Differences: $$\begin{aligned}
\label{eq:event-study}
    \Delta_{52} SpendPC_{it} 
    &= \sum_{\tau} \beta_{\tau} \mathbf{1}_{\left\{ t = \tau \right\}}SLBalPC_{i} \nonumber \\
    &+ \sum_{\tau} \gamma _\tau \mathbf{1}_{\left\{ t = \tau \right\}} \mathbf{X}_i + \delta_{s(i)t} + \alpha_{i} + \varepsilon_{it}.\end{aligned}$$
where our main object of interest is $\beta_\tau$, which captures the
time-varying response of consumer spending to differential exposure to
student debt per capita. Under the assumption that areas with different
student loan balances (but similar age, education, and income) would
have had similar underlying trends in consumer spending in the absence
of the resumption of student loan repayment, the coefficients recover
the effect of an additional dollar of student loan balances on weekly
consumer spending. Because areas with different demographic and income
characteristics may have different spending trends over this period, and
these factors are correlated with student loan balances, we allow for
trends in spending to vary with the ZIP code's share of college-educated
adults over the age of 25, as well as the share of the population in
different age and income bins. More specifically, we control for the
interaction of time dummies with the ZIP code-level: (i) college share;
(ii) share of population in various age ranges from the 2015-2019 five
year sample of the ACS; and (iii) share of tax filing units in various
adjusted gross income ranges from the SOI. We weight the regression by
the ACS adult population for each ZIP code. We also include state-time
fixed effects, $\delta_{s(i)t}$, to account for state-level differences
in spending patterns owing to weather or regional economic trends.

Identification rests upon the assumption that, absent the student loan
payment resumption, the difference between spending growth in high- and
low-student loan ZIP codes would be zero after controlling for
potentially different spending trends by income bins, age bins, college
share, as well as state-time and ZIP code fixed effects. While we cannot
test this assumption directly, we can test for conditional parallel
trends before the policy. We find no evidence of differential pre-trends
between areas with more or less student debt, all else equal, in the
months prior to the resumption of student loan payments. That is,
spending growth was not significantly different for high- and
low-student loan ZIP codes prior to announcement of the payment
resumption after controlling for local income and demographic factors.
This finding suggests---but does not rule definitively---that spending
growth would have evolved similarly in these ZIP codes absent the end of
forbearance, which helps to support our causal interpretation of the
empirical results.

To further assess the validity of our empirical design, we perform a
placebo test using auto loan debt, which was unaffected by the restart
of student loan payments in October 2023, and which we therefore expect
to be uncorrelated with any trends in consumer spending. To perform this
placebo test, we re-estimate equation
[\[eq:event-study\]](#eq:event-study){reference-type="eqref"
reference="eq:event-study"} using auto loan debt rather than student
loan debt as the main independent variable. Reassuringly, we find no
significant effect of auto loan debt on consumer spending during the
entirety of our sample period (see
[\[fig:auto\]](#fig:auto){reference-type="ref" reference="fig:auto"} in
Appendix [\[sec:appendix\]](#sec:appendix){reference-type="ref"
reference="sec:appendix"}). This finding suggests that the variation in
spending that we estimate in equation
[\[eq:event-study\]](#eq:event-study){reference-type="eqref"
reference="eq:event-study"} is indeed coming from policy related to
student loans, and not some other variation in policy during our sample
period.

Results [\[sec:results\]]{#sec:results label="sec:results"}
===========================================================

Average effects and aggregate implications
------------------------------------------

[\[fig:agg-spending\]](#fig:agg-spending){reference-type="ref"
reference="fig:agg-spending"} shows the causal effect of an extra
\$10,000 of student loan debt on consumer spending, measured at the
weekly level. The figure shows no significant change in consumer
spending between April 2023 and October 2023, consistent with our
assumption of conditional parallel trends. Following the resumption of
student loan payments in October 2023, we see a gradual and persistent
decline in consumer spending.[^12] On average, spending fell by roughly
\[\$6\] per week for every \$10,000 in balances per capita following the
resumption of student loan payments through April 2024, when our data
end. The response builds gradually following the policy change until it
stabilizes at around \[\$12\] per weak in April 2024. The gradual
decline in consumer spending is consistent with widely-documented
features of household behavior, including inattention, consumption
commitments, and habits.

[\[fig:agg-spending\]](#fig:agg-spending){reference-type="ref"
reference="fig:agg-spending"} also allows us to evaluate the response of
spending to two other changes in student loan policy. First, we see no
significant change in spending in June 2023, the vertical orange line,
when Congress enacted a law to prevent further extensions of the payment
pause, which was a significant news shock about loan repayment. Second,
we see no significant change in spending in September 2023, the vertical
blue line, when interest resumed on student loan balances. The above
findings suggest that consumer spending is more responsive to required
payments than news about future payments or the interest rate charged on
such debt.

[\[fig:agg-spending\]]{#fig:agg-spending label="fig:agg-spending"}

*Note:* This figure shows the evolution of spending following the
resumption of student loan repayment for all 18,178 ZIP codes in the
filtered sample, which is obtained from running the model specified in
[\[eq:event-study\]](#eq:event-study){reference-type="ref"
reference="eq:event-study"}. The gray shaded region indicates $95\%$
confidence bands. The solid [orange]{style="color: orange"} line
signifies **June 7, 2023**---the date at which Congress vetoed President
Biden's plan for student loan relief. The dotted
[blue]{style="color: blue"} line signifies **September 1, 2023**, which
is the date at which interest began to accrue on student loan balances.
The blue-shaded region is the month of **October 2023**, the period in
which monthly payments for student loan balances restarted following the
end of student loan forbearance.\
*Sources*: Verisk, CCP, ACS, and IRS SOI.

[\[fig:agg-spending-binned\]]{#fig:agg-spending-binned
label="fig:agg-spending-binned"}

*Note:* This figure shows the evolution of spending over for 18,178 ZIP
over three time periods: (1) "Pre-Annoucement", defined as **June 7,
2023** when Congress vetoed President Biden's plan for student loan
relief; (2) "Post-Announcement", defined as after June 7, 2023 and up
until payment resumption on **October 15, 2023**; (3) "Payment
Resumption", defined as after October 15, 2023. We obtain the estimates
by running the model specified in
[\[eq:event-study\]](#eq:event-study){reference-type="ref"
reference="eq:event-study"} using "Pre-Announcement" as the base period.
We include 95% confidence bands for the "Post-Announcement" and "Payment
Resumption" periods using standard errors clustered at the ZIP code
level.\
*Sources*: Verisk, CCP, ACS, and IRS SOI.

To contextualize our regression estimates, consider the following
back-of-the-envelope calculations for the implied partial equilibrium
effect of the end of student loan forbearance on the level of nominal
PCE and GDP. We begin with two estimates of the weekly elasticity of
spending to student loan balances: \$6 for the average post-policy
effect, and \$11.40 for the peak effect.[^13] These two elasticities
translate to an annualized reduction in spending of \[\$320\] and
\[\$610\] per year for every \$10,000 in student loan balances. For the
mean (median) household with student loans, these numbers imply an
annualized cutback in spending of \[\$800 (\$1,520)\] and \[\$1600
(\$3,040)\], respectively.[^14] Given that we find about \[\$1.25\]
trillion of eligible student debt at the time the policy ended, our
estimates imply that the resumption of student loan payments reduced
consumer spending by \[\$40\] to \$76 billion at an annual rate, which
is roughly \[0.1-0.3\] percent of GDP or \[0.2-0.4\] percent of personal
consumption expenditures in the quarter that forbearance ended.[^15]

While much of the literature on spending responses to policy changes
focuses on measuring the marginal propensity to consume (MPC) from a
shock, our results do not directly translate to comparable figures. In
particular, because servicers were not required to report required
payments to credit bureaus for at least a year after the end of
forbearance, the CCP provides reliable measures of debt levels, but not
required payments in the quarters immediately after payment resumption.
However, we are able to translate the spending response to debt into an
MPC by utilizing information after servicers began reporting required
payments and delinquencies in 2024q3. Overall, these data imply an
aggregate payment-to-balance ratio of roughly 5.75 percent per
year.[^16] Applying these ratios to our estimated effects
per-dollar-of-debt, we find MPCs ranging from 56 percent to 109
percent---somewhat high but not out of the realm of other estimates of
MPCs out of permanent income shocks.

Our study provides the first evidence on the response in consumer
spending following the restart of student loan repayment using actual
spending data, and we find evidence that the end of forbearance caused
meaningful drag on aggregate demand. Our results imply somewhat larger
responses than other studies that did not have actual spending data.
(Chakrabarti, Mangrum, et al. 2023), for example, conduct a survey where
they ask borrowers how they plan to adjust their spending between
October and December 2023 due to the resumption of student loan
repayment. Those authors find that borrowers expect to reduce
consumption by around \$56 per month. Our numbers suggest that the
effect of the resumption of payments may have been larger than their
survey suggested; we find that the average borrower reduced consumption
by about \[\$65-\$130\] per month.

Conclusion [\[sec:conclusion\]]{#sec:conclusion label="sec:conclusion"}
=======================================================================

In this paper, we estimate how household spending changed following the
end of student loan forbearance. To do this, we exploit ZIP code-level
variation in student loan balances, merged with high-frequency data on
ZIP code household spending. Our estimates imply that households reduced
spending meaningfully following the resumption of required payments in
October 2023. The average consumer had to cut back spending by roughly
\[XXX to YYY\] per year. Further, a partial-equilibrium exercise
suggests that the policy was supporting consumer spending by
\[\$40-\$80\] billion at an annual rate.

Our results show that debt forbearance policy can effectively support
aggregate demand. One appeal of this type of policy is that it could
extend additional liquidity to households in times of economic distress
at a lower cost than traditional stimulus checks. Our heterogeneity
analysis addresses a concern about these policies: namely, that they
provide the largest reduction in payments to high-income households, who
hold the most student debt but are the least likely to spend extra
liquidity. We find that higher-income households are indeed the most
exposed to this policy, but our results show that the lowest-income
households actually account for the largest share of the overall
consumption response.

::: {#refs .references .hanging-indent}
::: {#ref-albuquerque2022consumption}
Albuquerque, Bruno, and Alexandra Varadi. 2022. *Consumption Effects of
Mortgage Payment Holidays: Evidence During the Covid-19 Pandemic*.
International Monetary Fund.
:::

::: {#ref-amromin2016education}
Amromin, Gene, and Janice Eberly. 2016. "Education Financing and Student
Lending." *Annual Review of Financial Economics* 8 (1): 289--315.
:::

::: {#ref-armona2022student}
Armona, Luis, Rajashri Chakrabarti, and Michael F Lovenheim. 2022.
"Student Debt and Default: The Role of for-Profit Colleges." *Journal of
Financial Economics* 144 (1): 67--92.
:::

::: {#ref-blundell2008consumption}
Blundell, Richard, Luigi Pistaferri, and Ian Preston. 2008. "Consumption
Inequality and Partial Insurance." *American Economic Review* 98 (5):
1887--1921. <https://doi.org/10.1257/aer.98.5.1887>.
:::

::: {#ref-blundell1998consumption}
Blundell, Richard, and Ian Preston. 1998. "Consumption Inequality and
Income Uncertainty." *The Quarterly Journal of Economics* 113 (2):
603--40. <http://www.jstor.org/stable/2586914>.
:::

::: {#ref-boutros_borrow_2022}
Boutros, Michael, Nuno Clara, and Francisco Gomes. 2024. "Borrow Now,
Pay Even Later: A Quantitative Analysis of Student Debt Payment Plans."
*Journal of Financial Economics*, forthcoming.
<https://doi.org/10.2139/ssrn.4245812>.
:::

::: {#ref-catherine2023distributional}
Catherine, Sylvain, and Constantine Yannelis. 2023. "The Distributional
Effects of Student Loan Forgiveness." *Journal of Financial Economics*
147 (2): 297--316.
:::

::: {#ref-chakrabarti2023tuition}
Chakrabarti, Rajashri, Vyacheslav Fos, Andres Liberman, and Constantine
Yannelis. 2023. "Tuition, Debt, and Human Capital." *The Review of
Financial Studies* 36 (4): 1667--1702.
:::

::: {#ref-chakrabarti2023borrower}
Chakrabarti, Rajashri, Daniel Mangrum, Sasha Thomas, and Wilbert Van der
Klaauw. 2023. "Borrower Expectations for the Return of Student Loan
Repayment." Federal Reserve Bank of New York.
:::

::: {#ref-di2019second}
Di Maggio, Marco, Ankit Kalda, and Vincent Yao. 2019. "Second Chance:
Life Without Student Debt." National Bureau of Economic Research.
:::

::: {#ref-dinerstein2024debt}
Dinerstein, Michael, Constantine Yannelis, and Ching-Tse Chen. 2024.
"Debt Moratoria: Evidence from Student Loan Forbearance." *American
Economic Review: Insights* 6 (2): 196--213.
<https://doi.org/10.1257/aeri.20230032>.
:::

::: {#ref-field2009educational}
Field, Erica. 2009. "Educational Debt Burden and Career Choice: Evidence
from a Financial Aid Experiment at Nyu Law School." *American Economic
Journal: Applied Economics* 1 (1): 1--21.
:::

::: {#ref-ganong2019consumer}
Ganong, Peter, and Pascal Noel. 2019. "Consumer Spending During
Unemployment: Positive and Normative Implications." *American Economic
Review* 109 (7): 2383--2424.
:::

::: {#ref-ganong2020liquidity}
---------. 2020. "Liquidity Versus Wealth in Household Debt Obligations:
Evidence from Housing Policy in the Great Recession." *American Economic
Review* 110 (10): 3100--3138.
:::

::: {#ref-GossMangrumScaley2024}
Goss, Jacob, Daniel Mangrum, and Joelle Scally. 2024. "Assessing the
Relative Progressivity of the Biden Administration's Federal Student
Loan Forgiveness Proposal." *Education Finance and Policy* 19 (4):
716--33. <https://doi.org/10.1162/edfp_a_00429>.
:::

::: {#ref-herbst2023IDR}
Herbst, Daniel. 2023. "The Impact of Income-Driven Repayment on Student
Borrower Outcomes." *American Economic Journal: Applied Economics* 15
(1): 1--25. <https://doi.org/10.1257/app.20200362>.
:::

::: {#ref-jacob2024value}
Jacob, Brian A, Damon Jones, and Benjamin J Keys. 2024. "The Value of
Student Debt Relief and the Role of Administrative Barriers: Evidence
from the Teacher Loan Forgiveness Program." *Journal of Labor Economics*
42 (S1): S261--S292.
:::

::: {#ref-katz_saving_2023}
Katz, Justin. 2023. "Saving and Consumption Responses to Student Loan
Forbearance." SSRN Scholarly Paper. Rochester, NY.
<https://doi.org/10.2139/ssrn.4344262>.
:::

::: {#ref-krishnan2019cost}
Krishnan, Karthik, and Pinshuo Wang. 2019. "The Cost of Financing
Education: Can Student Debt Hinder Entrepreneurship?" *Management
Science* 65 (10): 4522--54.
:::

::: {#ref-lee2023household}
Lee, Sean Chanwook, and Omeed Maghzian. 2023. "Household Liquidity and
Macroeconomic Stabilization: Evidence from Mortgage Forbearance."
:::

::: {#ref-lochner2016student}
Lochner, Lance, and Alexander Monge-Naranjo. 2016. "Student Loans and
Repayment: Theory, Evidence, and Policy." In *Handbook of the Economics
of Education*, 5:397--478. Elsevier.
:::

::: {#ref-luo2019assets}
Luo, Mi, and Simon Mongey. 2019. "Assets and Job Choice: Student Debt,
Wages and Amenities."
:::

::: {#ref-ACS_NHGIS}
Manson, Steven, Jonathan Schroeder, David Van Riper, Katherine Nowles,
Tracy Kugler, Finn Roberts, and Steven Ruggles. 2024. "IPUMS National
Historical Geographic Information System: Version 19.0 \[Dataset\]."
<https://doi.org/10.18128/D050.V19.0>.
:::

::: {#ref-mezza2020student}
Mezza, Alvaro, Daniel Ringo, Shane Sherlund, and Kamila Sommer. 2020.
"Student Loans and Homeownership." *Journal of Labor Economics* 38 (1):
215--60.
:::

::: {#ref-mian2023partisan}
Mian, Atif, Amir Sufi, and Nasim Khoshkhou. 2023. "Partisan Bias,
Economic Expectations, and Household Spending." *The Review of Economics
and Statistics* 105 (3): 493--510.
<https://doi.org/10.1162/rest_a_01056>.
:::

::: {#ref-mueller2019rise}
Mueller, Holger M, and Constantine Yannelis. 2019. "The Rise in Student
Loan Defaults." *Journal of Financial Economics* 131 (1): 1--19.
:::

::: {#ref-mueller2022increasing}
Mueller, Holger, and Constantine Yannelis. 2022. "Increasing Enrollment
in Income-Driven Student Loan Repayment Plans: Evidence from the Navient
Field Experiment." *The Journal of Finance* 77 (1): 367--402.
:::

::: {#ref-NYFedEquifax}
New York Fed Consumer Credit Panel. 2023. "Federal Reserve Bank of New
York/Equifax Consumer Credit Panel."
:::

::: {#ref-Perry_et_al_brookings}
Perry, Marshall AND Romer, Andre AND Steinbaum. 2021. "Student Loans,
the Racial Wealth Divide, and Why We Need Full Student Debt
Cancellation." *Brookings Institution*.
<https://www.brookings.edu/articles/student-loans-the-racial-wealth-divide-and-why-we-need-full-student-debt-cancellation/>.
:::

::: {#ref-rothstein2011constrained}
Rothstein, Jesse, and Cecilia Elena Rouse. 2011. "Constrained After
College: Student Loans and Early-Career Occupational Choices." *Journal
of Public Economics* 95 (1-2): 149--63.
:::

::: {#ref-schneider2024household}
Schneider, Patrick, and Patrick Moran. 2024. "Household Liquidity
Policy." <https://doi.org/10.2139/ssrn.5066013>.
:::

::: {#ref-Verisk}
Verisk. 2024. "Verisk Commerce Signals Spend Tracker."
:::

::: {#ref-yannelis2022student}
Yannelis, Constantine, and Greg Tracey. 2022. "Student Loans and
Borrower Outcomes." *Annual Review of Financial Economics* 14 (1):
167--86.
:::
:::

[^1]: We are grateful for helpful comments from Rajashri Chakrabarti,
    Sarena Goodman, Alvaro Mezza, Urvi Neelakantan, as well as
    participants at the CFPB Consumer Finance Round Robin and the
    Federal Reserve System Conference on Heterogeneity. The views of
    this paper are solely the responsibility of the authors and should
    not be interpreted as reflecting the views of the Board of Governors
    of the Federal Reserve System or of any other person associated with
    the Federal Reserve System. Federal Reserve Board, 20th St. and
    Constitution Avenue, NW, Washington, DC, 20551. Email:
    <aditya.aladangady@frb.gov>,
    [edmund.s.crawley@frb.gov](mailto:EMAIL), <will.gamber@frb.gov>,
    [patrick.e.donnellymoran@frb.gov](mailto:EMAIL), and
    <jose.a.nino@frb.gov>.

[^2]: See: <https://www.gao.gov/products/gao-24-107150>.

[^3]: Notably, the larger response among lowest income quintile ZIP
    codes comes in spite of the fact that income-driven repayment (IDR)
    plans meant that the same debt balances translate into smaller debt
    payments for lower income borrowers.

[^4]: For more comprehensive reviews of the literature, see (Amromin and
    Eberly 2016), (Lochner and Monge-Naranjo 2016), and (Yannelis and
    Tracey 2022).

[^5]: Given that the standard repayment term is ten years, it is likely
    that borrowers with outstanding debt more than ten years after
    taking out their loan behaved differently than the average borrower.

[^6]: While anticipated negative income shocks are relatively rare, they
    have important implications for our understanding of
    consumption-saving behavior, as argued by (Ganong and Noel 2019).
    Similar to those authors, we see a decline in spending following the
    drop in income, despite this drop being anticipated---a result that
    is in conflict with standard buffer-stock-type models.

[^7]: Verisk is now owned by Transunion.

[^8]: Similar data at a monthly frequency from Verisk are used in (Mian,
    Sufi, and Khoshkhou 2023).

[^9]: As discussed by (Dinerstein, Yannelis, and Chen 2024), the payment
    pause only applied to "federal" loans, which included all loans in
    the Direct Loan program, as well as roughly \$100 billion in FFEL
    loans issued by private lenders and later bought by the Department
    of Education. These loans comprise the majority of overall student
    loan balances. Our approach allows us to identify which servicers
    were managing these eligible federal loans.

[^10]: We convert the ZCTA-level ACS aggregates from NHGIS to ZIP codes
    using the crosswalk provided by the Health Resources and Services
    Administration: <https://geocarenavigator.hrsa.gov/>.

[^11]: As we discuss later in the paper, we estimate that the average
    annual payment-to-balance ratio is roughly 5.75 percent per year.

[^12]: We note that student loan payments began at different times for
    different borrowers during the month of October, hence the blue
    shaded region in
    [\[fig:agg-spending\]](#fig:agg-spending){reference-type="ref"
    reference="fig:agg-spending"} captures the window during which
    payments resumed for student loan borrowers.

[^13]: It is useful to consider these two alternatives because our data
    end in April 2024, preventing us from tracking the spending response
    for longer. We think of these two assumptions as reasonable bounds
    on the overall annual effect.

[^14]: These numbers are based on the median debt level for a borrower
    in the 2022 Survey of Consumer Finances, about \$25,000 and the mean
    student loan debt per borrower, about \$47,000.

[^15]: Note that while our estimates may include very local (i.e.,
    within-ZIP code) general equilibrium effects, these estimates and do
    not consider the numerous national general equilibrium effects that
    could either offset or magnify this change in aggregate demand.

[^16]: Because of changes in IDR plans and deferrals due to the SAVES
    Act in 2024, we opt to utilize data after reporting resumed rather
    than older data on payments. We find the ratio of annual required
    payments to balances for federal loans in the CCP where required
    payments are positive was 10.4 percent per year in 2024:Q3 through
    2025:Q1. In addition, roughly 30 percent of federal loans by balance
    had required payments of 0. (Zero required payments reflect a
    combination of grace periods/deferrals, in-school status, and
    forbearance. See: <https://www.congress.gov/crs-product/IF12896>)
