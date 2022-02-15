# projecting-the-suicide-burden-of-climate-change-in-the-united-states
The R code for Projecting the Suicide Burden of Climate Change in the United States, as well as details on the R version and packages that should be helpful for results reproduction.

Projecting the Suicide Burden of Climate Change in the United States
EPA Climate Change Division Climate Change Impacts and Risk Analysis (CIRA) Project (https://www.epa.gov/cira)
Authors: Anna Belova1, Caitlin A. Gould2, Kate Munson3, Madison Howell4, Claire Trevisan6, Nicholas Obradovich5, Jeremy Martinich2

1 – ICF, Pittsburgh, PA, United States 2 – Climate Change Division, U.S. Environmental Protection Agency, Washington, DC, 20460, United States 3 – ICF, Cambridge, MA, 02140, United States 4 – Hewlett Packard, Indianapolis, IN, 46234, United States 5 – Max Planck Institute for Human Development, Berlin, Germany 6 – ICF, Fairfax, VA, United States

Address correspondence to: Caitlin A. Gould, gould.caitlin@epa.gov, WJC Building South Room 4226G 1200 Pennsylvania Ave NW, Washington, DC, 20460, United States

Funding: This work was funded by the U.S. Environmental Protection Agency’s Climate Science and Impacts Branch, Climate Change Division, Office of Atmospheric Programs, Office of Air & Radiation, to support advancements in understanding the relationship between climate change and mental health in the United States. The views expressed in this article are solely those of the authors and do not necessarily represent those of the U.S. Environmental Protection Agency or the federal government. All phases of this study were supported by ICF under EPA contract number 68HERH19D0029.

Data Sharing* Modeling code, inputs and results are accessible on the EPA’s Environmental Dataset Gateway.

Abstract
Background: Climate change is linked to many acute and chronic public health effects. There is growing research exploring its relationship to mental health outcomes. Costs of treating mental health conditions in the U.S. have increased over time. Objectives: We build on climate change and suicide-focused mental health research to quantify and monetize changes in suicide incidence across the U.S. in response to increasing levels of warming based on six climate projections.

Methods: We develop an integrated health impact assessment model using binned and linear specifications of temperature-suicide relationship estimates from Mullins and White (2019) in combination with monthly age- and sex-specific baseline suicide incidence rates, projections of six climate models, and population projections at the U.S. county scale. We evaluate the difference in annual numbers of suicides in the U.S. corresponding to 1°C-6°C warming compared to 1986-2005 average temperatures, and compute 2015 population attributable fractions (PAF). We use the U.S. Environmental Protection Agency’s Value of a Statistical Life to estimate the economic value of avoiding these mortality impacts.

Results: Assuming 2015 population size, warming of 1°C-6°C could result in an annual increase of 283-1,660 suicide cases, corresponding to a PAF of 0.7%-4.1%. The economic value of avoiding these impacts is $2 billion-$3 billion (2015 U.S. dollars, 3% discount rate, 2015 income level). Estimates based on linear temperature-suicide relationship specifications are 7% larger than those based on binned temperature specification. Accounting for displacement decreases estimates by 17%, while accounting for precipitation decreases estimates by 7%. Population growth between 2015 and future warming degree arrival year increases estimates by 15%-38%.

Discussion: We provide insights into the magnitude of climate change impacts on U.S. suicide incidence. Further research is needed to quantify and monetize other climate-related mental health outcomes (e.g., anxiety, depression, addiction disorders) and to characterize these risks in vulnerable populations.

Modeling Pipeline
Setup
Create RStudio project
Customize setup.sh to indicate locations of folders with data, workbooks, and source code. The data used for this project can be downloaded from EPA’s Environmental Dataset Gateway.
Confirm that paths specified in configuration.yaml are accurate.
Execute Simulations
A single scenario for a warming degree, population and income year can be run using runSimulation.sh.
simulate/estimate.R evaluates scenario.
simulate/testConvergence.R tests convergence of the Monte Carlo simulations for scenarios run in sampling mode.
All scenarios evaluated for the analysis can be run using runMultSimulations.sh
Produce Results Summaries
To produce the markdown results file run summarize_outputs/reportFinalResults.Rmd.
To procude maps run summarize_outputs/createMaps.R.
Results Files
Detailed point-mode results for warming degree XC
Results for 2015 population and 2015 income level (discounted at 3% to 2015): DX_Scenario_PTMODE-TRUE_POPYR-PRESENT_INCYR-PRESENT_DR-3_DY-2015...

Results for projected future population and 2015 income level (discounted at 3% to 2015): DX_Scenario_PTMODE-TRUE_POPYR-FUTURE_INCYR-PRESENT_DR-3_DY-2015...

Results for projected future population and income level (discounted at 3% to 2015): DX_Scenario_PTMODE-TRUE_POPYR-FUTURE_INCYR-FUTURE_DR-3_DY-2015...

All files are in CSV format with generation date/time stamp appended at the end.

File layout:

HIF : health impact function (binned, binned with displacement, linear, linear with precipitation)
ST : state abbreviation
FIPS : county FIPS code
SEX : sex category
AGE : age group
MODEL : GCM model
YEAR_CLIM : calendar year of climate projection
YEAR_POP : calendar year of population projection
D_le_30 : average monthly number of days below 30F
D_30_40 : average monthly number of days between 30F and 40F
D_40_50 : average monthly number of days between 40F and 50F
D_50_60 : average monthly number of days between 50F and 60F
D_80_ge : average monthly number of days above 80F
TEMP : average monthly temperature
PREC : average monthly cumulative precipitation
POP_SIZE : population size
IR100K : baseline suicide rate per 100K
CASES_PT : number of cases (point estimate)
YEAR_INC : calendar year for income level
INCGF : income growth factor
VSL_PT : projected value of a statistical life (point estimate)
PDV_PT : present discounted value (point estimate)
Summary point-mode results for warming degree XC
Summary results for 2015 population and 2015 income level (discounted at 3% to 2015): sum-DX_Scenario_PTMODE-TRUE_POPYR-PRESENT_INCYR-PRESENT_DR-3_DY-2015...

Summary results for projected future population and 2015 income level (discounted at 3% to 2015): sum-DX_Scenario_PTMODE-TRUE_POPYR-FUTURE_INCYR-PRESENT_DR-3_DY-2015...

Summary results for projected future population and income level (discounted at 3% to 2015): sum-DX_Scenario_PTMODE-TRUE_POPYR-FUTURE_INCYR-FUTURE_DR-3_DY-2015...

All files are in CSV format with generation date/time stamp appended at the end.

File layout:

HIF : health impact function (binned, binned with displacement, linear, linear with precipitation)
MODEL : GCM model
cases : total number of cases (point estimate)
pdv : present discounted value (point estimate)
DEG : warming degree
ST : state abbreviation
Detailed sampling-mode results for warming degree XC
Results for 2015 population and 2015 income level (discounted at 3% to 2015): DX_Scenario_PTMODE-FALSE_POPYR-PRESENT_INCYR-PRESENT_DR-3_DY-2015...

Results for projected future population and 2015 income level (discounted at 3% to 2015): DX_Scenario_PTMODE-FALSE_POPYR-FUTURE_INCYR-PRESENT_DR-3_DY-2015...

Results for projected future population and income level (discounted at 3% to 2015): DX_Scenario_PTMODE-FALSE_POPYR-FUTURE_INCYR-FUTURE_DR-3_DY-2015...

All files are in CSV format with generation date/time stamp appended at the end.

File layout:

HIF : health impact function (binned, binned with displacement, linear, linear with precipitation)
ST : state abbreviation
SEX : sex category
AGE : age group
MODEL : GCM model
YEAR_CLIM : calendar year of climate projection
YEAR_POP : calendar year of population projection
YEAR_INC : calendar year for income level
INCGF : income growth factor
ITER : sampled iteration number
D_le_30 : average monthly number of days below 30F
D_30_40 : average monthly number of days between 30F and 40F
D_40_50 : average monthly number of days between 40F and 50F
D_50_60 : average monthly number of days between 50F and 60F
D_80_ge : average monthly number of days above 80F
TEMP : average monthly temperature
PREC : average monthly cumulative precipitation
POP_SIZE : population size
IR100K : baseline suicide rate per 100K
CASES_PT : number of cases (point estimate)
HIF_NORM : Euclidean norm of the sampled health impact function parameters
VSL_PT : projected value of a statistical life (point estimate)
PDV_PT : present discounted value (point estimate)
CASES_ITER : iteration-specific number of estimated suicide cases
VSL_ITER : iteration-specific projected value of a statistical life
PDV_ITER : iteration-specific present discounted value
Summary sampling-mode results for warming degree XC
Summary results for 2015 population and 2015 income level (discounted at 3% to 2015): sum-DX_Scenario_PTMODE-FALSE_POPYR-PRESENT_INCYR-PRESENT_DR-3_DY-2015...

Summary results for projected future population and 2015 income level (discounted at 3% to 2015): sum-DX_Scenario_PTMODE-FALSE_POPYR-FUTURE_INCYR-PRESENT_DR-3_DY-2015...

Summary results for projected future population and income level (discounted at 3% to 2015): sum-DX_Scenario_PTMODE-FALSE_POPYR-FUTURE_INCYR-FUTURE_DR-3_DY-2015...

All files are in CSV format with generation date/time stamp appended at the end.

File layout:

HIF : health impact function (binned, binned with displacement, linear, linear with precipitation)
MODEL : GCM model
stat : statistic
NITER : number of iterations
PT : point estimate
MEAN : mean over iterations
LCB : 5th percentile over iterations
UCB : 95th percentile over iterations
cases : total number of cases (point estimate)
pdv : present discounted value (point estimate)
DEG : warming degree
ST : state abbreviation
Convergence tests for sampling-mode results for warming degree XC
Convergence testing results for 2015 population and 2015 income level (discounted at 3% to 2015): convTest-DX_Scenario_PTMODE-FALSE_POPYR-PRESENT_INCYR-PRESENT_DR-3_DY-2015...

Convergence testing results for projected future population and 2015 income level (discounted at 3% to 2015): convTest-DX_Scenario_PTMODE-FALSE_POPYR-FUTURE_INCYR-PRESENT_DR-3_DY-2015...

Convergence testing results for projected future population and income level (discounted at 3% to 2015): convTest-DX_Scenario_PTMODE-FALSE_POPYR-FUTURE_INCYR-FUTURE_DR-3_DY-2015...

All files are in CSV format with generation date/time stamp appended at the end.

File layout:

NITER : number of iterations
AGE : age category
SEX : sex category
ST : state abbreviation
HIF : health impact function
MODEL : GCM model
stat :
CASES_LCB : 5th percentile of cases over iterations
CASES_MEAN : mean cases over iterations
CASES_UCB : 95th percentile of cases over iterations
PDV_LCB : 5th percentile of PDV over iterations
PDV_MEAN : mean PDV over iterations
PDV_UCB : 95th percentile of PDV over iterations
value : value of the convergence test
TEST : TRUE if converged / FALSE if did not converge
Other files
ParSampleSet_100.csv: Sampled parameter values used in simulations (a set of 100)
results_SUMMARY_2021-1018.html: Journal paper tables
Pop2015.png: County-level population map
SIR100K.png: County-level baseline suicide incidence rate map
D1_cases.png, D3_cases.png: County-level excess suicide cases map (1C and 3C warming degree)
D1_inc.png, D3_inc.png: County-level excess suicide cases per 100K population map (1C and 3C warming degree)
D1_paf.png, D3_paf.png: County-level excess suicide cases as a share of baseline incidence map (1C and 3C warming degree)
D3_avgTemp_GCM.png: County-level map of average temperature for 3C warming degree projected by a GCM
D3_D80ge_GCM.png: County-level map of days above 80F for 3C warming degree projected by a GCM
D3_PREC_GCM.png: County-level map of precipitation for 3C warming degree projected by a GCM
