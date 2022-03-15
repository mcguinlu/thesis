## Chapter 1 - Background

**Key takeaway:**

* Dementia is a pressing public health concern.
* Evidence-based prevention, using readily modifiable risk factors, will be important as the population ages.
* Current evidence on lipid-dementia relationship is contradictory

**Favourite/most interesting aspect:**

* Definining a theoretical framework - wasn't something I had done before, and so it was interesting to have to formally define how my research interests fit together.

<hr>

## Chapter 2 - `medrxivr`

**Key takeaways:**

* Preprints can contain important information.

* Developed a new tool to search medical/health-related preprints, published in Journal of Open Source Software

* Also allows for access to preprint data, enabling two case studies:

    * Journal policies affect the availability of data.
    * Two-thirds of the preprints published allowing for a two-year lag.

**Strengths:**

* Transparent and reproducible searching, enabling the systematic review in Chapter 3 & 4.

* Advantages over previous tools:

    * Allows for searching of preprints, rather than just summarising social media engagement
    * Allows historical searching, not just most recent

**Weaknesses**

* Only searches metadata, not the full text of the preprint.

* Doesn't fully address publication bias, as preprints still represent a prepared manuscript.

**Favourite/most interesting aspect:**

* A large proportion of preprints are not formally published after a two-year lag.

* The peer review process for software is great.

<hr>

## Chapter 3/4 - Systematic review

**Key takeaways:**

* Review sought to identify all evidence, regardless of study design or publication status.

* Most studies:

    * were non-randomised studies (Figure 4.17)
    * examined all-cause dementia and Alzheimer's disease (Table 4.1)
    * were published (Figure 4.1)
    * were based in Western countries (Figure 4.2)
    * were poorly reported.

* **Some indication of a protective effect of statins on ACD/Alzheimers** in NRSI but not in RCT or MR, suggesting:

    * Presence of bias in NRSI
    * Different exposure windows are important

* Risk of bias was generally high, and ROB-ME was also high.

* For Mendelian randomisations studies:

    * Lots of overlap between those using two-sample approach
    * Lack of search filter/risk-of-bias tool
    * Strong potential for missing evidence due to multiple comparisons
    * Substantial attenuation following adjustment for ApoE4

* Suggestion of heterogeneity (high I2), but could only investigate for sex (no effect) because:

    * Small number of meta-analysis had 10 or more results
    * Reporting of important variables was poor

* Weak evidence of dose-response (Figure 4.16) or publication bias (Appendix A.4.2)

**Strengths:**

* Comprehensiveness of review, above existing reviews

* Sturctured approach to risk-of-bias assessment, including novel ROB-ME assessment

* Inclusion of preprints as an evidence source

* Piloting of new methodologies, including preprint searching, ROB-ME assessment, and risk-of-bias visualisation

**Weaknesses:**

* Only a sample of records were dual-screened

* Search may be out of date

* Preliminary version of ROBINS-E used

* Strong potential for missing evidence

**Favourite/most interesting aspect:**

* Opportunity to experiment with visualisation methods (map, paired forest plots, etc.)

* Risk of bias due to missing evidence - first time using this approach, and interesting to see how blatant it often is.

<hr>

## Chapter 5 - CPRD

**Key takeaways:**

* Aimed to address two issues identified in review:

    * Absence of evidence on vascular dementia
    * Bias due to immortal time, as per risk of bias assessments

* Examined 1.6 million participants, with median follow-up of 6 years.

* Fully adjusted Cox propotional hazards model with time-varying treatment indicator suggested:

    * No association of treatment with probable/possible Alzheimer's disease
    * Harmful association of treatment with all-cause, vascular and other dementia

* For specific treatments:
    * Main effect driven by statin group
    * Ezetimibe with increased risk of vascular and other dementia
    * Fibrates with increased risk of all-cause dementia and probable Alzheimers
    
* Control outcomes (IHD, Backpain, Type 2 Diabetes) showed strong potential for confounding by indication, likely related to vascular factors (Figure 5.12)

* Sensitivity analyses:

    * Results robust to imputation, covariate list and pregnancy status
    * Entry period: vairation in risk only in prob AD group, but examining diagnosis by period didn't explain it.
    * Statin properties
        * Hydophilic pull effect estimate to the left vs lipophilic 
        * Contrary to idea that lipophilic is better as can cross blood-brain barrier
    * Smeeth
        * Comparison of codelist didn't help (Figure 5.13)
        * Obtained hamrful results using their codelist, in contract with their paper

**Strengths:**

* Provides evidence on vascular dementia
* Size of the CPRD - one of the largest available studies, as per review
* Use of time-updating treatment covariate

**Weaknesses:**

* Differential misclassfication of the outcome on the basis of treatment
* Challenging comparing across studies using different codelists
* Uncontrolled confounding due to ApoE4
* Potential for reverse confoudning

**Favourite/most interesting aspect:**

* Value of including control outcomes to illustrate confounding
* Comparison with published work using target trial approach

<hr>

## Chapter 6 - Individual patient data analysis

**Key takeaway:**

* Aimed to collect and combine multiple cohorts to:

    * Identify participants most likely to benefit from lipid lowering (age/sex)
    * Produce more evidence on lipids and vascular dementia

* Applied for access to cohorts from the review and the DPUK (Figure 6.1):
    * 3 (8%) of 37 cohorts approached provided data
    * Even within the DPUK, just over half responded at all, and many excluded (Table 6.1)

* Three included studies were all based in the UK: Caerphilly, EPIC Norfolk, WhiteHall II

* Presented results for two models (results robust between them, Figure 6.4):
    * Minimally-adjusted (M1): age, sex, smoking, alcohol, education and diabetes
    * Maximally-adjusted (M2): M1 + ethnicity, prevalent ischemic heart disease, and BMI

* Across 11,000 participants, 1 mmol/L increase had no effect, except for *(tentative)* harmful association of triglycerides and vascular dementia (Figure 6.3)

* Interaction analysis:
    * All-cause dementia - protective effect of male gender on LDL-c/all-cause dementia
    * Vascular dementia - could only investigate age, and no evidence for an interaction.

* Qualitative difference between previous analysis of Whitehall II for AD/triglycerides (Figure 6.9), potentially due to use differnt handling of missing data and ApoE4 adjustment

**Strengths:**

* Used systematic approach to collect data from cohorts
* Incoporates previously unanalysed cohorts w.r.t this question
* Provides new evidence on vascular dementia
* Examined interaction between age/sex and lipid-dementia relationship

**Weaknesses:**

* Low response rate to requests for data (not unexpected)
* Uncontrolled confounding (ApoE4, etc.)
 
**Favourite/most interesting aspect:**

* Minimal impact of adjustment, similar to that observed in Chapter 5 (Figure 6.4)
* Low response rate from DPUK, despite it being a managed process

<hr>

## Chapter 7 - Triangulation

**Key takeaways:**

* Qualitative triangulation faces issues at scale:
    * Difficult to interpret (previous tri studies include only a few studies)
    * Summarising by study desing loses useful result-level information

* Proposed quantitative framework:
    1. Define the causal question of interest 
    2. Identify relevant evidence sources and standardise effect directions 
    3. Specify an idealised version of each study 
    4. Assess the extent and direction of bias/indirectness in each result 
    5. Define modifying terms for bias and indirectness in each result 
    6. Calculate bias-/indirectness-adjusted results and perform meta-analysis 

* Applied to two causal questions:
    * Midlife LDL-c on Alzheimer's disease
    * Midlife trigylcerides on vascular dementia 

* Calculating the absolute direction of bias (Figure 7.1)

* Values for prior bias/indirectness distributions obtained from previous analysis (Turner et al.)

**Strengths:**

* Based on comprehensive systematic review, combined with additional studies
* Flexibility to define prior a priori, avoiding misclassification
* Accounting for biases is no less problematic than synthesising effect estimates as if they were unbiased.

**weaknesses:**

* Defining reasonable prior distibutions
* Accuracy of risk of bias/indirectness assessment
* Handling of low and critical risk of bias judgements
* Combination of different effect estimates

**Favourite/most interesting aspect:**

<hr>

## Chapter 8 - Discussion

**Key takeaway:**

* Summary of findings
    * No consistent evidence of effect of lipids at midlife on dementia outcomes     

* Two new big studies recently published:
    * Iwagami - 1 million CPRD patients
        * Slight increased risk of Alzheimer's disease with increased LDL-c
        * No adjustment for ApoE4, and simulation illustrates this would explain this
        * Reportedly studied TG-vascular dementia relationship, but not reported
    * Gong - 500000 UK BioBank patients
        * No association between lipids and dementia outcomes in 500,000 UK Biobank 
        * Similar to Iwagami, may be subject to residual confounding due to ApoE4

* Implications for clinical practice
    * No clear guidance
    * Clinicians should be aware of uncertainty and be prepared to communicate it to patients

* Methodological contributions
    * Inclusion of preprinted evidence
    * Assessing and visualising risk of bias
    * Triangulation framework

* Future research
    * Long-term follow-up of existing trials of statins
    * Focus on obtaining additional evidence on vascular dementia (e.g. VaD GWAS)
    * Guidance on handling discrepancies between preprinted and published records
    * Tools for reviewing MR studies (search filter/ROB tool/duplication of TSMR)
    * Expansion of triangulation methods (empirical priors, incorporation of ROB-ME)

**Strengths:**

* Identification and triangulation of multiple sources of evidence
* Production of new evidence:
    * from previously unanalysed data
    * on understudied outcome
* Production of software to support new ES techniques

**Weaknesses:**

* Absence of well-understood mechanism for Alzheimer's disease
* Reliance on secondary data sources - no primary data collected
* Missing evidence was common (reported results/vascular dementia/data access in IPD)
* Geographical focus - lots of UK/Western based evidence, so may not generalise