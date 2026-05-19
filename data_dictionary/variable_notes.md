# Variable notes

These notes summarize the variable structure, column indexing, and input/output file conventions used across the analysis scripts.

## General dataset structure
- Column 1: participant identifier
- Columns 2–19: baseline variables
- Columns 20–151: questionnaire response variables used for accuracy analyses

## Accuracy item blocks
Across multiple scripts, questionnaire response columns are indexed as follows:

- Overall accuracy items: columns 20–151
- Risk / etiology items: columns 20–63
- Diagnostic items: columns 64, 66, 68, ..., 150
- Triage items: columns 65, 67, 69, ..., 151

These conventions are used in:
- primary GLMM analysis
- ITT deterministic boundary analyses
- crude pooled proportion analyses
- question-type accuracy plot generation
- subgroup analyses

## Baseline variables
Baseline table scripts use columns 1–19 and include variables such as:
- Sex
- Partner
- Educational_attainment
- Health_insurance_coverage
- Self_assessment_of_health_status
- Work
- Current_smoking_status
- The_frequency_and_types_of_alcohol_consumption
- Household_income
- The_number_of_chronic_diseases
- Territory
- TIME
- Height
- Weight
- Year / age-related variable
- The_number_of_visits_of_doctors
- Number_of_emergency_room_visits
- Number_of_hospitalizations

## Primary compliant-case input files
The primary compliant-case analyses use:
- `data/LungDiag.xlsx`
- `data/Control.xlsx`

## ITT deterministic boundary-analysis raw recoding inputs
The deterministic ITT boundary analyses begin from:
- `data/LungDiag_corrected.xlsx`
- `data/control_corrected.xlsx`

## Supplementary Table S2: deterministic ITT boundary-analysis datasets
Supplementary Table S2 comprises three predefined deterministic boundary scenarios for assigned-but-unanswered questionnaire items.

### Panel A
- LungDiag assigned-but-unanswered items coded as incorrect
- Control assigned-but-unanswered items coded as correct

Derived input files:
- `data/LungDiag_itt_imputed_na_as_incorrect.xlsx`
- `data/control_itt_imputed_na_as_correct.xlsx`

Output file:
- `itt_panelA_glmm_results.docx`

### Panel B
- All assigned-but-unanswered items coded as correct in both arms

Derived input files:
- `data/LungDiag_itt_panelB_na_as_correct.xlsx`
- `data/control_itt_panelB_na_as_correct.xlsx`

Output file:
- `itt_panelB_glmm_results.docx`

### Panel C
- All assigned-but-unanswered items coded as incorrect in both arms

Derived input files:
- `data/LungDiag_itt_panelC_na_as_incorrect.xlsx`
- `data/control_itt_panelC_na_as_incorrect.xlsx`

Output file:
- `itt_panelC_glmm_results.docx`

## Interpretation of "NA" and blanks
- text `"NA"` = assigned-but-unanswered item
- blank cell = unasked item

## Supplementary Table S1
Crude pooled item-level proportions are provided in:
- `supplementary_table_S1_crude_proportions.docx`

## Supplementary Table S2
Deterministic ITT boundary-analysis results are provided in:
- `itt_panelA_glmm_results.docx`
- `itt_panelB_glmm_results.docx`
- `itt_panelC_glmm_results.docx`

## Supplementary Figure S1
Crude item-level accuracy plot is provided in:
- `supplementary_figure_S1_accuracy_plot.pdf`

## Supplementary Figure S2
Subgroup forest plot is provided in:
- `supplementary_figure_S2_subgroup_forestplot.pdf`

## Supplementary Table S3
Supplementary Table S3 was based on manually tabulated source counts, which are provided in:
- `supplementary/Supplementary_Data_1_Triage_urgency_source_data.xlsx`
