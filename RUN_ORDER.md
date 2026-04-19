```md
# Run order

## Before running
1. Run all scripts from the repository root.
2. Make sure all required Excel files are present in the `data/` folder.
3. Make sure `reproducibility/sessionInfo.txt` has been generated using the final software environment.

## Recommended execution order

### Primary analysis
1. `analysis/01_primary_compliant_case_glmm.R`
   - reproduces Table 2

### ITT-principle deterministic boundary analyses

#### Panel A: AI assigned-but-unanswered = incorrect; control assigned-but-unanswered = correct
2. `analysis/02_prepare_itt_panelA_dataset_part1.R`
3. `analysis/03_prepare_itt_panelA_dataset_part2.R`
4. `analysis/04_itt_panelA_glmm.R`

#### Panel B: all assigned-but-unanswered = correct in both arms
5. `analysis/05_prepare_itt_panelB_all_correct.R`
6. `analysis/05b_prepare_itt_panelB_ai_all_correct.R`
7. `analysis/06_itt_panelB_glmm.R`

#### Panel C: all assigned-but-unanswered = incorrect in both arms
8. `analysis/07_prepare_itt_panelC_all_incorrect.R`
9. `analysis/07b_prepare_itt_panelC_control_all_incorrect.R`
10. `analysis/08_itt_panelC_glmm.R`

### Descriptive and supplementary analyses
11. `analysis/09_crude_proportions_S1.R`
    - reproduces Supplementary Table S1

12. `analysis/10_time_table.R`
    - reproduces Table 3

13. `analysis/11_accuracy_plot.R`
    - generates Figure 3

14. `analysis/12_baseline_table.R`
    - reproduces Table 1

15. `analysis/13_subgroup_analysis_overview.R`
    - reproduces Figure 4

## Supplementary Table S3
Supplementary Table S3 was based on manually tabulated source counts, which are provided in:
- `supplementary/Supplementary_Data_1_Triage_urgency_source_data.xlsx`

Final table formatting for S3 was performed outside R.
```
