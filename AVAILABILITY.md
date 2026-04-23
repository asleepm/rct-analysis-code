# Availability statement

## Public release

This repository is public at the time of manuscript resubmission.

## What is publicly provided in this repository

This public repository provides:
- statistical analysis code used to reproduce the reported results
- de-identified analysis input files used by the included scripts
- public non-proprietary supplementary materials
- documentation needed to understand run order, variable structure, and analysis scope

## Public non-proprietary supplementary materials

Public non-proprietary supplementary materials provided in this repository include:
- Supplementary Appendix A
- Supplementary Appendix B
- Supplementary Appendix C
- Supplementary Data 1

## Reviewer-only supplementary materials

The following materials were provided with the revised submission for confidential peer review, but are **not included in this public repository**:
- questionnaire materials
- gold-standard answer keys

## Intermediate derived input files

The deterministic ITT boundary-analysis input files included in `data/` are provided for convenience and can also be regenerated from the corresponding preparation scripts.

## Not publicly released in this repository

The following components are **not publicly released** because they contain proprietary implementation details and/or materials subject to data-governance restrictions:
- trial-specific WeChat mini-program code
- exact internal prompt text
- exact chunking and reranker thresholds
- proprietary retrieval-engine configuration
- proprietary retrieval resources and settings
- the LungDiag-derived respiratory knowledge layer itself

## Minimal reproducible specification

To support independent appraisal despite these restrictions, Supplementary Appendix A provides a minimal reproducible specification of the evaluated intervention at the architectural and workflow level, including:
- architecture and workflow boundaries
- knowledge-layer provenance, scope, governance, and freeze policy
- open-source reference stack and equivalent functional components
- mock prompt frameworks preserving the task/schema envelope
- guardrail layers
- logging categories
- failure-handling principles
- deployment boundaries during the trial

## Reproducibility scope

This repository is intended to support practical verification of:
- the primary compliant-case GLMM analysis
- the all-randomized deterministic ITT boundary analyses
- the crude descriptive analyses
- the baseline, completion-time, subgroup, and figure-generation analyses

It is **not** intended to release the production application, the full proprietary intervention stack, or the full restricted materials used only for confidential peer review.

## Interpretation boundary

The evaluated intervention was an integrated GPT-4o/LungDiag-derived mini-program, not GPT-4o alone. Accordingly, the public materials released here support reproducibility of the reported statistical analyses and released non-proprietary study documentation, rather than full recreation of the production deployment environment.
