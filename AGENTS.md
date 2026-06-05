# AGENTS

## Project Purpose

This public repository supports the paper "Predictors of Initial CPAP Prescription and Subsequent Course with CPAP in Patients with Central Sleep Apneas at a Single Center" (*Lung*, 2023;201(6):625-634; DOI `10.1007/s00408-023-00657-z`).

## Public And Data-Safety Rules

- Treat the repository as public.
- Do not add raw EHR exports, PHI, restricted row-level datasets, private workbooks, credentials, local paths, private drafts, or publisher-formatted article files.
- `coded_output.xlsx` is the expected private local input and must remain ignored.
- Derived `.dta` files and local rerun outputs belong under ignored `outputs/`, not under tracked `Results/`.
- The tracked `Results/` and `Figures/` files are aggregate paper-facing artifacts; review any additions for disclosure risk before committing.

## How To Orient Quickly

- Start with `README.md` for article identifiers, workflow, dependency notes, and data restrictions.
- Use `llms.txt` for a compact machine-readable repository summary.
- Use `CITATION.cff` for CFF 1.2 citation metadata.
- Use `data_dictionary.md` and `data_dictionary.csv` for expected private input variables, derived fields, and outputs.
- Main workflow: `Stata/CSA regressions.do`.

## Workflow

Canonical run path from the repository root:

```stata
do "Stata/CSA regressions.do"
```

Optional private input and local output roots:

```stata
do "Stata/CSA regressions.do" "data/private" "outputs/stata"
```

The script requires Stata 17 or newer plus the user-written commands and graph schemes documented in `README.md`.

## Verification Before Publishing Changes

- Run `git diff --check`.
- Validate `CITATION.cff` with `uvx --from cffconvert cffconvert --validate --infile CITATION.cff`.
- Search for hard-coded local paths, stale placeholders, and generic readiness appendices.
- Confirm no tracked `.dta`, private workbook, log, or local rerun output is present.
- If Stata is available, run a batch smoke check; missing private input or missing documented user-written packages is an acceptable documented failure.
