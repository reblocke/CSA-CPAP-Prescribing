# CSA-CPAP-Prescribing

[![DOI](https://img.shields.io/badge/DOI-10.1007%2Fs00408--023--00657--z-blue)](https://doi.org/10.1007/s00408-023-00657-z)
[![PubMed](https://img.shields.io/badge/PubMed-37987861-green)](https://pubmed.ncbi.nlm.nih.gov/37987861/)
[![PMCID](https://img.shields.io/badge/PMCID-PMC10869204-green)](https://pmc.ncbi.nlm.nih.gov/articles/PMC10869204/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Supporting Stata code and paper-facing output artifacts for **"Predictors of Initial CPAP Prescription and Subsequent Course with CPAP in Patients with Central Sleep Apneas at a Single Center"**.

## Links And Identifiers

- Version of record: Locke BW, Sellman J, McFarland J, Uribe F, Workman K, Sundar KM. *Lung*. 2023;201(6):625-634. DOI [10.1007/s00408-023-00657-z](https://doi.org/10.1007/s00408-023-00657-z).
- PubMed: PMID [37987861](https://pubmed.ncbi.nlm.nih.gov/37987861/).
- PubMed Central open-access version: PMCID [PMC10869204](https://pmc.ncbi.nlm.nih.gov/articles/PMC10869204/).
- Historical preprint record: DOI [10.21203/rs.3.rs-3199807/v1](https://doi.org/10.21203/rs.3.rs-3199807/v1), PMID [37547021](https://pubmed.ncbi.nlm.nih.gov/37547021/), PMCID [PMC10402256](https://pmc.ncbi.nlm.nih.gov/articles/PMC10402256/).
- Repository: [reblocke/CSA-CPAP-Prescribing](https://github.com/reblocke/CSA-CPAP-Prescribing). The paper code-availability statement names this repository.

## Authors, Funding, And Disclosures

Authors: Brian W. Locke (ORCID [0000-0002-3588-5238](https://orcid.org/0000-0002-3588-5238)), Jeffrey Sellman, Jonathan McFarland, Francisco Uribe, Kimberly Workman, and Krishna M. Sundar (ORCID [0000-0001-7220-5767](https://orcid.org/0000-0001-7220-5767)).

This work was supported by the National Institutes of Health Ruth L. Kirschstein National Research Service Award 5T32HL105321. B.W.L. also receives funding for an unrelated project from an American Thoracic Society program supported by ResMed, Philips Respironics, and Fisher & Paykel Healthcare. K.M. Sundar is co-founder of Hypnoscure LLC through the University of Utah Technology Commercialization Office and is an associate editor at *Lung*. The article is the authoritative source for affiliations, acknowledgments, and disclosures.

## Data Access

The study used restricted single-center EHR and sleep-center data under University of Utah IRB #00123537. Raw patient-level data are not public and must not be committed to this repository.

To rerun the Stata workflow, supply an authorized local workbook named `coded_output.xlsx` in the repository root or pass the directory containing that workbook as the first do-file argument. The expected variables and derived fields are documented in [data_dictionary.md](data_dictionary.md) and [data_dictionary.csv](data_dictionary.csv).

The derived row-level file `Results/csa_regressions.dta` was removed from the branch tip because it contains 588 row-level observations and model/data columns derived from restricted clinical data. Aggregate paper-facing tables and figures remain in `Results/` and `Figures/` as publication artifacts.

## Quick Start

Install Stata 17 or newer and the user-written commands listed below, then run from the repository root:

```stata
do "Stata/CSA regressions.do"
```

By default, the script reads `./coded_output.xlsx` and writes local rerun outputs under `outputs/stata/<date>/`. To use a separate private input folder and output folder:

```stata
do "Stata/CSA regressions.do" "data/private" "outputs/stata"
```

## Stata Dependencies

| Command or scheme | Purpose in workflow | Installation note |
|---|---|---|
| `coefplot` | Odds-ratio coefficient plots | `ssc install coefplot, replace` |
| `outreg2` | Regression result tables | `ssc install outreg2, replace` |
| `heatplot` | Heatmaps of CPAP prescription/response by CSA proportion and etiology | Install from SSC or the project Stata adopath used for the original analysis |
| `table1_mc` | Table 1 summaries | Install from SSC or the project Stata adopath used for the original analysis |
| `fitstat`, `adjrr` | Post-estimation summaries and adjusted relative risks | Install the SPost command suite used by the original analysis |
| `nmissing`, `mdesc`, `tab3way` | Missingness and cross-tabulation checks | Install from SSC or the project Stata adopath used for the original analysis |
| `cleanplots`, `plotplain`, `white_tableau` | Figure schemes | Install the corresponding Stata scheme packages used by the original analysis |

The do-file now checks for required user-written commands before reading private data and exits with a clear message when a dependency is missing.

## Repository Layout

| Path | Role |
|---|---|
| `Stata/CSA regressions.do` | Main Stata workflow for paper regression models and local rerun outputs. |
| `Results/` | Tracked aggregate paper-facing tables/text exports; local rerun tables are written under ignored `outputs/`. |
| `Figures/` | Tracked paper-facing figures and schematic flow diagrams. |
| `data_dictionary.md`, `data_dictionary.csv` | Human-readable and machine-usable input/derived variable documentation. |
| `CITATION.cff` | CFF 1.2 citation metadata for the software repository and article. |
| `llms.txt`, `AGENTS.md` | Machine-readable repository summary and agent instructions. |

## Workflow And Outputs

1. Prepare an authorized local `coded_output.xlsx` workbook from restricted source data.
2. Run `Stata/CSA regressions.do`.
3. The script imports the workbook, recodes model variables, fits multinomial/logistic models, calculates marginal effects and IPTW sensitivity models, and exports local rerun tables, figures, logs, and derived data under `outputs/stata/<date>/`.
4. Do not commit private input workbooks, derived `.dta` files, logs, or local rerun outputs.

## Results Mapping

| Paper item | Where generated or stored | Notes |
|---|---|---|
| Figure 4 odds-ratio model figure | `Stata/CSA regressions.do` and local rerun export `outputs/stata/<date>/Figures/figure4_cpap_or_models.png` | Main code-generated model figure. |
| Regression tables and marginal-effects exports | `Stata/CSA regressions.do`, local rerun tables under `outputs/stata/<date>/Results/` | Uses `outreg2`; tracked `Results/` files are historical aggregate paper artifacts. |
| Figures 1-3 and flow diagrams | `Figures/` | Schematic/project artifacts, not fully regenerated by the Stata script. |
| Derived model dataset | Local rerun output under `outputs/stata/<date>/Derived/` | Restricted row-level derived data; not tracked. |

## Citation

Please cite the article as the primary scholarly object and cite the repository commit or release if you reuse the code. Machine-readable metadata are in [CITATION.cff](CITATION.cff).

## License

Repository code is released under the [MIT License](LICENSE). Restricted clinical data, private local workbooks, generated row-level `.dta` files, publisher-formatted article files, and third-party materials are not included under this license.

## Contributing, Conduct, And Security

See [CONTRIBUTING.md](CONTRIBUTING.md), [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md), and [SECURITY.md](SECURITY.md). Keep changes focused, do not commit restricted data, and document any dependency or output-path changes in this README.

## Contact

Open a GitHub issue for repository-specific questions. For scientific correspondence, use the corresponding-author information in the published article.
