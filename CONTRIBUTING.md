# Contributing to CSA-CPAP-Prescribing

Thanks for your interest in improving the reproducibility and usability of this repository!

## Ways to contribute
- **Issues:** File a GitHub Issue for bugs, questions, or feature requests.
- **Pull requests:** Fork the repo and open a PR from a feature branch. Keep changes focused and documented.

## Development workflow
1. Describe the change in the PR description, including what paper result it affects (if any).
2. Keep analysis code in `Stata/` and exported artifacts in `Results/` and `Figures/`.
3. If you add new dependencies (e.g., Stata packages), add the install commands to the README under *Stata packages*.

## Style and conventions
- Stata do-files should be commented and organized into sections (setup, data prep, models, outputs).
- Prefer deterministic workflows: write outputs to `Results/` and `Figures/` using explicit file names.
- Avoid committing PHI/PII or any raw patient data. Do **not** commit EHR extracts.

## Tests and examples
- Where feasible, add a **smoke test** (e.g., run models on a 10-row synthetic dataset) to check that the code executes without error.
- If you contribute example data, ensure it is synthetic, de-identified, or publicly licensed for reuse.

## Code of Conduct
By participating, you agree to abide by the standards in `CODE_OF_CONDUCT.md`.

## License and attribution
By contributing, you agree that your contributions will be licensed under the repository’s `LICENSE`.
