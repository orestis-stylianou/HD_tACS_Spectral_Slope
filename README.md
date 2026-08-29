# 30 Hz High-Definition Transcranial Alternating Current Stimulation at the Left Frontal Cortex Reduces the Spectral Slope of the EEG in the Contralateral Hemisphere

## Project overview

The code in this project is organized around a research workflow:

1. Load raw electrophysiology recordings and subject metadata.
2. Preprocess EEG data using MNE Python tools, including resampling, filtering, re-referencing, ICA, ICLabel-based artifact removal, and current source density estimation.
3. Save cleaned EEG segments into a structured folder hierarchy.
4. Extract CTT (continuous task time / deviation) segments associated with stimulation conditions.
5. Compute spectral slopes using the IRASA approach.
6. Compare spectral slope before vs during stimulation, test significance, and correlate changes with CTT changes.
7. Generate output figures for manuscript reporting.

The project is built around a dataset from a published neuroscience study:

Dataset: https://www.nature.com/articles/s41597-021-01046-y

## Repository structure

- `preprocessing.R`  
  Main EEG preprocessing pipeline. Reads raw data, performs artifact cleaning, saves cleaned `.fif` files, and exports preprocessing figures.

- `spectral_slope_calculation.R`  
  Computes spectral slopes from the cleaned EEG using IRASA and stores outputs by subject, stimulation type, and segment.

- `ctt_grouping.R`  
  Aggregates CTT deviation values across subject and stimulation conditions and saves a grouped dataset.

- `spectral_slope_comparisons.R`  
  Builds a long-form dataset of spectral slopes and performs paired comparisons before vs during stimulation with normality checking and correction for multiple testing.

- `spectral_slope_ctt_change_correlations.R`  
  Correlates spectral-slope change with CTT change and saves a PDF report of the correlations.

- `Figure 1.R` to `Figure 7.R`  
  Individual plotting scripts used to generate publication figures.

- `Downloaded Data/`  
  Raw experimental data downloaded from the source dataset.

- `Clean EEG/`  
  Preprocessed EEG outputs grouped by subject and stimulation condition.

- `CTT Data/`  
  Extracted task-related segments and CTT files.

- `Spectral Slope/`  
  IRASA-derived outputs and spectral slope summaries.

- `Preprocessing Figures/`  
  Visualization outputs from the preprocessing pipeline.

## Typical execution order

1. Open the project in RStudio or Positron.
2. Ensure Python and package dependencies are installed.
3. Run `preprocessing.R`.
4. Run `spectral_slope_calculation.R`.
5. Run `ctt_grouping.R`.
6. Run `spectral_slope_comparisons.R`.
7. Run `spectral_slope_ctt_change_correlations.R`.
8. Run the relevant figure scripts as needed.

- `Preprocessing Figures/` contains GIF and image summaries of EEG before and after cleaning.
- `Clean EEG/` stores cleaned `.fif` files.
- `CTT Data/` stores task-related CTT segment files.
- `Spectral Slope/` stores IRASA outputs and spectral slope results.
- generated PDFs and PNGs hold the final results and publication-quality figures.

## Notes

- The code uses Windows-specific commands in a few places (for example, deleting temporary directories with `shell(...)`), so cross-platform use may require modification.
- The project is research-oriented and is best run in a controlled environment with the raw data available locally.
