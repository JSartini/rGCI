## rGCI

A package for calculating the Glucose Color Index (GCI) using continuous glucose monitoring (CGM) data with sufficient continuous wear. This metric aims to complement existing CGM variability metrics by capturing the "color", or shape of variation through summarization of the periodogram - a decomposition of variation into periodic signals across the fundamental frequencies. 

The GCI metric was derived and validated using CGM and diabetes comorbidity data from the Hyperglycemic profiles in Obstructive Sleep Apnea (HYPNOS) and Atherosclerosis Risk in Communities (ARIC) studies. The paper publishing the metric, along with its validation procedures and results, can be found at "journal".

This package aims to simplify and automate the calculation of this novel metric, providing in the process logical periodogram calculation parameter defaults as well as the the requisite weights for final GCI calculation (derived from the HYPNOS study population).

## Exported Functions

- pgram(): Calculate the log-normalized and smoothed periodogram estimate of the log spectral density for a provided CGM time series (values and temporal argument provided separately)
- linear_approx(): Summarize a CGM log-periodogram using disjoint linear models over 3 disjoint frequency bands (corresponding to periodicities longer than a day, between a day and 2.5 hours, and shorter than 2.5 hours)
- calculate_GCI(): Calculate the GCI using either the results of linear_approx() or a raw CGM time series in long-format contained within a dataframe
- default_pgram(): Function to provide the defaults for calculation of the CGM log-periodogram (trimming, filtering, smoothing, etc.) used in derivation/validation of the GCI

## Examples

First, calculation of GCI using a dataframe with a hypothetical CGM time series "cgm_readings" and its corresponding timestamps "cgm_times" in the appropriate long-format (with the appropriate column names)

```r
cgm_df = data.frame(glucose = cgm_readings, timestamp = cgm_times)
gci_df_ex = calculate_GCI(cgm_df)
```

Next, a more granular procedure calculating the smoothed log-periodogram, its 6 degree of freedom disjoint linear model summary, and the corresponding GCI sequentially. The same hypothetical CGM time series objects are used here.

```r
lpgram = pgram(cgm_readings, cgm_times)
sum6DF = linear_approx(lpgram)
gci_full_ex = calculate_GCI(sum6DF)
```

## Authors/Credit

<u>Creation of the Metric:</u> Joseph Sartini, Dr. Michael Fang, Dr. Mary R. Rooney, Dr. Elizabeth Selvin, Dr. Josef Coresh, Dr. Scott Zeger

<u>Website/Package</u>: Joseph Sartini



