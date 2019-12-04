# PSA002 log site

1. Before update this site, we have to download the lab data and confirm the validations. The R codes and information file are available in the project OSF: https://osf.io/2zxrw/

- `Lab_info.csv`: the latest information of labratories on the road.
- `download_osf.R`: download the collected data from lab OSFs.
- `data_validation.R`: confirm the validity of collected data; Save the rawdata to three csv files: `rawdata_SP_V.csv`, `rawdata_SP_M.csv`, and `rawdata_PP.csv`
- `lab_fin.csv`: Accumulate PSA ID who have finished data collection.

2. Run sequential analysis and plot the results. `data_seq_analysis.R` import `rawdata_SP_V.csv` and export results to `Seq_output.csv`.

3. In each language rmarkdown file, we draw the plot with the code:


```
lab_seq[grep(lab_seq, pattern="PSA_ID")] %>% read.csv() %>%
    seq_plot()
```