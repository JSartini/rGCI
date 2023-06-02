set.seed(1234)

# Timing variates
start = as.POSIXct("2000-01-01")
end = start + as.difftime(17, units = "days")
interval = 15

# Overly large time series from which to derive all eventualities
overall_ar1 = arima.sim(list(ar=.5), n=1632)
overall_ma1 = arima.sim(list(ma=.5), n=1632)
overall_time = seq(from=start, by=interval*60, to=end)
overall_time = overall_time[-length(overall_time)]

# Full data
full_data_ar1 = overall_ar1[1:1344]
full_data_ma1 = overall_ma1[1:1344]
full_times = overall_time[1:1344]

# Large gap
gap_data_ar1 = c(overall_ar1[1:288], overall_ar1[577:length(overall_ar1)])
gap_data_ma1 = c(overall_ma1[1:288], overall_ma1[577:length(overall_ma1)])
gap_times = c(overall_time[1:288], overall_time[577:length(overall_time)])

# Insufficient
ins_data_ar1 = overall_ar1[1:288]
ins_data_ma1 = overall_ma1[1:288]
ins_times = overall_time[1:288]

# Baselines computed from previous implementation
baseline_output = read_csv("testdata/pgram_ground.csv", show_col_types=FALSE)
