# currentloss
Current Losses Scripts

This code requires data organized in the associated Google Drive or
Code Ocean Capsule.

## System Requirements

With the exception of one script, all code is written for R and tested
on version 4.1.1 and 4.4.1. It requires the following libraries.
 - For loading data: PBSmapping, readxl, raster
 - For analysis: dplyr, reshape2, ranger, MASS, lfe, countrycode,
	 rstan, parallel, Hmisc, sf, maptools
 - For plotting and displaying results: ggplot2, scales, stargazer,
	 xtable, flextable

 - For auxilliary analyses: rpart, rpart.plot
 - For auxilliary display: cowplot, grid, GGally

The only expected version challenge is `PBSmapping`, which relies on
`maptools` (now deprecated). The code is tested under maptools version
1.1-5.

One script is written in python (tested on version 3.7.13), and uses
xagg (https://xagg.readthedocs.io/en/latest/) and cdsapi
(https://cds.climate.copernicus.eu/api-how-to) to collect and
spatially aggregate ERA5 data.

Installing these libraries with `install.packages` takes less than 1
hour on a desktop computer.

## Reproduction instructions

The associated data is expected to be in a `data` directory in the
same directory as the source code repository directory. All code
assumes that the working directory is set to this common parent
directory.

Each of the steps below specifies a given subdirectory of the code in
the section header, where its code files are stored.

## 1. Collect weather data (1-datacollect)

Run collect-era5.py, which generates year-specific ERA5 files, and
then combine-era5.R, which combines these into two files included in
the `data` directory: `era5-t2m-combo-adm0.csv` and
`era5-t2m-combo-adm1.csv`.

These steps take about 48 hours to complete.

## 2. Project all models and run meta-analysis (2-project)

1. Project all models: Run `2-project/doall.R`, which produces `mcres.RData`.
2. De-cumulate Kotz et al.: Run `2-project/decumul.R`, which produces `mcres-decumul.RData`.
3. Generate the random forest meta-analysis: Run
   `2-project/randforest.R`, which produces `mcrfres.RData`
4. As a robustness check, run `2-project/paper-weighted.R`, which
   produces mcpaperres-*.RData`.

These steps take about 24 hours to complete.

## 3. Generate other impact types (3-othereffects)

1. SLR effects: Run `3-othereffects/slr.R`, which produces
   `slrbyadm0-final.csv`.
2. Trade effects: Run `trade.R`, which produces `tradeloss-*`
   directories.
3. Capital model: Run `solow.R`, which produces `solow-*` directories.

The SLR effects take about 2 hours to complete and the trade effects
take about 24 hours to complete.  Please note that the capital model
takes a very long time to run (weeks on a low-powered desktop), since
it runs for each country under each Monte Carlo draw. Parallel
computing is used to increase the speed on machines with multiple
cores.

## 4. Generate result outputs (4-figures)

1. Synthesis preparation: Run `prepare.R`, which generates the
   `allyr-ww*.RData` files.
2. Synthesis 1: Run `timeseries.R`, which produces results over
   multiple years.
3. Synthesis 2: Run `figures.R`, which produces results reflective of
   the most recent years.

The synthesis preparation takes 1-2 hours to complete, and other
figure results take less than 1 hour to complete.
