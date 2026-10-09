# Data notes

Source: NOAA National Centers for Environmental Information (NCEI), Climate at a Glance: Global Time Series.
https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series

Downloaded October 4, 2026. Monthly data runs through August 2026. Annual data runs through 2025.

All values are temperature departures from the 1901 to 2000 average, in degrees Celsius, exactly as NOAA publishes them.

## raw/

The 34 original NOAA files, unchanged. 17 series, each at two resolutions:
- `<region>_<surface>_annual.csv` (January to December average, 1850 to 2025)
- `<region>_<surface>_monthly.csv` (every month, January 1850 to August 2026)

Each file starts with 3 comment lines (title, units, base period). In R: `read.csv(path, skip = 3)`.

Regions: globe, nhem, shem (each as land_ocean, land, ocean); africa, asia, europe, northAmerica, southAmerica, oceania (land); arctic, antarctic (land_ocean).

## Combined files

- `noaa_annual_anomalies_1850_2025.csv`: one row per year, one column per series
- `noaa_monthly_anomalies_1850_2026.csv`: one row per month, one column per series

These are the raw files joined side by side, with no values changed.

## Things to know

- No missing values.
- Continent series are land only. NOAA returns the same numbers for a continent with "land" or "land and ocean".
- The Antarctic series barely moves before the 1950s because there were almost no stations. Leave it out of comparisons or flag it.
- Early continental values (before about 1900) rest on few stations, especially in Africa and South America.

## APA 7 citation

NOAA National Centers for Environmental Information. (2026). *Climate at a glance: Global time series* [Data set]. Retrieved October 4, 2026, from https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series

Check the publication month on the NOAA page before finalizing the reference.
