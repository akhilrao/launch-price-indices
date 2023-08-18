
# Calculating Launch Price Indices

This repository contains the data, visualizations, and R code required to analyze and visualize launch price indices. Two indices are calculated:

* Payload-based Price Index: Average annual price per kg to LEO based on the number of payloads launched each year on various vehicles. Doesn't consider the mass of each payload due to missing data.

* Vehicle-based Price Index: Average annual price per kg to LEO based on the number and variety of vehicles launched each year. Considered more defensible due to data limitations of the payload-based index.

It also allows for calculation of the number of countries launching payloads to LEO every year.

This dataset is used in the paper "Space Exploration and Economic Growth: New Issues and Horizons". The final dataset can also be downloaded from the replication files for that paper:

This is a "living" version of the official repository and should not be cited. If you want to cite the code or data associated with this project, please reference the official repository.


## Repository Structure

* `/data` holds the primary data sources used in the analysis:
  - `spacetrack-satcat.csv`: Satellite catalog data sourced from Space-Track.
  - `launchlog.tsv`: A log of satellite launches. Sourced from Jonathan McDowell's site (URL: https://planet4589.org/space/gcat/tsv/derived/launchlog.tsv).
  - `iss.txt`: Log of payloads deployed from the International Space Station (ISS). Sourced from Jonathan McDowell's site (URL: https://planet4589.org/space/iss/iss.txt). It provides a list of ISS-related objects used for adjustments in payload country attributions.
  - `VehicleFamilies.csv`: Information on various launch vehicle families used to harmonize Launch Log names for price attribution. Provided by Thomas G. Roberts.
  - `csis-launch-costs.csv`: Launch cost data sourced from CSIS Aerospace Security Project. Contains data on space launch prices to LEO. 
    - *Citation*: Thomas G. Roberts, Space Launch to Low Earth Orbit: How Much Does It Cost?, Center for Strategic and International Studies, September 1, 2022, https://aerospace.csis.org/data/space-launch-to-low-earth-orbit-how-much-does-it-cost/.

* `/output` contains visualizations and processed data files:
  - Visualizations: 
    - `price_plot.png`: A plot of the vehicle-based launch price index, 1957-2023.
    - `price_plot_zoom.png`: A zoomed-in visualization of launch prices over 1961-2023, excluding initial years to better illustrate recent trends.
    - `price_missing_plot.png`: Visualization showing share of observations missing in each year, 1957-2023.
    - `price_plot_both.png`: A comparison of two types of launch price indices over 1957-2023, "payload-based" and "vehicle-based". "Vehicle-based" is considered the more-appropriate index given the price data and is what is used elsewhere. See `launch-price-calc.R` for more details.
    - `price_country_plot.png`: Visualization of launch prices and number of countries deploying payloads to space, 1957-2023.
    - `country_plot--iss-corrected.png`: A corrected visualization of the number of payloads launched by county, corrected for payloads with COSPAR numbers attributed to the ISS module from which they were launched. See `launch-price-calcs.R` for details on this adjustment. ISS data 
  - Data files:
    - `missing_lv_types.csv`: A list of launch vehicle types with missing data.
    - `launch_df_base.csv`: The main processed data file.

* `/bin` holds the primary script used for data processing and visualization:
  - `launch-price-calc.R`: The R script used to process the data in `/data` and generate the outputs in `/output`.

## ISS-Related Adjustments

The dataset incorporates adjustments related to the International Space Station (ISS). Some payloads have COSPAR numbers attributed to the ISS module from which they were launched. To avoid attributing these payloads to the year the ISS module was launched rather than the year the payloads were deployed, the data from `iss.txt` (a list of ISS-related objects) is used to make necessary corrections. See `launch-price-calc.R` for details.

## Generating the Outputs

To reproduce the visualizations and processed data:

1. Ensure `R` is installed along with the necessary packages.
2. Run the script `launch-price-calc.R` found in `/bin`.
3. The visualizations and processed data files will be generated in `/output`.
