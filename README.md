_your zenodo badge here_

# Kyle-etal_2022_EF

**Assessing Multi-Dimensional Impacts of Achieving Sustainability Goals by Projecting the Sustainable Agriculture Matrix into the Future**

Page Kyle<sup>1\*</sup>, Mary Ollenburger<sup>2</sup>, Xin Zhang<sup>2</sup>, Hassan Niazi<sup>1</sup>, Siddarth Durga<sup>1</sup>, and Yang Ou<sup>1</sup>

<sup>1 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory (JGCRI-PNNL), College Park, MD, USA

<sup>2 </sup> Appalachian Laboratory, University of Maryland Center for Environmental Science (UMCES), Frostburg, MD, USA

\* corresponding author: pkyle@pnnl.gov

## Abstract
In this paper sustainable agriculture matrix (SAM) is estimated to 2100 using Global Change Analysis Model (GCAM). We model combinatorial variations of yield intensification, dietary shift, and greenhouse gas mitigation scenarios. Findings include scenarios having significant tradeoffs across multiple environmental, economic, and social dimensions. Assessment of these multi-dimensional tradeoffs in a consistent framework improves the quality of information for decision-making.

## Journal reference
To be provided after publication. 

## Code reference
Code reference for the post-processing module. See release details. Include code citation, see meta repo as a reference and guidance on release and code citations. 

## Data reference

### Input data
Place `inputs` folder on zenodo. Link here. Place as `/inputs/` in the directory. Include data citation from zenodo, see meta repo as a reference 

### Output data
Place `outdata` folder on zenodo. Link here. Place as `/outdata/` in the directory. Include data citation from zenodo, see meta repo as a reference 

## Contributing modeling software

Should we merge gcam-sam branch in `model` folder of this repo? 

| Model | Version | Repository Link | DOI |
|-------|---------|-----------------|-----|
| GCAM | v6 | https://github.com/pkyle/gcam-core/tree/gpk/paper/sam | zenodo DOI dataset here |


## Reproduce my experiment
Follow the steps below to set up the directory and recreate the results and figures of this study. Place folders using specified names on specified paths to run the code scripts smoothly. 

1. Install the software components required to conduct the experiment from [Contributing modeling software](#contributing-modeling-software)
2. Download the supporting input data from [Input data](#input-data), and place here: `/inputs/`
3. The `inputs` folder conducts pre-processing of external data which is read by the main `R/` folder of the module. The data in this folder are generic to all scenarios in this study. It isn't expected that users would need to re-run the scripts in the `inputs/R` folder but they are provided just in case.
4. Download and unzip the output data from my experiment [Output data](#output-data) and place here: `/outdata/`.
5. Open the `output_processing.Rproj` file (RStudio project file) to access all R code files used to generate the data and figures. Make sure to install and load all R packages required to run scripts in `R` directory. 

The `R` folder contains the scripts, which are described here, in the order that they would be run in order to generate the plots.

| Script Name | Description |
| --- | --- |
| `db_extract.R` 	 |	This is used to query data from a GCAM output database, saving the output of the queries into `outdata/SAM-matrix.dat`. It is only necessary if the scenarios are being re-run. |
| `proj_load.R` 		 |	This loads the project, creates important strings used in other scripts, and links necessary file paths.  |
| `figure2_plots.R` 	 |	This is used to generate the 4 panels of Figure 2 in the paper |
| `maps_indicators.R` |	This runs all of the data processing code, saves the condensed indicators as `outdata/region_vals.csv`, and generates map figures for each of the 32 GCAM regions. Note, `outdata/region_vals.csv` could be directly read-in to plot all figures in the study (both main text and SI), instead of sourcing all processing scripts (`R/landuse.R`, `R/nutbal.R`, `R/water.R`, `R/ghg_emissions.R`, `R/ag_an_markets.R`, `R/food_afford_refdiet.R`) every time. |
| `SAM_plots.R`		|	This produces Figure 3 of the paper which shows the fraction of global population experiencing various SAM thresholds. |			 
| `transformations.R` |	This generates the radar plots in Figure 4 of the paper using `outdata/region_vals.csv` |
| `probsustainabilityscore.R`	| This script aggregates results in `outdata/region_vals.csv` assuming all indicators and scenarios weigh the same, and produces sustainability score maps as shown in Figure 5 of the paper.		|
| `extrafigures.R`	 |	This generates plots presented in supplementary information of the paper.|

6. All of the figures are saved in the `figures/` folder. Double to uncomment `ggsave()` commands. 

7. Open an issue on this repository should you experience any difficulties in reproducing the results, or get different outputs to those from the publication. 
