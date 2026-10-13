# LAP+Cellpose-SAM Cell Migration Analysis

## Overview
This project assembles a robust cell migration analysis pipeline that segments cells from *.tif* timelapse images using [Cellpose-SAM](https://github.com/wikk-chy/cellpose-SAM), tracks cells across timelapse frames using [LapTrack](https://github.com/yfukai/laptrack/), and analyses the resulting migration data in R. The workflow, which outputs a variety of static and animated visualisations, analyses a variety of characteristics of cell migration including measures of displacement, persistence (straightness), directional bias, and diffusivity (via ensemble lag-time mean squared displacement modelling). It also allows for comparison of observed migration behaviour to simulated Brownian and correlated random walks.

## Dependencies
This project was developed using Python 3.11 and R 4.5.1.
### This project requires the following Python packages:
- `numpy`, `pandas`, `pathlib`, `tifffile`, `tqdm`, `scipy`, `skimage`, `torch`, `os`, `IPython.display`
- `matplotlib` >= 3.10
- `cellpose` >= 4.0
- `laptrack`

### It also requires the following R packages:
`tidyverse`, `readr`, `snakecase`, `stringr`, `lme4`, `RColorBrewer`, `viridis`, `ggrepel`, `patchwork`, `gganimate`, `bestNormalize`, `RNOmni`, `lmerTest`, `car`, `emmeans`, `ggsignif`, and `factoextra`.

## Usage
### Cell segmentation (Cellpose-SAM) and tracking (LapTrack)
#### `scripts/cell tracking`
1. Create a GPU-enabled Conda environment using [`conda_env_for_seg_and_tracking_with_gpu.yml`](https://github.com/joanne-moseley/LAP-CellposeSAM_migration_analysis/blob/3a97f516f57e2d1586a4901b5c9d087930ef9e78/scripts/conda_env_for_seg_and_tracking_with_gpu.yml).
2. For each experiment (microscopy data collection session), prepare a single folder containing *.tif* timelapse images in TYX format. The folder name should represent the name of the experiment (e.g., 'experiment_1') as the folder name will be used to name the output file of tracking data derived from all images in that folder.
3. In the Conda environment, run either [`seg+tracking_singleimage.py`](https://github.com/joanne-moseley/LAP-CellposeSAM_migration_analysis/blob/3a97f516f57e2d1586a4901b5c9d087930ef9e78/scripts/cell%20tracking/seg%2Btracking_singleimage.py) or [`seg+tracking_multipleimages.py`](https://github.com/joanne-moseley/LAP-CellposeSAM_migration_analysis/blob/3a97f516f57e2d1586a4901b5c9d087930ef9e78/scripts/cell%20tracking/seg%2Btracking_multipleimages.py) depending on how many *.tif* timelapses there are to process. Be sure to change the paths and parameters (in the 'set user-changeable' paths' chunk) as needed. If processing a folder of *.tif* files with `seg+tracking_multipleimages.py`, note that all the timelapse images to be processed must be located in a single folder as described in step 2.

Note: This step takes a long time to run and users may wish to run these scripts on a high-performance computing cluster if available.

### (optional) Visualise cell segmentation and tracking
#### `scripts/visualise segmentation and tracking`
Cell segmentation and tracking performance can be visually evaluated if desired using the functions contained in [`cellpose_plotter.py`](https://github.com/joanne-moseley/LAP-CellposeSAM_migration_analysis/blob/3a97f516f57e2d1586a4901b5c9d087930ef9e78/scripts/visualise%20segmentation%20and%20tracking/cellpose_plotter.py). 
1. Open [`visualise_segmentation_and_tracking.ipynb`](https://github.com/joanne-moseley/LAP-CellposeSAM_migration_analysis/blob/3a97f516f57e2d1586a4901b5c9d087930ef9e78/scripts/visualise%20segmentation%20and%20tracking/visualise_segmentation_and_tracking.ipynb). Upload `cellpose_plotter.py`. For visualisation of segmentation, upload the original YX or TYX image, YX or TYX mask, and *.npy* segmentation results dictionary. For visualisation of tracking, upload the original TYX timelapse image stack and the tracking data *.csv* output by the LapTrack tracking step.
2. Run the first code chunk to load the required packages (including `cellpose_plotter`).
3. If visualising segmentation of a static image, proceed from the second code chunk, adjusting file names and parameters as needed.
4. If visualising segmentation or tracking of cells in a timelapse image, proceed from the fifth code chunk, adjusting file names and parameters as needed.
5. Figures (whole multipanel segmentation figure, individual panels of the segmentation figure, and tracking animation) will be saved depending on arguments passed to the plotting functions.

### Cell migration analysis
#### `scripts/migration analysis`
1. Load required packages by running [`install_packages.R`](https://github.com/joanne-moseley/LAP-CellposeSAM_migration_analysis/blob/3a97f516f57e2d1586a4901b5c9d087930ef9e78/scripts/migration%20analysis/install_packages.R).
2. Define required functions by running [`define_functions.R`](https://github.com/joanne-moseley/LAP-CellposeSAM_migration_analysis/blob/3a97f516f57e2d1586a4901b5c9d087930ef9e78/scripts/migration%20analysis/define_functions.R).
3. Assemble input files required by this workflow: tracking data (output by the cell tracking scripts in this pipeline) and manually prepared experiment plan for each experiment, as well as a metadata file. Example input files used in the example workflow [`R_workflow_example.qmd`](https://github.com/joanne-moseley/LAP-CellposeSAM_migration_analysis/blob/3a97f516f57e2d1586a4901b5c9d087930ef9e78/scripts/migration%20analysis/R_workflow_example.qmd) are provided in `data/R_data` for reference as to how input files should be formatted.
4. The functions provided can be used to locate, load, and pre-process the tracking data output from previous steps, and then to perform various analysis steps as required. Alternatively, an example analysis workflow is presented using simulated data in `R_workflow_example.qmd`. 

## Outputs
For all multi-panel or faceted figures, individual panels can be saved separately if desired.

### Cell segmentation
- **Multi-panel segmentation validation figure**: original image (or frame of timelapse image) with cell outlines, overlaid cell masks, and Cellpose-SAM flows. Visualisation of segmentation performance.
### Cell tracking
- **Cell track validation animation**: original timelapse animation overlaid with a selection of cell tracks (or trees - see LapTrack [documentation](https://laptrack.readthedocs.io/en/stable/examples/api_example.html) on `track_id` vs `tree_id`) showing trajectories over time. Visualisation of tracking performance.

### Migration analysis
#### Track visualisation
- **Cell track plot**: individual cell tracks for each treatment condition normalised to (0,0).
- **Cell track density plot**: density plot of normalised cell tracks for each treatment condition. Useful for displaying directional bias in cell trajectories, if applicable.
- **Cell track animation**: animation of normalised cell tracks for each treatment condition. Provides a more dynamic view (useful for presentations) of cell paths over time.

#### Analysis of measures of displacement
- **Migration features boxplots**: boxplots comparing total displacement, net displacement, maximum displacement, and meandering index (ratio of net displacement to total displacement) across each treatment condition.

#### Ensemble lag-time mean squared displacement (MSD) modelling
- **Multi-panel MSD figure**: MSD vs lag time (log-log) for each treatment condition, with anomalous diffusion exponent (α) for each treatment condition. Allows for comparison of diffusivity between cells in each treatment condition.

#### PCA analysis
- **PCA scatterplot**: scatterplot of PCA constructed using measures of displacement as well as the anomalous diffusion exponent (α) and motility coefficient (D) from the MSD modelling. As MSD is modelled per treatment per experiment, points represent experiments (instead of individual tracks) plotted against the first two PCs.
- **PCA scatterplot using measures of displacement only**: scatterplot of PCA constructed with only measures of displacement. Points represent individual tracks, plotted against the first two PCs.
