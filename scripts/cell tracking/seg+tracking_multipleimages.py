# import packages

import os
import numpy as np
import pandas as pd
import pathlib
import tifffile

import cellpose
from cellpose import io, models

import skimage
import scipy
import laptrack as lp
from skimage import measure
from skimage.measure import regionprops_table
from laptrack import LapTrack

print('Packages imported.\n')

import torch
print('torch imported. CUDA status:')
print("PyTorch version:", torch.__version__)
print("torch.version.cuda:", torch.version.cuda)
print("CUDA available:", torch.cuda.is_available())
print("CUDA device:", torch.cuda.get_device_name(0) if torch.cuda.is_available() else "No GPU")
print('----------------------')

# set user-changeable paths
proj_dir = pathlib.Path("/path/to/project/directory")   # project directory
seg_diameter = 'default'                                # diameter to use in segmentation. Usually only necessary to specify a value for fluorescence or zoomed images
seg_model_path = proj_dir / "models" / "cpsam"  		# location of Cellpose-SAM segmentation model
tif_path = proj_dir / 'tifs'			                # folder of tif files to process
allow_merges = False                                    # whether to allow merging events during tracking
save_masks_path = proj_dir / 'masks'			        # folder to save masks in
print('Paths defined.')

# initialise cellpose model for segmentation
model = models.CellposeModel(gpu=True, pretrained_model=str(seg_model_path))
print('Segmentation model initiated.')

# initialise LapTrack object
max_distance = 15

lt = LapTrack(
    metric='sqeuclidean',
    splitting_metric='sqeuclidean',
    merging_metric='sqeuclidean',
    cutoff=max_distance**2,
    splitting_cutoff=max_distance**2,
    merging_cutoff=max_distance**2 if allow_merges else False,
) 
print('LapTrack object initiated.')

# initiate empty dataframe to hold the combined tracking data from all files
tracking_data = pd.DataFrame(columns=[
    'cell_label',
    'centroid_x',
    'centroid_y',
    'frame',
    'tree_id',
    'track_id',
    'file_label'
])

# find all the tif files in the specified directory and count them
tifs = list(tif_path.rglob('*.tif'))
number_of_tifs = len(tifs)

# create iterator to track number of files that have been processed
i = 1

# process all the tif files in the directory and output one combined tracking df
for file in tifs:
  print(f'\nProcessing .tif file {i} out of {number_of_tifs}: {file}')

  # load original image
  img_stack = io.imread(str(file))
  #img_stack = img_stack[:5] # subset just the first 5 frames for testing
  print('Image stack loaded. Stack shape: ', img_stack.shape)

  # segment the images
  print('Segmenting...')

  all_masks = []
  all_flows = []
  all_styles = []

  for timepoint in range(len(img_stack)):
    frame = img_stack[timepoint]

    # segment the frame
    if seg_diameter != 'default':
        masks, flows, styles = model.eval(
        [frame],
        diameter=seg_diameter,
        do_3D=False
        )
    else:
        masks, flows, styles = model.eval(
        [frame],
        do_3D=False
        )

    # add the mask to the results
    all_masks.append(masks)
    all_flows.append(flows)
    all_styles.append(styles)

  print("Segmentation complete.")

  # get masks as an array
  all_masks = np.squeeze(np.array(all_masks, dtype=np.int32))

  # save masks as a multi-frame tif file
  save_masks_path_tif = save_masks_path / f"{file.stem}_masks_timeseries.tif"
  mask_stack = np.squeeze(np.stack(all_masks, axis=0)) # stack into TYX array
  tifffile.imwrite(
    str(save_masks_path_tif),
    mask_stack,
    photometric='minisblack'
  )
  print('Masks saved as .tif. ', 'Mask array shape: ', all_masks.shape)

  # check segmented mask
  if img_stack.shape != mask_stack.shape:
    print('\nImage stack and mask stack are not the same size.')

  # get df of centroids for each cell in each frame
  print('Predicting tracks...')
  centroids = []

  for frame, mask in enumerate(mask_stack):
    frame_centroids = pd.DataFrame(regionprops_table(mask, properties=["label", "centroid"]))
    frame_centroids["frame"] = frame
    centroids.append(frame_centroids)

  centroids_df = pd.concat(centroids)
  centroids_df = centroids_df.rename(columns={
    'label': 'cell_label',
    'centroid-0': 'centroid_y',
    'centroid-1': 'centroid_x'
  })

  # perform tracking
  track_df, split_df, merge_df = lt.predict_dataframe(
    centroids_df.copy(),
    coordinate_cols=["centroid_x", "centroid_y"],
    frame_col='frame'
  )

  # add file name and experiment label
  track_df['file_label'] = file.stem
  track_df['experiment'] = tif_path.stem

  # add the tracking data to the combined dataframe
  tracking_data = pd.concat([tracking_data, track_df], ignore_index=True)
  print('Tracking complete.')

  i += 1

# save the tracking data
save_tracks_path = proj_dir / f'{tif_path.stem}_combined_tracks.csv'
tracking_data.to_csv(save_tracks_path, index=False)
print('\nCombined tracking data saved as .csv.')
