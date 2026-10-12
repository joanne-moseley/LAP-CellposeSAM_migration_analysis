# import packages

import numpy as np
import pandas as pd
import pathlib
import tifffile

import cellpose
from cellpose import io, models

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
image_name = "image.tif"                                # name of unsegmented tif (multi-frame image)
seg_diameter = 'default'                                # diameter to use in segmentation. Usually only necessary to specify a value for fluorescence or zoomed images
proj_dir = pathlib.Path("/path/to/project/directory")   # project directory
seg_model_path = proj_dir / "models" / "cpsam"  		# location of Cellpose-SAM segmentation model
allow_merges = False                                    # whether to allow merging events during tracking
save_masks_path = proj_dir / 'masks'			        # folder to save masks in
print('Paths defined.')

# initialise model
model = models.CellposeModel(gpu=True, pretrained_model=str(seg_model_path))
print('Segmentation model initiated.')

# load image
img_path = proj_dir / 'tifs' / image_name
img = io.imread(str(img_path))
print(f'\nImage stack loaded. Stack shape: {img.shape}') # should be in TYX (or TCYX format if multi-channel)

# segment the images
print('Segmenting...')
all_masks = []
all_flows = []
all_styles = []

for timepoint in range(len(img)):
  frame = img[timepoint]

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

  #print(f' Segmented timepoint {timepoint}.')

  ## save the mask for that timepoint
  #mask_path_frame = save_masks_path / f"{img_path.stem}_{timepoint}_mask.tif"
  #io.imsave(str(mask_path_frame), masks)

print("Segmentation complete.")

# save masks as a multi-frame tif file
save_masks_path_tif = save_masks_path / f"{img_path.stem}_masks_timeseries.tif"
masks_timelapse = np.stack(all_masks, axis=0) # stack into TYX array
tifffile.imwrite(
  str(save_masks_path_tif),
  masks_timelapse,
  photometric='minisblack'
)
print('Masks saved as .tif.')

# load masks and check the stack shape matches the image stack
mask_stack = np.squeeze(masks_timelapse)
print(f'Segmented mask stack loaded. Stack shape: {mask_stack.shape}')

if img.shape != mask_stack.shape:
  print('\nImage stack and mask stack are not the same size.')

print('Predicting tracks...')
# get df of centroids for each cell in each frame
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

# predict tracks
track_df, split_df, merge_df = lt.predict_dataframe(
    centroids_df.copy(),
    coordinate_cols=["centroid_x", "centroid_y"],
    frame_col='frame'
  )
print('Tracking complete.')

# save the tracking data
save_tracks_path = proj_dir / f"{img_path.stem}_predicted_tracks.csv"
track_df.to_csv(save_tracks_path, index=False)
print('Tracking data saved as .csv.')
