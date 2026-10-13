"""
cellpose_plotter.py - functions to cleanly plot results of Cellpose-SAM segmentation and LapTrack tracking
"""

import numpy as np
import pandas as pd
import os
import matplotlib.animation as animation
from IPython.display import HTML
from pathlib import Path
try:
    from tqdm.auto import tqdm
except ImportError:
    from tqdm import tqdm
from cellpose import io, utils
from cellpose.plot import image_to_rgb, mask_overlay
from matplotlib import pyplot as plt

# create modified Cellpose-SAM plotting function to plot mask overlay with lower alpha
def show_segmentation_alpha(
  image_stack, mask_stack, seg,
  channels=[0, 0], frame_to_display=None, mask_alpha=1,
  save_fig_path=None, save_panel_path=None,
  colours=None, figsize=None, img_path=None):
    
    """
    Modified version of show_segmentation function of Cellpose-SAM (see cellpose docs) to allow for control of alpha value of mask overlay.
    Can save the full figure or individual panels using the corresponding save path options.
    Returns: None (saves the figure if save_fig_path and/or save_panel_path are specified).

    Args:
      image_stack (ndarray): 2D or 3D array. Image input into Cellpose-SAM.
      mask_stack (int, ndarray): Output from Cellpose-SAM segementation, either a static 2D integer mask or 3D TYX mask stack.
      seg (dict): A Cellpose-SAM result dictionary where 'flows' contains an RGB flow image matching the selected mask's spatial dimensions.
      channels (list of int, optional): Channels used to run Cellpose-SAM, no need to use if image is RGB. Defaults to [0, 0].
      frame_to_display (int, optional): Frame to display. Must be supplied if image is multiframe instead of a static image. Defaults to None.
      mask_alpha (float, optional): Alpha value of mask overlay (0 = transparent, 1 = opaque). Defaults to 1.
      save_fig_path (str or Path, optional): Exact output path for the full figure. Defaults to None.
      save_panel_path (str or Path, optional): Output path prefix for individual panel images (outlines, overlays, and flows - does not save the original image). Defaults to None.
      colours (RGB lookup array or None, optional): Array of colours for masks where each entry is a colour in 0-255 or 0-1 range. Passed to Cellpose-SAM's mask_overlay(). Defaults to None.
      figsize (tuple, optional): Size of figure. Defaults to None.
      img_path (str or Path, optional): Path to original image, for use in the figure title. If not supplied, figure will have no title. Defaults to None.
    """
    image_stack = np.asarray(image_stack)
    mask_stack = np.asarray(mask_stack)
    flows = seg["flows"]
    
    # check validity of inputs
    if mask_stack.ndim == 2: # static image
      if frame_to_display is not None:
        raise ValueError("frame_to_display must be None for a static mask.")
      img = image_stack
      mask = mask_stack
      flow_entry = flows
    elif mask_stack.ndim == 3: # multiframe (timelapse) image
      if frame_to_display is None:
        raise ValueError("frame_to_display is required for a time-series mask stack.")
      if frame_to_display < 0 or frame_to_display >= mask_stack.shape[0]:
        raise IndexError(
          f"frame_to_display must be between 0 and {mask_stack.shape[0] - 1}.")
      if image_stack.shape[0] != mask_stack.shape[0]:
        raise ValueError("Image and mask stacks must have the same number of frames.")
      img = image_stack[frame_to_display]
      mask = mask_stack[frame_to_display]
      flow_entry = flows[frame_to_display]
    else:
      raise ValueError(f"mask_stack must be a 2D mask or a TYX stack; got {mask_stack.shape}.")
    
    # find RBG flow
    def find_rgb_flow(value):
      if isinstance(value, np.ndarray):
        if value.shape == (*mask.shape, 3):
          return value
        return None
      if isinstance(value, (list, tuple)):
        for item in value:
          result = find_rgb_flow(item)
          if result is not None:
            return result
      return None

    flow = find_rgb_flow(flow_entry)
    if flow is None:
      raise ValueError(f"Could not find an RGB flow image with shape (*mask.shape, 3) for mask shape {mask.shape}.")
    if img.shape[:2] != mask.shape and not (img.ndim == 3 and img.shape[1:] == mask.shape):
      raise ValueError(f"Image spatial dimensions must match mask shape {mask.shape}; got {img.shape}.")
    
    if not 0 <= mask_alpha <= 1:
      raise ValueError("mask_alpha must be between 0 and 1.")
    
    # define figure dimensions
    title_space = 0.80 if img_path is not None else 1.0 # if img_path is None, title will not be displayed
    panel_gap = 0.015
    if figsize is None:
      panel_height = 3.5
      aspect_ratio = mask.shape[1] / mask.shape[0]
      figure_width = panel_height * aspect_ratio * (4 + 3 * panel_gap)
      figure_height = panel_height / title_space
      figsize = (figure_width, figure_height)
    fig = plt.figure(figsize=figsize)

    # generate title
    if img_path is not None:
      frame_title = (
        f"frame {frame_to_display}"
        if frame_to_display is not None # if original image is multiframe
        else "")
      fig.suptitle(
        f"Cellpose-SAM Segmentation: {Path(img_path).name} {frame_title}",
        fontsize=25)
  
    ax = fig.add_subplot(1, 4, 1)
    img0 = img.copy()

    # convert image to RGB
    if img0.ndim == 2:
        img0 = image_to_rgb(img0, channels=channels)
    elif img0.ndim == 3:
      if img0.shape[0] <= 4 and img0.shape[-1] not in (3, 4):
        img0 = np.transpose(img0, (1, 2, 0))
      if img0.shape[-1] < 3:
        img0 = image_to_rgb(img0, channels=channels)
      elif img0.max() <= 50.0:
        img0 = np.uint8(np.clip(img0, 0, 1) * 255)
    else:
      raise ValueError(f"Expected a 2D or 3D image; got shape {img0.shape}.")

    # show first panel (original image)
    ax.imshow(img0)
    ax.set_title("original image")
    ax.axis("off")

    # get predicted cell outlines
    outlines = utils.masks_to_outlines(mask)

    # show second panel (predicted outlines)
    ax = fig.add_subplot(1, 4, 2)
    outX, outY = np.nonzero(outlines)
    imgout = img0.copy()
    imgout[outX, outY] = np.array([255, 0, 0])  # red
    ax.imshow(imgout)
    ax.set_title("predicted outlines")
    ax.axis("off")

    # get cell masks with alpha
    overlay = mask_overlay(img0, mask, colors=colours)
    overlay_rgba = np.zeros((overlay.shape[0], overlay.shape[1], 4), dtype=np.float32)
    overlay_rgba[..., :3] = overlay / 255.0
    overlay_rgba[..., 3] = 0.0
    overlay_rgba[mask > 0, 3] = mask_alpha

    base_rgb = np.asarray(img0[..., :3], dtype=np.float32)
    if base_rgb.max() <= 1.0: base_rgb *= 255.0
    overlay_panel = np.clip(base_rgb, 0, 255)
    cell_pixels = mask > 0
    overlay_panel[cell_pixels] = (
        (1 - mask_alpha) * overlay_panel[cell_pixels]
        + mask_alpha * overlay[cell_pixels])

    # show third panel (predicted masks with alpha)
    ax = fig.add_subplot(1, 4, 3)
    ax.imshow(img0)
    ax.imshow(overlay_rgba)
    ax.set_title("predicted masks")
    ax.axis("off")

    # show fourth panel (predicted cell pose)
    ax = fig.add_subplot(1, 4, 4)
    ax.imshow(flow)
    ax.set_title("predicted cell pose")
    ax.axis("off")

    # arrange the subplots and title
    fig.subplots_adjust(
      left=0,
      right=1,
      bottom=0,
      top=title_space,
      wspace=panel_gap)
    
    # save panels if desired
    if save_panel_path is not None:
        panel_save_path = Path(save_panel_path).with_suffix("")
        panel_save_path.parent.mkdir(parents=True, exist_ok=True) # make directory if needed
        io.imsave(str(panel_save_path) + "_outlines.jpg", imgout) # save outlines
        io.imsave(str(panel_save_path) + "_overlay.jpg", np.rint(overlay_panel).astype(np.uint8)) # save masks
        io.imsave(str(panel_save_path) + "_flows.jpg", flow) # save flows

    # save whole figure if desired
    if save_fig_path is not None:
      save_fig_path = Path(save_fig_path)
      save_fig_path.parent.mkdir(parents=True, exist_ok=True) # make directory if needed
      fig.savefig(
        save_fig_path,
        dpi=300,
        bbox_inches="tight")

# define function to create an animated timelapse of selected cell tracks
def show_tracks_timelapse(
    image_stack, 
    track_df, experiment, tracked_id_col="track_id", 
    frame_offset=0, frame_start=0, frame_end=None,
    ids=None, n_ids=100, random_state=1,
    x_col="centroid_x", y_col="centroid_y",
    colour_palette='red', img_path=None,
    save_path=None, fps=6, display_scale=0.5):
      
  """Animate selected centroids and their track_id (or tree_id)-specific paths over TYX images.
    At each frame of the animation, a circle highlights the cell position for a given tree_id or track_id at that frame, while a line accumulates over time representing the cell track.
    tree_id represents the track of all cells in a lineage, while track_id represents the track of a single cell only.
    Returns: an IPython.display.HTML animation.
  
  Args:
    image_stack (ndarray): TYX array. Original image input into Cellpose-SAM.
    track_df: Dataframe of tracking data output from LapTrack. Must contain columns 'experiment', 'frame', 'track_id', 'tree_id' (if using), and a column of x and of y cell centroids.
    experiment (str): Name of experiment (to ensure the tracking data only contains tracks from the correct experiment).
    tracked_id_col (str, optional): 'track_id' or 'tree_id'. Column to plot tracks from (track_id = single cell tracks, tree_id = all track segments in a cell lineage). Defaults to 'track_id'. 
    frame_offset (int, optional): Offset of the frame column in the tracking data from 0. If the tracking data is 1-based instead of 0-based, use frame_offset=1. Defaults to 0.
    frame_start (int, optional): Frame to start animation at (inclusive). Defaults to 0.
    frame_end (int, optional): Frame to end animation at (inclusive). If not provided, the last frame in the stack will be used. Defaults to None.
    ids (list, optional): List of track or tree IDs to plot. If provided, supersedes n_ids. Defaults to None.
    n_ids (int, optional): Number of randomly selected (without replacement) track or tree IDs to plot. Superseded by ids if provided. Defaults to 100.
    random_state (int, optional): Random state to use when picking n_ids, for reproducibility. Defaults to 1.
    x_col (str, optional): Column of x-coordinates (horizontal pixel coordinates) to use. Defaults to 'centroid_x'.
    y_col (str, optional): Column of y-coordinates (vertical pixel coordinates) to use. Defaults to 'centroid_y'.
    colour_palette (str, optional): 'multicolour' (10 colours) or 'red' (red only). Defaults to 'red'.
    img_path (str or Path, optional): Path to original image, for use in the figure title. If not supplied, figure will have no title. Defaults to None.
    save_path (str or Path, optional): Exact output path for the animation. Defaults to None.
    fps (int, optional): Frames per second for the final animation. Defaults to 6.
    display_scale (num, optional): Scale (0,1] at which to display the animation in the console. Does not affect scale of the saved animation. Defaults to 0.5.
  """
  
  # ensure that tracking data used is from the correct experiment
  if 'experiment' not in track_df.columns: 
    raise KeyError('experiment column not present in track_df.')
  track_df = track_df[track_df['experiment'] == experiment]

  # check validity of inputs
  image_stack = np.asarray(image_stack)
  if image_stack.ndim != 3:
    raise ValueError(f"Expected image_stack in TYX format; got {image_stack.shape}")
  if image_stack.shape[0] == 0:
    raise ValueError("image_stack must contain at least one frame.")
  
  if tracked_id_col not in {"tree_id", "track_id"}:
    raise ValueError("tracked_id_col must be 'tree_id' or 'track_id'.")
  
  if not isinstance(frame_offset, (int, np.integer)):
    raise TypeError("frame_offset must be an integer.")

  num_frames, height, width = image_stack.shape
  if frame_end is None:
    frame_end = num_frames - 1
  if frame_start < 0 or frame_start >= num_frames:
    raise ValueError(f"frame_start {frame_start} is out of range [0, {num_frames - 1}]")
  if frame_end < frame_start or frame_end >= num_frames:
    raise ValueError(f"frame_end {frame_end} is out of range [{frame_start}, {num_frames - 1}]")

  if colour_palette not in {'multicolour', 'red'}:
    raise ValueError("colour_palette must be 'red' or 'multicolour'.")
  
  if fps <= 0:
    raise ValueError("fps must be greater than zero.")
  
  if not 0 < display_scale <= 1:
    raise ValueError("display_scale must be between 0 and 1.")
  
  # check whether any of the required columns are missing
  required_cols = {"frame", tracked_id_col, x_col, y_col}
  if tracked_id_col == "tree_id":
    required_cols.add("track_id")

  missing_cols = required_cols - set(track_df.columns)
  if missing_cols:
    raise KeyError(f"track_df is missing columns: {sorted(missing_cols)}")

  # apply frame offset to the tracking data
  tracks = track_df[list(required_cols)].dropna().copy()
  for column in required_cols:
    tracks[column] = pd.to_numeric(tracks[column], errors="raise")
  tracks["frame"] = tracks["frame"] - frame_offset
  tracks = tracks[tracks["frame"] <= frame_end].copy()

  # check tracking data
  for column in ("frame", tracked_id_col):
    values = tracks[column].to_numpy()
    if not np.all(np.isfinite(values)) or not np.all(values == np.floor(values)):
      raise ValueError(f"{column} values must be finite integers.")
  if not np.all(np.isfinite(tracks[[x_col, y_col]].to_numpy())):
    raise ValueError("Centroid coordinates must be finite.")
  tracks[["frame", tracked_id_col]] = tracks[["frame", tracked_id_col]].astype(np.int64)

  # check validity of frames and cell coordinates
  invalid_frames = ~tracks["frame"].between(0, num_frames - 1)
  if invalid_frames.any():
    bad_frames = sorted(tracks.loc[invalid_frames, "frame"].unique())
    raise ValueError(f"Mapped track frames {bad_frames} are outside the image stack. Check frame_offset.")
  if ((tracks[x_col] < 0) | (tracks[x_col] >= width)).any():
    raise ValueError(f"{x_col} coordinates must fall within image width [0, {width}).")
  if ((tracks[y_col] < 0) | (tracks[y_col] >= height)).any():
    raise ValueError(f"{y_col} coordinates must fall within image height [0, {height}).")
  
  # randomly select the specified number of IDs
  # or if IDs are provided, ensure they are valid
  available_ids = np.sort(tracks[tracked_id_col].unique())
  if ids is None:
    if not isinstance(n_ids, (int, np.integer)) or n_ids <= 0:
      raise ValueError("n_ids must be a positive integer.")
    if n_ids > len(available_ids):
      raise ValueError(f"n_ids ({n_ids}) is greater than the number of available IDs ({len(available_ids)}).")

    # subsample some random ids
    rng = np.random.default_rng(random_state)
    selected_ids = rng.choice(available_ids, size=n_ids, replace=False).tolist()
    #print(f"Selected {len(selected_ids)} random ids: {selected_ids}") # print the randomly selected IDs if desired
  else:
    selected_ids = np.asarray(list(ids), dtype=np.int64)
    if selected_ids.size == 0:
      raise ValueError("ids must contain at least one ID.")
    missing_ids = np.setdiff1d(selected_ids, available_ids)
    if missing_ids.size:
      raise KeyError(f"Requested IDs not found in track_df: {missing_ids.tolist()}")

  tracks = tracks[tracks[tracked_id_col].isin(selected_ids)]
  if tracks.empty:
    raise ValueError("No tracking rows found for the selected IDs.")

  # create the plot
  # use the same brightness range for each frame to avoid flicker
  vmin, vmax = np.percentile(image_stack, (1, 99))
  if vmax <= vmin: vmax = vmin + 1

  dpi = 400
  title_height = 100 if img_path is not None else 0 # title will only be displayed if img_path is provided
  canvas_height = height + title_height
  fig = plt.figure(figsize=(width / dpi, canvas_height / dpi), dpi=dpi)
  image_height_fraction = height / canvas_height
  ax = fig.add_axes([0, 0, 1, image_height_fraction])
  ax.set_axis_off()
  
  # generate title
  if img_path is not None:
    fig.text(
        0.5,
        image_height_fraction + (1 - image_height_fraction) / 2,
        f"{Path(img_path).name} frames {frame_start}-{frame_end} ({len(selected_ids)} selected {tracked_id_col}s)",
        ha="center",
        va="center",
        fontsize=11)
        
  image_artist = ax.imshow(image_stack[frame_start], cmap="gray", vmin=vmin, vmax=vmax, animated=True)
  ax.set_xlim(-0.5, width - 0.5)
  ax.set_ylim(height - 0.5, -0.5)

  # define whether to group by tree or track ID
  group_cols = (["tree_id", "track_id"] if tracked_id_col == "tree_id"
    else ["track_id"])

  # define colours to use (red or multicolour)
  if colour_palette == "red":
    id_colours = {int(value): "red" for value in selected_ids}
  else:
    # 10 colours similar to those used in cellpose visualisation functions
    palette = np.array([
        (255, 5, 75),     # red
        (255, 120, 20),   # orange
        (255, 210, 0),    # light orange
        (245, 255, 0),    # yellow
        (220, 0, 255),    # pink
        (180, 65, 255),   # purple
        (0, 240, 255),    # cyan
        (10, 150, 255),   # medium blue
        (10, 20, 255),    # dark blue
        (1, 255, 22),     # green
    ], dtype=np.float32) / 255.0

    # evenly assign one of 10 colours to each ID
    id_colours = {
        int(value): palette[index % len(palette)]
        for index, value in enumerate(selected_ids)}

  # create lines and points for each track
  track_artists = []
  for _, rows in tracks.groupby(group_cols, sort=False):
    rows = rows.sort_values("frame")
    colour = id_colours[int(rows[tracked_id_col].iloc[0])]

    line, = ax.plot(
        [], [],
        color=colour,
        linewidth=1.3,
        animated=True,
    )
    point = ax.scatter(
        [], [],
        s=16,
        facecolors="none", # use a hollow point to allow visualisation of the cell underneath
        edgecolors=[colour], # make the hollow point the specified colour
        linewidths=0.6,
        zorder=3,
        animated=True,
    )

    track_artists.append((
        line,
        point,
        rows["frame"].to_numpy(),
        rows[x_col].to_numpy(),
        rows[y_col].to_numpy(),
    ))

  # define function to generate each frame of the animation
  def update(frame_index):
    image_artist.set_data(image_stack[frame_index]) # get image for that frame
    updated_artists = [image_artist]
    
    # get line and point coordinates for each track
    for line, point, frames, x_values, y_values in track_artists:
        history = frames <= frame_index 
        line.set_data(x_values[history], y_values[history]) # get past x and y coordinates for the track

        # if there are x and y coordinates for the track in the current frame display the point
        current = frames == frame_index
        if current.any():
            point.set_offsets(np.column_stack((x_values[current], y_values[current])))
        else:
            point.set_offsets(np.empty((0, 2)))

        # add the line and point
        updated_artists.extend([line, point])

    return updated_artists

  # generate animation
  ani = animation.FuncAnimation(
      fig,
      update,
      frames=range(int(frame_start), int(frame_end) + 1),
      interval=1000 / fps,
      blit=True,
      repeat=False)

  # save animation if desired (create directory if needed)
  if save_path is not None:
    save_path = Path(save_path)
    save_path.parent.mkdir(parents=True, exist_ok=True)
    ani.save(str(save_path), writer=animation.PillowWriter(fps=fps))

  # display the animation
  fig.set_dpi(max(1, round(dpi * display_scale)))
  with plt.rc_context({"animation.embed_limit": 100}):  # set embedding limit in MB
    html = HTML(ani.to_jshtml())

  plt.close(fig)
  return html
