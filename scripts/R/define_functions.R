options(dplyr.summarise.inform=FALSE)

#----------------------Define Functions----------------------------------------
##---------------------Functions for loading data------------------------------
#### define helper function to find experiment plan files
## searches in a folder to identify files with a given keyword

# path: optional, defaults to 'exp_plan_path'. Path to the folder containing experiment plan files.
# keyword: optional, defaults to 'exp_plan'. Keyword to search for.
# recursive: optional, defaults to FALSE. If TRUE, searches recursively inside the directory specified.

find_exp_plan_files <- function(path=exp_plan_path, keyword='exp_plan', recursive=FALSE) {
  # make sure path exists
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }
  
  # search for files
  exp_plan_files <- list.files(path = path,           # folder to search in
                               pattern = keyword,     # keyword(s) to search for (can use pipe | for 'or')
                               ignore.case = TRUE,    # case-insensitive search
                               full.names = FALSE,    # only return name of the file, not the full path
                               recursive = recursive) # whether to search recursively
  
  # display results
  if (length(exp_plan_files) == 0) {
    message("No experiment plan files found containing keyword: ", keyword)
  } else {
    cat('Experiment plan files found:\n', paste(exp_plan_files, collapse = "\n"), "\n\n", sep = "")
  }
  
  return(exp_plan_files)
}

#### define helper function to find metadata files
## searches in a folder to identify files with a given keyword

# path: optional, defaults to 'metadata_path'. Path to the folder containing metadata files.
# keyword: optional, defaults to 'metadata'. Keyword to search for.
# recursive: optional, defaults to FALSE. If TRUE, searches recursively inside the directory specified.

find_metadata_files <- function(path=metadata_path, keyword='metadata', recursive=FALSE) {
  # make sure path exists
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }
  
  # search for files
  metadata_files <- list.files(path = path,           # folder to search in
                               pattern = keyword,     # keyword(s) to search for (can use pipe | for 'or')
                               ignore.case = TRUE,    # case-insensitive search
                               full.names = FALSE,    # only return name of the file, not the full path
                               recursive = recursive) # whether to search recursively
  
  # display results
  if (length(metadata_files) == 0) {
    message("No metadata files found containing keyword: ", keyword)
  } else {
    cat("Metadata files found:\n", paste(metadata_files, collapse = "\n"), "\n\n", sep = "")
  }
  
  return(metadata_files)
}

#### define helper function to find tracking data files
## searches in a folder to identify files with a given keyword

# path: optional, defaults to 'data_path'. Path to the folder containing tracking data files.
# keyword: optional, defaults to 'combined_tracks' as this is output by the tracking script. Tracking data file keyword to search for.
# recursive: optional, defaults to FALSE. If TRUE, searches recursively inside the directory specified.

find_data_files <- function(path=data_path, keyword='combined_tracks', recursive=FALSE) {
  # make sure path exists
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }
  
  # search for files
  data_files <- list.files(path = path,           # folder to search in
                           pattern = keyword,     # keyword(s) to search for (can use pipe | for 'or')
                           ignore.case = TRUE,    # case-insensitive search
                           full.names = FALSE,    # only return name of the file, not the full path
                           recursive = recursive) # whether to search recursively
  
  # display results
  if (length(data_files) == 0) {
    message("No tracking data files found containing keyword: ", keyword)
  } else {
    cat("Tracking data files found:\n", paste(data_files, collapse = "\n"), "\n\n", sep = "")
  }
  
  return(data_files)
}

#### define function to load experiment plans
## uses the experiment plan files specified either manually or with find_exp_plan_files and combine into one clean dataframe

# files: optional, defaults to NULL. If NULL, will use find_exp_plan_files to search for the files.
# path: optional, defaults to exp_plan_path. The directory to find experiment plan files in.
# experiments: optional, defaults to unique experiments found in the metadata file (this requires that load_metadata has been run first). Can also be specified manually as a list.
# keyword: optional, defaults to 'exp_plan'. Keyword to use if searching for the files.
# recursive: optional, defaults to FALSE. If TRUE, searches recursively inside the directory specified.

load_exp_plan <- function(files=NULL, path=exp_plan_path, experiments=unique(metadata$experiment),
                          keyword='exp_plan', recursive=FALSE) {
  # make sure path exists
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }
  
  if (missing(files)) {
    files <- find_exp_plan_files(path, keyword, recursive)
  }
  
  # initiate empty dataframe to import the experiment plans into
  exp_plan <- data.frame(
    replicate_name = character(),
    imgJ_id = integer(),
    Condition = character(),
    experiment = character()
  )
  
  # import the experiment plan files and merge them
  for (i in seq_along(files)) {
    # get the data from the file
    df <- read_csv(file.path(path, files[i]), show_col_types = FALSE)
    df$experiment <- experiments[[i]]
    
    # merge into a single experiment plan file
    exp_plan <- bind_rows(exp_plan, df, .id=NULL)
  }
    
  # make sure column names are in snakecase for readability
  exp_plan <- exp_plan %>%
    rename_with(to_snake_case)

  # remove duplicate entries
  exp_plan <- unique(exp_plan[c('experiment', 'replicate_name', 'condition')])
  
  return(exp_plan)
}

#### define function to load metadata
## uses the metadata file(s) specified either manually or with find_metadata_files and return one clean dataframe

# files: optional, defaults to NULL. If NULL, will use find_metadata_files to search for the files.
# path: optional, defaults to metadata_path. The directory to find metadata files in.
# keyword: optional, defaults to 'metadata'. Keyword to use if searching for the files.
# recursive: optional, defaults to FALSE. If TRUE, searches recursively inside the directory specified.

load_metadata <- function(files=NULL, path=metadata_path, keyword='metadata', recursive=FALSE) {
  # make sure path exists
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }
  
  if (missing(files)) {
    files <- find_metadata_files(path, keyword, recursive)
  }

  # initiate empty dataframe to import the metadata into
  metadata <- data.frame(
    Experiment = character(),
    Frames = integer(),
    Duration_s = double(),
    Scale_um_per_pixel = double()
  )
  
  # import the experiment plan files and merge them
  for (i in seq_along(files)) {
    # get the data from the file
    df <- read_csv(file.path(path, files[i]), show_col_types = FALSE)
    df <- select(df, names(metadata))

    # merge into a single experiment plan file
    metadata <- bind_rows(metadata, df, .id=NULL)

    # make sure column names are in snakecase for readability
    metadata <- metadata %>%
      rename_with(to_snake_case)
  }
  
  return(metadata)
}

#### define function to load tracking data
## loads the tracking data, combined experiment plan, combined metadata, and returns a single tracking dataframe with experimental information 
## also generates global track IDs to identify each cell track

# data_files: optional, defaults to NULL. If not specified, will be identified using find_data_files. Tracking data files.
# data_path: path to tracking data. Can specify or use default 'data_path'
# data_keyword: optional, defaults to 'combined_tracks' as this is output by the tracking script. Tracking data file keyword to search for.
# exp_plan_files: optional, defaults to NULL. If not specified, will be identified using find_exp_plan_files. Experiment plan files.
# exp_plan_path: path to experiment plan files. Can specify or use default 'exp_plan_path'
# exp_plan_keyword: optional, defaults to 'exp_plan'. Keyword to search for.
# metadata_files: optional, defaults to NULL. If not specified, will be identified using find_metadata_files. Metadata files.
# metadata_path: path to metadata files. Can specify or use default 'metadata_path'
# metadata_keyword: optional, defaults to 'metadata'. Keyword to search for.
# experiments: optional, defaults to unique experiments found in the metadata file (this requires that load_metadata has been run first). Can also be specified manually as a list.
# recursive: optional, defaults to FALSE. If TRUE, searches recursively inside the directory specified.

load_data <- function(tracking_data_files=NULL, tracking_data_path=data_path, data_keyword='combined_tracks',
                      experiment_plan_files=NULL, experiment_plan_path=exp_plan_path, exp_plan_keyword='exp_plan',
                      metadata_files_list=NULL, metadata_file_path=metadata_path, metadata_keyword='metadata',
                      experiments=unique(metadata$experiment), recursive=FALSE) {
  
  # make sure paths exist
  if (!dir.exists(tracking_data_path)) {
    stop("Directory does not exist: ", tracking_data_path)
  }
  if (!dir.exists(experiment_plan_path)) {
    stop("Directory does not exist: ", experiment_plan_path)
  }
  if (!dir.exists(metadata_file_path)) {
    stop("Directory does not exist: ", metadata_file_path)
  }
  
  if (missing(tracking_data_files)) {
    tracking_data_files <- find_data_files(tracking_data_path, data_keyword, recursive)
  }
  
  if (missing(experiment_plan_files)) {
    experiment_plan_files <- find_exp_plan_files(experiment_plan_path, exp_plan_keyword, recursive)
  }
  
  if (missing(metadata_files_list)) {
    metadata_files_list <- find_metadata_files(metadata_file_path, metadata_keyword, recursive)
  }
  
  # initiate empty dataframe to import the raw data into
  mig_raw <- data.frame(
    cell_label = integer(),
    centroid_x = double(),
    centroid_y = double(),
    frame = integer(),
    tree_id = integer(),
    track_id = integer(),
    file_label = character(),
    experiment = character()
  )
  
  # import the data files and merge them
  for (file in tracking_data_files) {
    # get the data from the file
    df <- read_csv(file.path(tracking_data_path, file), show_col_types = FALSE)
    
    # merge into a single data file
    mig_raw <- bind_rows(mig_raw, df, .id=NULL)
  }
  
  # rename file label column to match the experiment plan files
  # also make sure column names are in snakecase for readability
  mig_raw <- mig_raw %>%
    rename(replicate_name = file_label) %>%
    rename_with(to_snake_case)
  
  # add experimental data from the experiment plan
  exp_plan <- load_exp_plan(experiment_plan_files, experiment_plan_path, experiments)
  
  mig_raw <- left_join(mig_raw, exp_plan, by=c('experiment', 'replicate_name'))
  
  # add metadata from the metadata file
  metadata <- load_metadata(metadata_files_list, metadata_file_path)
  if (any(metadata$frames <= 1)) {
    stop("Each experiment must contain more than one frame.")
  }
  
  mig_raw <- mig_raw %>%
    left_join(metadata, by = "experiment") %>%
    mutate(
      frame = frame + 1,
      time_s = (frame - 1) * (duration_s / (frames - 1)),
      X_um   = centroid_x * scale_um_per_pixel,
      Y_um   = centroid_y * scale_um_per_pixel
    ) %>%
    select(-frames, -duration_s, -scale_um_per_pixel)
  
  # create global track IDs
  mig_raw <- mig_raw %>%
    arrange(experiment, replicate_name, track_id, frame) %>%
    group_by(global_ID = consecutive_id(experiment, replicate_name, track_id))
  
  return(mig_raw)
}

##---------------------Simulated walk functions------------------------------
# Some types of walk that can be simulated for comparison to real data:
# 1. Pure Brownian Motion             (baseline)
# 2. Persistent Random Walk/CRW       (baseline with memory)
# 3. Inhibited Motility               (reduced D - e.g. Cytochalasin D)
# 4. Inhibited Chemotaxis/Persistence (reduced kappa - e.g. ROCK inhibitor)
# 5. Inhibited Motility & Persistence (reduced D and kappa)
# 6. Confined Motion                  (reduced confinement radius)

#### define function to simulate Brownian motion
## simulates Brownian cell trajectories
## MSD log-log slope = 1

# n_cells: optional, defaults to 100. Number of cells to simulate.
# track_length: optional, defaults to 145. Number of frames to simulate.
# dt: optional, defaults to 300. Time interval between frames/timepoints (in seconds).
# D: optional, defaults to 0.003. Baseline diffusion coefficient (cell motility) in µm²/s.
# label: optional, defaults to "Brownian". What to label the tracks.

# NB: sigma = step size standard deviation. norm_X and norm_Y are in µm.

simulate_brownian <- function(n_cells=100, track_length=145, dt=300, D=0.003, label='Brownian') {
  sigma <- sqrt(2 * D * dt)
  map_dfr(seq_len(n_cells), function(id) {
    sx <- c(0, rnorm(track_length - 1, 0, sigma))
    sy <- c(0, rnorm(track_length - 1, 0, sigma))
    tibble(norm_X = cumsum(sx), norm_Y = cumsum(sy), track_id = id,
           frame = (seq_len(track_length) - 1), time_s = (seq_len(track_length) - 1) * dt,
           condition = label)
  })
}

#### define function to simulate a correlated random walk (CRW)
## simulates persistent cell trajectories (correlated random walks).
## persistence = straightness (cells more likely to travel in the previous direction than other directions)
## Each step is drawn relative to the PREVIOUS direction.
## kappa controls persistence: high kappa = straighter tracks.
## Step angle ~ wrapped Normal(prev_angle, 1/kappa)
## MSD log-log slope > 1 at short times, converges to 1 at long times

# n_cells: optional, defaults to 100. Number of cells to simulate.
# track_length: optional, defaults to 145. Number of frames to simulate.
# dt: optional, defaults to 300. Time interval between frames/timepoints (in seconds).
# D: optional, defaults to 0.003. Baseline diffusion coefficient (cell motility) in µm²/s.
# kappa: optional, defaults to 3. Persistence coefficient (higher kappa -> higher persistence).
# label: optional, defaults to "Persistent (CRW)". What to label the tracks.

simulate_crw <- function(n_cells=100, track_length=145, dt=300, D=0.003, kappa = 3.0,
                         label = "Persistent (CRW)") {
  sigma <- sqrt(2 * D * dt)
  map_dfr(seq_len(n_cells), function(id) {
    x <- y <- numeric(track_length)
    angle <- runif(1, -pi, pi)
    for (i in 2:track_length) {
      angle <- angle + rnorm(1, 0, 1 / sqrt(kappa))
      step  <- abs(rnorm(1, 0, sigma))
      x[i]  <- x[i-1] + step * cos(angle)
      y[i]  <- y[i-1] + step * sin(angle)
    }
    tibble(norm_X = x, norm_Y = y, track_id = id,
           frame = (seq_len(track_length) - 1), time_s = (seq_len(track_length) - 1) * dt, 
           condition = label)
  })
}

##---------------------Functions for pre-processing data------------------------------
#### define function to clean tracking data
## renames treatment conditions, filters out data from unwanted treatment conditions, and normalises tracks to start at (0,0)

# all arguments optional; may be simpler to define the lists of levels before calling the function
# data: combined tracking data with experimental information (output from load_data). Defaults to 'mig_raw'.
# treatment_levels_raw: all treatment levels present in the dataset. Defaults to 'treatment_levels_raw'.
# treatment_levels_clean: all treatment levels present in the dataset, with desired names. Defaults to 'treatment_levels_clean'. ***Levels must be in same order as treatment_levels_raw.
# treatment_levels_filt: treatment levels to retain for analysis, with desired names and desired order. Defaults to 'treatment_levels_filt'.

clean_and_normalise_data <- function(data=mig_raw,
                                     levels_raw=treatment_levels_raw, 
                                     levels_clean=treatment_levels_clean, 
                                     levels_filt=treatment_levels_filt) {
  
  # rename treatment conditions
  # raw and clean levels must be in the same order for accurate matching!
  mig_raw <- data %>%
    mutate(condition = treatment_levels_clean[match(condition, treatment_levels_raw)],
           condition = factor(condition, levels = treatment_levels_clean))
  if (anyNA(mig_raw$condition)) {
    stop("Some condition names were not found in levels_raw.")
  }
  
  # filter treatment conditions
  mig_clean <- mig_raw %>%
    filter(condition %in% treatment_levels_filt)
  
  # normalise data
  mig_norm <- mig_clean %>%
    group_by(experiment) %>%
    mutate(Y_um_corr = max(Y_um) - Y_um) %>%
    ungroup() %>%
    group_by(global_ID) %>%
    arrange(frame) %>%
    mutate(
      norm_X = X_um      - first(X_um),
      norm_Y = Y_um_corr - first(Y_um_corr),
      time_s = time_s    - first(time_s)) %>%
    ungroup() %>%
    arrange(global_ID, frame)
  }

#### define helper function to get a dataframe of track lengths
## takes in tracking data and returns the length of each track

# data: optional, defaults to mig_norm. Tracking data to analyse.

get_track_lengths <- function(data=mig_norm) {
  track_lengths <- data %>%
    group_by(global_ID) %>%
    summarise(n_slices = n(), .groups = "drop") %>%
    left_join(data %>%
                select(global_ID, experiment, replicate_name, condition) %>%
                distinct(),
              by = "global_ID")
  
  return(track_lengths)
}

#### define function to filter tracking data
## takes cleaned dataset and returns dataframe of tracks of desired minimum and maximum length (in number of frames).

# all arguments optional
# data: combined tracking data with experimental information (output from load_data or clean_and_normalise_data). Defaults to 'mig_norm'.
# min_length: defaults to 0. Filter to only tracks greater than or equal to this length (in frames).
# max_length: defaults to the maximum number of frames in the dataset. Filter to only tracks less than or equal to this length (in frames).

filter_data <- function(data=mig_norm, min_length=0, max_length=max(data$frame)) {
  # get track lengths
  track_lengths <- get_track_lengths(data)

  # get tracks that meet length filters
  valid_ids <- track_lengths %>%
    filter(n_slices >= min_length, n_slices <= max_length) %>%
    pull(global_ID)

  # filter out tracks that don't meet the length filters
  mig_filt <- data %>%
    filter(global_ID %in% valid_ids)
}

#### define function to get dataframes of track length summary statistics
## takes in track length data and returns a list of 3 summary dataframes:
## 'track_stats_by_replicate' contains track summary statistics for each replicate
## 'track_stats_by_condition' contains track summary statistics for each treatment condition
## 'track_stats_summary' contains overall track summary statistics (across all replicates and conditions)
## each dataframe in the output list can be accessed by calling e.g. 'results$track_stats_summary'

# data_unfilt: optional, defaults to mig_norm. Unfiltered tracking data.
# data_filt: optional, defaults to mig_filt. Filtered tracking data.

summarise_track_length <- function(data_unfilt=mig_norm, data_filt=mig_filt) {
  # summarise unfiltered tracks
  track_lengths <- get_track_lengths(data_unfilt) # get track lengths
  
  track_lengths_summ_raw <- track_lengths %>%
    group_by(condition, experiment, replicate_name) %>%
    summarise(n_tracks = n(),
              mean_length = mean(n_slices)) %>%
    arrange(condition, experiment, replicate_name)
  
  # summarise length-filtered tracks
  track_lengths_filt <- get_track_lengths(data_filt)
  
  track_lengths_summ_filt <- track_lengths_filt %>%
    group_by(condition, experiment, replicate_name) %>%
    summarise(n_tracks_filt = n(),
              mean_length_filt = mean(n_slices)) %>%
    arrange(condition, experiment, replicate_name)
  
  # create combined summary dataframes comparing unfiltered vs filtered tracks
  # stats by replicate
  track_stats_by_replicate <- track_lengths_summ_raw %>%
    left_join(track_lengths_summ_filt %>%
                select(condition, experiment, replicate_name, n_tracks_filt, mean_length_filt) %>%
                distinct(),
                by = c("condition", "experiment", "replicate_name")) %>%
    mutate(filt_percentage = n_tracks_filt/n_tracks) %>%
    ungroup()
  
  # stats by condition
  track_stats_by_condition <- track_stats_by_replicate %>%
    group_by(condition) %>%
    summarize(n_tracks_filt = sum(n_tracks_filt),
              mean_length_filt = mean(mean_length_filt),
              mean_filt_percentage = mean(filt_percentage))
  
  # stats across replicates and conditions
  track_lengths_summ_stats_helper <- track_stats_by_condition %>%
    summarize(mean_n_filt_tracks_per_cond = mean(n_tracks_filt),
              se_n_filt_tracks_per_cond = sd(n_tracks_filt) / sqrt(n()))
  
  track_stats_summary <- track_stats_by_replicate %>%
    summarize(mean_n_filt_tracks_per_well = mean(n_tracks_filt),
              se_n_filt_tracks_per_well = sd(n_tracks_filt) / sqrt(n()),
              mean_avg_filt_length = mean(mean_length_filt),
              se_mean_filt_length = sd(mean_length_filt) / sqrt(n()),
              mean_filt_pass_percentage = mean(filt_percentage),
              se_filt_pass_percentage = sd(filt_percentage) / sqrt(n())) %>%
    merge(track_lengths_summ_stats_helper) %>%
    select(mean_n_filt_tracks_per_well, se_n_filt_tracks_per_well,
           mean_n_filt_tracks_per_cond, se_n_filt_tracks_per_cond,
           mean_avg_filt_length, se_mean_filt_length,
           mean_filt_pass_percentage, se_filt_pass_percentage)
  
    return(
      list(
        track_stats_by_replicate = track_stats_by_replicate,
        track_stats_by_condition = track_stats_by_condition,
        track_stats_summary = track_stats_summary
  ))
}

##---------------------Functions for analysing migration data------------------------------

#### define function to get trackwise migration features
## takes cleaned, normalised, and filtered data and computes migration features for each track.
## features computed include:
## net_dx, net_dy, net_d: net displacement (from track origin) over x, y, and in 2D.
## net_d: net displacement (displacement between the origin and the final position).
## tot_d: total displacement over the track.
## max_d: maximum displacement from the origin.
## meand_index: meandering index, a measure of straightness. Net displacement divided by total displacement.
## avg_spd: mean speed over the track.

# data: optional, defaults to 'mig_filt'. Combined tracking data with experimental information (output from load_data, clean_and_normalise_data, or filter_data).

get_trackwise_features <- function(data=mig_filt) {
  # compute track-wise features
  mig_features <- data %>%
    arrange(global_ID, frame) %>%
    group_by(global_ID) %>%
    mutate(disp_um = c(0,(sqrt(
      (diff(norm_X))^2 + 
        (diff(norm_Y))^2
    )))) %>%
    ungroup() %>%
    group_by(global_ID) %>%
    summarize(
      net_dx = last(norm_X) - first(norm_X), # net displacement over x
      net_dy = last(norm_Y) - first(norm_Y), # net displacement over y
      net_d = sqrt(
        (net_dx^2) +
          (net_dy^2)), # net displacement
      tot_d = sum(disp_um), # total path length
      max_d = max(sqrt((norm_X^2)+(norm_Y^2))), # maximum distance from path start
      meand_index = if_else(tot_d > 0, net_d / tot_d, 0), # meandering index
      avg_spd = if_else(
        last(time_s) > first(time_s),
        tot_d / (last(time_s) - first(time_s)),
        NA_real_) # dist over time
    )
  
  # get a summary df of the trackwise features
  mig_features <- mig_features %>%
    left_join(data %>%
                select(global_ID, experiment, condition) %>%
                distinct(),
              by = "global_ID")
  
  return(mig_features)
}

#### define function to summarise trackwise features
## take trackwise migration features and summarise by experiment and condition

# feature_data: optional, defaults to mig_features. Migration feature data by track.
# msd_coeffs: optional, defaults to NULL. If provided, merges coefficients of LMM fitted to MSD data by experiment (model_coeffs_by_experiment).

summarise_migration_features <- function(feature_data=mig_features, msd_coeffs=NULL) {
  mig_features_summ <- feature_data %>%
    ungroup() %>%
    group_by(experiment, condition) %>%
    summarize(
      avg_net_dx = mean(net_dx),           # mean net displacement over x
      avg_net_dy = mean(net_dy),           # mean net displacement over y
      avg_net_d = mean(net_d),             # mean net displacement
      avg_tot_d = mean(tot_d),             # mean total path length
      avg_max_d = mean(max_d),             # mean maximum distance from path start
      avg_meand_index = mean(meand_index), # mean meandering index
      avg_spd = mean(avg_spd)             # mean speed over time
      )
  
  if (!missing(msd_coeffs)) {
    # merge coefficients with other migration features by condition and experiment
    mig_features_summ <- left_join(mig_features_summ, msd_coeffs, by=c('experiment', 'condition'))
  }
  
  return(mig_features_summ)
}

#### define helper function to compute lag-time MSD
## compute ensemble mean squared displacement for each lag time k

# max_lag_frac: optional, defaults to 0.5. Maximum fraction of the track length to use as lag; 0.5 is recommended.

compute_msd_lag <- function(df, max_lag_frac = 0.5) {
  n   <- nrow(df)
  x   <- df$norm_X
  y   <- df$norm_Y
  t_s <- df$time_s
  max_lag <- floor(n * max_lag_frac)
  purrr::map_dfr(seq_len(max_lag), function(k) {
    pairs   <- seq_len(n - k)
    sq_disp <- (x[pairs + k] - x[pairs])^2 + (y[pairs + k] - y[pairs])^2
    tibble(
      lag_slices = k,
      lag_s      = mean(t_s[pairs + k] - t_s[pairs]),
      msd_track  = mean(sq_disp),
      n_pairs    = length(pairs)
    )
  })
}

#### define function to compute lag-time MSD per track
## compute lag-time MSD for all filtered tracks
## this step may be very slow

# data: optional, defaults to mig_filt. Tracking data to analyse.

get_msd_per_track <- function(data=mig_filt) {
  # get MSD per track
  msd_per_track <- data %>%
    group_by(condition, global_ID) %>%
    arrange(frame) %>%
    group_modify(~ compute_msd_lag(.x)) %>%
    ungroup() %>%
    left_join(data %>%
                select(global_ID, experiment) %>%
                distinct(),
              by = "global_ID")
  
  return(msd_per_track)
}
  
#### define function to compute ensemble lag-time MSD
## compute ensemble lag-time MSD for all filtered tracks

# data: optional, defaults to msd_per_track. Lag-time MSD data for each track.

get_ensemble_msd <- function(msd_data=msd_per_track) {
  msd_ensemble <- msd_data %>%
    group_by(condition, lag_slices) %>%
    summarise(
      lag_s    = mean(lag_s),
      mean_msd = mean(msd_track),
      se_msd = if_else(n() > 1, sd(msd_track) / sqrt(n()), 0),
      n_tracks = n(),
      .groups  = "drop"
      )
  
  return(msd_ensemble)
}

#### define function to calculate ensemble lag-time MSD by treatment and experiment
## compute ensemble lag-time MSD by treatment and experiment

# data: optional, defaults to msd_per_track. Lag-time MSD data for each track.

get_ensemble_msd_by_experiment <- function(msd_data=msd_per_track){
  msd_ensemble_by_experiment <- msd_data %>%
    group_by(experiment, condition, lag_slices) %>%
    summarise(
      lag_s    = mean(lag_s),
      mean_msd = mean(msd_track),
      se_msd = if_else(n() > 1, sd(msd_track) / sqrt(n()), 0),
      n_tracks = n(),
      .groups  = "drop"
    )
  
  return(msd_ensemble_by_experiment)
}

#### define function to fit linear mixed models for ensemble lag-time MSD
## fits linear mixed models to the log10 ensemble MSD by log10 lag time, with condition as a random effect
## returns coefficients (D and α) for each condition
## performs Shapiro-Wilk test to check for normality of residuals

# msd_data: optional, defaults to msd_ensemble. Ensemble lag-time MSD data for each condition.
# levels: optional, defaults to treatment_levels_filt. Levels of the treatment condition.

ensemble_msd_fit <- function(msd_data=msd_ensemble, levels=treatment_levels_filt) {
  msd_for_fit <- msd_data %>%
    filter(lag_s > 0, mean_msd > 0)
  
  condition_models <- lmList(
    log10(mean_msd) ~ log10(lag_s) | condition,
    data = msd_for_fit
  )
  
  # get alpha exponent from ensemble MSD linear models (for all experiments)
  model_coeffs <- coef(condition_models) %>%
    as.data.frame() %>%
    rownames_to_column("condition") %>%
    mutate(
      condition = factor(condition, levels=levels)) %>%
    rename(
      intercept = `(Intercept)`,
      alpha = `log10(lag_s)`) %>%
    mutate(
      D_um2_per_s = 10^intercept / 4,
      alpha        = round(alpha, 3),
      D_um2_per_s  = round(D_um2_per_s, 4)
    )
  
  # perform Shapiro-Wilk test to check that the standardised residuals are Gaussian
  # Shapiro-Wilk p-val should be > 0.05, W close to 1
  # if not, the lm is not valid, and slope and intercept are less reliable
  shapiro_results <- lapply(condition_models, function(x) shapiro.test(rstandard(x)))
  model_coeffs$shapiro_p_value <- sapply(shapiro_results, '[[', "p.value")
  model_coeffs$shapiro_W_statistic <- sapply(shapiro_results, '[[', "statistic")
  
  model_coeffs <- model_coeffs %>%
    mutate(
      shapiro_result = if_else(
        shapiro_p_value > 0.05 &
          shapiro_W_statistic > 0.85,
        "Pass",
        "Fail"
      )
    )
  
  # display results
  violates_shapiro <- with(
    model_coeffs,
    shapiro_p_value <= 0.05 | shapiro_W_statistic <= 0.9
  )
  
  if (any(violates_shapiro, na.rm = TRUE)) {
    warning(
      "Shapiro-Wilk test failed for at least one treatment condition. ",
      "The linear models that failed may not be valid, and their coefficients may be less reliable.")
  } else {
    cat("Shapiro-Wilk test passed for all treatment conditions, suggesting that the standardised residuals are Gaussian.")
  }
  
  return(model_coeffs)
}

#### define function to get MSD LMM coefficients for each experiment
## get MSD linear mixed model coefficients from each experiment, to get the error

# msd_data: optional, defaults to msd_ensemble_by_experiment. Ensemble lag-time MSD data for each experiment.

ensemble_MSD_fit_by_experiment <- function(msd_data=msd_ensemble_by_experiment) {
  # get a list of experiments
  experiments <- unique(msd_data$experiment)
  
  # initiate dataframe to hold coefficients
  model_coeffs_by_experiment <- data.frame(
    experiment = character(),
    condition = character(),
    intercept = double(),
    alpha = double(),
    D_um2_per_s = double(),
    shapiro_p_value = double(),
    shapiro_W_statistic = double()
  )
  
  # get coefficients and statistics for each experiment, then bind the rows into one df
  for(exp in experiments){
    # filter the data
    msd_for_fit_exp <- msd_data %>%
      filter(experiment == exp) %>%
      filter(lag_s > 0, mean_msd > 0)
    
    # fit linear mixed model
    condition_models_exp <- lmList(
      log10(mean_msd) ~ log10(lag_s) | condition,
      data = msd_for_fit_exp
    )
    
    # get alpha exponent from ensemble MSD lms
    model_coeffs_exp <- coef(condition_models_exp) %>%
      as.data.frame() %>%
      rownames_to_column("condition") %>%
      rename(intercept = `(Intercept)`, alpha = `log10(lag_s)`) %>%
      mutate(
        experiment  = exp,
        D_um2_per_s = 10^intercept / 4,
        alpha       = round(alpha, 3),
        D_um2_per_s = round(D_um2_per_s, 4)
      )
    
    # perform Shapiro-Wilk test to check that the standardised residuals are Gaussian
    # Shapiro-Wilk p-val should be > 0.05, W close to 1
    # if not, the lm is not valid, and slope and intercept are less reliable
    shapiro_results_exp <- lapply(condition_models_exp, function(x) shapiro.test(rstandard(x)))
    model_coeffs_exp$shapiro_p_value <- sapply(shapiro_results_exp, '[[', "p.value")
    model_coeffs_exp$shapiro_W_statistic <- sapply(shapiro_results_exp, '[[', "statistic")
    
    # merge with coeffs from other experiments
    model_coeffs_by_experiment <- bind_rows(
      model_coeffs_by_experiment,
      model_coeffs_exp, .id=NULL)
  }
  
  model_coeffs_by_experiment <- model_coeffs_by_experiment %>%
    mutate(
      shapiro_result = if_else(
        shapiro_p_value > 0.05 &
          shapiro_W_statistic > 0.85,
        "Pass",
        "Fail"
      )
    )
  
  return(model_coeffs_by_experiment)
}

#### define function to summarise anomalous diffusion exponent (alpha)
## get the mean, SD, and SEM of alpha (useful for plotting error bars)

# coeffs: optional, defaults to model_coeffs_by_experiment. Coefficients of linear mixed model fitted to ensemble MSD by experiment.
# levels: optional, defaults to treatment_levels_filt. Levels of the treatment condition.

summarise_alpha <- function(coeffs=model_coeffs_by_experiment, levels=treatment_levels_filt){
  alpha_summ_stats <- coeffs %>%
    group_by(condition) %>%
    mutate(condition = factor(condition, levels=levels)) %>%
    summarise(
      n_experiments=n(),
      mean=mean(alpha),
      sd=sd(alpha),
      se=sd / sqrt(n_experiments)
    )
}

##---------------------Plotting functions------------------------------
#### define helper function to save plots
## used to save plots generated with the plotting functions

# plot: plot to save
# save_as: file name to save the plot under
# save_path: directory to save file in. Will be created if it does not already exist.
# plot_height: height of plot.
# plot_width: width of plot.
# plot_units: optional, defaults to inches. Unit that height and width are specified in.
# dpi: optional, defaults to 400. Resolution in dots per inch (DPI).

save_plot <- function(plot, save_as, save_path, plot_height, plot_width, plot_units='in', dpi = 400) {
  dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  
  ggsave(
    filename = file.path(save_path, save_as),
    plot = plot,
    units = plot_units,
    width = plot_width,
    height = plot_height,
    dpi = dpi
  )
  
  plot
}

#### define function to plot cell tracks
## plot the trajectories of cells in each treatment condition

# data: optional, defaults to mig_filt. Normalised tracking data.
# conditions: optional, defaults to the first condition in treatment_levels_filt. The list of condition(s) to display.
# labels: optional, defaults to treatment_levels_filt. Labels to use for each condition.
# name: optional, defaults to 'cell_tracks.png'. What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# rows: optional, defaults to 1. Number of rows of subplots.
# height: optional, defaults to NULL (if not specified, it is calculated inside the function). Plot height in pixels.
# width: optional, defaults to NULL (if not specified, it is calculated inside the function). Plot width in pixels.

plot_cell_tracks <- function(data=mig_filt,
                             conditions=treatment_levels_filt[1], labels=treatment_levels_filt,
                             name='cell_tracks.png', path="../../figures",
                             rows=1, height=NULL, width=NULL) {
  
  # filter tracks by condition if necessary
  plot_data <- data %>%
    filter(condition %in% conditions)
  
  if (length(unique(plot_data$condition)) == 0) {
    stop("No data remains after filtering by condition.")
  } else if (length(unique(plot_data$condition)) < length(unique(conditions))) {
    warning("At least one condition was not present in the data.")}
  
  # identify maximum absolute value of x or y, so that the axes can be scaled appropriately
  axis_len <- max(
    max(abs(plot_data$norm_X)),
    max(abs(plot_data$norm_Y))
  )
  
  # generate plot
  tracks_plot <- plot_data %>%
    ggplot(aes(group=global_ID, x=norm_X, y=norm_Y)) +
    geom_path(alpha=0.5, color = 'darkgrey', linewidth=0.5, show.legend=FALSE) +
    geom_point(data = filter(plot_data, frame == min(frame, na.rm = TRUE)),
               color = 'black', size = 1.2, shape = 21, fill = "white",
               show.legend = FALSE) +
    facet_wrap(~condition, scales = "fixed", nrow = rows, labeller = as_labeller(labels)) +
    xlim((-axis_len)-10,(axis_len+10)) + # center the plots
    ylim((-axis_len)-10,(axis_len+10)) +
    labs(x="Normalised x (µm)", y="Normalised y (µm)") +
    theme_classic() +
    theme(
      legend.position="none",
      panel.grid=element_blank(),
      axis.title.x=element_text(size=14, face="bold"),
      axis.title.y=element_text(size=14, face="bold"),
      axis.text.x=element_text(angle=45, hjust=1, size=11),
      axis.text.y=element_text(size=11),
      panel.border=element_rect(color='black', fill=NA, linewidth=1)) +
    coord_fixed(ratio=1)
  
  # define dimensions of plot based on number of conditions and layout
  panel_size <- 900
  left_margin <- 0
  right_margin <- 0
  top_margin <- 20
  bottom_margin <- 20
  
  n_conditions <- dplyr::n_distinct(plot_data$condition)
  n_columns <- ceiling(n_conditions / rows)
  n_rows <- min(rows, n_conditions)
  
  if (is.null(height)) {
    height <- n_rows * panel_size + top_margin + bottom_margin}
  if (is.null(width)) {
    width <- n_columns * panel_size + left_margin + right_margin}
  
  # save plot
  save_plot(plot=tracks_plot, save_as=name, save_path=path, plot_height=height, plot_width=width, plot_units='px')
  
  return(tracks_plot)
}

#### define function to make density plots of cell tracks in each condition
## plot the density of trajectories of cells in each treatment condition

# data: optional, defaults to mig_filt. Normalised tracking data.
# conditions: optional, defaults to the first condition in treatment_levels_filt. The list of condition(s) to display.
# labels: optional, defaults to treatment_levels_filt. Labels to use for each condition.
# name: optional, defaults to 'cell_track_density.png'. What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# rows: optional, defaults to 1. Number of rows of subplots.
# height: optional, defaults to NULL (if not specified, it is calculated inside the function). Plot height in pixels.
# width: optional, defaults to NULL (if not specified, it is calculated inside the function). Plot width in pixels.

plot_cell_track_density <- function(data=mig_filt, conditions=treatment_levels_filt[1],
                                    labels=treatment_levels_filt,
                                    name='cell_track_density.png', path="../../figures",
                                    rows=1, height=NULL, width=NULL) {
  
  # filter tracks by condition if necessary
  plot_data <- data %>%
    filter(condition %in% conditions)
  
  if (length(unique(plot_data$condition)) == 0) {
    stop("No data remains after filtering by condition.")
  } else if (length(unique(plot_data$condition)) < length(unique(conditions))) {
    warning("At least one condition was not present in the data.")}
  
  if (length(unique(plot_data$condition)) < rows) {
    warning('The number of treatment conditions is fewer than the number of rows specified.')
  }
  
  # generate plot
  density_plot <- plot_data %>%
    ggplot(aes(x=norm_X, y=norm_Y)) +
    geom_density_2d(color='darkgrey', contour_var = "ndensity", linewidth = 0.5) +
    geom_point(data = filter(plot_data, frame == min(frame, na.rm = TRUE)),
               color = 'black', size = 1.2, shape = 21, fill = "white",
               show.legend = FALSE) +
    facet_wrap(~condition, scales = "fixed", nrow = rows, labeller = as_labeller(labels)) +
    labs(x="Normalised x (µm)", y="Normalised y (µm)") +
    theme_classic() +
    theme(
      legend.position="none",
      panel.grid=element_blank(),
      axis.title.x=element_text(size=14, face="bold"),
      axis.title.y=element_text(size=14, face="bold"),
      axis.text.x=element_text(angle=45, hjust=1, size=11),
      axis.text.y=element_text(size=11),
      panel.border=element_rect(color='black', fill=NA, linewidth=1)) +
    coord_fixed(ratio=1)
  
  # define dimensions of plot based on number of conditions and layout
  panel_size <- 900
  left_margin <- 0
  right_margin <- 0
  top_margin <- 20
  bottom_margin <- 20
  
  n_conditions <- dplyr::n_distinct(plot_data$condition)
  n_columns <- ceiling(n_conditions / rows)
  n_rows <- min(rows, n_conditions)
  
  if (is.null(height)) {
    height <- n_rows * panel_size + top_margin + bottom_margin}
  if (is.null(width)) {
    width <- n_columns * panel_size + left_margin + right_margin}
  
  # save plot
  save_plot(plot=density_plot, save_as=name, save_path=path, plot_height=height, plot_width=width, plot_units='px')
  
  return(density_plot)
}

#### define function to animate tracks
## takes the filtered tracking data and animates a selection of tracks for one or multiple treatment conditions.
## it selects tracks that start at start_frame and that have at least one other point within the track_length specified.
## therefore, it does not necessarily require that tracks do not have gaps between frames.
## this function may take a very long time to render especially with high numbers of conditions, tracks, or track lengths.

# data: optional, defaults to mig_filt. Filtered tracking data.
# conditions: optional, defaults to the first condition in treatment_levels_filt. The list of condition(s) to display.
# labels: optional, defaults to treatment_levels_filt. The labels to display for each condition.
# start_frame: optional, defaults to 1. The frame number to start the animation at.
# ntracks: optional, defaults to 50. The number of tracks to display per condition.
# track_length: optional, defaults to 60. How long (in steps/points) the tracks should be.
# name: optional, defaults to 'cell_migration_animation.gif'. What to save the plot as.
# path: optional, defaults to '../../figures/'. Where to save the plot.
# rows: optional, defaults to 1. The number of rows to display the animations in.
# height: optional, defaults to NULL (if not specified, it is calculated inside the function). Plot height in pixels.
# width: optional, defaults to NULL (if not specified, it is calculated inside the function). Plot width in pixels.

animate_tracks <- function(data=mig_filt,
                           conditions=treatment_levels_filt[1], labels=treatment_levels_filt,
                           start_frame=1, ntracks=50, track_length=60,
                           name='cell_migration_animation.gif', path="../../figures",
                           rows=1, height=NULL, width=NULL) {
  
  if (track_length <= 1) {
    stop('Tracks must must be greater than one step long to generate animation.')
  }
  if (rows > length(unique(conditions))) {
    stop('The number of rows specified is too large for the number of conditions.')
  }
  
  set.seed(123) # set random seed for reproducibility
  
  # filter tracks by condition and minimum length
  # also make sure they start in the first frame
  end_frame <- start_frame + track_length - 1
  
  eligible_tracks <- data %>%
    filter(
      condition %in% conditions,
      frame >= start_frame,
      frame <= end_frame
    ) %>%
    group_by(condition, global_ID) %>%
    filter(
      n() >= 2,
      any(frame == start_frame)
    ) %>%
    ungroup() %>%
    distinct(condition, global_ID)
  
  anim_track_ids <- eligible_tracks %>%
    group_by(condition) %>%
    group_modify(~ slice_sample(
      .x,
      n = min(ntracks, nrow(.x)),
      replace = FALSE
    )) %>%
    ungroup() %>%
    pull(global_ID)
  
  mig_anim <- data %>%
    filter(
      global_ID %in% anim_track_ids,
      condition %in% conditions,
      frame >= start_frame,
      frame <= end_frame
    ) %>%
    arrange(condition, global_ID, frame) %>%
    mutate(time = as.numeric(frame))
  
  # create a duplicate first point to avoid geom_path error for first point
  # doesn't visibly affect path
  path_data <- mig_anim %>%
    group_by(global_ID) %>%
    slice_head(n = 1) %>%
    mutate(time = time - 1e-6) %>%
    ungroup()
  
  mig_anim_plot <- bind_rows(path_data, mig_anim) %>%
    arrange(global_ID, time)
  
  # check whether there is sufficient track data for each condition
  eligible_start <- data %>%
    filter(condition %in% conditions) %>%
    group_by(condition, global_ID) %>%
    filter(frame == min(frame, na.rm = TRUE)) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  track_counts <- eligible_tracks %>%
    count(condition, name = "eligible_tracks") %>%
    right_join(
      tibble(condition = conditions),
      by = "condition"
    ) %>%
    mutate(
      eligible_tracks = tidyr::replace_na(eligible_tracks, 0L)
    )
  
  short_conditions <- track_counts %>%
    filter(eligible_tracks < ntracks)
  
  missing_conditions <- setdiff(conditions, unique(mig_anim$condition))
  
  if (nrow(mig_anim) == 0) {
    stop("No track data remains after filtering. Check frame numbering, conditions, and track_length.")
  }
  else if (anyNA(mig_anim$norm_X) || anyNA(mig_anim$norm_Y) || anyNA(mig_anim$time)) {
    stop("Animation data contains missing norm_X, norm_Y, or frame values.")
  }
  if (length(missing_conditions) > 0) {
    stop('No tracks were found of minimum length (', track_length, ' points) for the conditions: ', paste(missing_conditions, collapse = ", "))
  } else if (nrow(short_conditions) > 0) {
    warning(
      "Fewer than ", ntracks, " tracks of minimum length ", track_length, " are available for: ", 
      paste(short_conditions$condition, " (", short_conditions$eligible_tracks, ")", collapse = ", ")
    )
  }
  
  # identify maximum absolute value of x or y, so that the axes can be scaled appropriately
  axis_len <- max(
    max(abs(mig_anim_plot$norm_X)),
    max(abs(mig_anim_plot$norm_Y))
  )
  
  # For the trail: at each frame, keep all rows where time <= frame
  # gganimate's shadow_wake / transition_reveal handles this natively
  anim_plot <- mig_anim_plot %>%
    ggplot(aes(x = norm_X, y = norm_Y, group = global_ID)) +
    # Trail: path up to current frame
    geom_path(#color = 'darkgrey', 
      alpha = 0.5, linewidth = 0.5) +
    # Cell head: current position
    geom_point(size = 2.5, alpha = 0.9) +
    # Keep a fading shadow of recent steps
    shadow_wake(wake_length = 0.15, alpha = TRUE, size = TRUE) +
    facet_wrap(~condition, scales = "fixed", nrow = rows, labeller = as_labeller(labels)) +
    xlim((-axis_len)-10,(axis_len+10)) +
    ylim((-axis_len)-10,(axis_len+10)) +
    theme_classic() +
    theme(
      legend.position  = "none",
      panel.grid       = element_blank(),
      panel.border     = element_rect(color='black', fill=NA, linewidth=1),
      strip.text       = element_text(size = 18),
      axis.text.x      = element_text(size = 16, hjust=1, angle=45),
      axis.text.y      = element_text(size = 16),
      axis.title       = element_text(size = 20, face='bold'),
      plot.title       = element_text(size = 30, face = "bold"),
      plot.subtitle    = element_text(size = 25, colour = "grey40")
    ) +
    coord_fixed(ratio=1) +
    labs(
      title = 'Animated Cell Tracks',
      subtitle = paste0(ntracks, ' cells per condition over ', track_length, ' timepoints'),
      x = "Normalised x (µm)", y = "Normalised y (µm)"
    ) +
    # Animate through time steps
    transition_reveal(time)
  
  # define dimensions of plot based on number of conditions and layout
  panel_size <- 500
  left_margin <- 20
  right_margin <- 40
  top_margin <- 20
  bottom_margin <- 20
  
  n_conditions <- dplyr::n_distinct(mig_anim_plot$condition)
  n_columns <- ceiling(n_conditions / rows)
  n_rows <- min(rows, n_conditions)
  
  if (is.null(height)) {
    height <- n_rows * panel_size + top_margin + bottom_margin}
  if (is.null(width)) {
    width <- n_columns * panel_size + left_margin + right_margin}
  
  # render and save animation
  # fps = 10 gives ~5 second GIF; increase for smoother playback
  cat('Animating tracks for the conditions:\n', paste(conditions, collapse=', '), '.\n\n', sep="")
  
  #suppressWarnings(
    animate(
      anim_plot,
      nframes = track_length,
      fps = 10,
      width = width,
      height = height,
      units = "px",
      renderer = gifski_renderer(
        file = file.path(path, name),
        width = width,
        height = height
      ))#)
  
  cat("Rendering and saving as: ", file.path(path, name), '. This may take a while...', "\n", sep = "")  
  
  return(anim_plot)
}

#### define function to plot log10 MSD vs log10 lag time
## plot the log10 ensemble lag time MSD for cells in each treatment condition over log10 lag time
## for each condition, plot a fitted linear model

# data: optional, defaults to mig_filt. Normalised tracking data.
# labels: optional, defaults to treatment_levels_filt. Labels to use for each condition.
# name: optional, defaults to 'msd_vs_time.png'. What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# palette: optional, defaults to NULL (discrete and non-sequential). Colour palette to use when plotting.
# height: optional, defaults to 5. Plot height in inches.
# width: optional, defaults to 8. Plot width in inches.

plot_MSD_vs_lag_time <- function(data=msd_ensemble, labels=treatment_levels_filt,
                                 name='msd_vs_time.png', path="../../figures",
                                 palette=NULL, height=5, width=8) {
                                   
  msd_vs_time_plot <- data %>%
    ggplot(aes(x=lag_s, y=mean_msd, colour = condition)) +
    geom_point() +
    geom_smooth(method="lm", formula = y ~ x, show.legend = FALSE) +
    scale_y_log10() +
    scale_x_log10() + 
    annotation_logticks() +
    theme_classic() +
    theme(
      panel.grid= element_blank(),
      panel.border=element_rect(color='black', fill=NA, linewidth=1),
      axis.title.x=element_text(size=14, face="bold"),
      axis.title.y=element_text(size=14, face="bold"),
      axis.text.x=element_text(hjust=1, size=11),
      axis.text.y=element_text(size=11),
      legend.key = element_rect(fill = NA, colour = NA),
      legend.position = 'bottom'
    ) +
    labs(x = "Lag time (s)", y = "MSD (µm²)",
         colour="Condition")
  
  # assign a colourblind-safe colour palette
  if (identical(palette, 'paired')) {
    # use paired palette from RColorBrewer
    msd_vs_time_plot <- msd_vs_time_plot +
      scale_color_brewer(palette = "Paired", labels=labels)
  } else if (identical(palette, 'sequential')) {
    # use sequential discrete palette from viridis
    msd_vs_time_plot <- msd_vs_time_plot +
      scale_color_viridis(discrete=TRUE, labels=labels)
  } else {
    # use discrete and non-sequential custom color palette
    discrete_palette <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00",
                          "#CC79A7","#999999","#A6761D","#666666","#1B9E77","#7570B3")
    
    msd_vs_time_plot <- msd_vs_time_plot +
      scale_color_manual(values=discrete_palette, labels=labels)
  }
  
  # save plot
  save_plot(plot=msd_vs_time_plot, save_as=name, save_path=path, plot_height=height, plot_width=width, plot_units='in')
  
  return(msd_vs_time_plot)
}


#### define function to plot alpha
## plot the anomalous diffusion exponent (alpha) with SEM

# data: optional, defaults to mig_filt. Normalised tracking data.
# coeffs: optional, defaults to model_coeffs. Coefficients (including alpha) of fitted MSD models.
# alpha_data: optional, defaults to alpha_summ_stats. Summary statistics for alpha.
# labels: optional, defaults to treatment_levels_filt. Labels to use for each condition.
# name: optional, defaults to 'alpha_plot.png'. What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# height: optional, defaults to 4.5. Plot height in inches.
# width: optional, defaults to 8. Plot width in inches.

plot_alpha <- function(data=mig_filt,
                       coeffs=model_coeffs, alpha_data=alpha_summ_stats,
                       labels=treatment_levels_filt,
                       name='alpha_plot.png', path="../../figures",
                       height=4.5, width=8) {
  
  # set appropriate y axis maximum for alpha bar chart
  experiments <- unique(data$experiment)
  if (length(experiments) > 1) {
    ymax_alpha <- max(coeffs$alpha + alpha_data$se) + 0.1
  } else {
    ymax_alpha <- max(coeffs$alpha) + 0.1
  }
  
  coeffs <- left_join(coeffs, alpha_data, by='condition')
  
  # make bar chart of alpha for each treatment
  alpha_plot <- coeffs  %>% 
    ggplot(aes(x = condition, y = alpha)) +
    geom_bar(stat = "identity", show.legend=FALSE, fill = NA, color = 'black') +
    geom_errorbar(aes(
      x=coeffs$condition, 
      ymin=coeffs$alpha - coeffs$se,
      ymax=coeffs$alpha + coeffs$se),
      width=0.4
    ) +
    labs(y = "Anomalous Diffusion exponent (α)", x = NULL) +
    scale_y_continuous(expand = c(0,0), limits=c(0,ymax_alpha), breaks = seq(0, ymax_alpha, by = 0.2))+
    scale_x_discrete(labels=labels) +
    theme_classic()+
    geom_hline(yintercept=1, color='red', linetype='dashed', linewidth=1) +
    theme(
      axis.title=element_text(size=14, face="bold"),
      axis.text.x=element_text(angle=45, hjust=1, size=11),
      axis.text.y=element_text(size=11),
      panel.border=element_rect(color='black', fill=NA, linewidth=1)
    )
  
  # save plot
  save_plot(plot=alpha_plot, save_as=name, save_path=path, plot_height=height, plot_width=width, plot_units='in')
  
  return(alpha_plot)
}

#### define function to combine MSD vs time and alpha plots into one panel
## combined the MSD vs lag time and alpha plots into one panel figure

# data: optional, defaults to mig_filt. Normalised tracking data.
# msd_plot: optional, defaults to NULL. MSD vs lag time plot. Will be created by default unless provided (useful if you want to add labels with ggrepel before making the panel).
# coeffs: optional, defaults to model_coeffs. Coefficients (including alpha) of fitted MSD models.
# alpha_data: optional, defaults to alpha_summ_stats. Summary statistics for alpha.
# labels: optional, defaults to treatment_levels_filt. Labels to use for each condition.
# name: optional, defaults to 'alpha_msd_panel.png'. What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# height: optional, defaults to 10. Figure height in inches.
# width: optional, defaults to 8. Figure width in inches.

plot_alpha_and_msd_panel <- function(data=mig_filt, msd_plot=NULL, msd_data=msd_ensemble,
                              coeffs=model_coeffs, alpha_data=alpha_summ_stats,
                              labels=treatment_levels_filt, palette=NULL,
                              name='alpha_msd_panel.png', path="../../figures",
                              height=10, width=8) {
    
  # make MSD vs lag time plot (if not provided)
  if (missing(msd_plot)) {
    msd_vs_time_plot <- plot_MSD_vs_lag_time(data=msd_data, palette=palette, labels=labels, path=path)
  } else {
    msd_vs_time_plot <- msd_plot
  }

  # make alpha plot
  alpha_plot <- plot_alpha(data=data, labels=labels, path=path)
  
  # combine the MSD and alpha plots into one panel
  MSD_alpha_plot <- (msd_vs_time_plot / alpha_plot) + 
    plot_annotation(tag_levels = 'A',
                    theme = theme(plot.caption = element_text(hjust=0, size=14))
                    )
  
  # save plot
  save_plot(plot=MSD_alpha_plot, save_as=name, save_path=path, plot_height=height, plot_width=width, plot_units='in')
  
  return(MSD_alpha_plot)
}

##---------------------Migration features boxplots------------------------------
#### define function to get boxplots of migration features
## create linear mixed model for migration feature
## use experiment as a random factor
## if likelihood-ratio test does not indicate the random factor is needed, get linear model instead

# data: optional, defaults to mig_features. The track-wise migration feature data.
# feature: the migration feature to use (a column in the data). Can be transformed beforehand if needed.
# experiment: optional, defaults to 'experiment'. Name of the experiment column.

get_linear_mixed_model <- function(data=mig_features, feature, experiment = "experiment") {
  
  # define formulae for LM and LMM
  fixed_formula <- reformulate(
    termlabels = 'condition',
    response = feature)
  
  mixed_formula <- reformulate(
    termlabels = c('condition', paste0("(1 | ", experiment, ")")),
    response = feature)
  
  # fit LM and LMM
  lm_fit <- lm(fixed_formula, data = data)
  lmm_fit <- lmer(mixed_formula, 
                  data = data, 
                  REML = FALSE, 
                  control = lmerControl(check.conv.singular = 'ignore')
  )
  
  # Likelihood-ratio test for the random effect
  random_effect_test <- rand(lmm_fit)
  p_value <- random_effect_test[["Pr(>Chisq)"]]
  p_value <- p_value[!is.na(p_value)][1]
  
  if (!is.na(p_value) && p_value < 0.05) {
    selected_model <- lmm_fit
    selected_type <- "linear mixed model"
  } else {
    warning(paste0(
      "The likelihood-ratio test did not support the use of the random effect. ",
      "Returning a linear model for ", feature, ".\n"))
    selected_model <- lm_fit
    selected_type <- "linear model"
  }
  
  return(
    list(
      model = selected_model,
      model_type = selected_type,
      lm = lm_fit,
      lmm = lmm_fit,
      random_effect_test = random_effect_test,
      random_effect_p = p_value
    ))
}

#### define function to check the assumptions of the linear model for each feature
## check assumptions of the model: normality of residuals and homogeneity of variance
## Shapiro-Wilk test: if not rejected (p>0.05), residuals are likely to be normally distributed
## Levene's test: if not rejected (p>0.05), there is likely to be equal variance

# linear_model: Linear mixed model (or linear model) to evaluate.
# data: optional, defaults to mig_features. Trackwise migration feature data to use.

check_model_assumptions <- function(linear_model, data=mig_features) {
  
  # get model information
  model <- linear_model$model
  model_data <- model.frame(model)
  
  residual_values <- residuals(model)
  fitted_values <- fitted(model)
  condition_values <- model_data[['condition']]
  
  # Shapiro-Wilk test
  shapiro_result <- NULL
  #shapiro_note <- NULL
  
  if (length(residual_values) >= 3 && length(residual_values) <= 5000) {
    shapiro_result <- shapiro.test(residual_values)
    if (shapiro_result$p.value > 0.05) {
      message("Residuals are consistent with normality (Shapiro-Wilk p > 0.05).")
    } else {
      message("Residuals deviate from normality (Shapiro-Wilk p <= 0.05); interpret with caution.")}
  } else {
    warning(
      "Shapiro-Wilk test not applied because sample size is outside the valid range ",
      "(n must be between 3 and 5000). Interpret residual normality from the Q-Q plot and histogram instead.\n")
    #shapiro_note <- "Shapiro-Wilk test not applied; normality assessed visually from a Q-Q plot/histogram."
  }
  
  # Levene's test
  levene_result <- NULL
  #levene_note <- NULL
  
  if (length(unique(condition_values)) > 1) {
    if (length(residual_values) < 10 || length(residual_values) > 5000) {
      warning(
        "Levene's test is less reliable when the sample size is very low or very high. ",
        "Use it as a supporting diagnostic; assess variance visually from the residuals-vs-fitted plot.")
      message("Levene's test was applied, but interpret with caution as sample size is extreme.")
    } #else {
      #message("Levene's test interpreted in the usual way.")
    #}
    
    levene_result <- leveneTest(residual_values ~ condition_values)
    levene_p <- as.data.frame(levene_result)[1, "Pr(>F)"]
    if (is.na(levene_p)) {
      warning("Levene's test p-value could not be computed.")
    } else if (levene_p > 0.05) {
      message("Equal variance is not rejected (Levene's test p > 0.05).")
    } else {
      message("Equal variance is rejected; heterogeneity may be present (Levene's test p <= 0.05).")
    }
  } else {
    levene_result <- "Levene's test not applied because only one condition level is present."
    #levene_note <- "Levene's test not applied; only one condition level exists."
  }
  
  # arrange diagnostic plots panel
  old_par <- par(mfrow = c(2, 2))
  on.exit(par(old_par), add = TRUE)
  
  # Q-Q plot
  qqnorm(
    residual_values,
    main = paste("Normal Q-Q:", linear_model$model_type))
  qqline(residual_values)
  
  # histogram of residuals
  hist(
    residual_values,
    main = "Residual histogram",
    xlab = "Residuals")
  
  # residuals vs fitted plot
  plot(
    fitted_values,
    residual_values,
    xlab = "Fitted values",
    ylab = "Residuals",
    main = "Residuals vs fitted")
  abline(h = 0, lty = 2)
  
  return(
    list(
      `shapiro-wilk` = shapiro_result,
      #`shapiro-note` = shapiro_note,
      levene = levene_result#,
      #`levene-note` = levene_note
      )
      )
}

#### define function to get post-hoc comparisons from a model
## use estimated marginal means to compute pairwise comparisons from the model.

# model: linear model to use.
# contrasts_to_plot: treatment condition pairs to compare.

# NB: contrasts_to_plot must be in the format 'treatment1 - treatment2' (or a list of these strings)

get_posthoc_comparisons <- function(model, contrasts_to_plot) {
  # if model is a list (ie, the output from get_linear_mixed_model), get fit
  if (is.list(model) && !is.null(model$model)) {
    model <- model$model
  }
  
  # compute pairwise comparisons
  comparisons <- emmeans::emmeans(model, ~ condition) %>%
    emmeans::contrast(method='pairwise', adjust = "bonferroni") %>%
    as.data.frame() %>%
    mutate(
      signif = case_when(
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        TRUE ~ "n.s.")) %>%
    filter(contrast %in% contrasts_to_plot)
  
  return(
    list(
      table = comparisons,
      comparisons = comparisons$contrast,
      annotations = comparisons$signif
  ))
}

#### define function to generate boxplots for a single migration feature
## make a boxplot figure for one migration feature
## can be combined into a composite figure with make_migration_boxplot_figure

# model: linear model to use for the feature.
# data: optional, defaults to mig_features. The trackwise migration feature data.
# labels: optional, defaults to treatment_levels_filt. Labels to use for each condition.
# feature: the migration feature to plot (name of the column in the data).
# title: optional, defaults to NULL. Title to use for the plot.
# y_label: optional, defaults to the feature name. Y-axis label.
# contrasts_to_plot: treatment condition pairs to compare.

# NB: contrasts_to_plot must be in the format 'treatment1 - treatment2' (or a list of these strings)

make_feature_boxplot <- function(
    model, data=mig_features, labels=treatment_levels_filt,
    feature, title=NULL,
    y_label=feature,
    contrasts_to_plot
) {
  
  posthoc <- suppressMessages(get_posthoc_comparisons(model, contrasts_to_plot))
  
  # guard against empty comparisons
  if (nrow(posthoc$table) == 0) {
    boxplot <- ggplot(data, aes(x = condition, y = .data[[feature]])) +
      geom_boxplot() +
      scale_x_discrete(labels = labels) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(title = title, x = "Condition", y = y_label) +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        axis.ticks.y = element_line(),
        axis.line = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
      )
    return(boxplot)
  }
  
  comparison_list <- lapply(
    strsplit(posthoc$table$contrast, " - "),
    function(x) trimws(x))
  
  boxplot <- ggplot(data, aes(x = condition, y = .data[[feature]])) +
    geom_boxplot() +
    geom_signif(
      comparisons = comparison_list,
      annotations = posthoc$table$signif,
      margin_top = 0.1,
      step_increase = 0.1,
      vjust = 0.1) +
    scale_x_discrete(labels = labels) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, x = "Condition", y = y_label) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      axis.ticks.y = element_line(),
      axis.line = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )

  return(boxplot)
}

#### define function to create 4-panel composite boxplot figure
## make a boxplot figure for one migration feature
## can be combined into a composite figure with make_migration_boxplot_figure

# tot_d_boxplot: optional, defaults to tot_d_plot. Total displacement boxplot made with make_feature_boxplot.
# net_d_boxplot: optional, defaults to net_d_plot. Net displacement boxplot made with make_feature_boxplot.
# max_d_boxplot: optional, defaults to max_d_plot. Maximum displacement boxplot made with make_feature_boxplot.
# meand_boxplot: optional, defaults to meand_plot. Meandering index boxplot made with make_feature_boxplot.
# name: optional, defaults to 'migration_features_boxplots.png'. What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# height: optional, defaults to 12. Plot height in inches.
# width: optional, defaults to 12. Plot width in inches.

make_migration_boxplot_figure <- function(
    tot_d_boxplot=tot_d_plot, net_d_boxplot=net_d_plot,
    max_d_boxplot=max_d_plot, meand_boxplot=meand_plot,
    name = 'migration_features_boxplots.png', path="../../figures",
    height = 12, width = 12) {
  
  # use patchwork to assemble the plots into one figure
  boxplot_figure <- tot_d_boxplot + net_d_boxplot + max_d_boxplot + meand_boxplot +
    plot_annotation(tag_levels = 'A')
  
  # save plot
  save_plot(plot=boxplot_figure, save_as=name, save_path=path,
              plot_height=height, plot_width=width, plot_units='in')
  
  for (plot in c(tot_d_boxplot, net_d_boxplot, max_d_boxplot, meand_boxplot, boxplot_figure)) {
    print(plot)
  }
  
  invisible(list(
    plots = list(
      total_d = tot_d_boxplot,
      net_d = net_d_boxplot,
      max_d = max_d_boxplot,
      meand_index = meand_boxplot),
    composite = boxplot_figure
  ))
}

##---------------------PCA functions------------------------------
#### define function to construct a PCA
## constructs a PCA for a list of migration features

# data: optional, defaults to mig_features_summ. Migration feature data summarised per condition and experiment.
# features: optional, defaults to avg_net_d, avg_tot_d, avg_max_d, avg_meand_index, 0.05, D_um2_per_s. Migration features to consider in the PCA.

# NB: the features used should not be highly collinear (i.e., representing the same biological effect). For example, total displacement and average speed are related so only one should be used in the PCA.

get_pca <- function(data=mig_features_summ,
                    features=c('avg_net_d','avg_tot_d','avg_max_d','avg_meand_index','alpha','D_um2_per_s')) {
  # construct PCA
  pca <- prcomp(data[features], scale. = TRUE)
  
  # get principal component (PC) scores
  scores <- as.data.frame(pca$x)
  scores$condition <- data$condition
  
  return(
    list(
      model = pca,
      scores = scores,
      eigenvalues = get_eigenvalue(pca),
      variables = get_pca_var(pca)
  ))
}

#### define function to generate scree plot
## creates a scree plot (or elbow plot) showing what percentage of the variance is explained by each PC.

# pca: optional, defaults to pca_results. Output of get_pca.
# save: optional, defaults to FALSE. Whether the scree plot should be saved.
# name: optional, defaults to 'PCA_scree.png'. What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# height: optional, defaults to 4.5. Figure height in inches.
# width: optional, defaults to 6. Figure width in inches.

plot_pca_scree <- function(pca=pca_results,
                           save=FALSE, 
                           name = "PCA_scree.png", path = "../../figures",
                           height = 4.5, width = 6) {
  
  # get eigenvalues
  eig <- get_eigenvalue(pca$model)
  
  # get data for the scree plot
  scree_data <- tibble(
    component = factor(
      seq_len(nrow(eig)),
      levels = seq_len(nrow(eig))),
    variance_percent = eig$variance.percent)
  
  # generate the scree plot
  scree_plot <- ggplot(
    scree_data,
    aes(x = component, y = variance_percent)) +
    geom_col(width = 0.7, fill = NA, color = "black") +
    geom_line(aes(group = 1), color = "black", alpha=0.5, linewidth = 1) +
    geom_point(color = "black", size = 2) +
    geom_text(aes(label = round(variance_percent, 1)), vjust = -1.5) +
    scale_y_continuous(expand = c(0,0), limits=c(0,100), breaks = seq(0, 100, by = 20))+
    labs(
      x = 'Principal Component',
      y = 'Variance Explained (%)') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text.x=element_text(hjust=1, size=11),
      axis.text.y=element_text(size=11),
      panel.border = element_rect(color = "black",fill = NA,linewidth = 1)
    )
  
  # save plot
  if (save) {
    save_plot(plot=scree_plot, save_as=name, save_path=path, plot_height=height, plot_width=width, plot_units='in')
  }

  return(scree_plot)
}

#### define function to plot PC contribution
## plots the contribution of each feature to each PC (or dimension).

# pca: optional, defaults to pca_results. Output of get_pca.
# axis: optional, defaults to 1. The PC to plot with.
# top: optional, defaults to 10. The number of highest-contributing features to show.
# save: optional, defaults to FALSE. Whether the scree plot should be saved.
# name: optional, defaults to NULL (if save=TRUE, will be generated inside the function). What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# height: optional, defaults to 4.5. Figure height in inches.
# width: optional, defaults to 6. Figure width in inches.

plot_pca_contribution <- function(pca=pca_results, 
                                  axis = 1, top = 10,
                                  save = FALSE, name = NULL, path = "../../figures",
                                  height = 4.5, width = 6) {
  
  if (save && is.null(name)) {
    name <- paste0("PCA_PC", axis, "_contribution.png")
  }
  
  contribution <- factoextra::get_pca_var(pca$model)$contrib
  
  contribution_data <- tibble(
    variable = rownames(contribution),
    contribution_percent = contribution[, axis]) %>%
    arrange(desc(contribution_percent)) %>%
    slice_head(n = top) %>%
    mutate(
      variable = factor(variable, levels = variable)
    )
  
  pca_contribution_plot <- ggplot(
    contribution_data,
    aes(x = variable, y = contribution_percent)) +
    geom_col(width = 0.7, fill = NA, color = "black") +
    geom_text(aes(label = round(contribution_percent, 1)), vjust = -0.5, size = 3.5) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(contribution_data$contribution_percent) * 1.15)) +
    labs(
      x = "Feature",
      y = paste0("Contribution to PC", axis, " (%)")
    ) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
  
  # save plot
  if (save==TRUE) {
    save_plot(plot=pca_contribution_plot, save_as=name, save_path=path, plot_height=height, plot_width=width, plot_units='in')}
  
  return(pca_contribution_plot)
}

#### define function to plot variable biplot
## generates a biplot showing the contribution of each feature to the first 2 PCs.

# pca: optional, defaults to pca_results. Output of get_pca.
# save: optional, defaults to FALSE. Whether the plot should be saved.
# name: optional, defaults to NULL (if save=TRUE, will be generated inside the function). What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# height: optional, defaults to 5. Figure height in inches.
# width: optional, defaults to 6. Figure width in inches.

plot_pca_biplot_variables <- function(pca=pca_results,
                               save=FALSE, name = "PCA_variables.png", path = "../../figures",
                               height = 5, width = 6) {
  
  variable_biplot <- fviz_pca_var(
    pca$model, col.var = "contrib", gradient.cols = c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 11),
      panel.border = element_rect(color = "black",fill = NA, linewidth = 1)
    )
  
  # save plot
  if (save==TRUE) {
    save_plot(plot=variable_biplot, save_as=name, save_path=path, plot_height=height, plot_width=width, plot_units='in')}
  
  return(variable_biplot)
}

#### define function to plot PCA sample scatterplot
## generates a scatterplot of the samples (each treatment in each condition) relative to the first 2 PCs.

# pca: optional, defaults to pca_results. Output of get_pca.
# labels: optional, defaults to treatment_levels_filt. Labels to use for each condition.
# name: optional, defaults to "PCA_scatter.png". What to save the plot as.
# path: optional, defaults to "../../figures". Where to save the plot.
# palette: optional, defaults to NULL (discrete and non-sequential). Colour palette to use when plotting.
# height: optional, defaults to 5. Figure height in inches.
# width: optional, defaults to 7. Figure width in inches.

plot_pca_scatter <- function(pca=pca_results, labels = treatment_levels_filt,
                             condition_order=treatment_levels_filt,
                             name = "PCA_scatter.png", path = "../../figures",
                             palette=NULL, height = 5, width = 7) {
  
  scores <- pca$scores
  
  scores$condition <- factor(
    as.character(scores$condition),
    levels = as.character(condition_order)
  )
  
  if (anyNA(scores$condition)) {
    stop(
      "Some condition values do not match condition_order: ",
      paste(setdiff(unique(as.character(pca$scores$condition)), 
                    condition_order), collapse = ", "))
  }
  if (length(labels) != length(condition_order)) {
    stop("labels and condition_order must have the same length.")}
  
  scores <- pca$scores %>%
    mutate(
      condition = factor(
        as.character(condition),
        levels = condition_order),
      condition_rank = match(condition, condition_order)) %>%
    arrange(condition_rank) %>%  # higher ranks are drawn later
    select(-condition_rank)
  
  pca_scatterplot <- ggplot(
    scores,
    aes(x = PC1, y = PC2, colour = condition)) +
    geom_point(size = 2) +
    labs(
      x = "PC1",
      y = "PC2",
      colour = "Condition"
    ) +
    theme_classic() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
  
  # assign a colourblind-safe colour palette
  if (identical(palette, 'paired')) {
    # use paired palette from RColorBrewer
    pca_scatterplot <- pca_scatterplot +
      scale_color_brewer(palette = "Paired", breaks = condition_order, labels=labels)
  } else if (identical(palette, 'sequential')) {
    # use sequential discrete palette from viridis
    pca_scatterplot <- pca_scatterplot +
      scale_color_viridis(discrete=TRUE, breaks = condition_order, labels=labels)
  } else {
    # use discrete and non-sequential custom color palette
    discrete_palette <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00",
                          "#CC79A7","#999999","#A6761D","#666666","#1B9E77","#7570B3")
    
    pca_scatterplot <- pca_scatterplot +
      scale_color_manual(values=discrete_palette, breaks = condition_order, labels=labels)
  }
  
  # save plot
  save_plot(plot=pca_scatterplot, save_as=name, save_path=path, plot_height=height, plot_width=width, plot_units='in')
  
  return(pca_scatterplot)
}