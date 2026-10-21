library(tidyverse)

#' Serpentine (boustrophedon) numbering along a major/minor axis pair
#'
#' Odd-numbered "major" strips (e.g. columns) count minor positions
#' increasing (bottom-to-top); even strips count decreasing (top-to-bottom).
serpentine_number <- function(major, minor, n_minor) {
  if_else(
    major %% 2 == 1,
    (major - 1) * n_minor + minor,
    (major - 1) * n_minor + (n_minor + 1 - minor)
  )
}

#' Straight (non-serpentine) numbering along a major/minor axis pair
#'
#' Every "major" strip counts minor positions in the same direction
#' (increasing), unlike `serpentine_number()` which alternates direction
#' each strip.
straight_number <- function(major, minor, n_minor) {
  (major - 1) * n_minor + minor
}

#' Serpentine numbering over a rectangular grid, from a chosen corner
#'
#' Numbers `n_rows * n_cols` grid cells 1:(n_rows*n_cols) in serpentine
#' (boustrophedon) fashion, starting from a chosen corner and running
#' either up/down (column-major) or left/right (row-major).
#'
#' @param row,col Row and column indices, where `row = 1` is the top row
#'   and `col = 1` is the left column (matching typical `expand_grid(row =
#'   1:n_rows, col = 1:n_cols)` usage).
#' @param n_rows,n_cols Grid dimensions.
#' @param start Which corner numbering begins at: one of "bottomleft",
#'   "bottomright", "topleft", "topright".
#' @param direction Serpentine direction: "updown" numbers each column
#'   top-to-bottom or bottom-to-top in turn (column-major), moving across
#'   columns; "leftright" numbers each row left-to-right or right-to-left
#'   in turn (row-major), moving down/up across rows.
#' @param pattern "serpentine" (default) alternates direction each strip
#'   (the usual boustrophedon pattern). "straight" numbers every strip in
#'   the same direction instead — e.g. with `direction = "updown"`, every
#'   column counts bottom-to-top (or top-to-bottom, per `start`), rather
#'   than alternating.
#'
#' @return A numeric vector of grid numbers, same length as `row`/`col`.
#'
#' Quick reference — `start` x `direction` combinations (pattern = "serpentine"):
#'
#' | start       | direction  | cell 1 is at... | numbering runs...                        |
#' |-------------|------------|------------------|-------------------------------------------|
#' | bottomleft  | updown     | bottom-left      | up each column, left to right across cols |
#' | bottomleft  | leftright  | bottom-left      | right along each row, bottom to top       |
#' | bottomright | updown     | bottom-right      | up each column, right to left across cols |
#' | bottomright | leftright  | bottom-right      | left along each row, bottom to top        |
#' | topleft     | updown     | top-left          | down each column, left to right across cols |
#' | topleft     | leftright  | top-left          | right along each row, top to bottom       |
#' | topright    | updown     | top-right         | down each column, right to left across cols |
#' | topright    | leftright  | top-right         | left along each row, top to bottom        |
#'
#' With `pattern = "straight"`, every strip counts in the same direction
#' (towards the far side from `start`) instead of alternating.
grid_serpentine_number <- function(row, col, n_rows, n_cols,
                                    start = c("bottomleft", "bottomright", "topleft", "topright"),
                                    direction = c("updown", "leftright"),
                                    pattern = c("serpentine", "straight")) {
  start <- match.arg(start)
  direction <- match.arg(direction)
  pattern <- match.arg(pattern)

  row_idx <- if (startsWith(start, "top")) row else n_rows + 1 - row
  col_idx <- if (endsWith(start, "left")) col else n_cols + 1 - col

  numbering_fn <- if (pattern == "serpentine") serpentine_number else straight_number

  if (direction == "leftright") {
    numbering_fn(row_idx, col_idx, n_cols)
  } else {
    numbering_fn(col_idx, row_idx, n_rows)
  }
}

#' Per-axis ranks and sequential number for a position within a block
#'
#' Shared by `plot_block_grid()` and `plot_selected_block()` so both derive
#' `plot_number` and letter/number labels the same way. `vert_idx`/`horiz_idx`
#' are 1-based positions along the vertical/horizontal axis with 1 at the
#' bottom/left respectively (as used throughout this file for `sub_row`/
#' `sub_col`-style indices); `row_idx`/`col_idx` are the same but
#' direction-adjusted so 1 is at whichever side `start` begins from.
#'
#' @param vert_idx,horiz_idx 1-based position along the vertical/horizontal
#'   axis (1 = bottom / left), same length.
#' @param n_vert,n_horiz Counts along the vertical/horizontal axis.
#' @param start,pattern As in `grid_serpentine_number()`.
#'
#' @return A tibble with columns `row_idx`, `col_idx`, `plot_number`.
grid_axis_ranks <- function(vert_idx, horiz_idx, n_vert, n_horiz, start, pattern) {
  row_for_fn <- n_vert + 1 - vert_idx
  tibble(
    row_idx = if (startsWith(start, "top")) row_for_fn else n_vert + 1 - row_for_fn,
    col_idx = if (endsWith(start, "left")) horiz_idx else n_horiz + 1 - horiz_idx,
    plot_number = grid_serpentine_number(
      row = row_for_fn, col = horiz_idx,
      n_rows = n_vert, n_cols = n_horiz,
      start = start, direction = "updown", pattern = pattern
    )
  )
}

#' Build a (possibly non-rectangular) trial grid layout
#'
#' Creates a plot-level data frame for a trial laid out as an
#' `n_block_rows` x `n_block_cols` grid of blocks, each subdivided into
#' `sub_rows` x `sub_cols` plots.
#'
#' @param n_block_rows,n_block_cols Number of blocks along each axis.
#' @param sub_rows,sub_cols Number of plots along each axis within a block.
#' @param exclude_blocks Block numbers (in the `grid_number` scheme
#'   determined by `grid_start`/`grid_direction`, numbered
#'   1:(n_block_rows * n_block_cols)) that are physically absent from the
#'   trial — e.g. because the field boundary is irregular. These blocks are
#'   kept as rows (with valid coordinates) but get `Pset`,
#'   `within_grid_number`, and `plot_label` set to NA, producing an
#'   effectively non-rectangular layout. The remaining blocks are then
#'   renumbered 1:n as `Pset`, per `pset_start`/`pset_direction`.
#' @param grid_start,pset_start Corner numbering starts from for the initial
#'   `grid_number` pass and the post-exclusion `Pset` renumbering pass,
#'   respectively. One of "bottomleft" (default for both), "bottomright",
#'   "topleft", "topright".
#' @param grid_direction,pset_direction Serpentine direction for the
#'   `grid_number` and `Pset` passes, respectively. "updown" (default for
#'   both) numbers up/down each column in turn (column-major); "leftright"
#'   numbers left/right along each row in turn (row-major).
#' @param grid_pattern,pset_pattern Numbering pattern for the `grid_number`
#'   and `Pset` passes, respectively. "serpentine" (default for both)
#'   alternates direction each strip; "straight" numbers every strip in the
#'   same direction (e.g. every column bottom-to-top, with no alternation),
#'   per `grid_direction`/`pset_direction` and `grid_start`/`pset_start`.
#' @param sub_orientation Which of `sub_row`/`sub_col` runs along which axis
#'   within a block, matching the `orientation` argument of
#'   `plot_block_grid()`. One of:
#'   - "row_vertical" (default): `sub_row` (1:sub_rows) runs vertically
#'     (contributing to `plot_row`); `sub_col` (a, b, c, ...) runs
#'     horizontally (contributing to `plot_col`).
#'   - "row_horizontal": `sub_row` runs horizontally (contributing to
#'     `plot_col`); `sub_col` runs vertically (contributing to `plot_row`).
#' @param within_grid_direction Serpentine direction for `within_grid_number`,
#'   relative to whichever corner `sub_start` begins at. One of:
#'   - "vertical" (default): serpentines up/down within each vertical strip,
#'     advancing across the horizontal axis (matches the previous, only,
#'     behaviour when `sub_start = "bottomleft"`).
#'   - "horizontal": serpentines left/right within each horizontal strip,
#'     advancing across the vertical axis.
#'   This is independent of `sub_orientation` — it only affects the
#'   direction `within_grid_number` snakes through a block, not which of
#'   `sub_row`/`sub_col` maps to `plot_row`/`plot_col`.
#' @param sub_start Which corner `sub_row`, `sub_col`, and
#'   `within_grid_number` originate from within each block: one of
#'   "bottomleft" (default), "bottomright", "topleft", "topright" — using
#'   the same convention as `start` in `plot_block_grid()`/
#'   `plot_selected_block()`, so e.g. `plot_block_grid(..., start =
#'   "bottomright")` will correctly match `grid_plot_coords` built with
#'   `sub_start = "bottomright"`. `plot_col`/`plot_row` (the plot's true
#'   physical position) are never affected by `sub_start` — only the
#'   sub_row/sub_col/within_grid_number labelling within each block shifts.
#'
#' @return A tibble with one row per plot, with columns:
#'   grid_number, Pset, plot_col, plot_row, sub_row, sub_col,
#'   within_grid_number, plot_label.
build_trial_grid <- function(n_block_rows, n_block_cols,
                              sub_rows = 6, sub_cols = 6,
                              exclude_blocks = integer(0),
                              grid_start = "bottomleft", grid_direction = "updown",
                              pset_start = "bottomright", pset_direction = "updown",
                              grid_pattern = c("serpentine", "straight"),
                              pset_pattern = c("serpentine", "straight"),
                              sub_orientation = c("row_vertical", "row_horizontal"),
                              within_grid_direction = c("vertical", "horizontal"),
                              sub_start = c("bottomleft", "bottomright", "topleft", "topright")) {
  sub_orientation <- match.arg(sub_orientation)
  within_grid_direction <- match.arg(within_grid_direction)
  grid_pattern <- match.arg(grid_pattern)
  pset_pattern <- match.arg(pset_pattern)
  sub_start <- match.arg(sub_start)

  n_vert <- if (sub_orientation == "row_vertical") sub_rows else sub_cols
  n_horiz <- if (sub_orientation == "row_vertical") sub_cols else sub_rows

  blocks <- expand_grid(row = 1:n_block_rows, col = 1:n_block_cols) |>
    mutate(
      row_label = n_block_rows + 1 - row,       # 1 = bottom row
      grid_number = grid_serpentine_number(
        row, col, n_block_rows, n_block_cols,
        start = grid_start, direction = grid_direction, pattern = grid_pattern
      ),
      is_excluded = grid_number %in% exclude_blocks
    ) |>
    arrange(
      grid_serpentine_number(
        row, col, n_block_rows, n_block_cols,
        start = pset_start, direction = pset_direction, pattern = pset_pattern
      )
    ) |>
    mutate(
      Pset = if_else(is_excluded, NA_real_, cumsum(!is_excluded))
    ) |>
    select(grid_number, block_row = row, block_col = col, row_label, Pset, is_excluded)

  blocks |>
    cross_join(expand_grid(sub_row = 1:sub_rows, sub_col = 1:sub_cols)) |>
    mutate(
      # plot_col/plot_row are always tied to true physical position —
      # unaffected by sub_start.
      plot_col = if (sub_orientation == "row_vertical") {
        (block_col - 1) * sub_cols + sub_col
      } else {
        (block_col - 1) * sub_rows + sub_row
      },
      plot_row = if (sub_orientation == "row_vertical") {
        (row_label - 1) * sub_rows + sub_row
      } else {
        (row_label - 1) * sub_cols + sub_col
      },
      # Raw (sub_start = "bottomleft"-equivalent) vertical/horizontal rank
      # within the block, 1 = bottom/left — whichever of sub_row/sub_col
      # this is depends only on sub_orientation.
      vert_idx_raw = if (sub_orientation == "row_vertical") sub_row else sub_col,
      horiz_idx_raw = if (sub_orientation == "row_vertical") sub_col else sub_row,
      # Direction-adjusted ranks, 1 at whichever side `sub_start` begins
      # from — same convention as grid_axis_ranks()/plot_block_grid().
      vert_idx_adj = if (startsWith(sub_start, "top")) n_vert + 1 - vert_idx_raw else vert_idx_raw,
      horiz_idx_adj = if (endsWith(sub_start, "left")) horiz_idx_raw else n_horiz + 1 - horiz_idx_raw,
      within_grid_number = if (within_grid_direction == "vertical") {
        serpentine_number(horiz_idx_adj, vert_idx_adj, n_vert)
      } else {
        serpentine_number(vert_idx_adj, horiz_idx_adj, n_horiz)
      },
      # sub_row/sub_col keep tracking the same raw dimension they always
      # did (the "number" dimension sized sub_rows vs. the "letter"
      # dimension sized sub_cols) — only their direction/origin shifts
      # with sub_start.
      sub_row = if (sub_orientation == "row_vertical") vert_idx_adj else horiz_idx_adj,
      sub_col = letters[if (sub_orientation == "row_vertical") horiz_idx_adj else vert_idx_adj],
      plot_label = paste(Pset, within_grid_number, sep = "_")
    ) |>
    select(
      grid_number, Pset, plot_col, plot_row,
      sub_row, sub_col, within_grid_number, plot_label
    )
}

# Example usage:
# grid_plot_coords <- build_trial_grid(
#   n_block_rows = 10, n_block_cols = 8,
#   sub_rows = 6, sub_cols = 6,
#   exclude_blocks = 4:8
# )

#' Display an n_rows x n_cols grid, labelled column-wise serpentine
#'
#' Standalone visualization of the serpentine numbering used elsewhere in
#' this file (e.g. for `Pset` or `within_grid_number`), for any grid size.
#' Column 1 numbers bottom-to-top, column 2 top-to-bottom, and so on,
#' alternating direction across all columns.
#'
#' @param n_rows,n_cols Grid dimensions.
#' @param label_type Either "number" (sequential 1:(n_rows*n_cols)) or
#'   "letter" (sub_col letter + sub_row number, e.g. "a1"). Letter mode
#'   requires n_cols <= 26.
#' @param orientation Which of `sub_row`/`sub_col` runs along which axis.
#'   One of:
#'   - "row_vertical" (default): `sub_row` (1:n_rows) runs vertically
#'     (y-axis); `sub_col` (a, b, c, ...) runs horizontally (x-axis). This
#'     matches the convention used elsewhere in this file (e.g.
#'     `within_grid_number` in `build_trial_grid()`).
#'   - "row_horizontal": `sub_row` (1:n_rows) runs horizontally (x-axis);
#'     `sub_col` (a, b, c, ...) runs vertically (y-axis).
#'   In both cases the numbering runs up/down along whichever axis
#'   `sub_row` occupies, advancing across `sub_col` positions (or vice versa
#'   when the axes are swapped) — i.e. it always runs vertically within a
#'   strip, matching the "up/down" convention used throughout this file.
#' @param pattern "serpentine" (default) alternates direction each strip
#'   (the usual boustrophedon pattern, matching `build_trial_grid()`'s
#'   default); "straight" numbers every strip in the same direction
#'   instead, with no alternation.
#' @param start Which corner numbering begins at: one of "bottomleft"
#'   (default), "bottomright", "topleft", "topright". The grid's physical
#'   layout (which cell is plotted where, and the letter/number labels
#'   themselves) is unaffected — only where numbering 1 starts and which
#'   way strips run changes.
#'
#' @return A ggplot object.
plot_block_grid <- function(n_rows, n_cols, label_type = c("number", "letter"),
                             orientation = c("row_vertical", "row_horizontal"),
                             pattern = c("serpentine", "straight"),
                             start = c("bottomleft", "bottomright", "topleft", "topright")) {
  label_type <- match.arg(label_type)
  orientation <- match.arg(orientation)
  pattern <- match.arg(pattern)
  start <- match.arg(start)

  if (label_type == "letter" && n_cols > length(letters)) {
    stop("label_type = 'letter' supports at most ", length(letters), " columns.")
  }

  grid <- expand_grid(sub_row = 1:n_rows, sub_col = 1:n_cols)

  # vert_idx/n_vert = index/count along the vertical (y) axis, 1 = bottom;
  # horiz_idx/n_horiz = index/count along the horizontal (x) axis, 1 = left.
  if (orientation == "row_vertical") {
    grid <- grid |> mutate(x = sub_col, y = sub_row, vert_idx = sub_row, horiz_idx = sub_col)
    n_vert <- n_rows; n_horiz <- n_cols
  } else {
    grid <- grid |> mutate(x = sub_row, y = sub_col, vert_idx = sub_col, horiz_idx = sub_row)
    n_vert <- n_cols; n_horiz <- n_rows
  }

  grid <- grid |>
    bind_cols(grid_axis_ranks(grid$vert_idx, grid$horiz_idx, n_vert, n_horiz, start, pattern)) |>
    mutate(
      label = if (label_type == "number") {
        as.character(plot_number)
      } else if (orientation == "row_vertical") {
        # sub_col (the letter) is horizontal here, so its adjusted rank is
        # col_idx; sub_row (the number) is vertical, so row_idx.
        paste0(letters[col_idx], row_idx)
      } else {
        # sub_col is vertical here, so its adjusted rank is row_idx;
        # sub_row is horizontal, so col_idx — matching build_trial_grid()'s
        # sub_col/sub_row assignment for the same orientation.
        paste0(letters[row_idx], col_idx)
      }
    )

  ggplot(grid, aes(x, y)) +
    geom_tile(color = "black", fill = "lightblue") +
    geom_text(aes(label = label), size = 4) +
    coord_equal() +
    theme_void()
}

# Example usage:
# plot_block_grid(6, 6)
# plot_block_grid(10, 8, label_type = "letter")
# plot_block_grid(6, 6, label_type = "letter", orientation = "row_horizontal")

#' Plot a single selected block from a trial grid
#'
#' Filters `data` down to one block and displays its plots, using
#' `plot_col`/`plot_row` as x/y coordinates. The label shown on each plot is
#' recomputed from `sub_row`/`sub_col` using the same letter/number scheme as
#' `plot_block_grid()` (via `grid_axis_ranks()`), so `orientation`, `pattern`,
#' and `start` here should match whatever was used to build `data` (e.g. the
#' `sub_orientation` passed to `build_trial_grid()`) for the labels to make
#' sense.
#'
#' @param data A data frame with columns `plot_col`, `plot_row`, a block
#'   identifier column, and either `sub_row` (number) + `sub_col` (letter,
#'   e.g. "a", "b", ...) columns, or a `plot_pos` column combining them as a
#'   single string (e.g. "a1", "b12") — as produced by merging
#'   `build_trial_grid()` output into a design import file.
#' @param block Value of the block to select (matched against `pset_col`).
#' @param pset_col Unquoted name of the block identifier column
#'   (default `Pset.x`).
#' @param x_col,y_col Unquoted names of the x/y coordinate columns
#'   (default `plot_col`, `plot_row`).
#' @param orientation,pattern,start Same meaning as in `plot_block_grid()` —
#'   keep these in sync with the corresponding `build_trial_grid()` call.
#'
#' @return A ggplot object.
plot_selected_block <- function(data, block, pset_col = Pset.x,
                                 x_col = plot_col, y_col = plot_row,
                                 orientation = c("row_vertical", "row_horizontal"),
                                 pattern = c("serpentine", "straight"),
                                 start = c("bottomleft", "bottomright", "topleft", "topright")) {
  orientation <- match.arg(orientation)
  pattern <- match.arg(pattern)
  start <- match.arg(start)

  block_data <- data |> filter({{ pset_col }} == block)

  if (nrow(block_data) == 0) {
    stop("No rows found for block ", block, ".")
  }

  if (all(c("sub_row", "sub_col") %in% names(block_data))) {
    sub_row <- block_data$sub_row
    sub_col_letter <- block_data$sub_col
  } else if ("plot_pos" %in% names(block_data)) {
    sub_col_letter <- tolower(sub("([a-zA-Z]+).*", "\\1", block_data$plot_pos))
    sub_row <- as.integer(sub("^[a-zA-Z]+", "", block_data$plot_pos))
  } else {
    stop("`data` needs either sub_row/sub_col columns or a plot_pos column.")
  }
  sub_col_num <- match(sub_col_letter, letters)

  if (orientation == "row_vertical") {
    vert_idx <- sub_row; horiz_idx <- sub_col_num
  } else {
    vert_idx <- sub_col_num; horiz_idx <- sub_row
  }
  n_vert <- max(vert_idx); n_horiz <- max(horiz_idx)

  block_data <- block_data |>
    bind_cols(grid_axis_ranks(vert_idx, horiz_idx, n_vert, n_horiz, start, pattern)) |>
    mutate(
      label = if (orientation == "row_vertical") {
        paste0(letters[col_idx], row_idx)
      } else {
        # sub_col (the letter) is vertical here, so its adjusted rank is
        # row_idx; sub_row (the number) is horizontal, so col_idx —
        # matching build_trial_grid()'s sub_col/sub_row assignment for the
        # same orientation.
        paste0(letters[row_idx], col_idx)
      }
    )

  # Axis breaks/labels are the actual plot_col (x) / plot_row (y) values
  # for this block — i.e. the trial's real coordinates, not a relabelled
  # 1:n within-block position.
  x_vals <- sort(unique(pull(block_data, {{ x_col }})))
  y_vals <- sort(unique(pull(block_data, {{ y_col }})))

  ggplot(block_data, aes(x = {{ x_col }}, y = {{ y_col }})) +
    geom_tile(color = "black", fill = "lightblue") +
    geom_text(aes(label = label), size = 4) +
    scale_x_continuous(breaks = x_vals) +
    scale_y_continuous(breaks = y_vals) +
    coord_equal() +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    labs(x = "Trial column", y = "Trial row", title = paste("Block", block))
}

# Example usage:
# plot_selected_block(df_grid, 1)
# plot_selected_block(df_grid, "30", pset_col = Pset.x)
