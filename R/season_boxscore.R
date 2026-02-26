################################  Get Season Box Score #########################
#' Get Season Box Score
#'
#' Gets team schedule for requested season.
#'
#' @param team Team to get schedule for
#' @param season Season to get schedule for. In form "2020-21". Default equals current season.
#' @param aggregate One of 'average' (per-game average statistics), 'total' (sums of season stats) or 'raw'
#' (just return all box scores binded together). 'average' is the default.
#' @return A data frame of the team's schedule for requested season
#' @export
season_boxscore <- function(team, season = current_season, aggregate = "average") {
  ### Error Testing
  if (is.na(team)) stop("team is missing with no default")
  if (!aggregate %in% c("average", "total", "raw")) {
    stop("aggregate argument must be one of c('average', 'total', 'raw')")
  }

  if (!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  } else {
    if (exists("ids", inherits = TRUE)) ids <- get("ids", inherits = TRUE) else ids <- create_ids_df()
  }
  if (!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }

  ### Team Schedule
  schedule <- get_schedule(team, season)
  schedule <- dplyr::filter(schedule, date <= Sys.Date() & !is.na(game_id))

  game_ids <- schedule$game_id
  if (length(game_ids) == 0) return(dplyr::tibble())

  ### Team PBP Name
  box_team <- if (team == "UConn") {
    team
  } else {
    tmp <- dict$ESPN_PBP[dict$ESPN == team]
    if (length(tmp) == 0 || is.na(tmp[1])) team else tmp[1]
  }
  season_box <- NULL
  for(i in 1:length(game_ids)) {
    message(paste0("Pulling Box Score Data for Game: ", i, " of ", length(game_ids)))
    box <- get_boxscore(game_id = game_ids[i])
    box_team_use <- if (box_team %in% names(box)) {
      box_team
    } else if (team %in% names(box)) {
      team
    } else {
      names(box)[1]
    }
    bx <- box[[box_team_use]]
    bx$game_id <- game_ids[i]
    season_box <- dplyr::bind_rows(season_box, bx)
  }

  season_box <- season_box %>%
    dplyr::filter(player != "TEAM") %>%
    dplyr::mutate(player_id = as.character(player_id))

  # --- Join roster -> position (by player_id) ---
  roster <- get_roster(team, season)
  if (!is.null(roster) && nrow(roster) > 0 && "player_id" %in% names(roster)) {
    roster_pos <- roster %>%
      dplyr::transmute(
        player_id = as.character(player_id),
        position_roster = as.character(position)
      ) %>%
      dplyr::distinct(player_id, .keep_all = TRUE)
    season_box <- season_box %>%
      dplyr::left_join(roster_pos, by = "player_id")
    # If season_box already includes position, keep it; otherwise, use the one from the roster.
    if ("position" %in% names(season_box)) {
      season_box <- season_box %>%
        dplyr::mutate(position = dplyr::coalesce(as.character(position), position_roster, "UNK"))
    } else {
      season_box <- season_box %>%
        dplyr::mutate(position = dplyr::coalesce(position_roster, "UNK"))
    }
    season_box <- season_box %>% dplyr::select(-position_roster)
  } else {
    if (!"position" %in% names(season_box)) season_box$position <- "UNK"
  }

  if (aggregate == "total") {
    season_box <- season_box %>%
      dplyr::mutate(
        GP = 1L,
        GS = as.integer(as.logical(starter))
      ) %>%
      dplyr::group_by(player_id, player, position) %>%
      dplyr::summarise(
        dplyr::across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (aggregate == "average") {
    season_box <- season_box %>%
      dplyr::group_by(player_id, player, position) %>%
      dplyr::mutate(
        GP = dplyr::n(),
        GS = sum(as.logical(starter), na.rm = TRUE)
      ) %>%
      dplyr::summarise(
        dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # if (aggregate == "raw") return(season_box)

  return(season_box)
}
