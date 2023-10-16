import_adjustments <- function(path) {
  filename <- "outcome-adjustments.xlsx"

  # # import outcome adjustments
  sheet_names <- c("original", "adjusted")
  sheets <- lapply(
    X = sheet_names,
    FUN = \(sheet) {
      rd <- readxl::read_xlsx(
        file.path(path, filename),
        sheet = sheet
      )
      # recoding of drnum variable means it cannot be used
      rd$drnum <- NULL
      # sort by row_n and check that they are identical
      return(rd[order(rd$row_n), ])
    }
  )
  names(sheets) <- sheet_names

  if (!all(sheets$original$row_n == sheets$adjusted$row_n)) {
    cli::cli_alert_danger("Ordering of modifications has failed.")
  }

  # check each variable for changes
  check_vars <- c("outcome", "trtstdat", "trtendat")
  # for variables with changes:
  # check if variable has changed between two
  changed_dfs <- lapply(
    X = check_vars,
    FUN = \(var) {
      has_changed <- !sheets$original[[var]] == sheets$adjusted[[var]]

      if (!any(has_changed)) {
        return(NULL)
      }

      # identify rows which have been adjusted
      a_df <- sheets$adjusted[
        which(has_changed),
        c("row_n", var)
      ]

      # create data frame with globalrecordid, variable name & updated value
      m_df <- merge(
        a_df,
        sheets$original[, c("row_n", "globalrecordid")],
        by = "row_n",
        all.x = TRUE
      )

      # assign codes instead of outcome strings
      if (var == "outcome") {
        lut_outcomes <- tbmstr:::internal$lut[
          which(tbmstr:::internal$lut$name == "outcome"),
        ]

        outcome_codes <- lut_outcomes$value[match(
          m_df$outcome,
          lut_outcomes$description
        )]

        m_df$outcome <- outcome_codes
        m_df <- m_df[which(!is.na(m_df$outcome)), ]
      }

      m_df$row_n <- NULL
      return(m_df)
    }
  )

  names(changed_dfs) <- check_vars

  # remove rows where outomes have not changed
  # mod_adj <- mod_id[which(mod_id$outcome != mod_id$revised), ]
  #
  #
  # final_mods <- mod_adj[which(mod_adj$revised %in% valid_outcomes), ]


  mv_adjust <- lapply(
    check_vars,
    FUN = \(var_name) {
      lapply(
        split(
          changed_dfs[[var_name]],
          changed_dfs[[var_name]]$globalrecordid
        ),
        \(row) {
          list(
            id = row$globalrecordid,
            var = var_name,
            value = row[[var_name]]
          )
        }
      )
    }
  )

  change_lst <- unname(unlist(
    mv_adjust,
    recursive = FALSE
  ))

  list(list(
    name = "baseline",
    id = "globalrecordid",
    adjustments = c(
      unname(change_lst),
      list(
        list(
          id = "e5bb946c-d336-4ec8-a7ee-34c38c851a83",
          var = "endat",
          value = as.POSIXct("2022-06-09", tz = "UTC")
        ),
        list(
          id = "60acaeed-ed4a-4ffa-bcf3-9386fc54081e",
          var = "endat",
          value = as.POSIXct("2022-04-06", tz = "UTC")
        ),
        list(
          id = "a9154701-97ec-4af3-a848-1b2e757670a1",
          var = "endat",
          value = as.POSIXct("2021-10-31", tz = "UTC")
        ),
        list(
          id = "ba8c16ee-31da-44b8-87ff-9afbf1662381",
          var = "endat",
          value = as.POSIXct("2021-09-04", tz = "UTC")
        ),
        list(
          id = "493077b6-a5eb-4945-9f1c-90ebbd47e129",
          var = "endat",
          value = as.POSIXct("2022-04-15", tz = "UTC")
        ),
        list(
          id = "cbe03f3e-f483-42fd-af64-8a714bbb585b",
          var = "endat",
          value = as.POSIXct("2022-04-06", tz = "UTC")
        ),
        list(
          id = "7a00eda8-4f7b-427a-8259-a4c12689d8d6",
          var = "endat",
          value = as.POSIXct("2021-11-12", tz = "UTC")
        ),
        list(
          id = "f75a3333-3c51-4182-b001-37af3e462115",
          var = "endat",
          value = as.POSIXct("2022-02-20", tz = "UTC")
        ),
        list(
          id = "7cf1d06e-04f9-49c6-b30b-ec67a0cd44c4",
          var = "endat",
          value = as.POSIXct("2021-10-11", tz = "UTC")
        ),
        list(
          id = "bab078f0-1d3d-47cd-a61e-997b25865fd1",
          var = "endat",
          value = as.POSIXct("2021-04-20", tz = "UTC")
        ),
        list(
          id = "b80f75da-d93b-4013-b008-1ee7a7243a84",
          var = "endat",
          value = as.POSIXct("2022-03-10", tz = "UTC")
        ),
        list(
          id = "5ba38fdd-9ce4-4510-a70e-098580256436",
          var = "endat",
          value = as.POSIXct("2022-02-25", tz = "UTC")
        ),
        list(
          id = "b1150192-333c-4998-b78e-4da02dc4b31b",
          var = "endat",
          value = as.POSIXct("2021-12-16", tz = "UTC")
        ),
        list(
          id = "b1150192-333c-4998-b78e-4da02dc4b31b",
          var = "trtendat",
          value = as.POSIXct("2021-12-16", tz = "UTC")
        ),
        list(
          id = "a18c6003-5c0c-4b36-bb29-4d9c65347fad",
          var = "trtendat",
          value = as.POSIXct("2021-12-02", tz = "UTC")
        ),
        list(
          id = "a18c6003-5c0c-4b36-bb29-4d9c65347fad",
          var = "endat",
          value = as.POSIXct("2021-12-02", tz = "UTC")
        ),
        list(
          id = "40ec3c72-e594-481e-acf6-5b4ca4faa41c",
          var = "endat",
          value = as.POSIXct("2021-07-05", tz = "UTC")
        ),
        list(
          id = "ce7bd3e3-561f-4942-97bc-ed1c74bbd563",
          var = "endat",
          value = as.POSIXct("2021-11-10", tz = "UTC")
        )
      )
    )
  ))
}
