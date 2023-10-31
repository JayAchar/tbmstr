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

      if (nrow(m_df) != sum(has_changed)) {
        cli::cli_alert_warning(
          "{var}: Only {nrow(m_df)}/{sum(has_changed)}",
          " adjustments have been applied")
      }

      m_df$row_n <- NULL
      return(m_df)
    }
  )

  names(changed_dfs) <- check_vars

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
        ),
        list(
          id = "3b18e790-fc73-4289-a5cc-9034b7902aaa",
          var = "evldat12",
          value = as.POSIXct("2022-10-29", tz = "UTC")
        ),
        list(
          id = "3b18e790-fc73-4289-a5cc-9034b7902aaa",
          var = "fuendat",
          value = as.POSIXct("2022-10-30", tz = "UTC")
        ),
           # changed from withdrawn to failed
           # failed to complete treatment on time
        list(
          id = "772b9359-aa0e-4fd2-ba34-492cca8565ef",
          var = "outcome",
          value = 3
        ),
           # changed from withdrawn to cured
           # originally marked as failed to complete treatment in time,
           # howwever, 253 doses was sufficient
           # No AEs, multiple negative cultures, acceptable
           # baseline DST
        list(
          id = "1626fdda-f2d9-427b-813c-21e9e06492f1",
          var = "outcome",
          value = 1
        ),
           # missing outcome
           # prtclviol comment states Determined ineligible after starting
        list(
          id = "02d5267e-48e7-4955-8bfe-8bd38a4d91c8",
          var = "outcome",
          value = 7
        ),
          # Lost contact with pt after 5m and 173 doses
           # originally classified as Not evaluated
        list(
          id = "d830f560-a4ed-4717-8dad-4b1598f6ed1d",
          var = "outcome",
          value = 5
        ),
          # from withdrawn to failed
           # ceased 2 drugs due to side effects
        list(
          id = "fd5fb3c6-3e02-47c2-b37e-6d88072cd1ae",
          var = "outcome",
          value = 3
        ),
         # culture negative at baseline
           # change reason for withdrawal
        list(
          id = "5e2c4e3d-cbae-4a15-a596-84fee16ae8ed",
          var = "prtclviol",
          value = 4
        ),
           # missing reason for withdrawal written as treatment comment 
           # by study team - added here
        list(
          id = "b1150192-333c-4998-b78e-4da02dc4b31b",
          var = "prtclviol",
          value = 3
        ),
           # EOT outcome missing - EOS outcome = No TB
           # FU time correct, cultures all negative
        list(
          id = "d6bafb32-9df9-4f1c-9b36-6118e4593b7f",
          var = "outcome",
          value = 1
        ),
           # total dosees was too short so outcome chnaged to fail
           # treatment comment also adjusted
        list(
          id = "2ae2d669-4884-4300-80c4-e1380b37e5b3",
          var = "outcome",
          value = 3
        ),
        list(
          id = "2ae2d669-4884-4300-80c4-e1380b37e5b3",
          var = "trtcom",
          value = "Treatment prevented by war" 
        ),
           # total dosees was too short so outcome chnaged to fail
           # treatment comment also adjusted
        list(
          id = "a67ea512-fb5e-45b5-8e75-2bdf695b90ff",
          var = "outcome",
          value = 3
        ),
        list(
          id = "a67ea512-fb5e-45b5-8e75-2bdf695b90ff",
          var = "trtcom",
          value = "Treatment prevented by war" 
        ),
           # insufficient doses in the permitted time 
           # outcome changed to failure
        list(
          id = "207edd1a-47c9-474c-8bbe-dcf0e71d41b0",
          var = "outcome",
          value = 3
        ),
        list(
          id = "207edd1a-47c9-474c-8bbe-dcf0e71d41b0",
          var = "trtcom",
          value = "Insufficient treatment doses within allowed time" 
        ),
           # insufficient doses in the permitted time 
           # outcome changed to failure
        list(
          id = "e5dcccdd-b057-4911-9ad2-81a8455e7b4a",
          var = "outcome",
          value = 3
        ),
        list(
          id = "e5dcccdd-b057-4911-9ad2-81a8455e7b4a",
          var = "trtcom",
          value = "Insufficient treatment doses within allowed time" 
        )
      )
    )
  ))
}
