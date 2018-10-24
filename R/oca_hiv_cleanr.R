#' OCA HIV data cleanr
#'
#' Function to clean data from the OCA HIV Excel tool
#'
#' @param x data frame containing raw programme data from OCA TB/HIV Excel tool (sheet = HIV_ART)
#' @return data frame with variables renamed
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @export
#' @importFrom assertthat assert_that
#' @importFrom purrr map map_df
#' @importFrom stringr str_extract
#' @importFrom dplyr group_by ungroup vars arrange mutate starts_with ends_with filter mutate_at
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom utils type.convert
#' @seealso \code{\link{hivcleanr}}
#'

oca_hiv_cleanr <- function(x) {

    # checks
    assert_that(is.data.frame(x))
    assert_that(ncol(x) == 171)
    assert_that(x[3, 1] == "DB#")

    # fix globalvariable R CMD check note
    . <- NULL

# rename all columns
    # define repetitive column names
    # simple - 2 columns repeated
            rep_colnames_simple <- function(months) {
                map(.x = months, .f = function(x) {
                a <- paste0("status_", x)
                b <- paste0("tb_status_", x)
                list(a, b)}) %>%
                unlist()
            }

    # complex - 7 columns repeated
            rep_colnames_complex <- function(months) {
                colnames <- map(.x = months, .f = function(x) {
                    a <- paste0("date_", x)
                    b <- paste0("status_", x)
                    c <- paste0("cd4_", x)
                    h <- paste0("vl_", x)
                    d <- paste0("tb_status_", x)
                    e <- paste0("ks_", x)
                    f <- paste0("ka_", x)
                    g <- paste0("cm_", x)
                    list(a, b, c, h, d, e, f, g)}) %>%
                    unlist()

                if (months %in% c(6, 18, 30)) {
                    colnames <- colnames[-4] # remove HIV VL columns
                }

                colnames
            }

# generate repetitive column names - simple
    months <- c(0:5, 8, 10, seq(15, 69, by = 6))
        
    rep_col_simple <- map(months, rep_colnames_simple) %>%  unlist
        assert_that(length(rep_col_simple) == 36)
        assert_that(rep_col_simple[1] == "status_0")

# generate repititive column names - complex
    months <- seq(6, 72, by = 6)

    rep_col_complex <- map(months, rep_colnames_complex) %>% unlist()
        assert_that(length(rep_col_complex) == 93)
        assert_that(rep_col_complex[1] == "date_6")
    
# merge repetitive names and reorder by month
    full_names <- c(rep_col_simple, rep_col_complex) %>%
        data.frame(names = .,
                   stringsAsFactors = FALSE) %>%
        mutate(month = as.numeric(str_extract(names, "\\d+$"))) %>%
        group_by(.data$month) %>%
        arrange(.data$month) %>%
        ungroup()

    assert_that(nrow(full_names) == 129)
    

    new_names <- c("id",
                   "recent_fu",
                   "recent_status",
                   "clinic",
                   "art_start",
                   "art_id",
                   "pre_art_id",
                   "address",
                   "sex",
                   "age",
                   "age_group",
                   "height",
                   "weight",
                   "bmi",
                   "who_stage_start",
                   "cd4_start",
                   "cotrim_proph",
                   "inh_proph",
                   "tb_tx_date",
                   "tb_tx_id",
                   "hep_start",
                   "ks_start",
                   "ka_start",
                   "cm_start",
                   "arv_hx_start",
                   "arv_hx_regimen",
                   "arv_hx_fl_regimen",
                   "arv_sub_fl_rgm_1",
                   "arv_sub_fl_rgm_1_date",
                   "arv_sub_fl_rgm_1_reason",
                   "arv_sub_fl_rgm_2",
                   "arv_sub_fl_rgm_2_date",
                   "arv_sub_fl_rgm_2_reason",
                   "arv_sub_sl_rgm_1",
                   "arv_sub_sl_rgm_1_date",
                   "arv_sub_sl_rgm_1_reason",
                   "arv_sub_sl_rgm_2",
                   "arv_sub_sl_rgm_2_date",
                   "arv_sub_sl_rgm_2_reason",
                   full_names$names,
                   "remarks",
                   "drop",
                   "date_approx")

    # rename
    names(x) <- new_names

    # remove spare 4 rows at top
    x <- x[-c(1:4), ]

    # remove rows with no data
    x <- x %>%
        filter(!is.na(.data$clinic) & !is.na(.data$sex), !is.na(.data$age))

    # reformat variables
    x <- x %>%
        map_df(type.convert, as.is = TRUE) %>%
        mutate_at(vars(.data$art_start, .data$cotrim_proph, .data$arv_hx_start,
                       .data$date_approx, starts_with("date_"),
                       ends_with("_date")),
                  as.Date, format = "%d-%b-%y")

x
}
