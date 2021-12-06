#' @title Environment for \link[RSelenium]{remoteDriver}
dr_env <- new.env()

#' @title Get an instantiated remoteDriver or initiate a new one
#' @inheritParams RSelenium::remoteDriver
#' @param e \code{(calling environment)}
#' @return \code{(remoteDriver)}
#' @importFrom rlang "%||%"
#' @export
#'
the_rD <-
  function(browser = "chrome",
           chromever = "95.0.4638.69",
           port = 5901L,
           e = rlang::caller_env()) {
    rD <- get0("rD", envir = e$self %||% dr_env)
    if (!UU::is_legit(rD)) {
      cli::cli_alert_info("Creating remoteDriver")
      rD <- tryCatch({
        # eCaps <- list(chromeOptions = list(
        #   args = list('--disable-notifications'),
        #   chrome_binary = '"C:/Program Files/Google/Chrome Beta/Application/chrome.exe"'
        # ))
        rD <- RSelenium::rsDriver(browser = browser,
                                      port = port,
                                      # , extraCapabilities = eCaps
                                      chromever = chromever
        )

      })

    }

    if (!UU::is_legit(rD))
      rlang::abort("Could not create remoteDriver")
    if (all(c("server", "client") %in% names(rD))) {
      assign("rD", rD, envir = dr_env)
      rD <- rD$client
    }
    if (!UU::is_legit(rD$getSessions()))
      rD$open()


    invisible(rD)
  }




js.click <- function(el, el_type = c("css","xpath")[1]) {
  if (el_type == "css")
    glue::glue("$('{el}').click()")
  else
    paste0("function getElementByXPath(xpath) {
  return new XPathEvaluator()
    .createExpression(xpath)
    .evaluate(document, XPathResult.FIRST_ORDERED_NODE_TYPE)
    .singleNodeValue
};
", glue::glue("$(getElementByXPath(\"{el}\")).click()"))
}
js.val <- function(el, val) glue::glue("$('{el}').val('{val}')")

#' @title Extract a previously downloaded HUD Export archive
#'
#' @param browser_dl_folder \code{(character)} path to the browser's download folder
#' @param extract_path \code{(character)} path to the folder where the archive is to be extracted
#' @param delete_archive \code{(logical)} Delete the archive after extracting?
#' @param moment \code{(POSIXct/Date)} The time point which the archive creation time should be greater than to ensure it's recent.
#' @param wait \code{(Duration)} to wait for the file to appear in the download directory. Relevant when using browser automation.
#' @return \code{(logical)} as to whether the extraction was successful
#' @export

hud_export_extract <- function(browser_dl_folder = "~/../Downloads", extract_path = "data", delete_archive = TRUE, moment = Sys.Date(), wait = lubridate::minutes(2)) {
  downloads <- path.expand(browser_dl_folder)
  dls <- list.files(downloads, full.names = TRUE, pattern = "^hudx")
  dl_times <- do.call(c, purrr::map(dls, ~file.info(.x)$mtime))
  if (!UU::is_legit(dl_times))
    cli::cli_alert(paste0("No HUD Export found in ", path.expand(downloads), " waiting ", wait))
  wait = lubridate::now() + wait
  .recent <- dl_times > moment
  while (!any(.recent) && Sys.time() < wait) {
    Sys.sleep(5)
    dls <- list.files(downloads, full.names = TRUE, pattern = "^hudx")
    dl_times <- do.call(c, purrr::map(dls, ~file.info(.x)$mtime))
    .recent <- dl_times > moment
  }

  if (any(.recent)) {
    f <- dls[.recent]
    archive::archive_extract(f, path.expand(extract_path))
  } else
    cli::cli_alert("No HUD Export found in ", path.expand(downloads), " with creation time greater than ", moment)

  if (delete_archive)
    file.remove(dls[dl_times > moment])
}

hud_export_download <- function(rD, browser_dl_folder, extract_path, delete_archive) {
  rD$findElement("css", "#queueItems")$clickElement()
  reports <- rD$findElements("xpath", "//ul[@id='completed']/descendant::div[@class='completed_item']")
  if (UU::is_legit(reports)) {
    report_text <- reports |>
      purrr::map(~.x$getElementText()[[1]])
    has_report <- report_text |>
      purrr::map_lgl(~stringr::str_detect(.x, "HUDX-111-AD"))
    if (any(has_report)) {
      idx <- which(has_report)
      is_processed <- stringr::str_detect(report_text[[idx]], "Processed")
      if (is_processed) {
        rD$navigate(reports[[idx]]$findChildElement("css", ".open-completed-report")$getElementAttribute("href")[[1]])
        hud_export_extract(browser_dl_folder, extract_path, delete_archive)
      }
    }
  }
}

hud_export_schedule <- function(rD) {
  self$rD$executeScript(js.click("//a[@report-id='297'][@class='report-run']", "xpath"))
  self$rD$executeScript(js.click("#schedule_297"))
  Sys.sleep(5)
  # Set all CoCs
  purrr::walk(list("cocId", "agencyStatus", "agencyId", "programType", "programStatus", "programId"), ~rD$findElement("xpath", glue::glue("//select[@id='{.x}']/descendant::option[contains(text(), 'All')]"))$clickElement())

  rD$findElement("css", "#datestart")$setElementAttribute("value", "2019/01/01")
  rD$executeScript(js.val("#datestart_mdy", "2019/01/01"))
  rD$findElement("css", "#dateend")$setElementAttribute("value", format(Sys.Date() + 1, "%Y/%m/%d"))

  rD$executeScript(js.val("#dateend_mdy", format(Sys.Date() + 1, format = '%m/%d/%Y')))

  rD$findElement("xpath", "//input[@id='flagDeleted_1']/following-sibling::span[@class='mdl-radio__outer-circle']")$clickElement()


  rD$findElement("xpath", "//*[@id='form_clarity_input_id']/div[2]/input")$clickElement()

}
#' @title Create \link[RSelenium]{remoteDriver} and login to Clarity
#' @description An R6 Class to manage the Clarity automation
#' @inheritParams setup_clarity
#' @param clarity_url \code{(character)} the full URL to the Clarity instance to log in to
clarity <- R6::R6Class("Clarity",
                       lock_objects = FALSE,
                       public = list(
                         initialize = function(ini_path = file.path("inst","auth","Clarity.ini"), clarity_url = "https://cohhio.clarityhs.com/", ...) {
                           eval(parse(ini_path))
                           self$rD <- the_rD(...)
                           self$rD$navigate(clarity_url)
                           self$clarity_url <- httr::parse_url(clarity_url)
                           if (self$rD$getCurrentUrl()[[1]] == "https://cohhio.clarityhs.com/login") {
                             purrr::walk(c("username", "password"), ~self$rD$findElement("css", glue::glue("#{.x}"))$sendKeysToElement(list(get0(.x))))
                             self$rD$findElement("css", ".btn-submit")$clickElement()
                           }


                         },
                         debug_int = function() browser(),
                         get_hud_export = function(end_date = Sys.Date(), schedule = TRUE, browser_dl_folder = "C:\\\\Users\\\\Administrator\\\\Downloads", extract_path = "~/R/Contributor_Repos/COHHIO/Rm_data/data", delete_archive = TRUE) {
                           # Check for existing
                           hud_export_download(
                             self$rD,
                             browser_dl_folder = browser_dl_folder,
                             extract_path = extract_path,
                             delete_archive = delete_archive
                           )
                           self$rD$navigate(private$url_modify(path = "report"))
                           Sys.sleep(2)
                           # Schedule
                           if (schedule) {
                             hud_export_schedule(self$rd)
                           }
                         }
                       ),
                       private = list(url_modify = function(...) {
                         httr::build_url(purrr::list_modify(self$clarity_url, ...))
                       }))
#"C:/Program Files/Google/Chrome Beta/Application/chrome.exe"
