load_ui_modules <- function() {
  ui_files <- c(
    'tab_panels/geographic_catches_ui.R',
    'tab_panels/main_panel_ui.R',
    'tab_panels/geographic_catches_by_variable_ui.R',
    'tab_panels/ggplot_indicator_11_ui.R',
    'tab_panels/additional_info_ui.R',
    'tab_panels/filterUI.R',
    'tab_panels/data_explorer_combined_ui.R',
    'tab_panels/total_catch_plot.R',
    'tab_panels/sidebar_ui.R',
    'tab_panels/mapCatchesmodules.R',
    'tab_panels/create_logo_panel.R',
    'tab_panels/dataset_choice.R',
    'tab_panels/sqlqueriesui.R',
    'tab_panels/data_explorer_i11_ui.R',
    'tab_panels/more_about.R',
    'rmd/rendering_rmd_files_to_html.R',
    'modules/generateRmdNavMenu.R',
    'modules/TimeSeriesbyDimension.R',
    'modules/categoryGlobalPieChart.R',
    'modules/pieMapTimeSeriesUI.R',
    'modules/plotTotalCatches.R',
    "modules/dataset_choice.R"
  )
  lapply(ui_files, function(file) {
    source(here::here(file))
    flog.info(paste("Loaded UI module:", file))
  })
}
