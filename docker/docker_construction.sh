docker build \
  -f Dockerfile.imagewithdata \
  --build-arg APP_IMAGE=ghcr.io/firms-gta/tunaatlas_pie_map_shiny:latest \
  --build-arg DATA_IMAGE=ghcr.io/firms-gta/gta-data:latest \
  --build-arg DATASET_FILE=global_catch_ird_level1_1950_2024.qs \
  -t tunaatlas:level1 \
  .