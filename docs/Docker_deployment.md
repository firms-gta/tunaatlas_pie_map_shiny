# Tuna Atlas Shiny Docker deployment

## Design goals

The deployment keeps application dependencies stable while allowing a chosen
dataset to be changed without maintaining one Dockerfile per dataset.

```text
docker/Dockerfile                 -> tunaatlas-app
docker/Dockerfile.data            -> tunaatlas-data (all DOI datasets)
tunaatlas-app + selected QS file  -> deployment image
```

`docker/Dockerfile.imagewithdata` is the only parameterised assembly stage.
`docker/Dockerfile.data` always contains all datasets listed in `DOI.csv`.

## Dataset naming contract

For a `DOI.csv` row such as:

```csv
DOI,Filename
10.5281/zenodo.20834708,global_catch_ird_level2_1950_2024_harmonized.csv
```

the data image stores:

```text
/data/global_catch_ird_level2_1950_2024_harmonized_20834708.qs
```

The deployment build receives that QS name as `DATASET_FILE`, but its generated
`DOI.csv` retains the original Zenodo filename. This distinction is required by
the application loader.

## Build arguments

### Application image

| Argument | Default | Purpose |
|---|---|---|
| `BASE_IMAGE` | `rocker/r-ver:4.2.3` | R runtime for production |
| `MODE` | `prod` | `prod` for Shiny or `dev` for RStudio |
| `BRANCH` | `main` | Traceability label exposed as `BUILD_BRANCH` |

### Deployment image

| Argument | Required | Purpose |
|---|---|---|
| `APP_IMAGE` | No | Application base image |
| `DATA_IMAGE` | No | Image containing all DOI datasets |
| `DATASET_FILE` | Yes | Exact QS filename copied from the data image |
| `DOI_CSV` | Yes | Header plus the row describing the selected source dataset |

## Local workflow

```bash
docker build -f docker/Dockerfile -t tunaatlas-app:local .
docker build -f docker/Dockerfile.data -t tunaatlas-data:local .

docker build \
  -f docker/Dockerfile.imagewithdata \
  --build-arg APP_IMAGE=tunaatlas-app:local \
  --build-arg DATA_IMAGE=tunaatlas-data:local \
  --build-arg DATASET_FILE=global_catch_ird_level2_1950_2024_harmonized_20834708.qs \
  --build-arg $'DOI_CSV=DOI,Filename\n10.5281/zenodo.20834708,global_catch_ird_level2_1950_2024_harmonized.csv\n' \
  -t tunaatlas:level2-2024 \
  .

docker run --rm --name tunaatlas-level2 \
  -p 3838:3838 \
  tunaatlas:level2-2024
```

Smoke-test the HTTP endpoint:

```bash
curl --fail --retry 20 --retry-delay 3 http://127.0.0.1:3838/
```

## Runtime volume alternative

The base application image can receive data at runtime instead of producing a
new deployment image:

```bash
docker run --rm -p 3838:3838 \
  -v /absolute/path/to/app-data:/root/tunaatlas_pie_map_shiny/data:ro \
  -v /absolute/path/to/DOI.csv:/root/tunaatlas_pie_map_shiny/DOI.csv:ro \
  ghcr.io/firms-gta/tunaatlas-app:main
```

The mounted directory must contain the QS name expected from the DOI and record
ID. Mounting data read-only is suitable only when the precomputed application
cache is present or the application does not need to write `data/data.qs`.
Otherwise, provide a writable cache volume separately or use the assembled
deployment image.

## Server deployment

Use an immutable image tag or digest and a restart policy:

```bash
docker run -d \
  --name tunaatlas \
  --restart unless-stopped \
  -p 3838:3838 \
  ghcr.io/YOUR_NAMESPACE/tunaatlas:IMMUTABLE_TAG
```

Place a reverse proxy and TLS termination in front of port 3838 when the app is
exposed beyond a trusted network. Do not publish RStudio port 8787 from a
production image.

## Shared infrastructure and SSP Cloud

For SSP Cloud or another Kubernetes platform, deploy the assembled application
image as a `Deployment` with one container exposing port 3838. Use:

- a `Service` targeting container port 3838;
- an `Ingress` only if provided by the platform;
- a Secret for database variables;
- resource requests and limits based on measured startup and filtering peaks;
- a readiness probe on `/`;
- an immutable image tag.

The existing files under `deploy/` are a starting point and must be reviewed for
the target namespace, registry, ingress class, TLS, and secret names before
submission.

## CI/CD behaviour

The active workflow is `.github/workflows/build-base-images.yml`.

| Change | Application image | Data image | Development image |
|---|---:|---:|---:|
| R/app source or `renv.lock` | Built | Not necessarily | Built |
| `DOI.csv` or data Dockerfile | Built on push | Built | Not necessarily |
| Manual dispatch | Built | Built | Built |

Each published image receives a branch tag and a source commit SHA tag. The SHA
tag is the preferred deployment reference.

Older workflows are kept under `.github/deprecated/` for history and must not be
reactivated without reviewing image names, secrets, and Dockerfile paths.

## Validation checklist

For each release:

1. build `tunaatlas-app` from a clean checkout;
2. build `tunaatlas-data` after verifying every DOI row;
3. assemble one deployment image for each intended dataset;
4. confirm `global.R` succeeds during the final build;
5. launch the container and receive HTTP 200 from `/`;
6. confirm the Dataset selection panel shows only the supplied DOI row;
7. apply a representative year, species, and geography filter;
8. inspect logs for download, cache, geometry, or database errors;
9. record image digests and the source commit;
10. verify the server restart policy or Kubernetes readiness probe.

## Maintenance recommendations

- Keep R 4.2.3 only while it is required by the locked application; test runtime
  upgrades on a dedicated branch.
- Update `renv.lock` and application code in the same reviewed change when a
  package API changes.
- Keep the small fallback dataset stable and versioned so the base image always
  has a demonstrable offline mode.
- Rebuild the data image only when `DOI.csv` or its conversion logic changes.
- Retain commit-SHA image tags even if readable branch tags move.
- Remove or archive obsolete Dockerfiles after confirming that no active workflow
  references them.
