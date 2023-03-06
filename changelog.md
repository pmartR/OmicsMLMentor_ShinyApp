## Changelog

#### 2021

- August
  - Added SPLS-DA to statistics tab (3aba3e24)
  - Added basic integration tab with SPLS, PALLID, and Reactome plots. (3aba3e24)

#### 2022

- March
  - Ingest of pmart projects and midpoints
  
- May
  - Correlation-based integration methods
  
- July
  - Update to pmartR >= 2.0.0.  Handling for paired data.
  
- November
  - Options for dimensions, scale, format when downloading plots.

## Container Versions:

#### Top/Code
- 0.1.2 (0.1.2 base)
  - Fixes to minio ingest.
  
- 0.2.0, 0.2.1 
  - UI updates for pmartR 2.0.8
  
#### Base
- 0.1.2
  - rlang 0.4.1 -> 1.0.2
  - shinyalert 2.0.0 -> 3.0.0
  
- 0.2.0, 0.2.1
  - pmartR to 2.0.8, corresponding updates to mapDataAccess
  - Restructure to install active development packages outside renv.
  
- 0.2.3
  - Remove Orca in favor of Kaleido 




