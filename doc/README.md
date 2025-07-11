# Documentation

## Dataset description

The table [`raw_data_description.csv`](./raw_data_description.csv) contains the column number and the the column name of the main dataset `dat/raw/all_data.csv`. It also includes a short description for all variables, as well as a unit or data type (if applicable).

The following factor levels are used for the corresponding ordinal and nominal features:

- `geol_class`:
    - Anthropogenic debris (AnD)
    - Cover layer on carbonate sediment (CLC)
    - Cover layer on metamorphic bedrock (CLM)
    - Glacial sediment (GS)
    - Glacial sediment, possibly solidified (GSps)
    - Glacial sediment, potentially very loose (GSpl)
    - Talus/Scree (TS)
  - `soil_class`:
    - Albic-Podzol Entic-Podzol (APE)
    - Cambisol Cambic-Phaeozem (CCP)
    - Cambion Cambic-Phaeozem Cambic-Umbrisol (CCU)
    - Disturbed Soil (DS)
    - Planosol-Stagnosol (PS)
    - Regosol Phaeozem Umbrisol Folic-Histosol (RPUF)
- `pasture`:
    1. No
    2. Low
    3. Medium
    4. Intensive 
- `topography`:
    - escarpment
    - lower-slope
    - mid-slope
    - plateau
    - ridge
    - slope-flattening
    - trench
    - upper-slope
- `vegetation_class`:
    - forest
    - grassland
    - shrubs
- `embedded_rock_type`:
    1. loose (LOC)
    2. intermediate (EHO)
    3. cohesive (EGE)
- `soiltexture`:
    - loamy sand (lS)
    - loamy silt (lU)
    - sand (S)
    - sandy loam (sL)
    - sandy silt (sU)
    - silty loam (uL)
    - silty sand (uS)

## Code

The main analysis scripts can be found in `dev` directory, with additional helper functions in `dev/helper` and plotting scripts in `dev/plots`.
