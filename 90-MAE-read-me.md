

Some notes about key `mae@colData` columns:



- `sample_type`: a `factor` specifying the sample type. 

  - `Clinical sample`

  - `Positive control`

  - `Negative control`
  - (`Standard`) (only for assay-specific datasets)
  - `Control` (when unknown - but that should be fixed).
  - Note that positive and negative controls were only included for DNA-based assays (metagenomics, 16S rRNA amplicon sequencing, and qPCR assays) since they were shared across these assays, and there is value in comparing them. Controls for Luminex and Flow were assay-specific and were consequently not included in the MAE.

- `control_type`
  - `Mock1` 
  - `Mock2`
  - `Blank`
  - ==Check if other values are possible==
  - Clinical samples have no `control_type` (`NA`) 
  
- `pid`: the participant ID 
  - Starts with the study code (`068`)
  - Followed by the site code: `10` for the MGH (US) site, and `20` for the CAPRISA (SA) site.
  - Ends with a 4-digit number to discriminate between participants within sites
  - ➞ All participant IDs should be 3+2+4 = 9-digit long, stored as `character`.
  - Positive and negative controls have no `pid` (`NA`)
- `visit_code`: the visit code for the sample/entry
  - Should be a 4-character long `character`
  - Positive and negative controls have no `visit_code`
  - Can take the following values
    - ==include dictionary==
- `uid` "participant x visit" unique identifier. This is the smallest common denominator for all assays (*i.e.*, what is used to link assays together) 
  - Defined from of the `pid` and the `visit_code`
  - Specifically, it has the format "`pid`_`visit_code`".
  - Positive and negative controls have a `uid` defined from the `sample_type` (see below) and a cross-assay identifier (*e.g.*, `ext_lib_plate_nb` for DNA-based data such as metagenomics, qPCR, and 16S amplicon sequencing)

