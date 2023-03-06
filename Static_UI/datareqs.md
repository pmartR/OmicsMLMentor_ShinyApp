### <span style = 'color:navy'><strong>SLOPE – General Requirements</strong></span>

***

SLOPE will integrate between 2-6 datasets arising from a single project, e.g. where different ‘omics methods were used to analyze the same samples. The datasets may each be one of the following types: 
* **Metabolomics** (0, 1, or 2 datasets)
    * NMR
        * By identified metabolites identified in the spectra
        * By bins of spectra
    * GC-MS
* **Lipidomics** (0, 1, or 2 datasets)
    * Single ionization
    * Both negative and positive ionizations
* **Proteomics** (0, 1, or 2 datasets)
    *	Peptide data or protein data (not both)
        * Label-free
        * Isobaric labeled

***

For each dataset type, .csv files containing the following information must be uploaded:
* e_data is a cross-tab with rows for each biomolecule and columns for each sample plus another column that specifies the unique identifier for each biomolecule row. A header row contains the sample names and biomolecule type name (e.g. Metabolite, Lipid, Peptide).
* e_meta (only required for peptide data) is a cross-tab with one column containing the unique biomolecule identifiers, and additional columns containing mappings to other identifiers or metainformation. For proteomics data, this cross-tab contains the peptide-to-protein mapping. 

For the project as a whole, a single .csv file containing sample-level information must be uploaded:
* f_meta is a cross-tab with rows for each sample and columns containing sample names for each dataset and additional information about the project such as experimental groups or conditions. 
