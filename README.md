# AI-Washing in Earnings Calls

This repository contains the code and data pipeline used for my Master's thesis analyzing AI disclosure in earnings calls transcripts and its relationship with stock market reactions.

## Project Structure

```
data/
├── raw/
│   └── (intial raw datasets)
│
├── processed/
│   └── (intermediate datasets, product of the textual analysis phase)
│
output/
│   └── (generated datasets for regression & graphs)
│
program/
├── notebooks/
│   └── (Jupyter notebooks for exploration and pipeline execution)
│
└── scripts/
    └── (Python scripts mainly for text processing, AI dictionary construction, and analysis)
```

### Folder descriptions

**data/raw/**  
Original datasets collected from WRDS and LSEG, including S&P100 constituents, financial dictionaries, and raw earnings call transcripts.

**data/processed/**  
Product of the textual analysis phase, stored here to later combine textual information with companies' financials.

**program/notebooks/**  
Jupyter notebooks used to run the pipeline and inspect intermediate outputs.

**program/scripts/**  
Reusable Python scripts for text processing, AI dictionary construction, transcript parsing, and dataset generation.

**output/**  
Generated datasets for regression & graphs


## Methodology & Workflow
`(program/scripts/ai_dictionary.py)`
1. Define the AI dictionaries

`(program/scripts/text_processing.py)`
2. Define textual processing rules. Count each dictionary hits over each section of or whole transcripts.

`(program/scripts/metadata.py)`
3. Define structured metadata extraction from the transcript header text. (call date, fiscal quarters, and year).

`(program/scripts/lm_sentiment.py)`
4. Define LM sentiment/uncertainty scoring. Import `(data/raw/Loughran-McDonald_MasterDictionary_1993-2024.csv)` to compute LM metrics.

`(program/scripts/pipeline.py)`
5. Combine all of above functions into one finalized pipeline. Define results dataclass and an engine to run all the previous steps.

`(program/scripts/run_ai_counts.py)`
6. Define the execution script to run the pipeline across all company folders.

`(program/notebooks/textual_analysis_2.0.ipynb)`
8. Use the notebook to trigger execution and inspect outputs.

