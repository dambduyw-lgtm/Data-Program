from pathlib import Path
from pipeline import run_corpus

# Set paths:
BASE_DIR = Path(__file__).resolve().parents[2]

INPUT_ROOT = BASE_DIR / "data" / "raw" / "transcript"
OUT_CSV = BASE_DIR / "data" / "processed" / "textual_analysis_results.csv"
OUT_XLSX = BASE_DIR / "data" / "processed" / "textual_analysis_results.xlsx"
LM_DICT = BASE_DIR / "data" / "raw" / "Loughran-McDonald_MasterDictionary_1993-2024.csv"

# Set a single caller script
if __name__ == "__main__":
    df = run_corpus(
        input_root=str(INPUT_ROOT),
        out_csv=str(OUT_CSV),
        out_xlsx=str(OUT_XLSX),
        lm_dict_path=str(LM_DICT),
    )
    print(df.head())
    print(f"\nDone. Saved {len(df)} rows.")