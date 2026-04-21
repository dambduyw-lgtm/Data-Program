from dataclasses import dataclass
from pathlib import Path
from typing import Optional
import re
import pandas as pd

# Import relevant helper functions:
from metadata import extract_call_date, extract_fiscal_quarter_year
from text_processing import (
    split_presentation_and_qa,
    word_count,
    count_dictionary_hits,
    sum_counts,
    CORE_PATTERNS,
    ADJ_PATTERNS,
)
from lm_sentiment import lm_tone_uncertainty

# Define structure of output row
@dataclass
class TranscriptResult:
    company: str
    ticker: Optional[str]
    date: Optional[str]
    file: str
    total_words: int
    pres_words: int
    qa_words: int
    core_hits_total: int
    core_hits_pres: int
    core_hits_qa: int
    adj_hits_total: int
    adj_hits_pres: int
    adj_hits_qa: int

    lm_positive_total: int
    lm_negative_total: int
    lm_uncertainty_count_total: int
    lm_tone_total: float
    lm_uncertainty_total: float

    lm_positive_pres: int
    lm_negative_pres: int
    lm_uncertainty_count_pres: int
    lm_tone_pres: float
    lm_uncertainty_pres: float

    lm_positive_qa: int
    lm_negative_qa: int
    lm_uncertainty_count_qa: int
    lm_tone_qa: float
    lm_uncertainty_qa: float

    fiscal_quarter: Optional[int]
    fiscal_year: Optional[int]
    fiscal_period: Optional[str]

# Sanity check to ensure that:
# -1 ≤ tone ≤ 1
# 0 ≤ uncertainty ≤ 1
def validate_lm_scores(section_name: str, scores: dict, file_name: str) -> None:
    tone = scores.get("lm_tone", 0.0)
    uncertainty = scores.get("lm_uncertainty", 0.0)

    if not (-1.0 <= tone <= 1.0):
        raise ValueError(
            f"Invalid lm_tone in {section_name} for file '{file_name}': "
            f"{tone}. "
            f"pos={scores.get('lm_positive')}, "
            f"neg={scores.get('lm_negative')}, "
            f"unc={scores.get('lm_uncertainty_count')}, "
            f"total_tokens={scores.get('lm_total_tokens')}"
        )

    if not (0.0 <= uncertainty <= 1.0):
        raise ValueError(
            f"Invalid lm_uncertainty in {section_name} for file '{file_name}': "
            f"{uncertainty}. "
            f"unc={scores.get('lm_uncertainty_count')}, "
            f"total_tokens={scores.get('lm_total_tokens')}"
        )

# Extract ticker from Refinitiv/LSEG transcript filename.
# Example:
# 2022-Apr-26-GOOGL.OQ-138254363287-Transcript.txt -> GOOGL
# 2022-Apr-26-BRK.B.N-123456-Transcript.txt -> BRK
def extract_ticker_from_filename(filename: str) -> Optional[str]:
    m = re.search(r"^\d{4}-[A-Za-z]{3}-\d{2}-([A-Z]+)", filename)
    if m:
        return m.group(1)
    return None


# Implementing one process pipeline for one single file:
def process_transcript(path: Path, company: str, lm_dict_path: str) -> TranscriptResult:
    raw = path.read_text(encoding="utf-8", errors="ignore")

    fiscal_quarter, fiscal_year, fiscal_period = extract_fiscal_quarter_year(raw)
    pres, qa = split_presentation_and_qa(raw)

    ticker = extract_ticker_from_filename(path.name)
    total_words = word_count(raw)
    pres_words = word_count(pres) if pres else 0
    qa_words = word_count(qa) if qa else 0

    core_total = sum_counts(count_dictionary_hits(raw, CORE_PATTERNS))
    core_pres = sum_counts(count_dictionary_hits(pres, CORE_PATTERNS)) if pres else 0
    core_qa = sum_counts(count_dictionary_hits(qa, CORE_PATTERNS)) if qa else 0

    adj_total = sum_counts(count_dictionary_hits(raw, ADJ_PATTERNS))
    adj_pres = sum_counts(count_dictionary_hits(pres, ADJ_PATTERNS)) if pres else 0
    adj_qa = sum_counts(count_dictionary_hits(qa, ADJ_PATTERNS)) if qa else 0

    lm_total = lm_tone_uncertainty(raw, lm_dict_path)
    lm_pres = lm_tone_uncertainty(pres, lm_dict_path) if pres else {
        "lm_positive": 0, "lm_negative": 0, "lm_uncertainty_count": 0,
        "lm_total_tokens": 0, "lm_tone": 0.0, "lm_uncertainty": 0.0
    }
    lm_qa = lm_tone_uncertainty(qa, lm_dict_path) if qa else {
        "lm_positive": 0, "lm_negative": 0, "lm_uncertainty_count": 0,
        "lm_total_tokens": 0, "lm_tone": 0.0, "lm_uncertainty": 0.0
    }

    validate_lm_scores("total", lm_total, path.name)
    validate_lm_scores("presentation", lm_pres, path.name)
    validate_lm_scores("q_and_a", lm_qa, path.name)

    call_date = extract_call_date(raw)

    return TranscriptResult(
        company=company,
        ticker=ticker,
        date=call_date,
        file=path.name,
        total_words=total_words,
        pres_words=pres_words,
        qa_words=qa_words,
        core_hits_total=core_total,
        core_hits_pres=core_pres,
        core_hits_qa=core_qa,
        adj_hits_total=adj_total,
        adj_hits_pres=adj_pres,
        adj_hits_qa=adj_qa,

        lm_positive_total=lm_total["lm_positive"],
        lm_negative_total=lm_total["lm_negative"],
        lm_uncertainty_count_total=lm_total["lm_uncertainty_count"],
        lm_tone_total=lm_total["lm_tone"],
        lm_uncertainty_total=lm_total["lm_uncertainty"],

        lm_positive_pres=lm_pres["lm_positive"],
        lm_negative_pres=lm_pres["lm_negative"],
        lm_uncertainty_count_pres=lm_pres["lm_uncertainty_count"],
        lm_tone_pres=lm_pres["lm_tone"],
        lm_uncertainty_pres=lm_pres["lm_uncertainty"],

        lm_positive_qa=lm_qa["lm_positive"],
        lm_negative_qa=lm_qa["lm_negative"],
        lm_uncertainty_count_qa=lm_qa["lm_uncertainty_count"],
        lm_tone_qa=lm_qa["lm_tone"],
        lm_uncertainty_qa=lm_qa["lm_uncertainty"],

        fiscal_quarter=fiscal_quarter,
        fiscal_year=fiscal_year,
        fiscal_period=fiscal_period,
    )

def per_1000(n, denom):
    return (n / denom * 1000.0) if denom and denom > 0 else 0.0

# Loop through all transcripts to process all
def run_corpus(input_root: str, out_csv: str, out_xlsx: str, lm_dict_path: str) -> pd.DataFrame:
    input_root = Path(input_root)
    rows = []

    for company_dir in sorted([p for p in input_root.iterdir() if p.is_dir()]):
        company = company_dir.name
        for txt in sorted(company_dir.glob("*.txt")):
            r = process_transcript(txt, company, lm_dict_path)
            rows.append(r.__dict__)

    df = pd.DataFrame(rows)

    df["core_per_1000_total"] = [per_1000(n, w) for n, w in zip(df["core_hits_total"], df["total_words"])]
    df["core_per_1000_pres"] = [per_1000(n, w) for n, w in zip(df["core_hits_pres"], df["pres_words"])]
    df["core_per_1000_qa"] = [per_1000(n, w) for n, w in zip(df["core_hits_qa"], df["qa_words"])]

    df["adj_per_1000_total"] = [per_1000(n, w) for n, w in zip(df["adj_hits_total"], df["total_words"])]
    df["adj_per_1000_pres"] = [per_1000(n, w) for n, w in zip(df["adj_hits_pres"], df["pres_words"])]
    df["adj_per_1000_qa"] = [per_1000(n, w) for n, w in zip(df["adj_hits_qa"], df["qa_words"])]

    df.to_csv(out_csv, index=False)
    df.to_excel(out_xlsx, index=False)

    return df