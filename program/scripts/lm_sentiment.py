from pathlib import Path
from collections import Counter
import re
from functools import lru_cache
from typing import Dict, Set, Tuple

import pandas as pd

WORD_RE = re.compile(r"\b[a-zA-Z]{2,}\b")


@lru_cache(maxsize=1)
def load_lm_word_sets(dict_path: str) -> Tuple[Set[str], Set[str], Set[str]]:
    lm = pd.read_csv(dict_path)

    positive = set(lm.loc[lm["Positive"] > 0, "Word"].str.lower())
    negative = set(lm.loc[lm["Negative"] > 0, "Word"].str.lower())
    uncertainty = set(lm.loc[lm["Uncertainty"] > 0, "Word"].str.lower())

    return positive, negative, uncertainty


def tokenize_alpha(text: str):
    if not isinstance(text, str):
        return []
    return WORD_RE.findall(text.lower())


def lm_tone_uncertainty(text: str, dict_path: str) -> Dict[str, float]:
    """
    Returns LM-based tone and uncertainty.

    Tone here follows your A1 notebook logic:
    (positive - negative) / (positive + negative)

    Uncertainty:
    uncertain_words / total_words
    """
    if not isinstance(text, str) or not text.strip():
        return {
            "lm_positive": 0,
            "lm_negative": 0,
            "lm_uncertainty_count": 0,
            "lm_total_tokens": 0,
            "lm_tone": 0.0,
            "lm_uncertainty": 0.0,
        }

    positive, negative, uncertainty = load_lm_word_sets(dict_path)

    words = tokenize_alpha(text)
    total = len(words)

    if total == 0:
        return {
            "lm_positive": 0,
            "lm_negative": 0,
            "lm_uncertainty_count": 0,
            "lm_total_tokens": 0,
            "lm_tone": 0.0,
            "lm_uncertainty": 0.0,
        }

    counts = Counter(words)

    pos = sum(counts[w] for w in counts if w in positive)
    neg = sum(counts[w] for w in counts if w in negative)
    unc = sum(counts[w] for w in counts if w in uncertainty)

    tone = (pos - neg) / (pos + neg) if (pos + neg) > 0 else 0.0
    unc_score = unc / total

    return {
        "lm_positive": pos,
        "lm_negative": neg,
        "lm_uncertainty_count": unc,
        "lm_total_tokens": total,
        "lm_tone": tone,
        "lm_uncertainty": unc_score,
    }