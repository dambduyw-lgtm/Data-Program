import re
from typing import Dict, List, Tuple

from ai_dictionary import CORE_AI_TERMS, AI_ADJACENT_TERMS

PRESENTATION_HEADER = r"^Presentation\s*$"
QA_HEADER = r"^(Questions\s+and\s+Answers|Q-and-A)\s*$"
SECTION_DIVIDER_LINE = r"^-{20,}\s*$"

WORD_RE = re.compile(r"[A-Za-z0-9]+(?:'[A-Za-z0-9]+)?")

def split_presentation_and_qa(text: str) -> Tuple[str, str]:
    lines = text.splitlines()

    pres_idx = None
    qa_idx = None

    for i, line in enumerate(lines):
        if pres_idx is None and re.match(PRESENTATION_HEADER, line.strip(), flags=re.IGNORECASE):
            pres_idx = i
        if qa_idx is None and re.match(QA_HEADER, line.strip(), flags=re.IGNORECASE):
            qa_idx = i

    if pres_idx is None and qa_idx is None:
        return "", ""

    if pres_idx is not None:
        pres_start = pres_idx + 1
        while pres_start < len(lines) and (
            re.match(SECTION_DIVIDER_LINE, lines[pres_start].strip())
            or lines[pres_start].strip() == ""
        ):
            pres_start += 1
    else:
        pres_start = None

    if qa_idx is not None:
        qa_start = qa_idx + 1
        while qa_start < len(lines) and (
            re.match(SECTION_DIVIDER_LINE, lines[qa_start].strip())
            or lines[qa_start].strip() == ""
        ):
            qa_start += 1
    else:
        qa_start = None

    if pres_start is not None and qa_start is not None:
        presentation = "\n".join(lines[pres_start:qa_idx]).strip()
        qa = "\n".join(lines[qa_start:]).strip()
        return presentation, qa

    if pres_start is not None and qa_start is None:
        return "\n".join(lines[pres_start:]).strip(), ""

    if pres_start is None and qa_start is not None:
        return "", "\n".join(lines[qa_start:]).strip()

    return "", ""

def normalize_for_matching(s: str) -> str:
    return re.sub(r"\s+", " ", s.lower()).strip()

def word_count(text: str) -> int:
    return len(WORD_RE.findall(text))

def build_term_patterns(terms: List[str]) -> List[Tuple[str, re.Pattern]]:
    patterns = []
    for t in terms:
        t_norm = normalize_for_matching(t)
        if " " in t_norm:
            phrase = re.escape(t_norm).replace(r"\ ", r"\s+")
            pat = re.compile(rf"\b{phrase}\b", flags=re.IGNORECASE)
        else:
            pat = re.compile(rf"\b{re.escape(t_norm)}\b", flags=re.IGNORECASE)
        patterns.append((t, pat))
    return patterns

CORE_PATTERNS = build_term_patterns(CORE_AI_TERMS)
ADJ_PATTERNS = build_term_patterns(AI_ADJACENT_TERMS)

def count_dictionary_hits(text: str, patterns: List[Tuple[str, re.Pattern]]) -> Dict[str, int]:
    counts = {}
    for term, pat in patterns:
        counts[term] = len(pat.findall(text))
    return counts

def sum_counts(counts: Dict[str, int]) -> int:
    return int(sum(counts.values()))