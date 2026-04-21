import re
from typing import Optional, Tuple

def extract_call_date(text: str) -> Optional[str]:
    month_map = {
        "JANUARY": "01", "FEBRUARY": "02", "MARCH": "03", "APRIL": "04",
        "MAY": "05", "JUNE": "06", "JULY": "07", "AUGUST": "08",
        "SEPTEMBER": "09", "OCTOBER": "10", "NOVEMBER": "11", "DECEMBER": "12"
    }

    for line in text.splitlines()[:80]:
        line = line.strip()
        m = re.match(r"^([A-Z]+)\s+(\d{1,2}),\s+(\d{4})\s*/", line)
        if m:
            month, day, year = m.group(1), m.group(2), m.group(3)
            month = month.upper()
            if month in month_map:
                return f"{year}-{month_map[month]}-{int(day):02d}"
    return None

FISCAL_Q_RE = re.compile(r"\bQ([1-4])\s*(20\d{2})\b", flags=re.IGNORECASE)

# Matches "full year 2022", "full-year 2022", "full year fiscal 2022",
# "fiscal year 2022", "annual 2022", "FY 2022" — all map to Q4.
FULL_YEAR_RE = re.compile(
    r"\b(?:full[\s\-]year|fiscal\s+year|annual|FY)\s*(?:fiscal\s+)?(20\d{2})\b",
    flags=re.IGNORECASE
)

def extract_fiscal_quarter_year(text: str) -> Tuple[Optional[int], Optional[int], Optional[str]]:
    head = "\n".join(text.splitlines()[:120])

    # Primary pass: standard "Q3 2023" format
    m = FISCAL_Q_RE.search(head)
    if m:
        q = int(m.group(1))
        y = int(m.group(2))
        return q, y, f"Q{q} {y}"

    # Second pass: "full year" / "fiscal year" / "annual" → treat as Q4
    m = FULL_YEAR_RE.search(head)
    if m:
        y = int(m.group(1))
        return 4, y, f"Q4 {y}"

    return None, None, None