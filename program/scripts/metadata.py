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

def extract_fiscal_quarter_year(text: str) -> Tuple[Optional[int], Optional[int], Optional[str]]:
    head = "\n".join(text.splitlines()[:120])
    m = FISCAL_Q_RE.search(head)
    if not m:
        return None, None, None

    q = int(m.group(1))
    y = int(m.group(2))
    return q, y, f"Q{q} {y}"