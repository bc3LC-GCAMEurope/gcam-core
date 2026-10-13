"""
hindcast2005_build_capital_costs.py

Builds the electricity capital-cost overrides for the 2005 hindcast (GCAM-Europe
branch GCAM-Europe-v8.7.1-hindcasting2005), option A: OBSERVED cost trajectory.

Inputs (in input/gcamdata/data/inputs/):
  L223.GlobalTechCapital_elec_2015replaceHINDC.csv     (dispatchable techs, from the 2015 hindcast)
  L223.GlobalIntTechCapital_elec_2015replaceHINDC.csv  (intermittent techs, from the 2015 hindcast)

Outputs (same folder):
  L223.GlobalTechCapital_elec_2005replaceHINDC.csv
  L223.GlobalIntTechCapital_elec_2005replaceHINDC.csv
  hindcast2005_capital_costs.csv   (documentation: value used per tech/year, derivation, source)

What changes (all values in 1975 USD/kW, as in GCAM):
  * PV, wind (onshore) and wind_offshore: 2005, 2010, 2015 and 2021 are set to observed
    costs (below); 1975 and 1990 take the 2005 value (flat, as before). After 2021 the
    existing ATB-based trajectory is kept in shape but rescaled so that it starts from the
    observed 2021 value (only matters beyond the 2021 evaluation horizon).
  * PV_storage and wind_storage (GlobalTechCapital): the storage adder of the original file
    (storage tech minus parent tech, per year) is kept and added to the new parent cost.
  * CSP, CSP_storage, rooftop_pv and all dispatchable technologies are left unchanged
    (IRENA CSP costs include large storage and are not comparable with GCAM's CSP).

Run from input/gcamdata:   python hindcast2005_build_capital_costs.py
"""
import csv
import os

HERE = os.path.dirname(os.path.abspath(__file__))
DIR = os.path.join(HERE, "data", "inputs")

# --- GDP deflator: same BEA series (A191RD3A086NBEA) hard-coded in gcamdata::gdp_deflator ---
_GDP = [8.778, 8.457, 7.587, 6.700, 6.514, 6.871, 7.012, 7.097, 7.402, 7.190, 7.120, 7.205, 7.692,
        8.304, 8.683, 8.890, 9.120, 10.296, 11.426, 12.067, 12.046, 12.195, 13.060, 13.286, 13.447,
        13.572, 13.801, 14.271, 14.744, 15.080, 15.287, 15.495, 15.660, 15.850, 16.032, 16.276,
        16.574, 17.039, 17.533, 18.280, 19.176, 20.189, 21.212, 22.130, 23.342, 25.443, 27.800,
        29.330, 31.152, 33.343, 36.110, 39.371, 43.097, 45.759, 47.552, 49.267, 50.826, 51.849,
        53.134, 55.008, 57.165, 59.305, 61.310, 62.707, 64.194, 65.564, 66.939, 68.164, 69.340,
        70.119, 71.111, 72.722, 74.360, 75.515, 77.006, 79.077, 81.556, 84.071, 86.349, 88.013,
        88.556, 89.632, 91.481, 93.185, 94.771, 96.421, 97.316, 98.241, 100.000, 102.291, 104.008,
        105.381, 110.213, 117.973, 122.273]
GDP = dict(zip(range(1929, 2024), _GDP))


def to_1975(value, year):
    """Convert a value in `year` USD to 1975 USD (gcamdata gdp_deflator(1975, year))."""
    return value * GDP[1975] / GDP[year]


SRC_IRENA21 = ("IRENA (2022) Renewable Power Generation Costs in 2021, Table ES.1, global weighted-average "
               "total installed cost, 2021 USD/kW")
SRC_IRENA18 = ("IRENA (2019) Renewable Power Generation Costs in 2018, Figs S.4-S.6, global weighted-average "
               "total installed cost, 2018 USD/kW (used only for the 2015/2010 ratio)")
SRC_BSW = ("BSW-Solar via Solarserver (2008-08-07): ~5,000 EUR/kWp net in 2006 (rooftop <=100 kWp, DE); "
           "Photovoltaikumfrage via BauLinks (2011-01-23): 2,740-2,950 EUR/kWp in 2010")
SRC_EWEA = ("EWEA, Wind Energy - The Facts, 'Development and investment costs of offshore wind power', "
            "Table 2.7 (projects 2003-2005: 1.3-2.0 MEUR/MW)")
SRC_VOORMOLEN = ("Voormolen, Junginger & van Sark (2016), Energy Policy 88: 'Unravelling historical cost "
                 "developments of offshore wind energy in Europe' (1.5 MEUR/MW 2000 -> 4.0 MEUR/MW 2010)")
SRC_EWEA_ON = ("EWEA (2009) The Economics of Wind Energy: average onshore investment cost ~1,225 EUR/kW "
               "(2006 prices); 1,150 EUR/kW assumed for 2005")
SRC_LBNL = "LBNL 2009 Wind Technologies Market Report (US: ~1,300 $/kW 2001-04, 2,120 $/kW 2009, 2009 USD)"

# IRENA 2021 report (2021 USD/kW): 2010 and 2021 values
IRENA21 = {"PV": (4808, 857), "wind": (2042, 1325), "wind_offshore": (4876, 2858)}
# IRENA 2018 report (2018 USD/kW): 2010 and 2015 values -> ratio 2015/2010
IRENA18 = {"PV": (4621, 1825), "wind": (1913, 1609), "wind_offshore": (4572, 4982)}

rows = []  # documentation rows
OBS = {}   # tech -> {year: 1975 USD/kW}
for tech in IRENA21:
    v2010 = to_1975(IRENA21[tech][0], 2021)
    v2021 = to_1975(IRENA21[tech][1], 2021)
    r1510 = IRENA18[tech][1] / IRENA18[tech][0]
    v2015 = v2010 * r1510
    OBS[tech] = {2010: v2010, 2015: v2015, 2021: v2021}
    rows += [(tech, 2010, v2010, "IRENA 2010 value, 2021 USD -> 1975 USD", SRC_IRENA21),
             (tech, 2015, v2015, f"IRENA-2021 2010 value x IRENA-2018 ratio 2015/2010 ({r1510:.3f})",
              SRC_IRENA21 + " | " + SRC_IRENA18),
             (tech, 2021, v2021, "IRENA 2021 value, 2021 USD -> 1975 USD", SRC_IRENA21)]

# 2005 values
# PV: German rooftop price ratio 2006 (~5,000 EUR/kWp) / 2010 (~2,850 EUR/kWp), deflated to real terms,
#     applied to the IRENA 2010 level. 2005 prices were at least as high as 2006 (silicon shortage).
pv_ratio = (5000 / 2850) * (GDP[2006] / GDP[2010])
OBS["PV"][2005] = OBS["PV"][2010] * pv_ratio
rows.append(("PV", 2005, OBS["PV"][2005],
             f"IRENA 2010 x real price ratio 2006/2010 of German rooftop systems ({pv_ratio:.2f})", SRC_BSW))
# Onshore wind: European level (EWEA) ~1,150 EUR/kW in 2005, EUR->USD 1.245 (2005 avg), 2005 USD -> 1975 USD
on2005 = to_1975(1150 * 1.245, 2005)
OBS["wind"][2005] = on2005
rows.append(("wind", 2005, on2005,
             "European level 1,150 EUR/kW x 1.245 USD/EUR (2005), 2005 USD -> 1975 USD. "
             "Alternative (US cost ratio 2001-04/2009 applied to IRENA 2010): ~316",
             SRC_EWEA_ON + " | " + SRC_LBNL))
# Offshore wind: European project costs 2003-2005 ~1.75 MEUR/MW vs 4.0 MEUR/MW in 2010 (nominal),
#     deflated to real terms, applied to the IRENA 2010 level.
off_ratio = (1.75 / 4.0) * (GDP[2005] / GDP[2010])
OBS["wind_offshore"][2005] = OBS["wind_offshore"][2010] * off_ratio
rows.append(("wind_offshore", 2005, OBS["wind_offshore"][2005],
             f"IRENA 2010 x real cost ratio 2005/2010 of European offshore projects ({off_ratio:.2f})",
             SRC_EWEA + " | " + SRC_VOORMOLEN))

OBS_YEARS = (2005, 2010, 2015, 2021)
STORAGE_PARENT = {"PV_storage": "PV", "wind_storage": "wind"}


def read(path):
    with open(path, newline="", encoding="utf-8") as fh:
        r = list(csv.reader(fh))
    return r[0], r[1:]


def build_series(old):
    """old: {year: value}. Returns {year: new value} for an observed technology."""
    new = {}
    obs = OBS_TECH
    k = obs[2021] / old[2021]                  # rescale post-2021 ATB trajectory
    for y, v in old.items():
        if y in OBS_YEARS:
            new[y] = obs[y]
        elif y < 2005:
            new[y] = obs[2005]
        elif y > 2021:
            new[y] = v * k
        else:                                   # no other years expected <= 2021
            new[y] = v
    return new


def process(fname_in, fname_out, tech_col):
    global OBS_TECH
    header, data = read(os.path.join(DIR, fname_in))
    i_t, i_y, i_v = header.index(tech_col), header.index("year"), header.index("capital.overnight")
    series = {}
    for r in data:
        series.setdefault(r[i_t], {})[int(r[i_y])] = float(r[i_v])
    parent_old = {}
    newvals = {}
    for tech in OBS:
        if tech in series:
            OBS_TECH = OBS[tech]
            newvals[tech] = build_series(series[tech])
            parent_old[tech] = series[tech]
    # storage variants: keep the original storage adder on top of the new parent cost
    # (parents live in the intermittent file, processed first)
    for st, parent in STORAGE_PARENT.items():
        if st in series and parent in PARENTS:
            old_p, new_p = PARENTS[parent]
            newvals[st] = {y: new_p[y] + (v - old_p[y]) for y, v in series[st].items()}
    out = [header]
    changed = 0
    for r in data:
        t, y = r[i_t], int(r[i_y])
        if t in newvals:
            r = list(r)
            r[i_v] = str(int(round(newvals[t][y])))
            changed += 1
        out.append(r)
    with open(os.path.join(DIR, fname_out), "w", newline="", encoding="utf-8") as fh:
        csv.writer(fh, lineterminator="\r\n").writerows(out)
    print(f"{fname_out}: {changed} values changed ({', '.join(sorted(newvals))})")
    return {t: (parent_old[t], newvals[t]) for t in parent_old}


OBS_TECH = None
PARENTS = {}
PARENTS = process("L223.GlobalIntTechCapital_elec_2015replaceHINDC.csv",
                         "L223.GlobalIntTechCapital_elec_2005replaceHINDC.csv", "intermittent.technology")
process("L223.GlobalTechCapital_elec_2015replaceHINDC.csv",
        "L223.GlobalTechCapital_elec_2005replaceHINDC.csv", "technology")

with open(os.path.join(DIR, "hindcast2005_capital_costs.csv"), "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh, lineterminator="\r\n")
    w.writerow(["technology", "year", "capital_overnight_1975USD_per_kW", "derivation", "source"])
    for t, y, v, d, s in sorted(rows):
        w.writerow([t, y, int(round(v)), d, s])
print("hindcast2005_capital_costs.csv written")
for t in sorted(OBS):
    print(t, {y: int(round(OBS[t][y])) for y in sorted(OBS[t])})
