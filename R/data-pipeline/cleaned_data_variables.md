# Variables in Cleaned Dataset from `1-data_cleaning.R`

## Output Structure
The cleaned dataset has **one row per participant per measurement date** in long format.

---

## Variable List by Category

### 1. IDENTIFIERS & DATES (5 variables)

| Variable | Type | Description | Source |
|----------|------|-------------|--------|
| `id` | Integer | Unique participant identifier | Base data |
| `date` | Date | Date of measurement | Base/Follow-up |
| `date_entry` | Date | Date participant enrolled in study | Base data |
| `date_last` | Date | Date of participant's last measurement | Calculated |
| `last_measurement` | Logical | TRUE if this is participant's last record | Calculated |

---

### 2. DATA QUALITY (1 variable)

| Variable | Type | Description | Values |
|----------|------|-------------|--------|
| `anomaly` | Factor | Data quality classification | "included", "excluded_rate" (>10% daily change), "excluded_bmi" (BMI <10 or >60), "excluded_weight" (weight <30 or >180kg), "excluded_conflict" (conflicting double record), "missing" |

---

### 3. PARTICIPANT TIMELINE (7 variables starting with "participant_")

| Variable | Type | Description |
|----------|------|-------------|
| `participant_recorded` | Logical | TRUE if weight measured on this date |
| `participant_cumulative_days_enrolled` | Integer | Days since study entry (1, 2, 3...) |
| `participant_cumulative_days_recorded` | Integer | Cumulative number of measurements to date |
| `participant_in_followup` | Logical | TRUE if participant has >1 measurement total |
| `participant_timepoint` | Factor | "Study entry" or "Follow up" |
| `participant_days_since_previousmeasurement` | Integer | Days since previous measurement |

---

### 4. BASELINE CHARACTERISTICS (7 variables - from base data)

| Variable | Type | Description | Example Values |
|----------|------|-------------|----------------|
| `organisation` | Factor | Employer | "UNRWA", "Save the Children International" |
| `sex` | Factor | Sex | "male", "female", "other/prefer not to answer" |
| `age` | Factor | Age group (created) | "Age under 30", "Age 30-45", "Age over 45" |
| `children_feeding` | Factor | Dependent children | "0", "1", "2", "3+" |
| `role` | Factor | Staff role | "expatriate", "national staff member", "consultant or contractor", "casual staff/daily worker", "other" |
| `governorate` | Factor | Location | "North Gaza", "Gaza City", "Deir Al Balah", "Khan Yunis", "Rafah" |
| `height` | Integer | Height in cm | Fixed per participant |

---

### 5. WEIGHT VARIABLES (8 variables starting with "weight")

| Variable | Type | Description |
|----------|------|-------------|
| `weight_prewar` | Numeric | Self-reported pre-war weight (kg) |
| `weight_entry` | Numeric | Weight at study enrollment (kg) |
| `weight_daily` | Numeric | Weight on this date (kg) |
| `weight_last` | Numeric | Participant's most recent weight (kg) |
| `weight_change_unit_entry` | Numeric | Change from enrollment (kg) |
| `weight_change_unit_prewar` | Numeric | Change from pre-war (kg) |
| `weight_change_percent_entry` | Numeric | % change from enrollment |
| `weight_change_percent_prewar` | Numeric | % change from pre-war |
| `weight_change_percent_daily_rate_entry` | Numeric | Daily rate of % change since enrollment (%/day) |
| `weight_change_percent_previousmeasurement` | Numeric | % change from previous measurement |

---

### 6. BMI VARIABLES (13 variables starting with "bmi")

| Variable | Type | Description |
|----------|------|-------------|
| `bmi_prewar` | Numeric | Pre-war BMI (kg/m²) |
| `bmi_entry` | Numeric | BMI at enrollment (kg/m²) |
| `bmi_daily` | Numeric | BMI on this date (kg/m²) |
| `bmi_last` | Numeric | Participant's most recent BMI (kg/m²) |
| `bmi_category_prewar` | Factor | Pre-war BMI category |
| `bmi_category_daily` | Factor | Current BMI category |
| `bmi_change_unit_entry` | Numeric | BMI change from enrollment (kg/m²) |
| `bmi_change_unit_prewar` | Numeric | BMI change from pre-war (kg/m²) |
| `bmi_change_percent_entry` | Numeric | % BMI change from enrollment |
| `bmi_change_percent_prewar` | Numeric | % BMI change from pre-war |
| `bmi_change_percent_daily_rate_entry` | Numeric | Daily rate of % BMI change since enrollment (%/day) |
| `bmi_change_percent_previousmeasurement` | Numeric | % BMI change from previous measurement |

**BMI Categories:** "Underweight" (<18.5), "Normal" (18.5-25), "Overweight" (25-30), "Obese" (≥30)

---

### 7. EXTRA VARIABLES (from "everything()")
Any remaining columns from base data not explicitly selected above.

---

## TOTAL VARIABLE COUNT: ~48 variables

---

## Key Statistics for Simulation

### Numeric Variables - Need Mean & SD:

**Weight-related:**
- `weight_daily`: Mean ~67-68kg (from summary data)
- `weight_prewar`: Typically higher than current
- `weight_entry`: Similar to daily weights at baseline
- `height`: Mean ~165-170cm (varies by sex)
- Weight changes: Mostly negative (weight loss trend)

**BMI-related:**
- `bmi_daily`: Mean ~24-25 kg/m²
- `bmi_prewar`: Typically higher
- BMI changes: Negative trend

**Time variables:**
- `participant_cumulative_days_enrolled`: Range 1 to max study duration
- `participant_cumulative_days_recorded`: Range 1 to total measurements per person
- `participant_days_since_previousmeasurement`: Varies, often 1-7 days

### Factor Variables - Need Proportions:

From data dictionary levels:
- `sex`: Proportions of male/female/other
- `age`: Proportions across 3 age groups
- `children_feeding`: Distribution of 0, 1, 2, 3+
- `role`: Distribution across staff types
- `governorate`: Distribution across 5 regions
- `organisation`: Proportions per organization
- `bmi_category_prewar` & `bmi_category_daily`: Distribution across 4 BMI categories
- `anomaly`: Mostly "included", small % excluded

### Logical Variables:
- `participant_in_followup`: Proportion with follow-up (appears ~83/438 = 19%)
- `last_measurement`: One TRUE per participant

---

## Notes for Simulation

1. **Temporal structure:** Create realistic enrollment dates + follow-up dates
2. **Correlations:** Height correlated with sex; weight changes over time within individual
3. **Constraints:**
   - Weight 30-180kg, BMI 10-60 for "included" data
   - Daily change rate <10% for "included" data
   - Follow-up dates must be after enrollment
4. **Missingness:** Some variables only present after first measurement (change variables)
