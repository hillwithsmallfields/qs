# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A personal quantified self (QS) data tracking and visualization system that collects, aggregates, and displays life metrics through an integrated web-based dashboard. Combines financial data, health metrics, inventory tracking, scheduling, and other personal data streams.

## Core Commands

### Running the System

```bash
# One-time update and dashboard generation
./update/update.py

# Watch mode (continuous monitoring with inotify)
./update/update.py --watch

# Verbose output for debugging
./update/update.py --verbose

# Serial processing (easier debugging, no parallelization)
./update/update.py --serial

# Force updates even if files are recent
./update/update.py --force

# Skip external API calls (weather, etc.)
./update/update.py --no-externals

# Custom date range for charts
./update/update.py --begin 2024-01-01 --end 2024-12-31

# Testing mode (uses alternate directory)
./update/update.py --testing
```

### Development

```bash
# Install dependencies
pip install -r requirements.in

# The main entry point orchestrates the entire pipeline
./update/update.py --verbose --serial  # Recommended for development
```

## Architecture

### High-Level Data Flow

```
FETCH (external data) → UPDATE (merge/process) → PREPARE IMAGES (charts) → GENERATE DASHBOARD (HTML)
```

### Key Components

**`/update/update.py`** - Main orchestrator
- Coordinates all channels (data sources)
- Manages parallel/serial execution via ThreadPoolExecutor
- Handles file watching with inotify for automatic updates
- Creates backups before modifications
- Entry point: Run this to update everything

**`/dashboard/dashboard.py`** - Dashboard generator
- Constructs HTML page using expressionive library
- Coordinates chart generation across all channels
- Inlines CSS and JavaScript
- Outputs to `~/private_html/dashboard/index.html`

**`/channels/`** - Plugin architecture for data sources
- Each channel implements the `DashboardPanel` interface (defined in `panels.py`)
- Available channels: finances, physical, inventory, agenda, weather, timetable, reflections, parcels, perishables, contacts, ringing, bible, travel, startpage
- Interface: `fetch()` → `update()` → `prepare_page_images()` → `html()`

**`/qsutils/`** - Shared utilities
- `qschart.py`: Matplotlib-based chart generation with pandas DataFrames
- `qsutils.py`: Date handling, CSV operations, common functions

**`/financial/`** - Financial-specific processing
- `categorise.py`: Hierarchical category mapping from `conf/cats.yaml`
- `collate.py`: Transaction aggregation by time period
- `spending_chart.py`: Interactive HTML spending tables

**`/conf/`** - YAML configuration files
- `cats.yaml`: Transaction category hierarchy (153 lines)
- `budgetting-thresholds.yaml`: Spending limits per category
- `default-classes.yaml`, `detail-classes.yaml`: Category mappings

### Storage Manager Pattern

The system uses the `dobishem.storage` library extensively for file I/O abstraction:

**Storage Manager** - Template-based file resolution:
```python
store = storage.Storage(
    templates={'scratch': "var/%(scratch)s", 'texts': "texts/%(texts)s"},
    base="$SYNCED"
)
# Resolves file paths: store.resolve(scratch="data.csv") → "$SYNCED/var/data.csv"
```

**`make()` Function** - Smart incremental merge:
```python
dobishem.storage.make(
    destination="output.csv",
    combiner=merge_function,        # Combines multiple sources
    origins={
        "file1.csv": converter_fn,  # Transform incoming data
        "file2.csv": converter_fn,
    },
    reloader=fixup_fn              # Applied when reloading existing file
)
```

Only regenerates files when sources are newer than destination (timestamp-based). This enables efficient incremental updates.

**File Format Support**: Automatically dispatches readers/writers based on extension:
- `.csv`: List of dicts sorted by date
- `.json`, `.yaml`, `.txt`, `.pkl`, `.table` (org-mode tables)

### Three-Tier Storage

1. **Source Files** (user-managed): Bank downloads, manual entries, external exports in `~/Downloads/`, `$SYNCED/finances/`, etc.

2. **Accumulated Files** (system-managed): Merged historical data in canonical format at `$SYNCED/<domain>/` (e.g., `finances-new.csv`, `exercise.csv`)

3. **Generated Artifacts** (ephemeral): Charts (PNG) and dashboard HTML at `~/private_html/dashboard/`, regenerated from accumulated files

## Important Patterns

### DashboardPanel Interface

All channels inherit from this abstract base class (`channels/panels.py`):

```python
class DashboardPanel(ABC):
    def __init__(self, store, outputs)  # store=data storage, outputs=chart storage
    def name()                          # Channel identifier
    def label()                         # Display name
    def fetch()                         # Fetch from external sources
    def update()                        # Process and cache data
    def prepare_page_images()           # Generate charts/visualizations
    def html()                          # Generate HTML representation
    def files_to_write()               # List files for backup
```

### HTML Generation with expressionive

HTML is built with Python data structures, not string templates:

```python
from expressionive.expressionive import htmltags as T

T.div(class_='section')[
    T.h2["Title"],
    T.table[
        T.tr[T.th["Column1"], T.th["Column2"]],
        [T.tr[T.td[row[0]], T.td[row[1]]] for row in data]
    ]
]
```

CSS classes: `class_='name'`, IDs: `id_='name'`

### Multi-Period Chart Generation

Charts are generated for multiple time windows:

```python
periods = {
    'all_time': datetime.date(1973, 1, 1),
    'past_week': back_from(today, None, None, 7),
    'past_month': back_from(today, None, 1, None),
    'past_quarter': back_from(today, None, 3, None),
    'past_year': back_from(today, 1, None, None)
}
```

Chart filenames follow template: `{chart_name}-{period}-{size}.png`

### Configuration via Environment Variables

Create `.env` files in directories with:
- `$SYNCED`: Base directory for synced data
- `$ORG`: Org-mode files location
- `OWM_API_KEY`: OpenWeatherMap API key
- `$MY_ELISP`: Emacs Lisp scripts directory

## Key Channel Details

**FinancesPanel** (`channels/finances.py`):
- Merges: bank statements, Monzo transactions, manual spending records
- Categorizes transactions using `conf/cats.yaml` hierarchy
- Generates spending charts by category over time
- Tracks unknown payees for manual categorization

**PhysicalPanel** (`channels/physical.py`):
- Health data: weight (multiple unit systems), exercise (cycling, running, walking, swimming)
- Sources: Garmin downloads, manual measurements, isometric logs
- Generates weight trends and exercise activity charts

**InventoryPanel** (`channels/inventory.py`):
- Tracks possessions, project parts, consumable stock, books/media
- Calculates storage capacity metrics
- Integrates with coimealta library

**AgendaPanel** (`channels/agenda.py`):
- Spawns Emacs in batch mode to query org-mode files
- Runs `$MY_ELISP/special-setups/dashboard/dashboard-emacs-query.el`
- Extracts TODO items, shopping lists, project tasks

**WeatherPanel** (`channels/weather.py`):
- Fetches hourly forecast from OpenWeatherMap API
- Multi-day weather with JavaScript-based tab switching
- Stores forecast in CSV for offline viewing

### Backup System

Before any updates, `backup.py` creates timestamped `.tgz` archives in `~/archive/` of all files that will be modified (from each channel's `files_to_write()` method).

## Dependencies

**Core Python Libraries**:
- pandas/numpy: Data manipulation
- matplotlib: Chart generation
- pyyaml: Configuration files
- python-decouple: Environment variables
- inotify: File watching

**Custom Libraries** (by same author):
- dobishem: Storage managers, date utilities, nested messages
- expressionive: HTML generation DSL
- coimealta: Inventory/contacts management

**External Services**:
- OpenWeatherMap API (weather forecasts)
- Manual downloads: Bank statements, Garmin exports, Monzo exports

## Working with This Codebase

### Adding a New Channel

1. Create `channels/your_channel.py` inheriting from `DashboardPanel`
2. Implement required methods: `name()`, `label()`, `fetch()`, `update()`, `prepare_page_images()`, `html()`, `files_to_write()`
3. Import in `update/update.py`
4. Add to handlers list in `update.py`

### Modifying Financial Categories

Edit `conf/cats.yaml` with hierarchical structure:
```yaml
Parent Category:
  Child Category:
    Specific Category: {}
```

Run update to regenerate categorizations.

### Adding New Chart Types

Use `qsutils.qschart.qscharts()` for multi-period generation:
```python
from qsutils import qschart

qschart.qscharts(
    dataframe,
    chart_store,        # Storage manager for outputs
    chart_name="my_chart",
    date_column='Date',
    value_columns=['Value1', 'Value2'],
    periods=periods,    # Dict of period_name: start_date
    sizes={'small': {'figsize': (5,4)}, 'large': {'figsize': (11,8)}},
    colors=['red', 'blue']
)
```

### Debugging Tips

- Use `--verbose --serial` for detailed sequential output
- Check `$SYNCED/var/` for intermediate/cached files
- Inspect generated charts in `~/private_html/dashboard/`
- Watch mode logs file change events to stdout
- Use `--no-externals` to skip API calls during testing

## Current Development Status

**Branch**: `use-storage-managers` - Ongoing standardization of dobishem.storage patterns throughout codebase

**Known Issues** (from README):
- Code needs refactoring (author's note: "badly needs either a re-write or a major refactoring")
- Some financial logic uses embedded Lisp interpreter (small usage, questionable design)

## References

- Main README: `/README.md` (brief overview)
- Panel base class: `channels/panels.py:27`
- Storage manager docs: dobishem library
- Update orchestration: `update/update.py:226` (argument parsing), `update/update.py:50` (main update logic)
