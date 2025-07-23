# MTSCR Shiny Application

A clean, modular Shiny application for Multidimensional Top Scoring for Creativity Research.

## File Structure

```
├── ui.R                    # Main UI definition with card-based sidebar
├── server.R                # Main server logic with module integration
├── mts.R                   # Multidimensional Top Scoring module
├── sts.R                   # Simple Top Scoring module
├── functions.R             # Utility functions and helpers
├── README.md              # This documentation
├── rsconnect/             # Deployment configuration
└── www/                   # Static assets
    ├── styles.css
    └── article_citation.html
```

## Features

### Modular Design
- **UI Cards**: Clean sidebar with separate cards for data import and each analysis type
- **Shiny Modules**: Proper module structure for MTS and STS functionality
- **Reusable Functions**: Common utilities in `functions.R`

### Functionality
- **Data Import**: Support for multiple data sources (file, environment, copy-paste, etc.)
- **MTS Analysis**: Full multidimensional top scoring with configurable parameters
- **STS Analysis**: Simple top scoring analysis
- **Error Handling**: Comprehensive validation and error reporting
- **Download Options**: Export results in CSV and Excel formats

## Module Structure

### MTS Module (`mts.R`)
- `mtsUI()`: User interface for MTS parameters
- `mtsServer()`: Server logic for MTS analysis
- Includes all MTS-specific parameters (ties method, normalization, self-ranking, etc.)

### STS Module (`sts.R`)
- `stsUI()`: User interface for STS parameters  
- `stsServer()`: Server logic for STS analysis
- Simplified interface for basic top scoring

## Model Generation Functions

Located in `server.R`, these functions handle the actual model creation:

### `generate_mts_model(inputs)`
Generates a multidimensional top scoring model with the following parameters:
- `data`: Input dataset
- `id_col`: ID column name
- `item_col`: Item column name (optional)
- `score_col`: Score column name
- `ties_method`: Method for handling ties ("random" or "average")
- `top`: Sequence of top answers to include
- `normalise`: Whether to normalize scores
- `self_ranking`: Self-ranking column (optional)

### `generate_sts_model(inputs)`
Generates a simple top scoring model with basic parameters:
- `data`: Input dataset
- `id_col`: ID column name
- `item_col`: Item column name (optional)
- `score_col`: Score column name
- `top`: Sequence of top answers to include

## Utility Functions (`functions.R`)

### Data Validation
- `validate_data()`: Validates input data structure
- `check_sufficient_data()`: Checks for minimum observations
- `safe_model_generation()`: Wraps model generation with error handling

### UI Helpers
- `format_model_summary()`: Formats model results for display
- `create_filename()`: Generates timestamped filenames
- `create_info_box()`: Creates styled information boxes
- `create_loading_spinner()`: Creates loading indicators

## Usage

1. **Import Data**: Click "Import data" to load your dataset
2. **Select Analysis Type**: Choose either MTS or STS from the respective cards
3. **Configure Parameters**: Set column mappings and analysis parameters
4. **Generate Model**: Click the "Generate Model" button
5. **View Results**: See model summary and scored data in the main panel
6. **Download**: Export results using the download buttons

## Requirements

Required R packages:
- `shiny`
- `bslib`
- `mtscr`
- `dplyr`
- `DT`
- `writexl`
- `datamods`
- `shinyWidgets`

## Development Notes

### Adding New Features
- Create new modules following the pattern in `mts.R` and `sts.R`
- Add utility functions to `functions.R`
- Update UI cards in `ui.R`
- Integrate module servers in `server.R`

### Error Handling
The app includes comprehensive error handling:
- Data validation before model generation
- User-friendly error messages
- Warnings for insufficient data
- Safe model generation wrapper

### Customization
- Modify card styling in `www/styles.css`
- Update model parameters in module UI functions
- Add new analysis types by creating additional modules
