# Plot Modules

This folder contains reusable plotting/data-prep modules designed for future package/Shiny migration.

## Files
- `mod_general.R`: small utilities, validation, debug helpers
- `mod_model_meta.R`: model metadata extraction and time-grid helpers
- `mod_fishery.R`: fishery map loading/normalization/augmentation
- `mod_cpue.R`: CPUE-specific selectors/mapping helpers
- `mod_tag.R`: shared tag processing pipeline functions
- `mod_overlay.R`: overlay capability checks (single vs multi-model)
- `load_plot_modules.R`: loader that sources all modules

## Shiny-friendly notes
- Functions are pure and input-driven where possible.
- Overlay checks are separated (`pm_overlay_capability`) so UI can disable tabs/buttons.
- Fishery map can be externally configured via `config/fishery_map.csv`.
