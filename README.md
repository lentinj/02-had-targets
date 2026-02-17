You can then use R studio within your project or change to the `02-had-targets` directory and load R.

You may see:

```
- One or more packages recorded in the lockfile are not installed.
- Use `renv::status()` for more details.
```

...in which case use renv to restore packages:

```r
renv::restore()
```

Then you can build the advice with the `run.R` script.
