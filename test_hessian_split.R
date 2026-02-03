#!/usr/bin/env Rscript
## Test Hessian parameter split calculation

npars <- 3067
nsplit <- 100

## Calculate parameter range for each part using balanced distribution
base_size <- floor(npars / nsplit)
remainder <- npars %% nsplit

cat("Total parameters:", npars, "\n")
cat("Number of splits:", nsplit, "\n")
cat("Base chunk size:", base_size, "\n")
cat("Parts with +1 extra:", remainder, "\n\n")

## Calculate all ranges
all_ranges <- data.frame(
  part = 1:nsplit,
  start_par = integer(nsplit),
  end_par = integer(nsplit),
  n_params = integer(nsplit)
)

for(hessian_part in 1:nsplit) {
  if(hessian_part <= remainder) {
    ## Larger chunks for first 'remainder' parts
    start_par <- (hessian_part - 1) * (base_size + 1) + 1
    end_par <- hessian_part * (base_size + 1)
  } else {
    ## Smaller chunks for remaining parts
    offset <- remainder * (base_size + 1)
    start_par <- offset + (hessian_part - remainder - 1) * base_size + 1
    end_par <- offset + (hessian_part - remainder) * base_size
  }
  
  all_ranges$start_par[hessian_part] <- start_par
  all_ranges$end_par[hessian_part] <- end_par
  all_ranges$n_params[hessian_part] <- end_par - start_par + 1
}

## Show first few and last few
cat("First 10 parts:\n")
print(head(all_ranges, 10))

cat("\nLast 10 parts:\n")
print(tail(all_ranges, 10))

## Verify
cat("\n--- Verification ---\n")
cat("Total parameters covered:", sum(all_ranges$n_params), "/", npars, "\n")
cat("Min parameters per part:", min(all_ranges$n_params), "\n")
cat("Max parameters per part:", max(all_ranges$n_params), "\n")

## Check for gaps or overlaps
gaps <- c()
overlaps <- c()
for(i in 1:(nsplit-1)) {
  if(all_ranges$end_par[i] + 1 != all_ranges$start_par[i+1]) {
    gaps <- c(gaps, i)
  }
  if(all_ranges$end_par[i] >= all_ranges$start_par[i+1]) {
    overlaps <- c(overlaps, i)
  }
}

if(length(gaps) > 0) {
  cat("⚠️  Gaps found after parts:", gaps, "\n")
} else {
  cat("✓ No gaps\n")
}

if(length(overlaps) > 0) {
  cat("⚠️  Overlaps found after parts:", overlaps, "\n")
} else {
  cat("✓ No overlaps\n")
}

## Check last parameter
if(all_ranges$end_par[nsplit] == npars) {
  cat("✓ Last parameter matches npars\n")
} else {
  cat("⚠️  Last parameter:", all_ranges$end_par[nsplit], "!= npars:", npars, "\n")
}
