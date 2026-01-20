# Obtaining the data dictionary

1. Download the ABCD 6.0 dd using R

```r
NBDCtools::get_dd_abcd(release = "6.0") |> write.csv("path/to/your/directory/dd-abcd-6_0.csv", row.names = FALSE)
```

2. Select only relevant columns and add a logical `substudy` column (TRUE if substudy, FALSE if core).

<img width="1292" height="1254" alt="image" src="https://github.com/user-attachments/assets/510add5f-7e14-4e10-a993-1dc0ce294574" />

> Dorota will add steps here.


