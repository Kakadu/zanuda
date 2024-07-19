let rec nonwrapped_fac n = if n <= 1 then 1 else n * nonwrapped_fac (n - 1)
