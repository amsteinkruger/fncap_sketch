# Explore processed FIA data to inform growth models.

# 0. Packages.
# 1. Data
# 2. Tables (Counts, Coverage)
# 3. Visualizations (Counts, Coverage)
# 4. More Tables (Summary Statistics, Relationships)
# 5. More Visualizations (Summary Statistics, Relationships)

# 0. Packages

# using

# 1. Data

dat = read_csv("output/dat_or_intermediate.csv")

# --- Breakpoint for heinous scratch work ---

# 2. Tables

#  Counts

#    Inventory Year

# but pivot into two columns, one for each end of the interval? or something related that's actually interpretable

dat_tab_1 = @chain dat begin
    @group_by(INVYR_0)
    @summarize(count = n())
    @ungroup
    @arrange(INVYR_0)
end

#   Measurement Year

#   Stand Age by 5-Year Bins

#   Field Age by 5-Year Bins

#   Volume (VOLCFNET) by Bins

#   Volume (VOLBFNET) by Bins

#   Volume (DRYBIO) by Bins

#   Disturbance Codes

#   Treatment Codes

#   Site Class Codes

#  Coverage

#   Volume (VOLCFNET, VOLBFNET, DRYBIO)

#   Disturbance Codes and Years

#   Treatment Codes and Years

#   Site Class Codes and Years?

# 3. Visualizations

#  Counts

#  Coverage

# 4. 

# 5. 
