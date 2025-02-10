# Process FIA data for Oregon plots, conditions, and trees into differences in volume by year for Douglas fir.

# 0. Get packages.
# 1. Get data.
# 2. Filter plots to those (a) in western Oregon (b) with at least one pair of observations.
# 3. Filter conditions to private Douglas fir.
# 4. Join filters on plot and condition.
# 5. Use the result of (4) to filter trees, then filter trees to Douglas fir.
# 6. Aggregate to conditions with units per acre, then represent change in volume for the first to the second year.
# 7. Check results in standard visualizations.

# 0. 

# Is the IDE working?

1 + 1

# Instead of learning new things, I'll stick to the Tidyverse. Cheers to the developers.

using Tidier

# 1. Get data.

dat_or_plot = read_csv("data/OR_PLOT.csv")
dat_or_cond = read_csv("data/OR_COND.csv")
dat_or_tree = read_csv("data/OR_TREE.csv")

# 2. Filter plots to those (a) in western Oregon (b) with at least one pair of observations.

dat_or_plot_less = @chain dat_or_plot begin
    @filter(LON < -120)
    @mutate(MATCH_CN = if_else(ismissing(PREV_PLT_CN), CN, PREV_PLT_CN))
    @group_by(MATCH_CN)
    @filter(n() > 1)
    @ungroup
    @select(STATECD, UNITCD, COUNTYCD, PLOT, MATCH_CN, INVYR, LON, LAT)
    @mutate(join = 1)
end

# 3. Filter conditions to private Douglas fir.

dat_or_cond_less = @chain dat_or_cond begin
    @filter(FORTYPCD in 201:203,  OWNGRPCD == 40)
    #  Select columns to keep for joins. 
    @select(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, CONDPROP_UNADJ, INVYR, STDAGE, SITECLCD, DSTRBCD1, DSTRBYR1, TRTCD1, TRTYR1)
    #  Select fewer columns (for now).
    @select(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, CONDPROP_UNADJ, INVYR)
end

# 4. Join subsets of plots and conditions.

dat_or_keep = @chain dat_or_cond_less begin
    @left_join(dat_or_plot_less)
    @filter(join == 1)
end

# 5. Use the result of (4) to filter trees, then filter trees to Douglas fir.

dat_or_tree_less = @chain dat_or_tree begin
    # Select columns that we might use.
    @select(CN, STATECD, UNITCD, COUNTYCD, PLOT, CONDID, INVYR, SPGRPCD, VOLCFNET, TPA_UNADJ)
    # Get plot and condition information.
    @left_join(dat_or_keep)
    # Filter on plot and condition.
    @filter(join == 1)
    @select(-join)
    # Filter on species group (down to Douglas firs). This is equivalent to SPCD == 202.
    @filter(SPGRPCD == 10)
end

# 6. Aggregate to volume-per-acre by condition, then pivot so that rows are plots with timesteps in columns.

dat_or_tree_wide = @chain dat_or_tree_less begin
    @filter(!ismissing(VOLCFNET) & !ismissing(TPA_UNADJ))
    @group_by(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, MATCH_CN, INVYR, LON, LAT)
    @summarize(VOLCFNET = sum(VOLCFNET * TPA_UNADJ))
    @ungroup
    @group_by(STATECD, UNITCD, COUNTYCD, PLOT, CONDID)
    @filter(n() == 2)
    @mutate(PLOT_UID = string(STATECD, "_", UNITCD, "_", COUNTYCD, "_", PLOT, "_", CONDID))
    @ungroup
    @select(PLOT_UID, MATCH_CN, LON, LAT, INVYR, VOLCFNET)
    @arrange(PLOT_UID, MATCH_CN, INVYR)
    @group_by(PLOT_UID)
    @mutate(WHICH = row_number())
    @mutate(WHICH = if_else(WHICH == 1, "First", "Second"))
    @ungroup
    # Things get silly in the following section: pivot_wider in Tidier doesn't appear to support pivoting multiple columns.
end

dat_or_tree_wide_left = @chain dat_or_tree_wide begin
    @select(-VOLCFNET)
    @pivot_wider(names_from = WHICH, values_from = INVYR)
    @rename(Year_First = First, Year_Second = Second)
end

dat_or_tree_wide_right = @chain dat_or_tree_wide begin
    @select(-INVYR)
    @pivot_wider(names_from = WHICH, values_from = VOLCFNET)
    @rename(Volume_First = First, Volume_Second = Second)
end


dat_or_tree_wider = @left_join(dat_or_tree_wide_left, dat_or_tree_wide_right)

dat_or_differences = @chain dat_or_tree_wider begin
    @select(-MATCH_CN)
    @mutate(Year_Difference = Year_Second - Year_First,
            Volume_Difference = Volume_Second - Volume_First)
end

write_csv(dat_or_differences, "output/data/dat_or_differences.csv")
