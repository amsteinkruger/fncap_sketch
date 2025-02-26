# Process FIA data for Oregon plots, conditions, and trees into differences in volume by year for Douglas fir.

# 0. Get packages.
# 1. Get data.
# 2. Filter plots to those (a) in western Oregon (b) with at least one pair of observations.
# 3. Filter conditions to private Douglas fir.
# 4. Join filters on plot and condition.
# 5. Use the result of (4) to filter trees, then filter trees to Douglas fir.
# 6. Aggregate to conditions with units per acre, then pivot time-varying variables.
# 7. Export.

# 0. 

# Is the IDE working?

1 + 1

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
    @select(STATECD, UNITCD, COUNTYCD, PLOT, MATCH_CN, INVYR, MEASYEAR, LON, LAT)
    @mutate(join = 1)
end

# 3. Filter conditions to private Douglas fir.

dat_or_cond_less = @chain dat_or_cond begin
    @filter(FORTYPCD in 201:203,  OWNGRPCD == 40)
    #  Select columns to keep for joins. 
    @select(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, CONDPROP_UNADJ, INVYR, STDAGE, FLDAGE, SITECLCD, DSTRBCD1, DSTRBYR1, TRTCD1, TRTYR1)
    #  Select fewer columns (for now).
    # @select(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, CONDPROP_UNADJ, INVYR, STDAGE, FLDAGE, SITECLCD)
end

# 4. Join subsets of plots and conditions.

dat_or_keep = @chain dat_or_cond_less begin
    @left_join(dat_or_plot_less)
    @filter(join == 1)
end

# 5. Use the result of (4) to filter trees, then filter trees to Douglas fir.

dat_or_tree_less = @chain dat_or_tree begin
    # Select columns that we might use.
    @select(CN, STATECD, UNITCD, COUNTYCD, PLOT, CONDID, INVYR, SPGRPCD, VOLCFNET, VOLBFNET, DRYBIO_AG, DRYBIO_BG, CARBON_AG, CARBON_BG, TPA_UNADJ)
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
    @group_by(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, MATCH_CN, LON, LAT, INVYR, MEASYEAR, STDAGE, FLDAGE, SITECLCD, DSTRBCD1, DSTRBYR1, TRTCD1, TRTYR1)
    @summarize(VOLCFNET = sum(VOLCFNET * TPA_UNADJ), 
               VOLBFNET = sum(VOLBFNET * TPA_UNADJ), 
               DRYBIO = sum((DRYBIO_AG + DRYBIO_BG) * TPA_UNADJ), 
               CARBON = sum((CARBON_AG + CARBON_BG) * TPA_UNADJ))
    @ungroup
    @group_by(STATECD, UNITCD, COUNTYCD, PLOT, CONDID)
    @filter(n() == 2)
    @mutate(PLOT_UID = string(STATECD, "_", UNITCD, "_", COUNTYCD, "_", PLOT, "_", CONDID))
    @ungroup
    @select(PLOT_UID, LON, LAT, INVYR, MEASYEAR, STDAGE, FLDAGE, SITECLCD, DSTRBCD1, DSTRBYR1, TRTCD1, TRTYR1, VOLCFNET, VOLBFNET, DRYBIO, CARBON)
    @arrange(PLOT_UID, INVYR)
    @group_by(PLOT_UID)
    @mutate(WHICH_STRING = if_else(INVYR == maximum(INVYR), "Second", "First"), 
            WHICH_1 = if_else(INVYR == maximum(INVYR), 1, 0),
            WHICH_0 = if_else(INVYR == minimum(INVYR), 1, 0)) 
    @ungroup
    # This is a trick to get around Tidier's missing support for pivoting wide with multiple variables.
    #  Note that if any of the product operations return multiple values, then the maximum() call is destroying data. 
    #  That shouldn't be possible with the preceding @filter(n() == 2), but it's worth checking.
    @group_by(PLOT_UID, LON, LAT)
    @summarize(INVYR_0 = maximum(INVYR * WHICH_0),
               INVYR_1 = maximum(INVYR * WHICH_1),
               MEASYEAR_0 = maximum(MEASYEAR * WHICH_0),
               MEASYEAR_1 = maximum(MEASYEAR * WHICH_1),
               STDAGE_0 = maximum(STDAGE * WHICH_0),
               STDAGE_1 = maximum(STDAGE * WHICH_1),
               FLDAGE_0 = maximum(FLDAGE * WHICH_0),
               FLDAGE_1 = maximum(FLDAGE * WHICH_1),
               SITECLCD_0 = maximum(SITECLCD * WHICH_0),
               SITECLCD_1 = maximum(SITECLCD * WHICH_1),
               DSTRBCD1_0 = maximum(DSTRBCD1 * WHICH_0),
               DSTRBCD1_1 = maximum(DSTRBCD1 * WHICH_1),
               DSTRBYR1_0 = maximum(DSTRBYR1 * WHICH_0),
               DSTRBYR1_1 = maximum(DSTRBYR1 * WHICH_1),
               TRTCD1_0 = maximum(TRTCD1 * WHICH_0),
               TRTCD1_1 = maximum(TRTCD1 * WHICH_1),
               TRTYR1_0 = maximum(TRTYR1 * WHICH_0),
               TRTYR1_1 = maximum(TRTYR1 * WHICH_1),
               VOLCFNET_0 = maximum(VOLCFNET * WHICH_0),
               VOLCFNET_1 = maximum(VOLCFNET * WHICH_1),
               VOLBFNET_0 = maximum(VOLBFNET * WHICH_0),
               VOLBFNET_1 = maximum(VOLBFNET * WHICH_1),
               DRYBIO_0 = maximum(DRYBIO * WHICH_0),
               DRYBIO_1 = maximum(DRYBIO * WHICH_1),
               CARBON_0 = maximum(CARBON * WHICH_0),
               CARBON_1 = maximum(CARBON * WHICH_1))
    @ungroup
end

# 7. Tie things off with a descriptive name and export.

dat_or_intermediate = dat_or_tree_wide

write_csv(dat_or_intermediate, "output/dat_or_intermediate.csv")
