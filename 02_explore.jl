# Explore processed FIA data to inform growth models.

# 0. Packages.
# 1. Data
# 2. Tables (Counts, Coverage)
# 3. Visualizations (Counts, Coverage)
# 4. More Tables (Summary Statistics, Relationships)
# 5. More Visualizations (Summary Statistics, Relationships)

# 0. Packages

using Tidier

# 1. Data

dat = read_csv("output/dat_or_intermediate.csv")

# 2. Tables

#  Counts

#    Inventory Year

dat_tab_1_INVYR = 
@chain dat begin
    @select(INVYR_0, INVYR_1)
    @pivot_longer(cols = everything())
    @group_by(value, variable)
    @summarize(count = n())
    @ungroup
    @pivot_wider(names_from = variable, values_from = count)
    @rename(year = value)
    @arrange(year)
    coalesce.(0)
end

#   Measurement Year

dat_tab_2_MEASYEAR = 
@chain dat begin
    @select(MEASYEAR_0, MEASYEAR_1)
    @pivot_longer(cols = everything())
    @group_by(value, variable)
    @summarize(count = n())
    @ungroup
    @pivot_wider(names_from = variable, values_from = count)
    @rename(year = value)
    @arrange(year)
    coalesce.(0)
end

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
