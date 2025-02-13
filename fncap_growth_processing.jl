# Packages

using Tidier
using Random
using Plots
using FastHalton
using Distributions

# Seed

#  Question: is this pointless because Julia requires seeds to be defined by function call (unlike MATLAB and R)?

Random.seed!(112358)

# Data

#  Read

dat_growth = read_csv("output/dat_or_differences.csv")

#  Drop

dat_growth = @filter(dat_growth, Volume_Difference > 0)

# Plot

plot(dat_growth.Volume_First, dat_growth.Volume_Second, seriestype=:scatter)

plot(dat_growth.Volume_First, dat_growth.Volume_Difference, seriestype=:scatter)

# Initialize parameters.

#  Question: is there a good reason to define b_0 sooner rather than later? Might fit better in the minimization block.

b_0 = [100, 1, 1000, 0.5]
t_0 = 1
T_0 = 100
n_0 = 1000

# Get draws.

# Questions:
#  - Does the base in HaltonSeq matter?
#  - Does the absence of MATLAB's "leap" argument matter?
#  - Am I confusing the T and n arguments?
#  - Would pre-allocation be useful?

Base_Halton = 3
Draws_Halton = HaltonSeq(Base_Halton, T_0, n_0)
Vec_Halton = collect(Draws_Halton)
Out_Halton = Quantile(Normal(0, 1), Vec_Halton)

# Minimize.
