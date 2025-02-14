# Packages

using Tidier
using Random
using Plots
using FastHalton
using Distributions

# Data

#  Read

dat_growth = read_csv("output/dat_or_differences.csv")

#  Drop

dat_growth = @filter(dat_growth, Volume_Difference > 0)

# Plot

plot(dat_growth.Volume_First, dat_growth.Volume_Second, seriestype=:scatter)

plot(dat_growth.Volume_First, dat_growth.Volume_Difference, seriestype=:scatter)

# Seed

#  Question: is this pointless because Julia requires seeds to be defined by function call (unlike MATLAB and R)?

Random.seed!(112358)

# Initialize parameters.

#  Question: is there a good reason to define b_0 sooner rather than later? Might fit better in the minimization block.

# names names names names names

b = [100, 1, 1000, 0.5]
t = 1
T = 100
n = 1000
Draws = 1000

# Get draws.

# Questions:
#  - Does the base in HaltonSeq matter?
#  - Does the absence of MATLAB's "leap" argument matter?
#  - Am I confusing the T and n arguments?
#  - Would pre-allocation be useful?

# Note that Draws_Halton isn't related to Draws. Names are a work in progress.

Base_Halton = 3
Draws_Halton = HaltonSeq(Base_Halton, T, n)
Vec_Halton = collect(Draws_Halton)
Out_Halton = quantile(Normal(0, 1), Vec_Halton)

# Get a function to minimize.

#  Arguments: b (to be optimized on?), data, p (t, T, observations in data, draws, Vec_Halton, Out_Halton)

# function name(argument)
#     operations
# end

function fun_growth_ms(dat, par_siteclcd)

    par_siteclcd = par_siteclcd # It's clear that Julia requires declaration, not so clear why or how

    # Can't tell why the Tidier approach doesn't work, and it's not worth figuring out.
    # data2 = @filter(data, SITECLCD == par_siteclcd) 

    dat_look = dat[dat.SITECLCD .== par_siteclcd, :]

    par_count = nrow(dat_look)

    # mat_w_obv = zeros(par_count, 3)
    # mat_w_gen = zeros(par_count, 1)

    # bunch of loops just to filter on site class (still assuming that's SITECLCD), so dat_look <=> W_obv in .m

    W_obv = dat_look

    W_sim = [3 * ones(Draws, 1), zeros(Draws, T - 1)]

    # for loop with counts

    # for i = 1:nrow(data)
    #     if 

    W_sim

end

fun_growth_ms(dat_growth, 3)

# Minimize.
