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
T = maximum(dat_growth.Year_Difference)
skip = 1000
Draws = 1000

# Get draws.

# Questions:
#  - Does the base in HaltonSeq matter?
#  - Does the absence of MATLAB's "leap" argument matter?
#  - Am I confusing the T and n arguments?
#  - Would pre-allocation be useful?

# Note that Draws_Halton isn't related to Draws. Names are a work in progress.

Base_Halton = 3
Draws_Halton = HaltonSeq(Base_Halton, Draws * T, skip)
Vec_Halton = collect(Draws_Halton)
Out_Halton_Vector = quantile(Normal(0, 1), Vec_Halton)
Out_Halton_Matrix = reshape(Out_Halton_Vector, Draws, T)

# Get a function to minimize.

#  Arguments: b (to be optimized on?), data, p (t, T, observations in data, draws, Vec_Halton, Out_Halton)

# function name(argument)
#     operations
# end

function fun_growth_ms(dat, par_siteclcd) # worth going through to note global references that should be arguments for defensibility

    # par_siteclcd = par_siteclcd # It's clear that Julia requires declaration, not so clear why or how

    dat = dat_growth

    dat = dat[dat.SITECLCD .== par_siteclcd, :]

    par_count = nrow(dat_look)

    # kick halton draws into the function for u_t dimensions

    Base_Halton = 3
    Draws_Halton = HaltonSeq(Base_Halton, par_count * T, skip)
    Vec_Halton = collect(Draws_Halton)
    Out_Halton_Vector = quantile(Normal(0, 1), Vec_Halton)
    Out_Halton_Matrix = reshape(Out_Halton_Vector, par_count, T)

    # using 2 as a placeholder for second element of b, ditto 3, 4

    W_sim = zeros(par_count, T)
    # W_sim[:, 1] = W_sim[:, 1] .+ 1 .* 3

    # Instead,
    W_sim[:, 1] = reshape(dat.Volume_First, par_count, 1)

    for i in 2:T
        w_t = W_sim[:, i - 1]
        u_t = Out_Halton_Matrix[:, i - 1]
        W_sim[:, i] = w_t .* (2 ./ (1 .+ ((2 - 1) ./ 3) .* w_t)) .* exp.(4 .* u_t .- (1 / 2) * 4 .^ 2)
    end

    W_sim2 = sum(W_sim, dims = 2)

    W_out = W_sim2 - dat.Volume_Second

end

fun_growth_ms(dat_growth, 3)

# Minimize.
