# Estimate a model for a subset of growth data from Douglas fir plots in FIA DB.

# Packages

using Tidier
using DataFrames
using Random
using Plots
using FastHalton
using Distributions
using Optim
using LeastSquaresOptim

# Seed.

Random.seed!(0112358)

# Data

# The selection simplifies renaming, but drops initial observations. Keep those to test marginal effects in STDAGE.

dat = 
    @chain begin
        read_csv("output/dat_or_intermediate.csv")
        @filter(VOLCFNET_1 - VOLCFNET_0 > 0) # Drop negative growth.
        @filter(SITECLCD_1 == 3) # Pick a site class code.
        @select(STDAGE_1,
                SITECLCD_1,
                VOLCFNET_1)
        dropmissing()
        rename(Dict("STDAGE_1" => "Age", "SITECLCD_1" => "Class", "VOLCFNET_1" => "Volume"))
end

age_max = maximum(skipmissing(dat.Age))

# Seed

Random.seed!(0112358)

# Halton

#  Set parameters.

base_halton = 2
dim_1_halton = 10000 # Draws
dim_2_halton = age_max # Maximum Stand Age
skip_halton = 1000

#  Get draws. 

dat_noise =
    @chain begin
        HaltonSeq(base_halton, dim_1_halton * dim_2_halton, skip_halton) # Note docs reverse arguments.
        collect
        quantile(Normal(), _)
        reshape((dim_1_halton, dim_2_halton))
        shuffle
end

# Get functions for growth and optimization.

#  Set parameter guesses.

b = [3.0, 1.5, 7.5, 7.5] # Scaling for easier comparison.

#  Initialize a data object to loop over.

dat_initial = zeros(dim_1_halton, dim_2_halton)

#  Set up the growth function.

function fun_growth(b = b, T = age_max, dat_simulation = dat_initial, dat_noise = dat_noise)

    b = b .* [1e1, 1e0, 1e3, 1e-1] # This rescales parameter guesses.

    dat_simulation[:, 1] .= b[1]

    # Run the simulation.

    for i in 2:T
        w_t = dat_simulation[:, i-1]
        u_t = dat_noise[:, i-1]
        dat_simulation[:, i] = w_t .* (b[2] ./ (1 .+ ((b[2] - 1) ./ b[3]) .* w_t)) .* exp.(b[4] * u_t .- (1 / 2) * b[4] ^ 2)
    end

    # Get means over simulations.

    out = mean(dat_simulation, dims=1)

    return(out)

end

#  Test the growth function.

fun_growth()


#  Set up the objective function.

function fun_objective(b = b, T = age_max, dat_initial = dat_initial, dat_noise = dat_noise, dat_observed = dat)

    dat_prediction = DataFrame(Age = 1:T, Volume_Hat = Array(fun_growth(b, T, dat_initial, dat_noise)')[:, 1]) # Awful.
    dat = leftjoin(dat_observed, dat_prediction, on = :Age)
    out = sum(skipmissing((dat.Volume - dat.Volume_Hat) .^ 2))
    # out = sum(abs.(dat.Volume - dat.Volume_Hat))
    # Other loss functions go here, with a selection function defaulting to sum of squares.

    return(out)

end

# Test the objective function.

fun_objective()

# Optimize. 

estimate = optimize(fun_objective, b, LevenbergMarquardt())

# Get estimated parameters.

b_hat = estimate.minimizer
measure = estimate.ssr

# Visualize.

#  Set up single dataframe of observations and predictions. This could be tidier.

dat_prediction = DataFrame(Age = 1:age_max, Volume_Hat = Array(fun_growth(b_hat)')[:, 1])
dat_out = leftjoin(dat, dat_prediction, on =:Age)

#  Plot observed and predicted volumes on stand age.

plot(dat_out.Age, dat_out.Volume, seriestype=:scatter)
plot!(dat_out.Age, dat_out.Volume_Hat, seriestype=:scatter)
