# Estimate a model for a subset of growth data from Douglas fir plots in FIA DB.

# Packages

using Tidier
using Random
using Plots
using FastHalton
using Distributions
using Optim

# Seed.

Random.seed!(0112358)

# Data

dat = @chain begin
    read_csv("output/dat_or_intermediate.csv")
    @mutate(VOLCFNET_D = VOLCFNET_1 - VOLCFNET_0,
            MEASYEAR_D = MEASYEAR_1 - MEASYEAR_0)
    @filter(VOLCFNET_D > 0) # Drop negative growth.
    @filter(SITECLCD_1 == 3) # Pick a site class code.
end

# Parameters

b = [100, 1, 100, 0.5]
t = 1
T = 10 # maximum(dat.MEASYEAR_D) 
skip = 1000
obs = nrow(dat) 

# Get a data object for simulation.

dat_initial = zeros(obs, T)

# Get noise. 

function fun_halton(Base_Halton, Mean_Halton, SD_Halton, obs, T, skip)

    Base_Halton = Base_Halton
    Draws_Halton = HaltonSeq(Base_Halton, obs * T, skip)
    Vec_Halton = collect(Draws_Halton)
    Out_Halton_Vector = quantile(Normal(Mean_Halton, SD_Halton), Vec_Halton)
    Out_Halton_Matrix = reshape(Out_Halton_Vector, obs, T)

    Out_Halton_Matrix

end

dat_noise = fun_halton(3, 0, 1, obs, T, skip)

# Get a growth function.

function fun_growth(b = b, t = t, T = T, initial = dat_initial, noise = dat_noise)

    # Declare objects. 
    b_1 = b[1]
    b_2 = b[2]
    b_3 = b[3]
    b_4 = b[4]
    t = t
    T = T 
    dat_simulation = initial
    dat_simulation[:, 1] .= b_1
    dat_noise = noise

    # Run the simulation.
    for i in 2:T
        w_t = dat_simulation[:, i - 1]
        u_t = dat_noise[:, i - 1]
        dat_simulation[:, i] = w_t .* (b_2 ./ (1 .+ ((b_2 - 1) ./ b_3) .* w_t)) .* exp.(b_4 .* u_t .- (1 / 2) * b_4 .^ 2)
    end

    # Sum values up to the full period of interest. 
    sum(dat_simulation, dims = 2)

end

# Test the growth function.

fun_growth()

# Get an objective function to wrap the growth function.

function fun_objective(b = b, t = t, T = T, dat_initial = dat_initial, dat_noise = dat_noise, dat_change = dat.VOLCFNET_D)
    b = b
    t = t
    T = T
    dat_initial = dat_initial
    dat_noise = dat_noise
    dat_change = dat_change

    abs(sum(dat_change - fun_growth(b, t, T, dat_initial, dat_noise)))

end

# Test the objective function.

fun_objective()

# Minimize the wrapper function. 

estimate = optimize(fun_objective, b)

# Back parameters out of minimization.

b_hat = Optim.minimizer(estimate)

measure = Optim.minimum(estimate)

# Visualize

dat_predictions = copy(dat)
dat_predictions[!, :VOLCFNET_1_HAT] = dat_predictions.VOLCFNET_0 + vec(fun_growth(b_hat))
dat_predictions[!, :VOLCFNET_D_HAT] = vec(fun_growth(b_hat))

#  Plot second volume on first volume.

plot(dat_predictions.VOLCFNET_0, dat_predictions.VOLCFNET_1, seriestype=:scatter)
plot!(dat_predictions.VOLCFNET_0, dat_predictions.VOLCFNET_1_HAT, seriestype=:scatter)

#  Plot growth on first volume.

plot(dat_predictions.VOLCFNET_0, dat_predictions.VOLCFNET_D, seriestype=:scatter)
plot!(dat_predictions.VOLCFNET_0, dat_predictions.VOLCFNET_D_HAT, seriestype=:scatter)

# Problem: the model is practically returning a scalar (' ~= 0) instead of a nice nonlinear curve (' > 0, '' < 0).
