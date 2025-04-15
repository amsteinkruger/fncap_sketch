# Estimate a toy model on fake data.

# Packages

using Tidier
using Random
using Plots
using FastHalton
using Distributions
using Optim

# Seed.

Random.seed!(0112358)

# Set up fake data.
#  Omit complications: negative growth, uneven counts of periods between observations, . . ..

obs = 100
mean = 100
sd = 25

vec_volume_1 = 
@chain begin
    Normal(mean, sd)
    Truncated(0, Inf)
    rand(obs)
end

vec_volume_0 = 
@chain begin
    Normal(mean / 10, sd / 10)
    Truncated(0, Inf)
    rand(obs)
    *(-1)
    + vec_volume_1
end

dat = DataFrame(volume_0 = vec_volume_0, volume_1 = vec_volume_1, volume_d = vec_volume_1 - vec_volume_0)

# Set up parameters. 

b = [1, 1, 100, 0.5]
t = 1
T = 10
skip = 1000

# Initialize a data object for simulation.

dat_initial = zeros(obs, T)
# dat_initial[:, 1] = dat.volume_0

# Get noise. 

function fun_halton(base_halton, mean_halton, sd_halton, obs, T, skip)

    base_halton = base_halton
    draws_halton = HaltonSeq(base_halton, obs * T, skip)
    vector_halton_0 = collect(draws_halton)
    vector_halton_1 = quantile(Normal(mean_halton, sd_halton), vector_halton_0)
    matrix_halton = reshape(vector_halton_1, obs, T)

    matrix_halton

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

function fun_objective(b = b, t = t, T = T, dat_initial = dat_initial, dat_noise = dat_noise, dat_change = dat.volume_d)
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

# Visualize.

#  Set up single dataframe of observations and predictions. This could be tidier.

dat_predictions = copy(dat)
dat_predictions[!, :volume_1_hat] = dat_predictions.volume_0 + vec(fun_growth(b_hat))
dat_predictions[!, :volume_d_hat] = vec(fun_growth(b_hat))

#  Plot second volume on first volume.

plot(dat_predictions.volume_0, dat_predictions.volume_1, seriestype=:scatter)
plot!(dat_predictions.volume_0, dat_predictions.volume_1_hat, seriestype=:scatter)

#  Plot growth on first volume.

plot(dat_predictions.volume_0, dat_predictions.volume_d, seriestype=:scatter)
plot!(dat_predictions.volume_0, dat_predictions.volume_d_hat, seriestype=:scatter)
