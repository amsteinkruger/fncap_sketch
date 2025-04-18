# Estimate a toy model on fake data.

# Packages

using Tidier
using DataFrames
using Random
using Plots
using FastHalton
using Distributions
using Optim

# Seed.

Random.seed!(0112358)

# Set up fake data. Omit negative growth.

#  Parameters for fake data

obs = 100
age_lambda = 1
age_minimum = 1
interval_lambda = 10
growth_factor = 10
growth_power = 1 / 2
growth_mu = 0
growth_sigma = 5

#  Ages

vec_age_0 = rand(Poisson(age_lambda), obs) .+ age_minimum
vec_age_d = rand(Poisson(interval_lambda), obs)
vec_age_1 =  vec_age_0 + vec_age_d

#  Noise

vec_noise_0 = abs.(rand(Normal(growth_mu, growth_sigma), obs))
vec_noise_1 = abs.(rand(Normal(growth_mu, growth_sigma), obs))

#  Volume

vec_volume_0 = (vec_age_0 .* growth_factor) .^ growth_power + vec_noise_0
vec_volume_d = (vec_age_1 .* growth_factor) .^ growth_power - (vec_age_0 .* growth_factor) .^ growth_power + vec_noise_1
vec_volume_1 = vec_volume_0 + vec_volume_d

#  Bring it all together.

dat = 
DataFrame(
    age_0 = vec_age_0,
    age_1 = vec_age_1,
    age_d = vec_age_d,
    volume_0 = vec_volume_0, 
    volume_1 = vec_volume_1, 
    volume_d = vec_volume_d
)

# Set up parameters. 

#  Growth parameters

b = [1, 1, 100, 0.5]
t = 1
T = maximum(dat.age_1)

#  Halton parameters

base_halton = 3
mean_halton = 0
sd_halton = 1
draws_halton = 1000
skip_halton = 1000

# Get a data object for simulation.

dat_initial = zeros(draws_halton, T)

# Get noise. 

function fun_halton(base = base_halton, mean = mean_halton, sd = sd_halton, draws = draws_halton, T = T, skip = skip_halton)

    seq = HaltonSeq(base, draws * T, skip)
    vector_0 = collect(seq)
    vector_1 = quantile(Normal(mean, sd), vector_0)
    matrix = reshape(vector_1, (draws, T))
    
    return(matrix)

end

dat_noise = fun_halton()

# Get a growth function.

function fun_growth(b = b, t = t, T = T, dat_simulation = dat_initial, dat_noise = dat_noise)

    dat_simulation[:, 1] .= b[1]
    # Run the simulation.
    for i in 2:T
        w_t = dat_simulation[:, i - 1]
        u_t = dat_noise[:, i - 1]
        dat_simulation[:, i] = w_t .* (b[2] ./ (1 .+ ((b[2] - 1) ./ b[3]) .* w_t)) .* 1 # exp.(b[4] .* u_t .- (1 / 2) * b[4] .^ 2)
    end
    # Get means over simulations.
    out = mean(dat_simulation, dims = 1)

    return(out)

end

# Test the growth function.

fun_growth()

# Get an objective function to wrap the growth function.

function fun_objective(b = b, t = t, T = T, dat_initial = dat_initial, dat_noise = dat_noise, dat = dat)

    dat_prediction = DataFrame(age_1 = 1:T, prediction = Array(fun_growth(b, t, T, dat_initial, dat_noise)')[:, 1]) # Awful.
    dat = leftjoin(dat, dat_prediction, on = :age_1)
    sum(abs.(dat.volume_1 - dat.prediction))
    
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

dat_prediction = DataFrame(age_1 = 1:T, volume_1_hat = Array(fun_growth(b_hat)')[:, 1])
dat_out = leftjoin(dat, dat_prediction, on = :age_1)
dat_out[!, :volume_d_hat] = dat_out.volume_1_hat - dat_out.volume_0

#  Plot second volume on first volume.

plot(dat_out.volume_0, dat_out.volume_1, seriestype=:scatter)
plot!(dat_out.volume_0, dat_out.volume_1_hat, seriestype=:scatter)

#  Plot growth on first volume.

plot(dat_out.volume_0, dat_out.volume_d, seriestype=:scatter)
plot!(dat_out.volume_0, dat_out.volume_d_hat, seriestype=:scatter)

# Plot second volume on second age.

plot(dat_out.age_1, dat_out.volume_1, seriestype=:scatter)
plot!(dat_out.age_1, dat_out.volume_1_hat, seriestype=:scatter)

#  Plot growth on second age.

plot(dat_out.age_1, dat_out.volume_d, seriestype=:scatter)
plot!(dat_out.age_1, dat_out.volume_d_hat, seriestype=:scatter)
