# Estimate a model on data from an earlier case study.
#  This runs on a single, hand-selected site class as in MATLAB reference material.

# Packages

using Tidier
using DataFrames
using Random
using Plots
using FastHalton
using Distributions
using Optim

# Data

#  Read, then add names and mean volume by age.

dat =
    @chain begin
        "matlab/Growth_Data_SC_clean.csv"
        read_csv(col_names = false)
        rename(Dict("Column1" => "Age", "Column2" => "Volume", "Column3" => "Class"))
        @filter(Class == 4)
        @group_by(Age)
        @mutate(MeanVolumeAge = (sum(Volume) / n()))
        @ungroup
    end

# Halton 

# Seed.

Random.seed!(0112358)

## Break for 2025/04/25

#  Parameters.

base_halton = 3
mean_halton = 0
sd_halton = 1
draws_halton = 1000
skip_halton = 1000

# Get noise. 

function fun_halton(base=base_halton, mean=mean_halton, sd=sd_halton, draws=draws_halton, T=T, skip=skip_halton)

    seq = HaltonSeq(base, draws * T, skip)
    vector_0 = collect(seq)
    vector_1 = quantile(Normal(mean, sd), vector_0)
    matrix = reshape(vector_1, (draws, T))

    return (matrix)

end

dat_noise = fun_halton()

# Get a growth function.

#  Parameters.

b = [1, 1, 100, 0.5]
t = 1
T = maximum(dat.age_1)

dat_initial = zeros(draws_halton, T)

#  Function.

function fun_growth(b=b, t=t, T=T, dat_simulation=dat_initial, dat_noise=dat_noise)

    dat_simulation[:, 1] .= b[1]
    # Run the simulation.
    for i in 2:T
        w_t = dat_simulation[:, i-1]
        u_t = dat_noise[:, i-1]
        dat_simulation[:, i] = w_t .* (b[2] ./ (1 .+ ((b[2] - 1) ./ b[3]) .* w_t)) .* 1 # exp.(b[4] .* u_t .- (1 / 2) * b[4] .^ 2)
    end
    # Get means over simulations.
    out = mean(dat_simulation, dims=1)

    return (out)

end

# Test the growth function.

fun_growth()

# Get an objective function to wrap the growth function.

function fun_objective(b=b, t=t, T=T, dat_initial=dat_initial, dat_noise=dat_noise, dat=dat)

    dat_prediction = DataFrame(age_1=1:T, prediction=Array(fun_growth(b, t, T, dat_initial, dat_noise)')[:, 1]) # Awful.
    dat = leftjoin(dat, dat_prediction, on=:age_1)
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

dat_prediction = DataFrame(age_1=1:T, volume_1_hat=Array(fun_growth(b_hat)')[:, 1])
dat_out = leftjoin(dat, dat_prediction, on=:age_1)
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
