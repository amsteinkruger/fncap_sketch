# Estimate a model or models for growth data from Douglas fir plots in FIA DB.

# Done: define and minimize a growth function for a particular site productivity class.
# To do: transition into functional programming to estimate a model for each site class.

# Packages

using Tidier
using Random
using Plots
using FastHalton
using Distributions
using Optim

# Data

#  Read

dat_growth = read_csv("output/dat_or_differences.csv")

#  Drop

dat_growth = @filter(dat_growth, Volume_Difference > 0)

# Plot

plot(dat_growth.Volume_First, dat_growth.Volume_Second, seriestype=:scatter)

plot(dat_growth.Volume_First, dat_growth.Volume_Difference, seriestype=:scatter)

# Set up inputs to a growth function for minimization.

# Set parameters. 

b = [100, 1, 100, 0.5]
t = 1
T = maximum(dat_growth.Year_Difference)
skip = 1000

# Get data. 

dat_growth_3 = @filter(dat_growth, SITECLCD == 3) 

# Get length.

par_count = nrow(dat_growth_3)

# Initialize a data object for simulation.

W_sim = zeros(par_count, T)
W_sim[:, 1] = reshape(dat_growth_3.Volume_First, par_count, 1)
# W_sim[:, 1] = W_sim[:, 1] .+ b_1 # This follows the .m in initializing growth with a parameter.

# Get outcomes for comparison.

dat_growth_3_end = dat_growth_3.Volume_Second

# Get noise. 

function fun_halton(Base_Halton, Mean_Halton, SD_Halton, par_count, T, skip)

    Base_Halton = Base_Halton
    Draws_Halton = HaltonSeq(Base_Halton, par_count * T, skip)
    Vec_Halton = collect(Draws_Halton)
    Out_Halton_Vector = quantile(Normal(Mean_Halton, SD_Halton), Vec_Halton)
    Out_Halton_Matrix = reshape(Out_Halton_Vector, par_count, T)

    Out_Halton_Matrix

end

dat_noise = fun_halton(3, 0, 1, par_count, T, skip)

# Get an objective function.

function fun_growth(b, t, T, initialization, noise)

    # Declare objects. This works, but doesn't seem right.
    b_1 = b[1]
    b_2 = b[2]
    b_3 = b[3]
    b_4 = b[4]
    t = t
    T = T
    W_sim = initialization
    Out_Halton_Matrix = noise

    # Run the simulation.
    for i in 2:T
        w_t = W_sim[:, i - 1]
        u_t = Out_Halton_Matrix[:, i - 1]
        W_sim[:, i] = w_t .* (b_2 ./ (1 .+ ((b_2 - 1) ./ b_3) .* w_t)) .* exp.(b_4 .* u_t .- (1 / 2) * b_4 .^ 2)
    end

    # Sum values up to the full period of interest. 
    W_sim2 = sum(W_sim, dims = 2)

    return(W_sim2)

end

# Test function with initial values.

fun_growth(b, t, T, W_sim, dat_noise)

# Set up a wrapper function for minimization.

function fun_growth_wrapper(b)
    b = b
    dat_out = fun_growth(b, t, T, W_sim, dat_noise) 

    out = sum((dat_growth_3_end - dat_out) .^ 2) # Mind the global reference to dat_growth_3_end.

end

# Minimize the wrapper function. 

dat_results = optimize(fun_growth_wrapper, b)

# Back parameters out of minimization.

dat_b = Optim.minimizer(dat_results)

dat_sum_sq = Optim.minimum(dat_results)

# Visualize

#  Set up single dataframe of observations and predictions. This could be tidier.

dat_predictions_col = fun_growth(dat_b, t, T, W_sim, dat_noise)

dat_predictions = copy(dat_growth_3)

dat_predictions[!, :predictions] = vec(dat_predictions_col)

dat_predictions[!, :Volume_Difference_Predicted] = dat_predictions.predictions - dat_predictions.Volume_First

#  Plot second volume on first volume.

plot(dat_predictions.Volume_First, dat_predictions.Volume_Second, seriestype=:scatter)
plot!(dat_predictions.Volume_First, dat_predictions.predictions, seriestype=:scatter)

#  Plot growth on first volume.

plot(dat_predictions.Volume_First, dat_predictions.Volume_Difference, seriestype=:scatter)
plot!(dat_predictions.Volume_First, dat_predictions.Volume_Difference_Predicted, seriestype=:scatter)

# Transition into functional programming to estimate a model for each site productivity class.
