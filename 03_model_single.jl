# Estimate a model for a subset of growth data from Douglas fir plots in FIA DB.

# Packages

using Tidier
using Random
using Plots
using FastHalton
using Distributions
using Optim

# Data

#  Read

dat = read_csv("output/dat_or_intermediate.csv")

#  Keep observations with negative growth (or don't).

dat = @chain dat begin
    @mutate(VOLCFNET_D = VOLCFNET_1 - VOLCFNET_0,
            MEASYEAR_D = MEASYEAR_1 - MEASYEAR_0)
    @filter(VOLCFNET_D == VOLCFNET_D)
    # @filter(VOLCFNET_D > 0)
end

# Set up inputs to a growth function for minimization.

# Set parameters. Of these, T needs to be set up for functional programming. (Or all of them do, depending.)

b = [100, 1, 100, 0.5]
t = 1
# T = maximum(dat.MEASYEAR_D) 
T = 10
skip = 1000

# Set site class code. This introduces the first dimension for functional programming to iterate over.
#  i.e. meaningful changes start here
#  note Julia's native map(f, x) and f.(x); then TidierIteration all about piping

dat_3 = @filter(dat, SITECLCD_1 == 3) 

# Get length.

par_count = nrow(dat_3) 

# Initialize a data object for simulation. Note that the first commented-out line reappears in the objective function.

W_sim = zeros(par_count, T)

# W_sim[:, 1] = W_sim[:, 1] .+ b[1] # This follows code for MATLAB in initializing growth with a parameter. Has to be in the function.

W_sim[:, 1] = reshape(dat_3.VOLCFNET_0, par_count, 1) # This grabs observed initial volume. Does not have to be in the function.

# Get outcomes for comparison.

dat_3_end = dat_3.VOLCFNET_1

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
    T = T # might be better to take data as an argument, then push more operations with dimensions as arguments into here
    W_sim = initialization
    Out_Halton_Matrix = noise

    # Tag b_1 into the initialized data. 

    # W_sim[:, 1] = W_sim[:, 1] .+ b_1
    # W_sim[:, 1] = zeros(par_count, 1) .+ b_1 # Alternative for scratch work.

    # Run the simulation.
    for i in 2:T
        w_t = W_sim[:, i - 1]
        u_t = Out_Halton_Matrix[:, i - 1]
        W_sim[:, i] = w_t .* (b_2 ./ (1 .+ ((b_2 - 1) ./ b_3) .* w_t)) .* exp.(b_4 .* u_t .- (1 / 2) * b_4 .^ 2)
    end

    # Note that this is the spot to add manipulations following MATLAB code to get identical results.

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

    out = sum((dat_3_end - dat_out) .^ 2) # Fix this global reference.

end

# Minimize the wrapper function. 

dat_results = optimize(fun_growth_wrapper, b)

# Back parameters out of minimization.

dat_b = Optim.minimizer(dat_results)

dat_sum_sq = Optim.minimum(dat_results)

# Visualize

#  Set up single dataframe of observations and predictions. This could be tidier.

#   might need to be tidier to support dataframe of dataframes pulling from functional format

dat_predictions_col = fun_growth(dat_b, t, T, W_sim, dat_noise)

dat_predictions = copy(dat_3)

dat_predictions[!, :predictions] = vec(dat_predictions_col)

dat_predictions[!, :Volume_Difference_Predicted] = dat_predictions.predictions - dat_predictions.VOLCFNET_0

#  Plot second volume on first volume.

plot(dat_predictions.VOLCFNET_0, dat_predictions.VOLCFNET_1, seriestype=:scatter)
plot!(dat_predictions.VOLCFNET_0, dat_predictions.predictions, seriestype=:scatter)

#  Plot growth on first volume.

plot(dat_predictions.VOLCFNET_0, dat_predictions.VOLCFNET_D, seriestype=:scatter)
plot!(dat_predictions.VOLCFNET_0, dat_predictions.Volume_Difference_Predicted, seriestype=:scatter)
