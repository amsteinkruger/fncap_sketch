# Estimate a model or models for growth data from Douglas fir plots in FIA DB.

# So far: got a function to minimize. Next: minimization, functional programming for multiple models, visualization.

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

# Set parameters. 

b = [100, 1, 1000, 0.5]
t = 1
T = maximum(dat_growth.Year_Difference)
skip = 1000

# Get a function to minimize (actually do a bunch of other stuff first).

# Get data. 

dat_growth_3 = @filter(dat_growth, SITECLCD == 3) 

# Get length.
par_count = nrow(dat_growth_3)

# Initialize a data object for simulation.
W_sim = zeros(par_count, T)
W_sim[:, 1] = reshape(dat_growth_3.Volume_First, par_count, 1)
# W_sim[:, 1] = W_sim[:, 1] .+ b_1 # This follows the .m.

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

# Get function to minimize.

function fun_growth(b, t, T, par_count, initialization, noise, outcome)

    # Declare objects. This works, but doesn't seem right.
    b_1 = b[1]
    b_2 = b[2]
    b_3 = b[3]
    b_4 = b[4]
    t = t
    T = T

    # declare objects with stupid names
    W_sim = initialization
    Out_Halton_Matrix = noise

    # declare an object because ...
    # w_t = zeros(par_count, 1)
    # u_t = zeros(par_count, 1)

    # Run the simulation.
    for i in 2:T
        w_t = W_sim[:, i - 1]
        u_t = Out_Halton_Matrix[:, i - 1]
        W_sim[:, i] = w_t .* (b_2 ./ (1 .+ ((b_2 - 1) ./ b_3) .* w_t)) .* exp.(b_4 .* u_t .- (1 / 2) * b_4 .^ 2)
    end

    # Sum values up to the full period of interest. 
    W_sim2 = sum(W_sim, dims = 2)

    # Get residuals.
    W_out = outcome - W_sim2

    # return(W_out)

    # Get the sum of squared residuals?
    W_sq = W_out .^ 2
    W_sum_sq = sum(W_sq)

    return(W_sum_sq)

end

# Test function with initial values.

fun_growth(b, t, T, par_count, W_sim, dat_noise, dat_growth_3_end)

# Minimize.

# demo

f(x) = (1.0 - x[1])^2 + 100.0 * (x[2] - x[1]^2)^2

optimize(f, [0.0, 0.0])

# set up another function to pass parameters because that's idiomatic, I guess
# https://stackoverflow.com/questions/26158142/pass-additional-arguments-to-a-built-in-function

function fun_growth_wrapper(b)
    b = b
    fun_growth(b, t, T, par_count, W_sim, dat_noise, dat_growth_3_end) # object names are downright tragic
end

# Test wrapper function with initial values (this is silly, needs revision for order)

fun_growth_wrapper(b)

# actually minimize

dat_results = optimize(fun_growth_wrapper, b)

# get parameters back out of minimization

dat_b = Optim.minimizer(dat_results)

dat_sum_sq = Optim.minimum(dat_results) # that's a big fucking number

# Visualize?

# Compare to polynomials of order 1, 2, 3?

# turn into functional programming approach for each site productivity class
