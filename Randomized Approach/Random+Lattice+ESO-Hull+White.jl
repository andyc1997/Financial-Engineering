using Random, Distributions, StatsBase

function counting(m, d)
    return(zeros(m, m, d))
end

function buckets_generation(parameters, dim)
    # Get Parameters
    S = parameters["S"]
    r = parameters["r"]
    delta = parameters["delta"]
    sigma = parameters["sigma"]
    tau = parameters["tau"]
    m = dim["m"]
    d = dim["d"]
    
    # h: step size, time_mesh: time
    h = tau / d
    
    # Geometric Brownian Motion
    min_stock_price = 0.0001
    max_stock_price = 3*S
    stock_price_mesh = [min_stock_price:(max_stock_price - min_stock_price)/(m - 1):max_stock_price;] .+ 
    zeros(dim["m"], dim["d"])
    
    return(stock_price_mesh)
end

function help_map_index(S, buckets)
    # S is a number at j th column 
    # buckets also j th column of buckets matrix
    min_index = []
    for s in S
        append!(min_index, findmin(abs.(s .- buckets))[2])
    end 
    return(transpose(min_index))
end 

function map_index(stock_price_path, buckets, trial, d)
    # Putting simulated stock price to its closest bucket.
    count = zeros(trial, d)
    for i = 1:d
        count[:, i] = help_map_index(stock_price_path[:, i], buckets[:, i])
    end  
    return(count)
end 

function gaussian_rand(trial, dim)
    # Sampling from Gaussian Distribution
    d = dim["d"]
    gaussian_rand = randn(trial, d)
    return(gaussian_rand, -gaussian_rand)
end

function lattice_generation(parameters, buckets, dim, gaussian_rand, trial)
    # Get Parameters
    S = parameters["S"]
    r = parameters["r"]
    delta = parameters["delta"]
    sigma = parameters["sigma"]
    tau = parameters["tau"]
    m = dim["m"]
    d = dim["d"]
    
    # h: step size, time_mesh: time
    eps = 10 ^ -20
    h = tau / d
    time_mesh = transpose([h:h:tau;])
    count = counting(m, d)
    
    # Simulation
    stock_price_path = zeros(trial, d)
    
    # Geometric Brownian Motion (Vectorization)
    stock_price_path[:, 1] .= S .+ (r .- delta) .* S .* h .+ (sigma .* sqrt(h) .* S) .* gaussian_rand[:, 1]
    for i = 2:d
        stock_price_path[:, i] .= stock_price_path[:, i - 1] .+ (r .- delta) .* h .* stock_price_path[:, i - 1] .+ 
        (sigma .* sqrt(h)) .* stock_price_path[:, i - 1] .* gaussian_rand[:, i]
    end
    
    # Lattice generation
    stock_price_path = map_index(stock_price_path, buckets, trial, d)
    
    for i = 1:(d - 1)  
        # From layer i to layer i + 1
        for (from, to) in zip(stock_price_path[:, i], stock_price_path[:, i + 1])
            count[floor(Int64, from), floor(Int64, to), i] += 1
        end 
        count[:, :, i] ./= (sum(count[:, :, i], dims = 2) .+ eps)
    end 
    
    # From first layer to root
    count_root = countmap(stock_price_path[:, 1])
    count_keys = collect(keys(count_root))
    count_val = [count_root[i] for i in count_keys]
    count_root = zeros(1, m)
    
    for (key, val) in zip(count_keys, count_val)
        count_root[1, floor(Int64, key)] = val
    end
    count_root ./= (sum(count_root, dims = 2) .+ eps)
    
    # Transition Tensor
    return(count, count_root)
end 

function discounting_process(parameters, buckets, lattice_probability, root_probability, dim)
    # Get Parameters
    S = parameters["S"]
    K = parameters["K"]
    r = parameters["r"]
    delta = parameters["delta"]
    sigma = parameters["sigma"]
    tau = parameters["tau"]
    vest = parameters["vest"]
    M = parameters["M"]
    e = parameters["e"]
    m = dim["m"]
    d = dim["d"]
    
    # h: step size, time_mesh: time
    h = tau / d

    # Payoff matrix
    payoff_mesh = zeros(m, d)
    for t = reverse(1:d)
        if t == d
            # When option is expired
            payoff_mesh[:, t] = max.(buckets[:, t] .- K, 0)
        else
            # Random sampling of parameters
            m_sample, e_sample = sample(M), sample(e)
            for i = 1:m
                # Probability weighted average payoff
                weighted_payoff = transpose(lattice_probability[i, :, t]) * payoff_mesh[:, t + 1]
                # American Call Payoff
#                 payoff_mesh[i, t] = max(buckets[i, t] - K, exp(-r * h) * weighted_payoff)
                # ESO payoff        
                if (t * h >= vest) && (buckets[i, t] >= K * m_sample)
                    payoff_mesh[i, t] = buckets[i, t] - K
                elseif (t * h >= vest) && (buckets[i, t] < K * m_sample)
                    payoff_mesh[i, t] = (1 - e_sample * h) * exp(-r * h) * weighted_payoff + 
                    e_sample * h * max(buckets[i, t] - K, 0)
                else
                    payoff_mesh[i, t] = (1 - e_sample * h) * exp(-r * h) * weighted_payoff
                end
            end
        end
    end
    
    return(exp(-r * h) *(root_probability * payoff_mesh[:, 1]))
end 

function cal_price(parameters, dim)
    # Complete Model
    buckets = buckets_generation(parameters, dim);
    trial = 10000
    
    gaussian_rand_1, gaussian_rand_2 = gaussian_rand(trial, dim)
    lattice_probability_1, root_probability_1 = lattice_generation(parameters, buckets, dim, gaussian_rand_1, trial)
    lattice_probability_2, root_probability_2 = lattice_generation(parameters, buckets, dim, gaussian_rand_2, trial)
    
    # V
    option_price_1 = discounting_process(parameters, buckets, lattice_probability_1, root_probability_1, dim)
    option_price_2 = discounting_process(parameters, buckets, lattice_probability_2, root_probability_2, dim)
    antithetic_price = 0.5 * (option_price_1 + option_price_2)
    
    return(antithetic_price)
end
