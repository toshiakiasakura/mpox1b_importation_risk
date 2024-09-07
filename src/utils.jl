using Accessors
using CSV
using Dates
using DataFrames
using DataFramesMeta
using Distributions
using Evolutionary
using Pipe
using Plots

# Population size in 2024 from UN data.
N_COD = 1.09276265 * 1e8
N_BDI = 1.4048 * 1e7
N_UGA = 5.0015 * 1e7
N_KEN = 5.6433 * 1e7
N_RWA = 1.4256 * 1e7
N_5cnt = N_COD + N_BDI + N_UGA + N_KEN + N_RWA

"""
# Args
- `m_out` : Daily travel volume (already scaled to daily).
- `π_ij`: Proportion of travellesr within a country.
"""
function calculate_pa_given_daily_travel(m_out, N_catch; Td=10, π_ij=1 )
    return m_out * π_ij / N_catch * Td
end

function lklh_one_country(Ot, Ct, pa)
    if Ct < 1
        return -Inf
    end

    bin = Binomial(Ct, pa)
    return logpdf(bin, Ot)
end

function lklh_multi_country(Ct, p_lis::Vector, O_lis::Vector)
    ll = 0
    for (p, O) in zip(p_lis, O_lis)
        ll += lklh_one_country(O, Ct, p)
    end
    return ll
end

function optimise_Ct(p_lis::Vector, O_lis::Vector)
    # Prepare initial seeds for DE.
    m = 1000
    r = 1.1
    negbin = NegativeBinomial(r, r / (m + r))
    n_pop = 500
    x0 = [[Float64(xx)] for xx in rand(negbin, n_pop)]

    nlklh(Ct) = -lklh_multi_country(
        round(Int, Ct[1]),
        p_lis, O_lis)

    res = Evolutionary.optimize(
        nlklh,
        BoxConstraints([5.0], [Inf]),
        x0, DE(; populationSize=n_pop, F=0.9)
    )
    profile_likelihood_Ct(res, p_lis, O_lis)
    return res
end

function profile_likelihood_Ct(res, p_lis, O_lis)
    Ct_m = round(Int, Evolutionary.minimizer(res)[1])
    mle = lklh_multi_country(Ct_m, p_lis, O_lis)

    Ct_l = Inf
    Ct_h = Inf
    Δ_l = Inf
    Δ_h = Inf

    for i in 1:1_000_000
        ll_l = lklh_multi_country(Ct_m - i, p_lis, O_lis)
        Δ_l_tmp = abs(mle - ll_l - 1.96)
        if Δ_l_tmp < Δ_l
            Ct_l = Ct_m - i
            Δ_l = Δ_l_tmp
        end
        if (mle - ll_l - 2.0 > 0)
            break
        end
    end
    for i in 1:1_000_000
        ll_h = lklh_multi_country(Ct_m + i, p_lis, O_lis)
        Δ_h_tmp = abs(mle - ll_h - 1.96)
        if Δ_h_tmp < Δ_h
            Ct_h = Ct_m + i
            Δ_h = Δ_h_tmp
        end
        if (mle - ll_h - 2.0 > 0)
            break
        end
    end
    display("$Ct_m ($Ct_l, $Ct_h)")
end

function add_pa_and_observed_flag(df::DataFrame, N_catch)
    df_new = @chain df begin
        @transform :m_daily = :Volume ./30
        @transform :pa_7 =
            calculate_pa_given_daily_travel.(:m_daily, N_catch; Td=7)
        @transform :pa_10 =
            calculate_pa_given_daily_travel.(:m_daily, N_catch; Td=10)
        @transform :O1 = ifelse.(:Country .== "Sweden", 1, 0)
        @transform :O2 = ifelse.(
                in.(:Country, Ref(["Sweden", "Thailand"])),
                1, 0)
        @orderby -:Volume
    end
    return df_new
end

function estimate_four_patterns(df::DataFrame)
    df_top = df[1:n_top, :]
    display("Td=7, Sweden, Sweden and Thailand")
    optimise_Ct(df_top.pa_7, df_top.O1)
    optimise_Ct(df_top.pa_7, df_top.O2)

    display("Td=10, Sweden, Sweden and Thailand")
    optimise_Ct(df_top.pa_10, df_top.O1)
    optimise_Ct(df_top.pa_10, df_top.O2)
    return nothing
end