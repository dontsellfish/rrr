# Task: http://statmod.ru/wiki/_media/study:fall2022:intror:task2-8.pdf
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("latex2exp"))


sum_of_powers <- function(vec, sample_mean, power) {
    sum((vec - sample_mean) ^ power) / length(vec)
}

func_S <- function(vec) {
    sample_mean <- mean(vec)
    sum_of_powers(vec, sample_mean, 3) / (sum_of_powers(vec, sample_mean, 2)) ^ 1.5 
}

func_K <- function(vec) {
    sample_mean <- mean(vec)
    sum_of_powers(vec, sample_mean, 4) / (sum_of_powers(vec, sample_mean, 2)) ^ 2
}

func_JB <- function(vec) {
    length(vec) / 6 * (func_S(vec) ^ 2 + (func_K(vec) - 3) ^ 2 / 4)
}

perform_multiple_JB <- function(count, n, generator) {
    sapply(seq_len(count), function(x) func_JB(generator(n)))
}


# PART 1
demonstrate_JB_histogram_against_chisq <- function(n, filename=sprintf("plot_part_1_%d.jpg", n), count=1000, generator=rnorm, save=TRUE) {
    if (save) {
        jpeg(filename, width = 800, height = 1200)
    }

    h <- hist(perform_multiple_JB(count, n, generator), freq = FALSE, xlab = "JB",
            main = TeX(sprintf("Histogram of JB for $n=%d$ (The red line is $\\chi^2(2)$, %d distributions)", n, count)))
    chisq_x_axis <- seq(0, 10000, 0.01)
    lines(chisq_x_axis, dchisq(chisq_x_axis, 2), col = "red", lwd = 2)

    if (save) {
        dev.off()
    }
}

# PART 2
demostrate_JB_growth_for_unnormalized_vectors <- function(max_n, filename=NULL, step=500, mean_of=10,
    generator=function(n) runif(n,-1,1), dist_name="U[-1, 1]", dist_filename="uniform", save=TRUE) {
    get_mean_of_multiple_JBs <- function(n) {
        mean(perform_multiple_JB(mean_of, n, generator))
    }

    if (save) {
        if (is.null(filename)) {
            filename <- sprintf("plot_part_2_%s.jpg", dist_filename)
        }
        jpeg(filename, width = 800, height = 1200)
    }

    x_axis <- seq(step, max_n, step)
    plot(x_axis, sapply(x_axis, get_mean_of_multiple_JBs), type = "l",
        xlab = "n", ylab = sprintf("JB (mean of %d distributions)", mean_of),
        main = sprintf("Growth of JB for %s distribution", dist_name)
    ) # in this plot sapply is necessary, bc otherwise get_mean_of_multiple_JBs(x_axis) is a mean of means

    if (save) {
        dev.off()
    }
}

set.seed(777)

demonstrate_JB_histogram_against_chisq(500)
demonstrate_JB_histogram_against_chisq(5000)
demonstrate_JB_histogram_against_chisq(10000)

max_n <- 20000

demostrate_JB_growth_for_unnormalized_vectors(max_n = max_n)
demostrate_JB_growth_for_unnormalized_vectors(max_n = max_n,
    generator = function(n) rbinom(n, size = 10, prob = 0.3),
    dist_name = "Bin(100, 0.3)", dist_filename = "binomial"
)
demostrate_JB_growth_for_unnormalized_vectors(max_n = max_n,
    generator = function(n) rexp(n, rate = pi),
    dist_name = "Exp(pi)", dist_filename = "exponential"
)
