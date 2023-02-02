# The body weights of the chicks were measured at birth and every second day
# thereafter until day 20. They were also measured on day 21. There were four
# groups on chicks on different protein diets.

# weight: a numeric vector giving the body weight of the chick (gm).
# Time:   a numeric vector giving the number of days since birth when the
#         measurement was made.
# Chick:  an ordered factor, giving a unique identifier for the chick. The 
#         ordering of the levels groups chicks on the same diet together and
#         orders them according to their final weight (lightest to heaviest) 
#         within diet.
# Diet:   a factor with levels 1..4 indicating which experimental diet the chick
#         received.
#
# Task:
# 1. Write a dataframe with the following properties:
#    - Add a column with chicken mass increase speed
#    - Only top 15 chicks should be shown in final data frame (define your own
#      criterion for 'quality' of chicken)
#    - Use tab symbol as field separator
# 2. Print some information about dataset (either to file or to standard output)
#    - Find, which diet is the best
#    - Find, which diet is good for 'short-term' (gives best result on day 10)
#
# sidenote: rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf


suppressPackageStartupMessages(library("dplyr"))


df <- read.table("chickweight.txt", header = TRUE, sep = ",")


# 1. Let's say MassIncreaseSpeed on Day X is the difference of a chick weight on Day X and 
#    on the previous measuring (on Day Y), divided on (X - Y) (if X == 0, then MassIncreaseSpeed := weight on Day 0)
#
#    Quality of a chick is its weight on the last day of containment (21th, the chickweight.txt case)

the_last_day <- df %>% pull(Time) %>% max()
mass_increase_speed <- mapply(function(time, prev_time, weight, prev_weight) { 
        if (time == 0) {
            weight
        } else {
            (weight - prev_weight) / (time - prev_time)
        } 
    }, df$Time, lag(df$Time), df$weight, lag(df$weight))
df <- df %>% mutate(MassIncreaseSpeed = mass_increase_speed)

top_15_chicks <- df %>% 
        filter(Time == the_last_day) %>%
        top_n(15, weight) %>%
        pull(Chick)
top_15 <- df %>% filter(Chick %in% top_15_chicks)

write.table(top_15, file="top15.txt", sep = "\t") # should we drop original indexes? (the table looks better this way)


# 2. Let's say (Diet_A is better than Diet_B on day X) <=> (mean weight of chickens of Diet_A > Diet_B on day X) 
# The best diet is the "bettriest" diet on the last day.

diets_count <- df %>% pull(Diet) %>% n_distinct() 
get_the_best_diet_for_day <- function(day_n) {
    c(1:diets_count) %>%
        lapply(function(x) {
            df %>% 
                filter((Time == day_n) & (Diet == x)) %>% 
                pull(MassIncreaseSpeed) %>% 
                mean()
        }) %>% 
        which.max()
}

print(sprintf("The best Diet is %d'th.", get_the_best_diet_for_day(the_last_day)))
print(sprintf("The best Diet for getting short-term results (day 10) is %d'th.", get_the_best_diet_for_day(10)))