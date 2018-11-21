## REAL WORLD DATA
# column 1: rolls         - numbers rolled from 1 to 6
# column 2: magic_before  - has the die been enchanted in the previous roll?
# die was enchanted after rolling either a 1 or 2

dd <- read.csv("enchanted_die.csv", sep = ";")

## COMMENT FOR COMMIT
# Added argument die_outcomes to customise the possible outcomes from which die rolls are sampled
# now allows for n-sided dies or for outcomes that are not uniformly distributed
# included error message if not all numbers after which die is to be enchanted are included in
# possible outcomes of die

#####
## ALTERNATIVE: use randomly generated die rolls

# FUNCTION FOR ENCHANTED DIE ROLLS
# result: data.frame with two columns
# column 1: rolls         - die roll outcomes randomly sampled from numbers specified in die_outcomes 
#                           (default: numbers from 1 to 6)
# column 2: magic_before  - has the die been enchanted in the previous roll?
# die is enchanted after rolling the number(s) specified in magic_numbers

# output silent in order to prevent huge data.frame from being printed

# arguments:
# nrolls        = number of rolls
# magic_numbers = specify die outcomes after which you want to enchant your die; can be either
#                 a single number or several ones; default = c(1, 2) as in the real world
#                 example
# die_outcomes  = specify numbers from which die outcomes are sampled; uniform when each outcome
#                 is included only once; default: classical die (1:6)

enchant.die <- function(nrolls, magic_numbers = c(1, 2), die_outcomes = 1:6) {
  # error status: FALSE at the beginning
  error <- FALSE
  
  # if at least one number in magic_numbers is not included in die_outcomes, print warning
  # (if not each element of magic_numbers %in% die_outcomes is TRUE)
  if (sum(!magic_numbers %in% die_outcomes) > 0) {
    print("Woah there! You want to enchant at least one die outcome that your die cannot possibly roll! Per default, the die rolls 1:6. Specify any other outcomes via the argument die_outcomes.")
    error <- TRUE
  }
  
  # pre-define data.frame with as many rolls as specified in nrolls
  # column rolls:         randomly generate die rolls; as many as specified in nrolls;
  #                       sample from numbers as specified in die_outcomes
  # column magic_before:  pre-define; to be filled with enchantment information;
  #                       assign 2 as default value in order to keep track of possible errors 
  #                       (correct final column should only contain 1 and 0)
  dd <- data.frame(rolls = sample(die_outcomes, nrolls, replace = TRUE), 
                   magic_before = rep(2, nrolls))
  
  # first throw: definitely no enchantment in previous roll as there is no previous one (assign 0)
  dd$magic_before[1] <- 0
  
  # 2nd roll onwards: if magic number rolled in previous roll, assign 1
  # nrolls - 1 as there is no further roll after the last one
  for (i in 1:(nrolls - 1)) {
    ifelse(dd$rolls[i] %in% magic_numbers == TRUE, 
           dd$magic_before[i + 1] <- 1, dd$magic_before[i + 1] <- 0)
  }
  
  # print a magical message when finished (and no error has occured)!
  magic_message <- c("Mischief managed.", "Magical!", "Alea iacta est.", "You're a wizard!",
                     "Magic is in the air.", "Did you just feel that?", "Enchanting!",
                     "Thank you for choosing Enchanted Dies!", "I'm slightly bewichted.",
                     "With great power comes great responsibilty.", "That's a lot of die rolls",
                     "6 is my lucky number!", "Charming!", "I'm under your spell!")
  
  if (error == FALSE) {
  print(sample(magic_message, 1))
  }
  
  invisible(dd)
}

## ALTERNATIVE: use randomly generated die rolls from enchant.die function
# default parameters match conditions of real world die roll data
dd <- enchant.die(200)
