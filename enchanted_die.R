## REAL WORLD DATA
# load real world die data
# column 1: rolls         - die rolls from 1 to 6
# column 2: magic_before  - has the die been enchanted in the previous roll?
# die was enchanted after rolling either 1 or 2
dd <- read.csv("enchanted_die.csv", sep = ";")

## ALTERNATIVE: use randomly generated die rolls from enchant.die function

## FUNCTION FOR ENCHANTED DIE ROLLS
# result: data.frame with two columns
# column 1: rolls         - random numbers from 1 to 6
# column 2: magic_before  - has the die been enchanted in the previous roll?
# die is enchanted after rolling the number(s) specified in magic_numbers

# output silent in order to prevent huge data.frame from being printed

# arguments:
# nrolls = number of rolls
# magic_numbers = specify die outcomes after which you want to enchant your die; can be either
#                 a single number or several ones

enchant.die <- function(nrolls, magic_numbers = c(1,2)) {
  
  # randomly generate die rolls; as many as specified in nrolls
  rolls <- sample(1:6, nrolls, replace = TRUE)
  
  # pre-define 2nd column to be filled with enchantment information
  # assign 2 as default value in order to keep track of possible errors (correct column should
  # only contain 1 and 0)
  magic_before <- rep(2, nrolls)
  
  # first throw: definitely no enchantment in previous roll as there is no previous one (assign 0)
  magic_before[1] <- 0
  
  # 2nd roll onwards: if magic number rolled in previous roll, assign 1
  # nrolls - 1 as there is no further roll after the last one
  for (i in 1:(nrolls - 1)) {
    ifelse(rolls[i] %in% magic_numbers == TRUE, magic_before[i + 1] <- 1, magic_before[i + 1] <- 0)
  }
  
  ## ALTERNATIVE: use randomly generated die rolls from enchanted.die function
  # combine rolls and magic before into data.frame dd
  dd <- cbind.data.frame(rolls, magic_before)
  
  # print a magical message when finished!
  magic_message <- c("Mischief managed.", "Magical!", "Alea iacta est.", "You're a wizard!",
                     "Magic is in the air.", "Did you just feel that?", "Enchanting!",
                     "Thank you for choosing Enchanted Dies!", "I'm slightly bewichted.",
                     "With great power comes great responsibilty.", "That's a lot of die rolls",
                     "6 is my lucky number!", "Charming!", "I'm under your spell!")
  print(sample(magic_message, 1))
  
  # don't flood console with thousands of die rolls
  invisible(dd)
}

## ALTERNATIVE: use randomly generated die rolls from enchant.die function
dd <- enchant.die(200, c(1,2))
