phone_it_in <- function(phone, invalid = NA)
{
  phone <- gsub("[[:punct:]]", "", phone)          # remove punctuation
  phone <- trimws(phone)                           # remove whitespace
  phone[!nchar(phone) %in% c(7, 10)] <- invalid    # keep only 7 or 10 digit numbers
  phone[nchar(phone) == 7] <- gsub("(^\\d{3})(\\d{4}$)", 
                                   "\\1-\\2", 
                                   phone[nchar(phone) == 7])
  phone[nchar(phone) == 10] <- gsub("(^\\d{3})(\\d{3})(\\d{4}$)", 
                                    "\\1-\\2-\\3",
                                    phone[nchar(phone) == 10])
  phone
}

phone <- c("(123)-456-7890", "1234567890", "456890", "456-7890")
phone_it_in(phone)


# Pho呢 --------------------------------------------------------------------

# Phone numbers (I've added an additional number with the "/" character)
strings <- c("87225324","65-62983211","65-6298-3211","8722 5324",
             "(65) 6296-2995","(65) 6660 8060","(65) 64368308","+65 9022 7744",
             "+65 6296-2995","+65-6427 8436","+65 6357 3323/322", "+65 4382 6922/6921")

# Remove all non-numeric characters except "/" (your string doesn't include any
# text like "Work:" or "Home:", but I included a regex to deal with those cases
# as well)
strings.cleaned = gsub("[- .)(+]|[a-zA-Z]*:?","", strings)

# If you're sure there are no other non-numeric characters you need to deal with 
# separately, then you can also do the following instead of the code above: 
# gsub("[^0-9/]","", strings). This regex matches any character that's not 
# a digit or "/".

strings.cleaned


# Separate string vector into the cleaned strings and the two "special cases" that we 
# need to deal with separately
special.cases = strings.cleaned[grep("/", strings.cleaned)]
strings.cleaned = strings.cleaned[-grep("/", strings.cleaned)]

# Split each phone number with a "/" into two phone numbers
special.cases = unlist(lapply(strsplit(special.cases, "/"), 
                              function(x) {
                                c(x[1], 
                                  paste0(substr(x[1], 1, nchar(x[1]) - nchar(x[2])), x[2]))
                              }))
special.cases


# Put the special.cases back with strings.cleaned
strings.cleaned = c(strings.cleaned, special.cases)

# Select last 8 digits from each phone number
phone.nums = as.numeric(substr(strings.cleaned, nchar(strings.cleaned) - 7, 
                               nchar(strings.cleaned)))
phone.nums
