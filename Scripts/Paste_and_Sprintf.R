# Shows some uses of paste and sprintf
# Last updated on 2019.08.27 @ 08:00pm
# Update: minor reformatting, spaces, etc. 

# Two common ways of creating strings from variables are...
# the paste function, which is somewhat more useful for vectors
# the sprintf function, which allows flexible output formatting

# The examples are partly derived from the R help page for "sprintf" and the
# website http://www.cookbook-r.com/Strings/Creating_strings_from_variables/

#--------------------------------------------------

# This section discusses paste and paste0 functions
# Note: paste0 is "pastezero", not capital letter O

a <- "kiwi"
b <- "strawberry"

# Put a and b together, with a space in between

paste(a, b)

# Put a and b together, with no space in between

paste(a, b, sep = "")
paste0(a, b)

# Put a and b together, with comma and space between

paste(a, b, sep = ", ")

# As a sentence...

paste("I have the following fruit: a ", a, " and a ", b, ".", sep = "")

# With a vector

d <- c("pineapple", "starfruit", "papaya")

# Use collapse to put vector elements together

paste(d, collapse = " ")   # with spaces between
paste(d, collapse = ", ")  # with commas between

# For a single element and a vector, the single
# element is paired up with each vector element

paste(a, d)

# Additional combination formatting options...

paste(a, d, sep = "-", collapse = ", ")

#--------------------------------------------------

# This section discusses the sprintf function.

# Use %s for strings

a1 <- "string1"
a2 <- "string2"
sprintf("This is where %s goes.", a1)
sprintf("Print multiple strings, like %s and %s.", a1, a2)

# You can use %d for integers
# Or see floating point below
# Can print leading spaces
# Can also lead with zeros

x1 <- 9
x2 <- 11
sprintf("Print the integer %d.", x1)
sprintf("Three leading spaces and one integer:%4d, four total.", x1)
sprintf("Two leading spaces and two integers:%4d, four total.", x2)
sprintf("Three leading zeroes instead of spaces: %04d.", x1)

# For floating-point numbers...
# use %f for standard notation
# use %e for exponential notation 
# use %g for a smart" format that depends on significant digits
# Note that sprintf does not round, it just changes the display

sprintf("%f", pi)         # "3.141593"
sprintf("%.3f", pi)       # "3.142"
sprintf("%1.0f", pi)      # "3"
sprintf("%5.1f", pi)      # "  3.1"
sprintf("%05.1f", pi)     # "003.1"
sprintf("%+f", pi)        # "+3.141593"
sprintf("% f", pi)        # " 3.141593"
sprintf("%-10f", pi)      # "3.141593  "   (left justified)
sprintf("%e", pi)         # "3.141593e+00"
sprintf("%g", pi)         # "3.14159"
sprintf("%g",   1e6 * pi) # "3.14159e+06"  (exponential)
sprintf("%.9g", 1e6 * pi) # "3141592.65"   ("fixed")
sprintf("%g", 1e-6 * pi)  # "3.14159E-06"
